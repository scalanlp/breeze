/*
 Copyright 2009 David Hall, Daniel Ramage

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
package scalanlp.distributed;

import scala.collection.mutable.HashMap;
import scala.concurrent.Channel;

import java.io.{ObjectInputStream,ObjectOutputStream};
import java.net.{URI,ServerSocket};

/**
 * Dispates incoming connections to on a single socket to SocketService
 * instances by name.  Use static constructor methods in the companion
 * object to get instances of this class.
 *
 * @author dramage
 */
trait SocketDispatch {
  /** Returns the port on which the dispatch is listening. */
  def port : Int;

  /** Stops the dispatcher, shuttering the service. */
  def stop();

  /** Registers the given service at the given name. */
  protected[distributed] def register(name : String, service : SocketService);
}

object SocketDispatch {
  /** The primary dispatch for the current process. */
  protected lazy val global : SocketDispatch = apply(SocketUtils.freePort);

  /** Registry of SocketDispatch instances by port. */
  protected val registry = HashMap[Int,SocketDispatch]();

  /** Verbosity level.  Control with -DSocketDispatch.VERBOSE=true */
  var VERBOSE =
    System.getProperty("SocketDispatch.VERBOSE") == "true";

  /** Returns the global default dispatch for this process. */
  def apply() : SocketDispatch = global;

  /**
   * Returns a dispatch for the given port, creating it if necessary.
   */
  def apply(port : Int) : SocketDispatch =
    registry.getOrElseUpdate(port, { val rv = new Impl(port); rv.runAsDaemon; rv; } );

  protected[distributed] object Messages {
    sealed trait Incoming;
    case object IncomingShutdown extends Incoming;
    case class IncomingMessage(rid : Int, message : Any, reply : Channel[Outgoing]) extends Incoming;

    sealed trait Outgoing;
    case object OutgoingShutdown extends Outgoing;
    case class OutgoingMessage(rid : Int, message : ServiceMessages.ServiceMessage) extends Outgoing;
  }

  class Impl(val port : Int) extends SocketDispatch with Threadable {
    import SocketDispatch.Messages._;
    import ServiceMessages._;
    import ServiceUtil._;

    val uri = URI.create("socket://"+SocketUtils.hostName+":"+port);

    /** This services registered on this dispatcher. */
    val services = new HashMap[String, SocketService];

    /** Private logger method */
    protected def info(msg : String) =
      if (VERBOSE) System.err.println("[SocketDispatch] "+msg);

    /** The server socket we accept connections from. */
    protected val listener = new ServerSocket(port);

    // timeout is 200ms -- how often we check to see if still active.
    // listener.setSoTimeout(200);

    def stop() = {
      listener.close();
    }

    def register(name : String, service : SocketService) {
      if (services.contains(name)) {
        throw new ServiceException("Unable to register service: "+name+" already in use");
      }
      services(name) = service;
    }

    /** Establishes new connections, forwarding to SocketService instances. */
    override def run() {
      while (!listener.isClosed) {
        val socket =
          try {
            listener.accept();
          } catch {
            case se : java.net.SocketException => null;
          }

        // if socket is null, we've closed and will back out of loop
        // gracefully on the next pass.  if it's non-null, launch
        // connection deamons for it.

        if (socket != null) {
          // important: always create out before in.
          val out = new ObjectOutputStream(socket.getOutputStream);
          val in = new ObjectInputStream(socket.getInputStream);

          val service = try {
            // consume request for service by name
            val rid = in.readInt;
            val path = in.readUnshared.asInstanceOf[ServiceMessages.Connect].path;

            if (services.contains(path)) {
              out.writeInt(rid);
              out.writeUnshared(ServiceMessages.ConnectionMade(path));
            } else {
              out.writeInt(rid);
              out.writeUnshared(ServiceMessages.ConnectionFailed(path));
            }

            services(path);
          } catch {
            case _ =>
              info("Invalid request from "+socket.getInetAddress);
              null;
          }

          // if service is null, we don't have a valid incoming connection

          if (service != null) {
            // the channel that processes incoming requests
            val incoming = service.incoming;

            // the return channel passed to the relevant SocketService
            val outgoing = new Channel[SocketDispatch.Messages.Outgoing];

            // 1st deamon: incoming message dispatch for this connection
            daemon {
              var active = true;

              while(active) {
                // read request id
                val rid : Int = try {
                  in.readInt;
                } catch {
                  case ex : Throwable =>
                    // in particular, this is often java.io.EOFException
                    active = false;
                    try { in.close     } catch { case _ => (); }
                    try { out.close    } catch { case _ => (); }
                    try { socket.close } catch { case _ => (); }
                    outgoing.write(OutgoingShutdown);
                    -1;
                }

                if (active) {
                  // read control message
                  val control = try {
                    in.readUnshared
                  } catch {
                    case ex : Throwable =>
                      outgoing.write(OutgoingMessage(rid, ExceptionWrapper(uri, ex)));
                      outgoing.write(OutgoingShutdown);
                      throw(ex);
                  }

                  control match {
                    case Ping => {
                      info(""+Ping);
                      outgoing.write(OutgoingMessage(rid, Pong));
                    }

                    case Stop => {
                      info(""+Stop);
                      outgoing.write(OutgoingMessage(rid, Bye));
                      incoming.write(IncomingShutdown);
                    }

                    case Message(message) => {
                      info(""+Message+" "+rid+" @ " +out);
                      incoming.write(IncomingMessage(rid,message,outgoing));
                    }

                    case x : Any => {
                      throw new ServiceException("Unknown message "+x);
                    }
                  }
                }
              }
            }

            // 2nd deamon: outgoing message writer for this connection
            daemon {
              var active = true;

              while(active) {
                outgoing.read match {
                  case OutgoingMessage(rid, message) =>
                    info("Session "+out+" writing "+rid);
                    try {
                      write(rid, message, out);
                    } catch {
                      case ex : Exception => {
                        active = false;
                        try { in.close; }     catch { case _ => (); }
                        try { out.close; }    catch { case _ => (); }
                        try { socket.close; } catch { case _ => (); }
                        throw(ex);
                      }
                    }

                  case OutgoingShutdown =>
                    active = false;
                    try { in.close; }     catch { case _ => (); }
                    try { out.close; }    catch { case _ => (); }
                    try { socket.close; } catch { case _ => (); }
                }
              }
            }
          }
        }
      }
    }
  }
}
