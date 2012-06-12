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
package breeze.distributed;

import scala.collection.mutable.{HashMap,SynchronizedMap};
import scala.concurrent.Channel;

import java.net.{Socket,URI};
import java.io.{ObjectInputStream,ObjectOutputStream,NotSerializableException};


/**
 * A connection to a remote SocketService.  Supports actor-like
 * methods ! and !? as well as remote stop.
 *
 * URI should be of the form <pre>socket://machine:port/name</pre>
 *
 * TODO: check that daemon threads shut down correctly to avoid corrupting state of hub
 *
 * @author dramage
 */
trait SocketClient {
  /** URI of target connection. */
  val uri : URI;
  
  /** Sends a message without awaiting reply. */
  def ! (msg : Any) : Unit;

  /** Sends a message, blocking and returning its reply. */
  def !? (msg : Any) : Any;
  
  /** Closes the connection to the remote service. */
  def close();
  
  /** Stops the remote service and closes the connection, returing true on success. */
  def stop() : Boolean;
}

/**
 * Static constructors for SocketClient.
 *
 * @author dramage
 */
object SocketClient {

  def apply(uri : URI) : SocketClient =
    new Impl(uri);

  def apply(uri : String) : SocketClient =
    new Impl(URI.create(uri));

  /** Verbosity level.  Control with -DSocketClient.VERBOSE=true */
  var VERBOSE =
    System.getProperty("SocketClient.VERBOSE") == "true";

  def ping(uri : URI, timeout : Long = 3000l) : Boolean = {
    try {
      val client = new Impl(uri);
      client.ping(timeout);
    } catch {
      case _ => false;
    }
  }


  class Impl(val uri : URI) extends SocketClient {
    import ServiceMessages._;
    import ServiceUtil._;

    /** Private logger method */
    private def info(msg : String) =
      if (SocketClient.VERBOSE) System.err.println("[SocketClient] "+msg);

    if (uri.getScheme != "socket") {
      throw new IllegalArgumentException("Wrong scheme for URI: "+uri);
    }

    /** connection to the given host - could rightly throw an uncaught exception */
    private val socket = new Socket(uri.getHost, uri.getPort);
    socket.setSoTimeout(0);

    /** streams - NB always create out before in! */
    private val out = new ObjectOutputStream(socket.getOutputStream);
    private val in = new ObjectInputStream(socket.getInputStream);

    /** If session is active */
    private var sessionActive = true;

    /** Received message replies. */
    private val channels = new HashMap[Int,Option[Channel[ServiceReply]]] with
      SynchronizedMap[Int,Option[Channel[ServiceReply]]];

    /** Close the session */
    def close() {
      if (sessionActive) {
        sessionActive = false;
        info("closing");
      }
    }

    /** The client's request id.  Send increments this, so first will be 0. */
    private var rid = -1;

    /** Sends a message to the client, with reply going to the given channel. */
    protected def send(message : ServiceRequest, consumer : Option[Channel[ServiceReply]]) = synchronized {
      if (!sessionActive) {
        throw new ServiceException("Cannot send message on inactive session");
      }

      // next request id
      rid = if (rid == Int.MaxValue) 0 else rid + 1;

      info("sending message "+rid+" "+message.toString);

      channels(rid) = consumer;

      try {
        write(rid, message, out);
      } catch {
        case ex : java.io.NotSerializableException => {
          channels -= rid;
          // this is a bad error, throw it now
          throw(ex);
        }

        case ex : java.net.SocketException => {
          channels -= rid;
          // looks like the service died
          close;
          throw(ex);
        }

        case ex : Throwable => {
          // this is some other error, which might not be critical?
          channels -= rid;
          info("Error while writing: "+ex);
          close;
          throw(ex);
        }
      }

      rid;
    }

    /** Sends a message to the service and awaits a reply. */
    protected def query(msg : ServiceRequest) : ServiceReply = {
      val channel = new Channel[ServiceReply];
      val id = send(msg,Some(channel));

      val received : ServiceMessage = channel.read;

      received match {
        case ExceptionWrapper(host, ex) => throw new RemoteServiceException(host, ex);
        case x : ServiceReply => x;
        case o : Any => throw new ServiceException("Unexpected message: "+o);
      }
    }

    /** Sends a message without awaiting reply. */
    def ! (msg : Any) : Unit = {
      send(Message(msg), None);
    }

    /** Sends a message, blocking and returning its reply (Unit if nothing). */
    def !? (msg : Any) : Any = {
      query(Message(msg)) match {
        case Reply(reply) => reply;
        case NoReply => Unit;
        case o : Any => throw new ServiceException("Unexpected message: "+o);
      }
    }

    /**
     * Pings the given connection - returns true if and when the service
     * returns a pong unless timeout (ms) happens first.
     */
    def ping(timeout : Long) : Boolean = {
      val active = Thread.currentThread;
      scala.concurrent.ops.spawn {
        Thread.sleep(timeout);
        active.interrupt();
      }
      try {
        query(Ping) == Pong;
      } catch {
        case _ => false;
      }
    }

    /**
     * Stops the remote service by sending a Stop message.
     */
    def stop : Boolean = {
      val rv = query(Stop) == Bye;
      close();
      return rv;
    }

    /** Launch the daemon thread to watch the connection. */
    daemon {
      while (sessionActive) {
        val rid     = try { in.readInt }      catch { case eof : java.io.EOFException => close(); -1; }
        val control = try { in.readUnshared } catch { case eof : java.io.EOFException => close(); None; }

        if (rid >= 0) {
          info("received "+rid);

          if (!channels.contains(rid)) {
            throw new ServiceException("SocketService error: missing channel for "+rid);
          }

          channels(rid) match {
            case Some(channel) => {
              // some channel wants to consume the reply.  this will throw an
              // exception at the call point if the return is an ExceptionWrapper.
              control match {
                case msg : ServiceReply => {
                  channel.write(msg);
                }

                case x : Any => {
                  throw new ServiceException("SocketService error: unexpected message type "+x);
                }
              }
            }

            case None => {
              // this is a message whose reply we ignore.  still check for and throw
              // exceptions, however.
              control match {
                case ExceptionWrapper(host,ex) => {
                  throw new RemoteServiceException(host,ex);
                }

                case msg : ServiceMessage => {
                  // ignore regular reply
                }

                case x : Any => {
                  throw new ServiceException("SocketService error: unexpected message type "+x);
                }
              }
            }
          }

          channels -= rid;
        }
      }

      try { in.close; }     catch { case _ => (); }
      try { out.close; }    catch { case _ => (); }
      try { socket.close; } catch { case _ => (); }
    }

    // Ensure connection to correct path
    query(Connect(uri.getPath)) match {
      case ConnectionMade(path) =>
        (); // good!

      case ConnectionFailed(path) => {
        close();
        throw new ServiceException("Could not connect to "+uri);
      }

      case _ => {
        close();
        throw new ServiceException("Unexpected error while connecting to "+uri);
      }
    }
  }
}
