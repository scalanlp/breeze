/*
 * Distributed as part of ScalaRA, a scientific research tool.
 * 
 * Copyright (C) 2007 Daniel Ramage
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.

 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA 
 */
package scalara.distributed;

import scala.concurrent.ops.spawn;
  
import java.net.{ServerSocket,Socket,URI};
import java.io.{ObjectInputStream,ObjectOutputStream};
import java.io.{IOException,NotSerializableException};

/**
 * Messages that services respond to.
 * 
 * @author dramage
 */
object ServiceMessages {
  abstract class ServiceMessage;
  
  /** Request for alive state. */
  case object Ping extends ServiceMessage;
  
  /** Reply for alive state. */
  case object Pong extends ServiceMessage;
  
  /** Request to exit. */
  case object Stop extends ServiceMessage;
  
  /** Reply for exit. */
  case object Bye extends ServiceMessage;
  
  /** An error. */
  case class ExceptionWrapper(ex : Throwable) extends ServiceMessage;
  
  /** A user defined message. */
  case class Message(message : Any) extends ServiceMessage;
  
  /** No reply provided. */
  case object NoReply extends ServiceMessage;
}

/**
 * A Service-specific exception.
 * 
 * @author dramage.
 */
class ServiceException(msg : String) extends RuntimeException(msg);

/**
 * A remote service threw the wrapped exception.
 * 
 * @author dramage.
 */
class RemoteServiceException(ex : Throwable) extends RuntimeException(ex);


/**
 * Shared utilities for these classes.
 * 
 * @author dramage
 */
private[distributed] object ServiceUtil {
  import ServiceMessages._;
  
  /** Quietly do the given function discarding any exceptions. */
  def justDoIt(function : () => Unit) {
    try { function(); } catch { case _ => (); }
  }
  
  def daemon(p: => Unit) = {
    val t = new Thread() { override def run() = p }
    t.setDaemon(true);
    t.start();
  }
  
  /** Writes a message */
  def write(rid : Int, message : ServiceMessage, out : ObjectOutputStream) {
    out.reset;
    out.writeInt(rid);
    out.writeUnshared(message);
    out.flush;
  }
      
  /**
   * Evaluate the function, closing the session on
   * IOException and returning the given default.
   */
  def closeOnEOF[E](f : () => E, default : E, close : () => Unit) : E = {
    try {
      f()
    } catch {
      case eof : java.io.EOFException => {
        close();
        default;
      }
    }
  }

}

/**
 * A connection to a remote SocketService.  Supports actor-like
 * methods ! and !? as well as remote stop.
 * 
 * URI should be:
 * 
 *   socket://machine:port/name
 * 
 * @author dramage
 */
class SocketClient(uri : URI) {
  import ServiceMessages._;
  import ServiceUtil._;
  
  import scala.concurrent.Channel;
  import scala.collection.mutable.{HashMap,SynchronizedMap,HashSet,SynchronizedSet};
  
  /** Private logger method */
  private def log(msg : String) = SocketService.log("SocketClient: "+msg);
  
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
  private val channels = new HashMap[Int,Option[Channel[ServiceMessage]]] with SynchronizedMap[Int,Option[Channel[ServiceMessage]]];
  
  /** Close the session */
  def close() {
    if (sessionActive) {
      sessionActive = false;
      log("closing");
    }
  }
  
  /** The client's request id.  Send increments this, so first will be 0. */
  private var rid = -1;

  /** Sends a message to the client, with reply going to the given channel. */
  private def send(message : ServiceMessage, consumer : Option[Channel[ServiceMessage]]) : Unit = synchronized {
    if (!sessionActive) {
      throw new ServiceException("Cannot send message on inactive session");
    }
    
    // next request id
    rid = if (rid == Int.MaxValue) 0 else rid + 1;
    
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
        log("Error while writing: "+ex);
        close;
        throw(ex);
      }
    }

    log("sent "+rid);
    return rid;
  }
  
  /** Sends a message to the service and awaits a reply. */
  def query(msg : ServiceMessage) : ServiceMessage = {
    val channel = new Channel[ServiceMessage];
    val id = send(msg,Some(channel));
    
    val received : ServiceMessage = channel.read;
    
    received match {
      case ExceptionWrapper(ex) => throw new RemoteServiceException(ex);
      case x : ServiceMessage => x;
    }
  }
  
  /** Sends a message without awaiting reply. */
  def ! (msg : Any) : Unit = {
    send(Message(msg), None);
  }
  
  /** Sends a message, blocking and returning its reply. */
  def !? (msg : Any) : Any = {
    query(Message(msg)) match {
      case Message(reply) => reply;
      case x : Any => {
        throw new ServiceException("Unexpected reply: "+x);
      }
    }
  }
  
  /**
   * Pings the given connection - returns true if and when the service
   * returns a pong.
   */
  private[distributed] def ping : Boolean = {
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
      val rid = closeOnEOF(in.readInt, -1, close);
      val control = closeOnEOF(in.readUnshared, None, close);
      
      if (rid >= 0) {
        log("received "+rid);
      
        if (!channels.contains(rid)) {
          throw new ServiceException("SocketService error: missing channel for "+rid);
        }
        
        channels(rid) match {
          case Some(channel) => {
            // some channel wants to consume the reply
            control match {
              case msg : ServiceMessage => {
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
              case ExceptionWrapper(ex) => {
                throw new RemoteServiceException(ex);
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
    
    justDoIt { in.close; }
    justDoIt { out.close; }
    justDoIt { socket.close; }
  }
}

/**
 * Extra constructors for SocketClient.
 * 
 * TODO: add name check
 * 
 * @author dramage
 */
object SocketClient {
  def apply(uri : URI) =
    new SocketClient(uri);
  
  def apply(host : String, port : Int) =
    new SocketClient(new java.net.URI("socket",null,host,port,null,null,null));
}

/**
 * A service that listens on a port.  Supports actor-like
 * methods via a SocketClient.
 * 
 * @author dramage
 */
abstract class SocketService(val path : String, val port : Int) extends Runnable {
  import ServiceMessages._;
  import ServiceUtil._;
  
  /** Private logger method */
  private def info(msg : String) = SocketService.log("SocketService: "+msg);
  
  /** The server socket we accept connections from. */
  private val listener = new ServerSocket(port);
  
  /** Active state of this service. */
  // private var serviceActive = true;
  
  /** Incoming message queue. */
  private val incoming = java.util.Collections.synchronizedList(
    new java.util.LinkedList[(Int,Any,java.util.List[(Int,ServiceMessage)])]);
  
  /** Abstract method for getting reaction. */
  def react : PartialFunction[Any,Any];
  
  /** Hostname of this service */
  def host =
    HubUtils.hostName;
  
  /** Returns a URI describing how to connect to this service. */
  val uri = try {
    new java.net.URI("socket",null,host,port,path,null,null)
  } catch {
    case ue : java.net.URISyntaxException =>
      throw new IllegalArgumentException("Invalid path name "+path+": should start with /");
  }
  
  /** Registers this service in the given hub. */
  def register(hub : HubClient) =
    hub.register(uri);
  
  /** Tells the connection daemon to stop. */
  def stop() = {
    listener.close();
  }
  
  /** Runs this service in a new thread. */
  def runAsThread : Thread = {
    val thread = new Thread(this);
    thread.start;
    thread;
  }
  
  /** Runs this service in a new daemon thread (does not prevent JVM shutdown). */
  def runAsDaemon : Thread = {
    val thread = new Thread(this);
    thread.setDaemon(true);
    thread.start;
    thread;
  }
  
  /**
   * Process and return incoming requests within the current thread.
   * Spawns additional daemon threads to manage socket i/o per session.
   */
  override def run() {
    // timeout is 200ms -- how often we check to see if still active.
    // listener.setSoTimeout(200);
    
    // Primary daemon: establishes new connections
    daemon {
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
          // a new daemon thread to handle this client
          var sessionActive = true;
        
          // important: always create out before in.
          val out = new ObjectOutputStream(socket.getOutputStream);
          val in = new ObjectInputStream(socket.getInputStream);
        
          val outgoing = java.util.Collections.synchronizedList(new java.util.LinkedList[(Int,ServiceMessage)]);
          
          // reply puts things into the outgoing mailbox
          def reply(rid : Int, message : ServiceMessage) =
            outgoing.add((rid,message));
          
        
          def closeSession() {
            if (!sessionActive) {
              info("Session "+out+" closing");
              sessionActive = false;
            }
          }
      
          // 2nd deamon: per-connection outgoing message writer
          daemon {
            while (sessionActive || !outgoing.isEmpty) {
              Thread.sleep(20l);
            
              while (!outgoing.isEmpty) {
                val (rid,message) = outgoing.remove(0);
                info("Session "+out+" writing "+rid);
                try {
                  write(rid, message, out);
                } catch {
                  case ex : Exception => {
                    closeSession();
                    throw(ex);
                  }
                }
              }
            }
            
            justDoIt { in.close; }
            justDoIt { out.close; }
            justDoIt { socket.close; }
          }
        
          // 3rd deamon: per-connection incoming message reader
          daemon {
            while (sessionActive) {
              Thread.sleep(20l);
            
              val rid = closeOnEOF(in.readInt,-1,closeSession);
              val control =
                try {
                  closeOnEOF(in.readUnshared, None, closeSession);
                } catch {
                  case ex : Throwable => {
                    // non-eof error: return and throw it
                    reply(rid, ExceptionWrapper(ex));
                    closeSession();
                    throw(ex);
                  }
                };
              
              control match {
                case Ping => {
                  info("pinged");
                  reply(rid, Pong);
                }
          
                case Stop => {
                  info("stopping");
                  reply(rid, Bye);
                  closeSession();
                  stop();
                }
          
                case Message(message) => {
                  info("enqueueing message "+rid+" @ " +out);
                  incoming.add((rid,message,outgoing));
                }
          
                case None => {
                  // error, safe to ignore
                }
          
                case x : Any => {
                  throw new ServiceException("Unknown message "+x);
                }
              }
            }
          }
        }
      }
    }
  
    // do actual processing in only caller's thread
    while (!listener.isClosed) {
      Thread.sleep(20l);
      
      if (!incoming.isEmpty) {
        val (rid,message,outgoing) = incoming.remove(0);
      
        try {
          react(message) match {
            case reply : ServiceMessage => outgoing.add((rid, reply));
            case _ => outgoing.add((rid, NoReply));
          }
        } catch {
          case ex : Throwable => {
            outgoing.add((rid, ExceptionWrapper(ex)));
          }
        }
      }
    }
  }
  
  override def toString = uri.toString;
}

object SocketService {
  import ServiceMessages._;
  import ServiceUtil._;

  /** How verbose are the service packages. */
  var verbose = 0;
  
  private[distributed] def log(msg : String) = {
    if (verbose > 0) { System.err.println(msg); }
  }
  
  /**
   * Spawns a service on the given port with the given body
   * to do actual request processing.  Needs to be started.
   */
  def apply(name : String)(body : PartialFunction[Any,Any]) = {
    new SocketService(name, HubUtils.freePort) {
      override def react = body;
    };
  }
  
  def reply(x : Any) = Message(x);
  
  /**
   * Return a future that ping a remote host.  The
   * return value may take up to  three seconds before
   * it times out to false, or may return right away
   * with true.
   */
  def ping(uri : URI) : (() => Boolean) = {
    var connected : Boolean = false;
    var done : Boolean = false;
    
    val connecting = new Thread() {
      override def run() = {
        connected = try { SocketClient(uri).ping; } catch { case _ => false; }
        done = true;
      }
    }
    
    return scala.concurrent.ops.future {
      val start = System.currentTimeMillis();
      connecting.start();
    
      while (!done && System.currentTimeMillis() - start < 3000l) {
        Thread.sleep(20l);
      }
    
      if (!done) {
        connecting.stop();
      }
    
      connected;
    }
  }
}

object SampleService {
  import SocketService._;
  
  /** A test main method */
  def main(argv : Array[String]) {
    val server = SocketService("test") {
      case x : String =>
        reply { x.toUpperCase }
          
      case y : Int =>
        throw new RuntimeException("I don't like Ints!")
          
      case z : Double => {
        Thread.sleep(200l);
        reply { z * 2; }
      }
          
      case _ => None;
    };
    
    spawn { server.run; }
    
    Thread.sleep(1000l);
    
    val client = SocketClient(server.host,server.port);
    
    // should print 4.0 last
    spawn { System.err.println(client !? 2.0); }
    
    // should return true
    System.err.println(client.ping);
    
    // should return "HI"
    System.err.println(client !? "hi");
    
    // should get exception thrown remotely
    try {
      client !? 3
    } catch {
      case x : RuntimeException => System.err.println(x.getMessage);
    }
    
    client.stop;
  }
}
