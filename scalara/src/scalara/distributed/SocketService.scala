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

import scala.concurrent.Channel;
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
  @serializable sealed trait ServiceMessage;
  
  /** A request made of a service. */
  @serializable sealed trait ServiceRequest extends ServiceMessage;
  
  /** Requests a connection to the given path. */
  case class Connect(path : String) extends ServiceRequest;
  
  /** Request for alive state. */
  case object Ping extends ServiceRequest;
  
  /** Request to exit. */
  case object Stop extends ServiceRequest;
  
  /** A user defined message. */
  case class Message(message : Any) extends ServiceRequest;
  
  
  /** A reply returned by a service. */
  @serializable sealed trait ServiceReply extends ServiceMessage;
  
  /** Reply for alive state. */
  case object Pong extends ServiceReply;
  
  /** Reply for exit. */
  case object Bye extends ServiceReply;
  
  /** Connection successfully established to the given path. */
  case class ConnectionMade(path : String) extends ServiceReply;
  
  /** No object was found with the given path. */
  case class ConnectionFailed(path : String) extends ServiceReply;
  
  /** A user defined message. */
  case class Reply(message : Any) extends ServiceReply;
  
  /** No reply provided. */
  case object NoReply extends ServiceReply;
  
  /** An error. */
  case class ExceptionWrapper(ex : Throwable) extends ServiceReply;
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
}

/**
 * Enriches runnables with extra background job options.
 * 
 * @author dramage
 */
trait Threadable extends Runnable {
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
}


/**
 * A connection to a remote SocketService.  Supports actor-like
 * methods ! and !? as well as remote stop.
 * 
 * URI should be of the form <pre>socket://machine:port/name</pre>
 * 
 * @author dramage
 */
class SocketClient(uri : URI) {
  import ServiceMessages._;
  import ServiceUtil._;
  
  import scala.collection.mutable.{HashMap,SynchronizedMap,HashSet,SynchronizedSet};
  
  /** Private logger method */
  private def info(msg : String) = SocketService.log("SocketClient: "+msg);
  
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
  private def send(message : ServiceRequest, consumer : Option[Channel[ServiceReply]]) : Unit = synchronized {
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

    return rid;
  }
  
  /** Sends a message to the service and awaits a reply. */
  def query(msg : ServiceRequest) : ServiceReply = {
    val channel = new Channel[ServiceReply];
    val id = send(msg,Some(channel));
    
    val received : ServiceMessage = channel.read;
    
    received match {
      case ExceptionWrapper(ex) => throw new RemoteServiceException(ex);
      case x : ServiceReply => x;
      case o : Any => throw new ServiceException("Unexpected message: "+o);
    }
  }
  
  /** Sends a message without awaiting reply. */
  def ! (msg : Any) : Unit = {
    send(Message(msg), None);
  }
  
  /** Sends a message, blocking and returning its reply. */
  def !? (msg : Any) : Any = {
    query(Message(msg)) match {
      case Reply(reply) => reply;
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
      val rid     = try { in.readInt }      catch { case eof : java.io.EOFException => close(); -1; }
      val control = try { in.readUnshared } catch { case eof : java.io.EOFException => close(); None; }
      
      if (rid >= 0) {
        info("received "+rid);
        
        if (!channels.contains(rid)) {
          throw new ServiceException("SocketService error: missing channel for "+rid);
        }
        
        channels(rid) match {
          case Some(channel) => {
            // some channel wants to consume the reply
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

/**
 * Extra constructors for SocketClient.
 * 
 * @author dramage
 */
object SocketClient {
  def apply(uri : URI) =
    new SocketClient(uri);
  
  def apply(uri : String) =
    new SocketClient(new URI(uri));
}

/**
 * Companion object containing control messages used internally to the package.
 * 
 * @author dramage
 */
private object SocketServiceDispatch {
  sealed trait Incoming;
  case object IncomingShutdown extends Incoming;
  case class IncomingMessage(rid : Int, message : Any, reply : Channel[Outgoing]) extends Incoming;
  
  sealed trait Outgoing;
  case object OutgoingShutdown extends Outgoing;
  case class OutgoingMessage(rid : Int, message : ServiceMessages.ServiceMessage) extends Outgoing;
}


/**
 * A listens on a port, dispatching connections to SocketService instances.
 * 
 * @author dramage
 */
class SocketServiceDispatch(val port : Int) extends Runnable with Threadable {
  import ServiceMessages._;
  import ServiceUtil._;

  /** This services registered on this dispatcher. */
  val services = new scala.collection.mutable.HashMap[String, SocketService];
  
  /** Private logger method */
  protected def info(msg : String) = SocketService.log("SocketServiceDispatch: "+msg);
  
  /** The server socket we accept connections from. */
  protected val listener = new ServerSocket(port);
  
  // timeout is 200ms -- how often we check to see if still active.
  // listener.setSoTimeout(200);
  
  /** Tells the connection daemon to stop. */
  def stop() = {
    listener.close();
  }

  /** Registers a service with the given name. */
  def service(name : String)(body : PartialFunction[Any,Any]) : SocketService = {
    info("registering "+name);
    
    val service = new SocketService(this, name) {
      override def info(msg : String) = info(msg);
      override def react = body;
    };
    
    service;
  }
  
  def register(name : String, service : SocketService) {
    if (services.contains(name)) {
      throw new ServiceException("Unable to register service: "+name+" already in use");
    }
    services(name) = service;
  }
  
  /** Establishes new connections, forwarding to SocketService instances. */
  override def run() {
    info("listening on port "+port);
    
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
          val outgoing = new Channel[SocketServiceDispatch.Outgoing];
        
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
                  outgoing.write(SocketServiceDispatch.OutgoingShutdown);
                  -1;
              }
            
              if (active) {
                // read control message
                val control = try {
                  in.readUnshared
                } catch {
                  case ex : Throwable =>
                    outgoing.write(SocketServiceDispatch.OutgoingMessage(rid, ExceptionWrapper(ex)));
                    outgoing.write(SocketServiceDispatch.OutgoingShutdown);
                    throw(ex);
                }
            
                control match {
                  case Ping => {
                    info(""+Ping);
                    outgoing.write(SocketServiceDispatch.OutgoingMessage(rid, Pong));
                  }
        
                  case Stop => {
                    info(""+Stop);
                    outgoing.write(SocketServiceDispatch.OutgoingMessage(rid, Bye));
                    incoming.write(SocketServiceDispatch.IncomingShutdown);
                  }
          
                  case Message(message) => {
                    info(""+Message+" "+rid+" @ " +out);
                    incoming.write(SocketServiceDispatch.IncomingMessage(rid,message,outgoing));
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
                case SocketServiceDispatch.OutgoingMessage(rid, message) =>
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
            
                case SocketServiceDispatch.OutgoingShutdown =>
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

/**
 * A service that listens on a port.  Supports actor-like
 * methods via a SocketClient.
 * 
 * @author dramage
 */
abstract class SocketService(val dispatch : SocketServiceDispatch, val path : String)
extends Runnable with Threadable {
  
  import ServiceMessages._;
  import ServiceUtil._;
  
  dispatch.register(path, this);
  
  /** Private logger method */
  protected def info(msg : String) = SocketService.log("SocketService: "+msg);
  
  /** Incoming message queue. */
  protected[distributed] val incoming = new Channel[SocketServiceDispatch.Incoming]();

  /** Active state of this service. */
  protected var active = true;
  
  /** Abstract method for getting reaction. */
  def react : PartialFunction[Any,Any];
  
  /** Hostname of this service */
  def host =
    HubUtils.hostName;
  
  /** Returns a URI describing how to connect to this service. */
  val uri = try {
    new java.net.URI("socket",null,host,dispatch.port,path,null,null)
  } catch {
    case ue : java.net.URISyntaxException =>
      throw new IllegalArgumentException("Invalid path name "+path+": should start with /");
  }
  
  /** Registers this service in the given hub. */
  def register(hub : HubClient) =
    hub.register(uri);
  
  /**
   * Process and return incoming requests within the current thread.
   * Spawns additional daemon threads to manage socket i/o per session.
   */
  override def run() {
    while (active) {
      incoming.read match {
        case SocketServiceDispatch.IncomingMessage(rid, message, outgoing) =>
          try {
            react(message) match {
              case reply : ServiceMessage => outgoing.write(SocketServiceDispatch.OutgoingMessage(rid, reply));
              case _ => outgoing.write(SocketServiceDispatch.OutgoingMessage(rid, NoReply));
            }
          } catch {
            case ex : Throwable =>
              outgoing.write(SocketServiceDispatch.OutgoingMessage(rid, ExceptionWrapper(ex)));
          }
        
        case SocketServiceDispatch.IncomingShutdown =>
          active = false;
      }
    }
  }
  
  override def toString = uri.toString;
}

/**
 * Companion object with static constructor and convenience
 * methods for SocketService.
 * 
 * @author dramage
 */
object SocketService {
  import ServiceMessages._;
  import ServiceUtil._;

  /** How verbose are the service packages. */
  var verbose = 0;
  
  private[distributed] def log(msg : String) = {
    if (verbose > 0) { System.err.println(msg); }
  }
  
  /**
   * Global default dispatcher for this process.
   */
  lazy val dispatch = {
    val _dispatch = new SocketServiceDispatch(HubUtils.freePort);
    _dispatch.runAsDaemon;
    _dispatch;
  }
  
  /**
   * Spawns a service on the given port with the given body
   * to do actual request processing.  Needs to be started.
   */
  def apply(name : String)(body : PartialFunction[Any,Any]) =
    dispatch.service(name)(body);
  
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

/**
 * A simple example service.
 * 
 * @author dramage
 */
object SampleService {
  import ServiceMessages.Reply;
  
  /** A test main method */
  def main(argv : Array[String]) {
    val service = SocketService("/test") {
      case x : String =>
        Reply { x.toUpperCase }
          
      case y : Int =>
        throw new RuntimeException("I don't like Ints!")
          
      case z : Double => {
        Thread.sleep(200l);
        Reply { z * 2; }
      }
          
      case _ => None;
    };
    
    service.runAsThread;
    
    Thread.sleep(1000l);
    
    val client = SocketClient(service.uri);
    
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
