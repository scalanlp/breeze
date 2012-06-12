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

import scala.concurrent.Channel;
  
import java.net.URI;
import java.io.{ObjectOutputStream}

/**
 * Messages that services respond to.
 * 
 * @author dramage
 */
private[distributed] object ServiceMessages {
  sealed trait ServiceMessage extends Serializable;
  
  /** A request made of a service. */
  sealed trait ServiceRequest extends ServiceMessage;
  
  /** Requests a connection to the given path. */
  case class Connect(path : String) extends ServiceRequest;
  
  /** Request for alive state. */
  case object Ping extends ServiceRequest;
  
  /** Request to exit. */
  case object Stop extends ServiceRequest;
  
  /** A user defined message. */
  case class Message(message : Any) extends ServiceRequest;
  
  
  /** A reply returned by a service. */
  sealed trait ServiceReply extends ServiceMessage with Serializable;
  
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
  case class ExceptionWrapper(host : URI, ex : Throwable) extends ServiceReply;
}


/**
 * A service that listens on a port.  Supports actor-like
 * methods via a SocketClient.
 * 
 * @author dramage
 */
abstract class SocketService(val path : String, val dispatch : SocketDispatch = SocketDispatch())
extends Runnable with Threadable {
  import SocketDispatch.Messages._;
  import ServiceMessages._;
  import ServiceUtil._;
  
  /** The URI of this service. */
  val uri = try {
    new java.net.URI("socket",null,SocketUtils.hostName,dispatch.port,path,null,null)
  } catch {
    case ue : java.net.URISyntaxException =>
      throw new IllegalArgumentException("Invalid path name "+path+": should start with /");
  }

  // only register with the dispatch if we have a valid URI
  dispatch.register(path, this);

  /** Private logger method */
  protected def info(msg : String) = 
    if (SocketService.VERBOSE) System.err.println("[SocketService] "+msg);
  
  /** Incoming message queue. */
  protected[distributed] val incoming = new Channel[Incoming]();

  /** Active state of this service. */
  protected var active = true;
  
  /** Abstract method for getting reaction. */
  def react : PartialFunction[Any,Any];
  
  /**
   * Process and return incoming requests within the current thread.
   * Spawns additional daemon threads to manage socket i/o per session.
   */
  override def run() {
    while (active) {
      incoming.read match {
        case IncomingMessage(rid, message, outgoing) =>
          try {
            react(message) match {
              case reply : ServiceMessage => outgoing.write(OutgoingMessage(rid, reply));
              case _ => outgoing.write(OutgoingMessage(rid, NoReply));
            }
          } catch {
            case ex : Throwable =>
              outgoing.write(OutgoingMessage(rid, ExceptionWrapper(uri, ex)));
          }
        
        case IncomingShutdown =>
          active = false;

        case x : Any =>
          throw new ServiceException("Unexpected message: "+x);
      }
    }
  }

  def stop() =
    active = false;

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

  /** Verbosity level.  Control with -DSocketClient.VERBOSE=true */
  var VERBOSE =
    System.getProperty("SocketService.VERBOSE") == "true";

  /** Creates a reply message as part of the body of a PartialFunction. */
  def reply(msg : =>Any) : Any =
    Reply(msg);

  /**
   * Spawns a service on the given port with the given body
   * to do actual request processing.  Needs to be started.
   */
  def apply(name : String, dispatch : SocketDispatch = SocketDispatch())(body : PartialFunction[Any,Any]) = {
    val service = new SocketService(name, dispatch) {
      override def info(msg : String) = info(msg);
      override def react = body;
    };

    service;
  }
}

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
 * Network utility functions for classes that interact with hubs.
 *
 * @author dramage
 */
object SocketUtils {
  def freePort() : Int = {
    val server = new java.net.ServerSocket(0);
    val port = server.getLocalPort();
    server.close();
    return port;
  }

  def hostName() : String = {
    java.net.InetAddress.getLocalHost().getHostName();
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
    //
    // start a service
    //

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

    //
    // connect as a client
    //

    // connect to client
    val uri = service.uri;

    // should return true
    System.err.println(SocketClient.ping(uri));
    
    val client = SocketClient(service.uri);

    // should print 4.0 last
    scala.concurrent.ops.spawn { System.err.println(client !? 2.0); }
    
    // should return "HI"
    System.err.println(client !? "hi");
    
    // should get exception thrown remotely
    try {
      client !? 3
    } catch {
      case x : RuntimeException => System.err.println(x.getMessage);
    }

    // shut down the service
    client.stop;
  }
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
class RemoteServiceException(host : URI, ex : Throwable)
extends RuntimeException("An exception was thrown on "+host,ex);
