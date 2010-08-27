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

import java.net.URI;

/**
 * Remote provides general facilities for a remotely hosted object.
 *
 * Use Remote.service to start a service that hosts objects.  Create
 * an object on a remote service using Remote.create or connect using
 * Remote.connect.  Functions are applied to the object using the
 * returned client's .apply method, and the results are returned.
 *
 * @author dramage
 */
object Remote {

  protected object Messages {
    /** Requests that a remote object be created. */
    case class Create[R](fn : () => R);

    /** Returns the id for a created object. */
    case class CreateReply(id : Long);

    /** Dereference the object at the given id. */
    case class Free(id : Long);

    /** Reply to Free. */
    case object FreeReply;

    /** Apply fn to the object with the given id, returning InvokeReply at V. */
    case class Invoke[R,V](id : Long, fn : R => V);

    /** Success on calling a function on a path. */
    case class InvokeReply[V](value : V);

    /** Error - no id found. */
    case object InvokeErrorNotFound;
  }

  import Messages._;

  class Service
  (name : String,
   dispatch : SocketDispatch = SocketDispatch(),
   log : (String=>Unit) = System.err.println)
  extends SocketService(name, dispatch) {
    import ServiceMessages.Reply;

    val instances = scala.collection.mutable.Map[Long,Any]();
    var uid = 0l;

    override def react = synchronized {
      case Create(fn) => {
        while (instances contains uid) {
          uid += 1;
        }
        log("[remote] adding "+uid+" to "+instances.size+" active instances");
        instances(uid) = fn();
        Reply { CreateReply(uid); }
      }

      case Free(id) => {
        log("[remote] freeing "+uid+" from "+instances.size+" active instances");
        instances.remove(id);
        Reply { FreeReply; }
      }

      case Invoke(id, fn) => {
        Reply { InvokeReply(fn(instances(id))); }
      }
    }
  }

  class Client[R](val remote : SocketClient, val id : Long, free : Boolean = false) {
    override def finalize() =
      if (free) { try { remote ! Free(id) } finally { } }

    def apply[V](fn : R => V) = {
      (remote !? Invoke(id, fn)) match {
        case InvokeReply(rv)     => rv.asInstanceOf[V];
        case InvokeErrorNotFound => throw new RemoteInvokeObjectNotFound();
      }
    }
  }

  //
  // Static methods
  //

  /** Constructs a new service instance. */
  def service(name : String = "/remote", port : Int = -1, log : (String=>Unit) = System.err.println) =
    new Service(name = name, dispatch = SocketDispatch(port), log = log);

  /** Connects to the given id. */
  def connect[R](uri : URI, id : Long) : Client[R] = {
    new Client[R] (SocketClient(uri), id, false);
  }

  /** Create a new remote instance. */
  def create[R](uri : URI, fn : () => R) : Client[R] = {
    val socket = SocketClient(uri);
    val id = (socket !? Create(fn)).asInstanceOf[CreateReply].id;
    new Client[R](socket, id, true);
  }

  def main(args : Array[String]) {
    def usage() {
      println("Usage: " + this.getClass.getName + "(start|...) [args]");
      println();
      println("  start HUB_URI [NAME=/remote] [PORT]");
      println("    Start a service on this machine, optionally using the given port.");
    }

    def require(condition : Boolean, msg : String) {
      if (!condition) {
        println("Invocation error: " + msg);
        System.exit(-1);
      }
    }

    if (args.length == 0) {
      usage;
      System.exit(-1);
    }

    args(0) match {
      case "start" =>
        require(args.length <= 4, "Too many arguments to start");
        require(args.length >= 2, "Not enough arguments to start");

        val hub = Hub.connect(URI.create(args(1)));
        val name = if (args.length > 2) args(2) else "/remote";
        val port = if (args.length > 3) args(3).toInt else -1;

        val service = new Service(name = name, dispatch = SocketDispatch(port));
        hub.register(service.uri);

        System.err.println("[remote] running at "+service.uri);
        service.run;
      case _ =>
        usage();
    }
  }
}

/**
 * Exception thrown when attempting to apply a function to an object
 * that is not found in the remote.
 *
 * @author dramage
 */
class RemoteInvokeObjectNotFound extends RuntimeException;
