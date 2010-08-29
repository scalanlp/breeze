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
 * client's ! (asynchronous) and !? (reply) methods.
 *
 * @author dramage
 */
object Remote {

  protected object Messages {
    /** Requests that a remote object be created. Returns Long. */
    case class Create[R](fn : () => R);

    /** Apply the function to the object with the given id.  Returns nothing. */
    case class Invoke[R,V](fn : R => V);

    /** Apply fn to the object with the given id, returning result V. */
    case class Query[R,V](fn : R => V);
  }

  import Messages._;

  class Service
  (name : String,
   dispatch : SocketDispatch = SocketDispatch(),
   log : (String=>Unit) = System.err.println)
  extends SocketService(name, dispatch) {
    var instance : AnyRef = null;

    def create[R](fn : () => R) = {
      if (instance != null && instance.isInstanceOf[java.io.Closeable]) {
        instance.asInstanceOf[java.io.Closeable].close();
      }
      instance = fn().asInstanceOf[AnyRef];
    }
    
    override def react = synchronized {
      case Create(fn) => {
        log("[remote] create");
        create(fn);
      }

      case Invoke(fn) =>
        fn(instance);

      case Query(fn) =>
        SocketService.reply(fn(instance));
    }
  }

  class Client[R](val remote : SocketClient) {
    def create(fn : () => R) : Unit =
      (remote ! Create(fn));

    def invoke[V](fn : R => V) : Unit =
      (remote ! Invoke(fn));

    def query[V](fn : R => V) : V =
      (remote !? Query(fn)).asInstanceOf[V];
  }

  //
  // Static methods
  //

  /** Constructs a new service instance. */
  def service(name : String, port : Int = -1, log : (String=>Unit) = System.err.println) =
    new Service(name = name, dispatch = SocketDispatch(port), log = log);

  /** Connects to the given id. */
  def client[R](uri : URI) : Client[R] =
    new Client[R](SocketClient(uri));

  def main(args : Array[String]) {
    def usage() {
      println("Usage: " + this.getClass.getName + "(start|...) [args]");
      println();
      println("  start HUB_URI NAME [PORT]");
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
        require(args.length >= 3, "Not enough arguments to start");

        val hub = Hub.connect(URI.create(args(1)));
        val name = args(2);
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
