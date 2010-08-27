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

import scala.concurrent.ops.future;

import SocketService._;

/**
 * A Hub represents a central registry of active socket URIs as a starting
 * point for groups of objects that need to interact.
 *
 * @author dramage
 */
object Hub {
  /** Messages used internally by the Hub. */
  protected object Messages {
    /** Register an entry. */
    case class HubRegister(entry : URI);

    /** Unregister an entry. */
    case class HubUnregister(entry : URI);

    /** Request the registry. */
    case object HubListRequest;

    /** Registry response. */
    case class HubListResponse(registry : List[URI]);

    /** A message for the log. */
    case class HubLogMessage(message : String);
  }
  import Messages._;

  class Service
  (dispatch : SocketDispatch = SocketDispatch(),
   log : (String=>Unit) = System.err.println)
  extends SocketService("/hub", dispatch) {
    import ServiceMessages.Reply;

    protected val registry =
      new scala.collection.mutable.ArrayBuffer[URI];

    protected val loggers =
      new scala.collection.mutable.ArrayBuffer[SocketClient];

    /**
     * Removes all unpingable entries from the registry.
     */
    def cleanup() = synchronized {
      val origSize = registry.size;
      val valid = registry.map(uri => future { SocketClient.ping(uri, 3000l) });

      val newRegistry = (registry zip valid).filter(_._2()).map(_._1).toList;

      registry.clear;
      registry ++= newRegistry;

      val newSize = registry.size;

      if (origSize != newSize) {
        log("[hub] removed "+(origSize-newSize)+" entries");
      }
    }

    override def react = synchronized {
      case HubRegister(uri) =>
        cleanup();
        log("[hub] registering "+uri);
        registry += uri;
        // register as a logger
        if (uri.getPath == "/logger") {
          loggers += SocketClient(uri);
        }
      case HubUnregister(uri) =>
        cleanup();
        log("[hub] unregistering "+uri);
        registry -= uri;
        // unregister as a logger
        if (uri.getPath == "/logger") {
          for (connection <- loggers.find(_.uri == uri)) loggers -= connection;
        }
      case HubListRequest =>
        cleanup();
        log("[hub] listing");
        Reply { HubListResponse(registry.toList); }
      case HubLogMessage(msg) =>
        log(msg);
        for (logger <- loggers) try { log(msg); } catch { case _ => (); }
      case x:Any =>
        throw new ServiceException("Unexpected message: "+x);
    }
  }

  /**
   * Connection to a HubService.
   *
   * @author dramage
   */
  class Client(uri : URI) {
    lazy val remote = SocketClient(uri);

    def register(entry : URI) =
      remote ! HubRegister(entry);

    def unregister(entry : URI) =
      remote ! HubUnregister(entry);

    def registry : List[URI] =
      (remote !? HubListRequest).asInstanceOf[HubListResponse].registry;

    def stop = remote.stop;

    def select(group : (URI => Boolean)) : List[SocketClient] =
      registry.filter(uri => group(uri)).map(uri => SocketClient(uri));

    def log(msg : String) =
      remote ! HubLogMessage(msg);

    def addLogger(logger : String => Unit) : SocketService = {
      val dispatch = SocketDispatch(SocketUtils.freePort);
      val service = SocketService("/logger", dispatch) {
        case msg : String => logger(msg);
        case _ => throw new ServiceException("Unexpected message");
      }
      register(service.uri);
      service.runAsDaemon;
      service;
    }
  }


  //
  // Static methods
  //

  /** Constructs a new service instance. */
  def service(port : Int = -1, log : (String=>Unit) = System.err.println) =
    new Service(dispatch = SocketDispatch(port), log = log);

  /** Connects to the hub at the given URI. */
  def connect(uri : URI) = {
    new Client(uri);
  }

  /** Connects to the hub at the URI specified in the scalanlp.distributed.hub system property. */
  def connect() = {
    val uri = System.getProperty("scalanlp.distributed.hub");
    if (uri == null) {
      throw new IllegalArgumentException("System property scalanlp.distributed.hub not defined.")
    }
    new Client(URI.create(uri));
  }

  def main(args : Array[String]) {
    def usage() {
      println("Usage: " + this.getClass.getName + "(start|stop|list) [args]");
      println();
      println("  start [PORT]");
      println("    Start a service on this machine, optionally using the given port.");
      println();
      println("  stop HUB_URI");
      println("    Stop the hub at the given URI and all of its registered clients");
      println();
      println("  list HUB_URI");
      println("    List all registered entries and their status");
    }

    def require(condition : Boolean, msg : String) {
      if (!condition) {
        println("Hub invocation error: " + msg);
        System.exit(-1);
      }
    }

    if (args.length == 0) {
      usage;
      System.exit(-1);
    }

    args(0) match {
      case "start" =>
        require(args.length <= 2, "Too many arguments to start");
        val service = if (args.length == 2) {
          Hub.service(args(1).toInt);
        } else {
          Hub.service();
        }

        System.err.println("[hub] running at "+service.uri);
        service.run;

      case "list" =>
        require(args.length == 2, "list must be given one HUB_URI");

        val uri = if (!args(1).startsWith("socket://")) {
          URI.create("socket://" + args(1) + "/hub");
        } else {
          URI.create(args(1));
        }

        connect(uri).registry.foreach(println);

      case "stop" =>
        require(args.length == 2, "stop must be given one HUB_URI");

        val uri = if (!args(1).startsWith("socket://")) {
          URI.create("socket://" + args(1) + "/hub");
        } else {
          URI.create(args(1));
        }

        val hub = connect(uri);

        hub.registry.map(entry => future(SocketClient(entry).stop)).map(_.apply());
        hub.stop;
    }
  }
}
