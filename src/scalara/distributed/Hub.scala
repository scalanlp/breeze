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

import java.net.URI;

import SocketService._;

/** Messages for the hub. */
object HubMessages {
  /** Register an entry. */
  case class HubRegister(entry : URI);
  
  /** Unregister an entry. */
  case class HubUnregister(entry : URI);
  
  /** Request the registry. */
  case object HubListRequest;
  
  /** Registry response. */
  case class HubListResponse(registry : List[URI]);
}

class HubService(dispatch : SocketServiceDispatch) extends SocketService(dispatch, "/hub") {
  import HubMessages._;
  
  protected var registry =
    new scala.collection.mutable.ArrayBuffer[URI];
    
  /**
   * Removes all unpingable entries from the registry.
   */
  def cleanup() = synchronized {
    val origSize = registry.size;
    val valid = registry.map(entry => ping(uri));
    val newRegistry = (registry.elements zip valid.elements).filter(_._2()).map(_._1).toList;
    
    registry.clear;
    registry ++= newRegistry;
    
    val newSize = registry.size;
      
    if (origSize != newSize) {
      info("Hub: removed "+(origSize-newSize)+" entries");
    }
  }
  
  override def react = synchronized {
    case HubRegister(entry) =>
      cleanup();
      info("Hub: registering "+entry);
      registry += entry;
    case HubUnregister(entry) =>
      cleanup();
      info("Hub: unregistering "+entry);
      registry -= entry;
    case HubListRequest =>
      cleanup();
      info("Hub: listing");
      reply { HubListResponse(registry.toList); }
    case x:Any =>
      info("Hub: other message(?) "+x);
  }
}

/** A hub stores a list of active Workers. */
object HubService {
  import HubMessages._;
  
  /**
   * Calls apply using a new unique free port.
   */
  def apply() =
    new HubService(SocketService.dispatch);
  
  /**
   * Creates and starts a new hub service (in this thread)
   * using the given port.
   */
  def apply(port : Int) =
    new HubService(new SocketServiceDispatch(port));
}

/** Programmatic interface to a hub. */
@serializable class HubClient(uri : URI) {
  import HubMessages._;
  
  @transient lazy val remote = SocketClient(uri);
  
  def select(group : (URI => Boolean)) : List[SocketClient] = {
    registry.filter(uri => group(uri)).map(uri => SocketClient(uri));
  }
  
  def register(entry : URI) =
    remote ! HubRegister(entry);
  
  def unregister(entry : URI) =
    remote ! HubUnregister(entry);
  
  def registry : List[URI] =
    (remote !? HubListRequest).asInstanceOf[HubListResponse].registry;
  
  def stop = remote.stop;
}

/** Connects to a HubService. */
object HubClient {
  /** Constructs a hub instance form a machine and port. */
  def apply(uri : URI) : HubClient =
    new HubClient(uri);
}

/**
 * Utility functions for classes that interact with hubs.
 */
object HubUtils {
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

object HubStart {
  def main(argv : Array[String]) {
    val service = if (argv.length == 1) {
      HubService(argv(0).toInt);
    } else {
      HubService();
    }

    println("Starting hub at "+service);
    service.run;
  }
}

/**
 * Entry point to list registered actors.
 */
object HubListRegistry {
  def main(argv : Array[String]) {
    if (argv.length != 1) {
      System.err.println("Usage: host:port");
      System.exit(1);
    }
    
    val uri = if (!argv(0).startsWith("socket://")) {
      new URI("socket://" + argv(0) + "/hub");
    } else {
      new URI(argv(0));
    }
    
    HubClient(uri).registry.foreach(println);
  }
}

/**
 * Shut down all clients in the hub.
 */
object HubStopAll {
  def main(argv : Array[String]) {
    if (argv.length != 1) {
      System.err.println("Usage: host:port");
      System.exit(1);
    }
    
    val uri = if (!argv(0).startsWith("socket://")) {
      new URI("socket://" + argv(0) + "/hub");
    } else {
      new URI(argv(0));
    }
    
    val hub = HubClient(uri);
    
    for (entry <- hub.registry) {
      println("Stopping "+entry);
      SocketClient(entry).stop;
    }
    
    println("Stopping hub");
    hub.stop;
    
    println("Done");
  }
}
