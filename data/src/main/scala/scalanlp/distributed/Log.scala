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
package scalanlp.distributed;

/**
 * Distributed logger based on SocketServices.
 * 
 * @author dramage
 */
object Logger {
  /**
   * Creates a logger service (and daemon thread) that sends
   * received lines to the given reporter function.
   */
  def service(report : (String=>Unit)) : SocketService = {
    val service = SocketService("logger") {
      case line : String => report(line);
    }
    ServiceUtil.daemon { service.run }
    return service;
  }
  
  /** Returns a stream for connecting */
  def stream(id : String, loggers : SocketClient*) : java.io.PrintStream = {
    def sendline(line : String) = 
      for (logger <- loggers) { logger ! (id + ":" + line) };
    
    def close() =
      for (logger <- loggers) { logger.close };
    
    FlexPrintStream(sendline, close);
  }
}

/**
 * A PrintStream that redirects lines to the given function.
 * 
 * @author dramage
 */
object FlexPrintStream {
  def apply(tline : (String => Unit), tclose : (() => Unit)) = {
    val out = new java.io.ByteArrayOutputStream(2048) {
      var accumulated : String = "";
      val sep = System.getProperty("line.separator");
      
      override def flush() = synchronized {
        super.flush;
        accumulated += toString();
        reset();
        
        var pos = accumulated.indexOf(sep);
        while (pos >= 0) {
          tline(accumulated.substring(0,pos));
          accumulated = accumulated.substring(pos+sep.length,accumulated.length);
          pos = accumulated.indexOf(sep);
        }
      }
      
      override def close() = synchronized {
        flush();
        tclose();
        super.close();
      }
    }
    
    new java.io.PrintStream(out, true);
  }
}
