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
package scalara.ra;

import java.io.File;

import scalara.pipes.Pipes;
import scalara.serializer.{Loadable,Saveable};

//
// ResearchAssistant for Scala
//

case class FileResourceSet(roots : String*)(implicit val pipes : Pipes) {
  import java.io.File.separator;
  import pipes._;
  
  val _roots = roots.map(root =>
    if (root.endsWith(separator)) root.substring(0,root.length-separator.length) else root);
  
  /**
   * Returns the existing resource with the given name if it exists in the path,
   * otherwise returning the the implicit pipes' name.
   */
  def apply(name : String) : java.io.File = {
    val files = _roots.map(root => root + separator + name).filter(f => file(f).exists);
    if (files.length == 0) file(name) else file(files(0));
  }
}

case class RA(
  /** Returns the file system context of this branch. */
  val pipes : Pipes,
  
  /** Logs the given message to this context's logger. */
  val log : (String => Unit),
  
  /** Context random number generator. */
  val random : java.util.Random,
  
  /** Returns the parent of this context or null if it is the root context. */
  val parent : Option[RA]
) {
  
  RA.init();
  
  /** Source of where this context was created. */
  val source = Thread.currentThread.getStackTrace.dropWhile(
    _.getClassName.startsWith(classOf[RA].getPackage.getName));

  def parameter[E](name : String)(value : =>E) : E = {
    try {
      val v = value;
      log("RA.parameter: "+name+" = "+v);
      v;
    } catch {
      case ex : Throwable =>
        throw new ParameterException("Error getting value for "+name,ex);
    }
  }
  
  def task[E](name : String)(exec : =>E) : E = {
    log("RA: "+name+" started");
    val rv = exec;
    log("RA: "+name+" done");
    rv;
  }
  
  /** Returns a cached view of the given value, loading from a cell if possible. */
  def cache[V](cacheFile : java.io.File)(p : =>V)(implicit loadable : Loadable[V,File], saveable : Saveable[V,File]) =
    cell(cacheFile)(p).get;
  
  def cell[V](cacheFile : java.io.File)(p : =>V) =
    new Cell(cacheFile,p)(this);
  
  /** Gets a (named) child context in a named folder, creating it if necessary. */
  def branch(dir : File) : RA = {
    val _pipes = Pipes(pipes);
    
    if (dir.exists && !dir.isDirectory) {
      throw new IllegalArgumentException(dir + " already exists and is not a directory");
    }
    if (!dir.exists) {
      dir.mkdir();
    }
    
    _pipes.cd(dir);
    
    RA(_pipes, log, RA.nextRandom, Some(this));
  }
  
  /** Gets a (named) child context in a named subfolder, creating it if necessary. */
  def branch(name : String) : RA =
    branch(pipes.file(name));
}
  
class ParameterException(msg : String, cause : Throwable)
  extends RuntimeException(msg,cause);  

object RA {
  val bean = java.lang.management.ManagementFactory.getRuntimeMXBean();
  
  val seed = new java.util.Random().nextLong;
  
  val seeds = new scala.collection.mutable.HashMap[String,Int] with scala.collection.mutable.SynchronizedMap[String,Int] {
    override def default(x : String) = 0;
  }
  
  val pid = bean.getName+":"+bean.getStartTime;
  
  /** Global (default) RA context. */
  implicit val global =
    RA(Pipes.global, System.err.println, RA.nextRandom, None);
  
  def nextRandom : java.util.Random = {
    val stack = Thread.currentThread.getStackTrace.dropWhile(_.getClassName.startsWith(classOf[RA].getPackage.getName));
    def mkRandom(name : String) = new java.util.Random(name.hashCode + seeds(name) + seed);
    if (stack.isEmpty) {
      mkRandom(getClass.getName);
    } else {
      mkRandom(stack(0).getClassName);
    }
  }
  
  /** Simple empty method to ensure the static initializer runs. */
  protected[ra] def init() { }
}
