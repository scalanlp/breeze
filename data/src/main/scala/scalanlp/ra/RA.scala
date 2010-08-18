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

package scalanlp.ra;

import java.io.File;

import scalanlp.pipes.Pipes;
import scalanlp.serialization.FileSerialization;

/**
 * Represents a set of paths to examine for files with a given name.
 *
 * @author dramage
 */
case class FileResourceSet(roots : String*)(implicit val pipes : Pipes) {
  import File.separator;
  import pipes._;
  
  val _roots = roots.map(root =>
    if (root.endsWith(separator)) root.substring(0,root.length-separator.length) else root);
  
  /**
   * Returns the existing resource with the given name if it exists in the path,
   * otherwise returning the the implicit pipes' name.
   */
  def apply(name : String) : File = {
    val files = _roots.map(root => root + separator + name).filter(f => file(f).exists);
    if (files.length == 0) file(name) else file(files(0));
  }
}

/**
 * Runtime context object for experiments, providing a logging method, pipes context,
 * and a random number generator.
 *
 * @author dramage
 */
case class RA(
  /** Returns the file system context of this branch. */
  val pipes : Pipes,
  
  /** Logs the given message to this context's logger. */
  val log : (String => Unit),
  
  /** Context random number generator. */
  val random : java.util.Random,
  
  /** Returns the parent of this context or null if it is the root context. */
  val parent : Option[RA] = None)
{
  
  RA.init();

  protected val ra : this.type = this;
  
  /** Source of where this context was created. */
  val source = Thread.currentThread.getStackTrace.dropWhile(
    _.getClassName.startsWith(classOf[RA].getPackage.getName));

  def parameter[E](name : String)(value : =>E) : E = {
    try {
      val v = value;
      log("[RA.parameter] "+name+" = "+v);
      v;
    } catch {
      case ex : Throwable =>
        throw new ParameterException("Error getting value for "+name,ex);
    }
  }

  def task[E](name : String)(exec : =>E) : E = {
    log("[RA.task] "+name+" started");
    val rv = exec;
    log("[RA.task] "+name+" done");
    rv;
  }

  /** Returns a cached view of the given value, loading from a cell if possible. */
  def cache[V](cacheFile : File)(p : =>V)
  (implicit readable : FileSerialization.Readable[V], writable : FileSerialization.Writable[V]) =
    cell(cacheFile)(p).get;

  /** Returns a cell representing the state of computation backed by the given cache file. */
  def cell[V](cacheFile : File)(p : =>V) =
    new Cell(cacheFile,p,log);

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
  
object RA {
  protected val bean = java.lang.management.ManagementFactory.getRuntimeMXBean();
  
  val seed = new java.util.Random().nextLong;
  
  protected val seeds = new scala.collection.mutable.HashMap[String,Int] with scala.collection.mutable.SynchronizedMap[String,Int] {
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

class ParameterException(msg : String, cause : Throwable)
  extends RuntimeException(msg,cause);
