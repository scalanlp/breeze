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
package breeze;
package ra;

import java.io.File;

import pipes.Pipes;

import breeze.serialization.FileSerialization;

/**
 * A Cell represents a cached computation on disk.  Cells are safe to be
 * used by multiple processes -- a cell value that is pending won't be
 * over-written by the current process, but rather the process will wait
 * until the file is released, then load it from disk using the Serializer.
 *
 * @author dramage
 */
class Cell[V](val path : File, eval : => V, log : (String=>Unit)=System.err.println) {
  import Pipes.global._;
  import Cell._;

  /** The computed (or loaded) value. */
  protected var value : Option[V] = None;

  /** The lock file on disk marks this cell as being computed. */
  protected val lock = new File(path + ".status");

  /**
   * Returns the status of the cell as a function of its lock state.
   */
  def status : Status = {
    if (path.exists && !lock.exists) {
      return Ready;
    } else if (!path.exists && !lock.exists) {
      return Missing;
    } else { // lock exists
      return Pending;
    }
  }

  /**
   * Compute the value of this cell if it has not already been computed.
   * The behavior of this function depends on the cell's status: if 
   * status == Missing, then the cell is evaluated and stored by calling
   * cell.get.  Otherwise this call does nothing.
   */
  def compute(implicit loadable : FileSerialization.Readable[V], saveable : FileSerialization.Writable[V]) : Unit = {
    if (status == Missing) get;
  }

  /**
   * Returns the value of this cell, either by loading it if status == Ready
   * or by computing it fresh, in which case the value is saved.
   */
  def get(implicit loadable : FileSerialization.Readable[V], saveable : FileSerialization.Writable[V]) : V = {
    value match {
      // value already loaded
      case Some(v) => v;

        // value not yet loaded
      case None => {
        if (status == Pending) {
          log("[cell] waiting for "+path);
          while (status == Pending) {
            Thread.sleep(500l);
          }
        }

        val v : V = status match {
          case Ready   => {
            // value is ready, load and return it
            log("[cell] loading "+path);
            implicitly[FileSerialization.Readable[V]].read(path);
          }

          case Missing => {
            // missing value, compute it

            if (!lock.createNewFile) {
              // unexpected lock state for get: try again, this time waiting
              return get;
            }

            log("[cell] creating "+path);
            List(RA.pid) | lock;
            val rv = eval;
            implicitly[FileSerialization.Writable[V]].write(path, rv);
            lock.delete();
            rv;
          }

          case Partial => {
            // partial computation that we can help complete

            log("[cell] helping compute "+path);
            val rv = eval;
            rv;
          }

          case Pending =>
            throw new CellException("Unexpected Pending state for "+path);
        }

        value = Some(v);
        v;
      }
    }
  }

  override def toString =
    "Cell("+path+","+status+")";
}

object Cell {
  /** A Cell's status is one of Ready, Pending, Missing, or Partial. */
  sealed trait Status;
  case object Ready extends Status;
  case object Pending extends Status;
  case object Missing extends Status;
  case object Partial extends Status;

  def apply[V]
  (name : String, pipes : Pipes = Pipes.global,
   log : (String=>Unit) = System.err.println)
  (eval : => V) : Cell[V] =
    new Cell(pipes.file(name), eval, log);

  /** Evaluates the given eval function if no cache exists; otherwise loads value. */
  def cache[V:FileSerialization.Readable:FileSerialization.Writable]
  (name : String, pipes : Pipes = Pipes.global,
   log : (String=>Unit) = System.err.println)
  (eval : => V) : V =
    new Cell(pipes.file(name), eval, log).get;

  /** Evaluates the given eval function if no cache exists; otherwise loads value. */
  def cache[V:FileSerialization.Readable:FileSerialization.Writable]
  (path : File)(eval : => V) : V =
    new Cell(path, eval).get;

  /** Evaluates the given eval function if no cache exists; otherwise loads value. */
  def cache[V:FileSerialization.Readable:FileSerialization.Writable]
  (path : File, log : (String => Unit))(eval : => V) : V =
    new Cell(path, eval, log).get;
}

class CellException(msg : String) extends RuntimeException(msg);
