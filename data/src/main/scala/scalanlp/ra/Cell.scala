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

import scalanlp.serialization.FileSerialization;

/**
 * A Cell represents a cached computation on disk.  Cells are safe to be
 * used by multiple processes -- a cell value that is pending won't be
 * over-written by the current process, but rather the process will wait
 * until the file is released, then load it from disk using the Serializer.
 *
 * @author dramage
 */
class Cell[V](cache : File, eval : => V)(implicit ra : RA) {
  import ra.pipes._;
  import Cell._;

  /** The computed (or loaded) value. */
  protected var value : Option[V] = None;

  /** The lock file on disk marks this cell as being computed. */
  protected val lock = new File(cache + ".status");

  /**
   * Returns the status of the cell as a function of its lock state.
   */
  def status : Status = {
    if (cache.exists && !lock.exists) {
      return Ready;
    } else if (!cache.exists && !lock.exists) {
      return Missing;
    } else { // lock exists
      return Pending;
    }
  }

  def get(implicit loadable : FileSerialization.Readable[V], saveable : FileSerialization.Writable[V]) : V = {
    value match {
      // value already loaded
      case Some(v) => v;

        // value not yet loaded
      case None => {
        if (status == Pending) {
          ra.log("[RA.cell] waiting for "+cache);
          while (status == Pending) {
            Thread.sleep(500l);
          }
        }

        val v : V = status match {
          case Ready   => {
            // value is ready, load and return it
            ra.log("[RA.cell] loading "+cache);
            implicitly[FileSerialization.Readable[V]].read(cache);
          }

          case Missing => {
            // missing value, compute it

            if (!lock.createNewFile) {
              // unexpected lock state for get: try again, this time waiting
              return get;
            }

            ra.log("[RA.cell] creating "+cache);
            List(RA.pid) | lock;
            val rv = eval;
            implicitly[FileSerialization.Writable[V]].write(cache, rv);
            lock.delete();
            rv;
          }

          case Partial => {
            // partial computation that we can help complete

            ra.log("[RA.cell] helping compute "+cache);
            val rv = eval;
            rv;
          }

          case Pending =>
            throw new CellException("Unexpected Pending state for "+cache);
        }

        value = Some(v);
        v;
      }
    }
  }

  override def toString =
    "Cell("+cache+","+status+")";
}

object Cell {
  /** A Cell's status is one of Ready, Pending, Missing, or Partial. */
  sealed trait Status;
  case object Ready extends Status;
  case object Pending extends Status;
  case object Missing extends Status;
  case object Partial extends Status;
}

class CellException(msg : String) extends RuntimeException(msg);
