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

trait CellBroker {
  protected val ra: RA;
  import ra._;
  import ra.pipes._;
  /**
   * A Cell represents a cached computation on disk.  Cells are safe to be
   * used by multiple processes -- a cell value that is pending won't be
   * over-written by the current process, but rather the process will wait
   * until the file is released, then load it from disk using the Serializer.
   *
   * @author dramage
   */
  class Cell[V](cache : File, eval : => V) {

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

    def get(implicit loadable : ra.serializer.Readable[V], saveable : ra.serializer.Writable[V]) : V = {
      value match {
        // value already loaded
        case Some(v) => v;

          // value not yet loaded
        case None => {
            if (status == Pending) {
              ra.log("RA.Cell: waiting for "+cache);
              while (status == Pending) {
                Thread.sleep(500l);
              }
            }

            val v : V = status match {
              case Ready   => {
                  // value is ready, load and return it

                  ra.log("RA.Cell: loading "+cache);

                  val input = ra.serializer.openInput(cache);
                  val rv = loadable.read(input);
                  ra.serializer.closeInput(input);
                  rv
                  // Serializer.load(cache)(valType,ra);
                }

              case Missing => {
                  // missing value, compute it

                  if (!lock.createNewFile) {
                    // unexpected lock state for get: try again, this time waiting
                    return get;
                  }

                  ra.log("RA.Cell: creating "+cache);
                  List(RA.pid) | lock;
                  val rv = eval;
                  // Serializer.save(cache, rv)(valType,ra);
                  val output = ra.serializer.openOutput(cache);
                  saveable.write(output,rv);
                  ra.serializer.closeOutput(output);
                  lock.delete();
                  rv;
                }

              case Partial => {
                  // partial computation that we can help complete

                  ra.log("RA.Cell: helping compute "+cache);
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

  sealed trait Status;
  case object Ready extends Status;
  case object Pending extends Status;
  case object Missing extends Status;
  case object Partial extends Status;

  class CellException(msg : String) extends RuntimeException(msg);
}
