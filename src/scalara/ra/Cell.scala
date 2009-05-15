package scalara.ra;

/**
 * A Cell represents a cached computation on disk.  Cells are safe to be
 * used by multiple processes -- a cell value that is pending won't be
 * over-written by the current process, but rather the process will wait
 * until the file is released, then load it from disk using the Serializer.
 * 
 * @author dramage
 */
class Cell[V](
  cache : java.io.File, eval : => V)(
  implicit valType : scala.reflect.Manifest[V], ra : RA) {
  
  import ra.pipes._;
  
  var value : Option[V] = None;

  /** The lock file on disk marks this cell as being computed. */
  protected val lock = new java.io.File(cache + ".status");
  
  /**
   * Returns the status of the cell as a function of its lock state.
   */
  def status : Cell.Status = {
    if (cache.exists && !lock.exists) {
      return Cell.Ready;
    } else if (!cache.exists && !lock.exists) {
      return Cell.Missing;
    } else { // lock exists
      return Cell.Pending;
    }
  } 
  
  def get : V = {
    value match {
      // value already loaded
      case Some(v) => v;
      
      // value not yet loaded
      case None => {
        if (status == Cell.Pending) {
          ra.log("RA.Cell: waiting for "+cache);
          while (status == Cell.Pending) {
            Thread.sleep(500l);
          }
        }
        
        val v : V = status match {
          case Cell.Ready   => {
            // value is ready, load and return it
            
            ra.log("RA.Cell: loading "+cache);
            Serializer.load(cache)(valType,ra);
          }
          
          case Cell.Missing => {
            // missing value, compute it
            
            if (!lock.createNewFile) {
              // unexpected lock state for get: try again, this time waiting
              return get;
            }
            
            ra.log("RA.Cell: creating "+cache);
            List(RA.pid) | lock;
            val rv = eval;
            Serializer.save(cache, rv)(valType,ra);
            lock.delete();
            rv;
          }
          
          case Cell.Partial => {
            // partial computation that we can help complete
            
            ra.log("RA.Cell: helping compute "+cache);
            val rv = eval;
            rv;
          }
            
          
          case Cell.Pending =>
            throw new RuntimeException("Unexpected Pending state for "+cache);
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
  sealed trait Status;
  case object Ready extends Status;
  case object Pending extends Status;
  case object Missing extends Status;
  case object Partial extends Status;
  
  def apply[V](cache : java.io.File)(eval : => V)(implicit valType : scala.reflect.Manifest[V], ra : RA) =
    new Cell(cache, eval)(valType,ra);
}
