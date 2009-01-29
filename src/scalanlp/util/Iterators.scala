package scalanlp.util;

/**
 * Utilities and implicits for iterators. Nothing major.
 * 
 * @author dramage
 * @author dlwh
 */
object Iterators {

  /**
   * Procedural iterator creation with an explicit callback.
   * 
   * TODO: this could be done smarter with synchronized blocks
   * and waiting.
   */
  def iterator[E](func : ((E=>Unit)=>Unit)) : Iterator[E] = {
    import scala.concurrent.ops._;
    
    val queue = new java.util.concurrent.LinkedBlockingQueue[E](1);
    var running = true;
    var thrown : Throwable = null;
    
    def isRunning = running;
    def isPending = queue.size >= 1;
    
    spawn {
      try {
        func(queue.put)
      } catch {
        case x : Throwable => thrown = x;
      };
      running = false;
    }
    
    return new Iterator[E] {
      def waitIfNecessary() = {
        val method = classOf[Thread].getMethod("yield");
        while (isRunning && !isPending) {
          method.invoke();
        }
        if (thrown != null) {
          throw new RuntimeException(thrown);
        }
      }
      
      override def hasNext = {
        waitIfNecessary();
        isPending;
      }
      
      override def next = {
        waitIfNecessary();
        if (!isPending) {
          throw new RuntimeException("Called next on empty iterator");
        }
        queue.take;
      }
    }
  }
  
  /**
   * Return the total count and the number of doubles.
   */
  def accumulateAndCount(it : Iterator[Double]) = it.foldLeft( (0.0,0) ) { (tup,d) =>
    (tup._1 + d, tup._2 + 1);
  }

}
