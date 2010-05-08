package scalanlp.concurrent

import jsr166y.forkjoin.{RecursiveAction,ForkJoinTask, ForkJoinPool}
// based on josh cough's scala-parallel

trait FunctionalRecursiveAction[T] extends RecursiveAction {
  def getResult: T
  val seqentialThreshold: Int
}

trait Pool{
  def invokeAndGet[T](action:FunctionalRecursiveAction[T]): T
}

object ParallelOps {

  object DefaultPool extends Pool {
    private val fjPool = new ForkJoinPool
    def invokeAndGet[T](action: FunctionalRecursiveAction[T]): T = {
      fjPool.invoke(action)
      action.getResult
    }
  }

  implicit def parallelOps[T](seq: Seq[T]) = new {
    def par = new ParallelSeqOps(seq);
    def par(threshold: Int) = new ParallelSeqOps(seq,threshold);
  }

  final class ParallelSeqOps[T](seq: Seq[T], threshold: Int=16) {
    def withSequentialThreshold(threshold: Int) = new ParallelSeqOps(seq,threshold);
    def fold[B](id: =>B)(sequentialFold: (B,T)=>B)(finalFold: (B,B)=>B):B = {
      val action = new BinaryRecursiveAction(finalFold, { (start:Int,end:Int) =>
          seq.view(start, end).foldLeft(id)(sequentialFold)
        });
      DefaultPool.invokeAndGet(action);
    }

    def reduce[B>:T](f: (B,B)=>B):B = {
      val action = new BinaryRecursiveAction(f, { (start:Int,end:Int) =>
          seq.view(start, end).reduceLeft(f);
      });
      DefaultPool.invokeAndGet(action);
    }

    def mapReduce[U,B>:U](f: T=>U, r: (B,B)=>B) = {
      val action = new BinaryRecursiveAction(r, { (start:Int,end:Int) =>
          seq.view(start, end).map(f).reduceLeft(r);
        });
      DefaultPool.invokeAndGet(action);
    }

    class BinaryRecursiveAction[T](val reduce: (T, T) => T, val executeSequentially: (Int, Int) => T,
                                   val start: Int, val end: Int) extends FunctionalRecursiveAction[T] {

      def this(reduce: (T, T) => T, executeSequentially: (Int, Int) => T) = {
        this( reduce, executeSequentially, 0, seq.length)
      }

      val seqentialThreshold = threshold

      private var result: Option[T] = None
      private def size = end - start
      private def midpoint = size / 2

      def getResult = result.get

      private def executeInParallel: T = {
        val left = new BinaryRecursiveAction(reduce, executeSequentially, start, start + midpoint)
        val right = new BinaryRecursiveAction(reduce, executeSequentially, start + midpoint, end)
        right.fork();
        left.compute()
        right.join();
        reduce(left.result.get, right.result.get)
      }

      override def compute {
        result = Some(if (size < seqentialThreshold) executeSequentially(start, end) else executeInParallel)
      }
    }
  }
}
