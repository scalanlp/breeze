package scalanlp.concurrent
/*
 Copyright 2010 David Hall, Daniel Ramage

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
import jsr166y.forkjoin.{RecursiveAction,ForkJoinTask, ForkJoinPool}
// based on josh cough's scala-parallel


/**
 * Contains implicits for making sequences and arrays parallel. Basic usage looks like:
 *
 * {{{
 *  import ParallelOps._;
 *  (1 to 10000).par.reduce{_ + _};
 * }}}
 *
 * The implementation uses the ForkJoin framework to recursively split the data into smaller chunks until
 * a minimum size is reached. Once it is, the chunk is operated on. Default chunk size is 16, but can
 * be changed programmatically by specifying a value to the "par" method.
 *
 * Based on Josh Cough's scala-parallel
 *
 * @author dlwh
 *
 */
object ParallelOps {

  trait FunctionalRecursiveAction[T] extends RecursiveAction {
    def getResult: T
    val seqentialThreshold: Int
  }

  trait Pool{
    def invokeAndGet[T](action:FunctionalRecursiveAction[T]): T
  }

  object DefaultPool extends Pool {
    private val fjPool = new ForkJoinPool
    def invokeAndGet[T](action: FunctionalRecursiveAction[T]): T = {
      fjPool.invoke(action)
      action.getResult
    }
  }

  /**
   * The implicit to convert a sequence into a ParallelSeqOps. Usage:
   *
   * {{{
   * mySeq.par.reduce(_ + _);
   * mySeq.par(sequentialThreshold).reduce(_ + _);
   * }}}
   */
  implicit def parallelOps[T](seq: IndexedSeq[T]) = new {
    def par = new ParallelSeqOps(seq);
    def par(threshold: Int) = new ParallelSeqOps(seq,threshold);
  }

  implicit def parallelArrayOps[T](seq: Array[T]) = new {
    def par = new ParallelSeqOps(seq);
    def par(threshold: Int) = new ParallelSeqOps(seq,threshold);
  }

  /**
   * The class that houses all the parallel ops. Usually created by the parallelOps implicit.
   */
  final class ParallelSeqOps[T](seq: IndexedSeq[T], threshold: Int=16) {

    /**
     * Specifies a new threshold at which point the operation is calculated sequentially. Default is 16.
     */
    def withSequentialThreshold(threshold: Int) = new ParallelSeqOps(seq,threshold);
    
    /**
     * A very general operation. For each chunk, a new "id" is created, and sequentialFold is used to fold that chunk,
     * then the folds are combined using finalFold. No order is guaranteed in how the folds happen.
     */
    def fold[B](id: =>B)(sequentialFold: (B,T)=>B)(finalFold: (B,B)=>B):B = {
      val action = new BinaryRecursiveAction(finalFold, { (start:Int,end:Int) =>
          seq.view(start, end).foldLeft(id)(sequentialFold)
        });
      DefaultPool.invokeAndGet(action);
    }

    /**
     * Executes a simple reduce in parallel. No guarantee on order.
     */
    def reduce[B>:T](f: (B,B)=>B):B = {
      val action = new BinaryRecursiveAction(f, { (start:Int,end:Int) =>
          seq.view(start, end).reduceLeft(f);
      });
      DefaultPool.invokeAndGet(action);
    }

    def map[U](f: T=>U) = {
     val action = new BinaryRecursiveAction[IndexedSeq[U]](_ ++ _, { (start: Int, end: Int) =>
       val r = seq.view(start,end).map(f).toIndexedSeq
       r
     });
     DefaultPool.invokeAndGet(action);
    }

    def flatMap[U](f: T=>Traversable[U]) = {
      val action = new BinaryRecursiveAction[IndexedSeq[U]](_ ++ _, { (start:Int, end:Int) =>
        seq.view(start,end).flatMap(f).toIndexedSeq
      });
      DefaultPool.invokeAndGet(action);
    }

    /**
     * Transforms each element, and then reduces them using r.
     */
    def mapReduce[U,B>:U](f: T=>U, r: (B,B)=>B) = {
      val action = new BinaryRecursiveAction(r, { (start:Int,end:Int) =>
          seq.view(start, end).iterator.map(f).reduceLeft(r);
        });
      DefaultPool.invokeAndGet(action);
    }

    private class BinaryRecursiveAction[T](val reduce: (T, T) => T, val executeSequentially: (Int, Int) => T,
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
