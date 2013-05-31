package breeze.optimize

import breeze.stats.distributions.Rand

/**
* A diff function that supports subsets of the data. By default it evaluates on all the data
*/
trait BatchDiffFunction[T] extends DiffFunction[T] with ((T,IndexedSeq[Int])=>Double) { outer =>
  /**
  * Calculates the gradient of the function on a subset of the data
  */
  def gradientAt(x:T, batch: IndexedSeq[Int]) : T = calculate(x,batch)._2;
  /**
  * Calculates the value of the function on a subset of the data
  */
  def valueAt(x:T, batch: IndexedSeq[Int]) : Double = calculate(x,batch)._1
  /**
  * Calculates the value and gradient of the function on a subset of the data;
  */
  def calculate(x:T, batch: IndexedSeq[Int]): (Double,T)

  override def calculate(x:T):(Double,T) = calculate(x,fullRange);
  override def valueAt(x:T):Double = valueAt(x,fullRange)
  override def gradientAt(x:T):T = gradientAt(x,fullRange)

  def apply(x:T, batch:IndexedSeq[Int]) = valueAt(x,batch);

  /**
  * The full size of the data
  */
  def fullRange: IndexedSeq[Int];

  def withRandomBatches(size: Int):StochasticDiffFunction[T] = new StochasticDiffFunction[T] {
    val rand = Rand.subsetsOfSize(fullRange,size)
    def calculate(x: T) = outer.calculate(x, rand.get)
  }

  def withScanningBatches(size: Int):StochasticDiffFunction[T] = new StochasticDiffFunction[T] {
    var lastStop = 0
    def nextBatch = synchronized {
      val start = lastStop
      lastStop += size
      lastStop %= fullRange.size
      Array.tabulate(size)(i => fullRange((i+start)%fullRange.size))
    }

    def calculate(x: T) = outer.calculate(x, nextBatch)
  }
}