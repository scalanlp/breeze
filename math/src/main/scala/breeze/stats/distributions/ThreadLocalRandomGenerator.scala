package breeze.stats.distributions

import org.apache.commons.math3.random.RandomGenerator

/**
 * An Apache-compatible [[RandomGenerator]] that creates a new RandomGenerator per thread. The thunk should
 * be thread-safe, using atomics or something.
 *
 * @author dlwh
 **/
@SerialVersionUID(1L)
class ThreadLocalRandomGenerator(genThunk: => RandomGenerator) extends RandomGenerator with Serializable {
  @transient private lazy val genTL = new ThreadLocal[RandomGenerator] with Serializable {
    override def initialValue(): RandomGenerator = genThunk
  }
  def nextBytes(bytes: Array[Byte]) = genTL.get().nextBytes(bytes)

  def setSeed(seed: Long) = genTL.get().setSeed(seed)

  def setSeed(seed: Array[Int]) = genTL.get().setSeed(seed)

  def setSeed(seed: Int) = genTL.get().setSeed(seed)

  def nextInt(): Int = genTL.get().nextInt()

  def nextInt(n: Int): Int = genTL.get().nextInt(n)

  def nextLong(): Long = genTL.get().nextLong()

  def nextBoolean(): Boolean = genTL.get().nextBoolean()

  def nextFloat(): Float = genTL.get().nextFloat()

  def nextDouble(): Double = genTL.get().nextDouble()

  def nextGaussian(): Double = genTL.get().nextGaussian()
}
