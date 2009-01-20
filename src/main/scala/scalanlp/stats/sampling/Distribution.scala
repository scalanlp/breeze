package scalanlp.stats.sampling;

/**
 * Represents an unnormalized probability distribution.
 * 
 * @author(dlwh)
 */
trait Measure[T] extends Rand[T] with (T=>Double) {
  /** Returns the unnormalized value of the measure*/
  def apply(x:T): Double;
  /** Returns the log unnormalized value of the measure*/
  def logApply(x:T): Double = apply(x);
}

/**
 * Represents a continuous Distribution.
 * Why T? just in case.
 * @author dlwh
 */
trait ContinuousDistr[T] extends Measure[T] {
  /** Returns the probability density function at that point.*/
  def pdf(x: T): Double; 
  def logPdf(x:T): Double = Math.log(pdf(x));
  /** Returns the probability density function up to a constant at that point.*/
  def unnormalizedPdf(x:T): Double = pdf(x);
  def unnormalizedLogPdf(x:T): Double = Math.log(unnormalizedPdf(x));
  def logNormalizer : Double;
  
  def apply(x:T) = unnormalizedPdf(x);
  override def logApply(x:T) = unnormalizedLogPdf(x);
}

/**
 * Represents a discrete Distribution.
 * @author dlwh
 */
trait DiscreteDistr[T] extends Measure[T] {
  /** Returns the probability of that draw. */
  def probabilityOf(x: T): Double; 
  def logProbabilityOf(x:T): Double = Math.log(probabilityOf(x));
  /** Returns the probability of that draw up to a constant */
  def unnormalizedProbabilityOf(x:T): Double = probabilityOf(x);
  def unnormalizedLogProbabilityOf(x:T): Double = Math.log(unnormalizedProbabilityOf(x));
  
  def apply(x:T) = unnormalizedProbabilityOf(x);
  override def logApply(x:T) = unnormalizedLogProbabilityOf(x);
}
