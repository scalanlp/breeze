package scalanlp.stats;
trait Distribution[T] extends Rand[T] {
  def probabilityOf(t : T) :Double;
  def logProbabilityOf(t : T) : Double = Math.log(probabilityOf(t));
  // it can be normalized; it just need not be.
  def unnormalizedLogProbabilityOf(t :T) : Double = logProbabilityOf(t);
  def unnormalizedProbabilityOf(t :T) : Double = probabilityOf(t);
}

