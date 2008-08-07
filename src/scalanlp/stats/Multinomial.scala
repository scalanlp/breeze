package scalanlp.stats;
import scalanlp.counters.Counters._;
import scalanlp.counters._;

/**
 * Represents a multinomial Distribution over elements.
 */
trait Multinomial[T] extends Distribution[T] {
  def elements : Iterator[T];
  protected def total : Double;

  def get = {
    var prob = Rand.uniform.get() * total;
    if(prob.isNaN) {
      println("You got a NaN!");
    }
    var elems = elements;
    var e = elems.next;
    prob  = prob - unnormalizedProbabilityOf(e);
    while(prob > 0) {
      e  = elems.next;
      prob = prob - (unnormalizedProbabilityOf(e));
    }
    e
  }

  /**
   * Returns a Multinomial(x) \propto  this(x) * that(x);
   */
  def *[U>:T](that : Multinomial[U]) = {
    val c :DoubleCounter[U] = aggregate(that.elements.map(x => (x,that.probabilityOf(x))))
    for(x <- elements
      if c.keys contains x;
      p = probabilityOf(x))
        c(x) *= p;
    Multinomial(c);    
  }
}

/** 
 * Provides routines to create Multinomial
 * @author(dlwh)
 */
object Multinomial {

  /**
   * Returns a Multinomial where the probability of each element in the counter
   * is proportional to its count.
   */
  def apply[T](c : DoubleCounter[T])  = new Multinomial[T] {
    def total = c.total;
    def elements = c.keys;
    def probabilityOf(t : T) = c(t)/c.total();
    override def unnormalizedProbabilityOf(t: T) = c(t);
  }

  /**
   * Returns a Multinomial where the probability of each element in the counter
   * is proportional to its count.
   */
  def fromCounter[T](c:DoubleCounter[T]) = apply(c);

  /**
   * Returns a Multinomial where the probability is proportional to a(i)
   */
  def apply(a : Array[Double]) : Multinomial[Int] = apply(a,a.foldLeft(0.0)(_+_));

  /**
   * Returns a Multinomial where the probability is proportional to a(i).
   * Takes the total for speed.
   */
  def apply(arr : Array[Double], t: Double) = new Multinomial[Int] {
    def elements = (0 until arr.length ). elements;
    def total = t;
    def probabilityOf(x : Int) = arr(x)/t
    override def unnormalizedProbabilityOf(x: Int) = arr(x);
  }
}
