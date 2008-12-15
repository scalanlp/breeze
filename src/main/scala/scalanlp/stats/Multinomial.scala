package scalanlp.stats;
import scalanlp.counters.Counters._;
import scalanlp.counters._;

import scalanlp.util.Log;

/**
 * Represents a multinomial Distribution over elements.
 */
trait Multinomial[T] extends Distribution[T] {
  protected def components : scala.collection.Map[T,Double];
  protected def total : Double;

  // check rep
  for ((k,v) <- components.elements) {
    if (v < 0) {
      throw new IllegalArgumentException("Multinomial has negative mass at index "+k);
    }
  }
  
  def get = {
    var prob = Rand.uniform.get() * total;
    if(prob.isNaN) {
      Log(Log.ERROR)("You got a NaN!");
    }
    val elems = components.elements;
    var (e,w) = elems.next;
    prob  = prob - w;
    while(prob > 0) {
      val t  = elems.next;
      e = t._1;
      prob = prob - t._2;
    }
    e
  }

  /**
   * Returns a Multinomial(x) \propto  this(x) * that(x);
   */
  def *[U>:T](that : Multinomial[U]) = {
    val c :DoubleCounter[U] = aggregate(that.components);
    for((x,w) <- components
      if c.get(x) != None)
        c(x) *= w/ (c.total * total);
    Multinomial(c);    
  }

  def probabilityOf(e : T) = components.apply(e) / total;
  override def unnormalizedProbabilityOf(e:T) = components.apply(e);
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
    if(total.isNaN || total <= 0.) throw new IllegalArgumentException("total is " + total);
    protected def components = c;
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
    def components = new scala.collection.Map[Int,Double] {
      def elements = arr.elements.zipWithIndex.map{ case (x,y) => (y,x)}
      def get(x : Int) = if(x < arr.length) Some(arr(x)) else None;
      def size = arr.length;
    }
    def total = t;
  }
}
