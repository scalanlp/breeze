package scalanlp.stats;
import scalanlp.counters.Counters._;
import scalanlp.counters._;

trait Multinomial[T] extends Distribution[T] {
  def elements : Iterator[T];
  protected def total : Double;
  protected val r : Rand[Double]

  def get = {
    var prob = r.get() * total;
    var elems = elements;
    var e = elems.next;
    prob  = prob - unnormalizedProbabilityOf(e);
    while(prob > 0) {
      e  = elems.next;
      prob = prob - (unnormalizedProbabilityOf(e));
    }
    e
  }

  def *[U>:T](that : Multinomial[U]) = {
    val c :DoubleCounter[U] = aggregate(that.elements.map(x => (x,that.probabilityOf(x))))
    for(x <- elements
      if c.keys contains x;
      p = probabilityOf(x))
        c(x) *= p;
    Multinomial(c);    
  }
}

object Multinomial {
  def fromCounter[T](c:DoubleCounter[T]) = apply(c);
  def apply[T](rx : Rand[Double], c : DoubleCounter[T])  = new Multinomial[T] {
    val r = rx;
    def total = c.total;
    def elements = c.keys;
    def probabilityOf(t : T) = c(t)/c.total();
    override def unnormalizedProbabilityOf(t:T) = c(t);
  }



  def apply(a : Array[Double]) : Multinomial[Int] = new Multinomial[Int] {
    val r= Rand.uniform;
    val total = a.foldLeft(0.0)(_+_);
    val mult = a.map(_ / total).force;
    def elements = (0 until a.length).elements
    def apply( i : Int) = mult(i);
    def probabilityOf(t : Int) = a(t)/total;
  }

  def apply[T](c : DoubleCounter[T])  : Multinomial[T]= this(Rand.uniform,c);

  def apply(arr : Array[Double], t: Double) = new Multinomial[Int]{
    def elements = (0 until arr.length ). elements;
    def total = t;
    protected val r = Rand.uniform;
    def probabilityOf(x : Int) = arr(x)/t
    override def unnormalizedProbabilityOf(x:Int) = arr(x);
  }
}
