package scalanlp.math

trait Semiring[@specialized(Double) T] {
  def plus(t1: T, t2: T):T ;
//  def sum(t: Iterable) = t.reduceLeft(plus _);
  def times(t1: T, t2: T): T;
  val zero : T
  val one: T
  // Not a real semiring operation, but we need it
  def closeTo(t1: T, t2: T): Boolean = t1 == t2;

  /**
   * This is a complex function that I wouldn't add if
   * it weren't so necessary. If t1 is non-zero, the function may
   * reuse t1's storage to do the addition inplace, and
   * not at all of closeTo(t1,plus(t1,t2)) would return true.
   *
   * @return something equivalent to ( plus(t1,t2),closeTo(t1,plus(t1,t2))
   */
  def maybe_+=(t1: T, t2: T): (T,Boolean) = {
    val res = plus(t1,t2);
    (res,closeTo(t1,res))
  }

  /**
  * Also not a guaranteed property, but we need it for most ops.
  * Should be equiv to \sum_{k=1}^\infty t^k.
  */
  def closure(t: T):T
}

/**
 * Models a Weakly-Left-Divisible Semiring, which means that
 * there is some operation leftDivide s.t. leftDivide(z,times(z,x)) = x
 */
trait WLDSemiring[T] extends Semiring[T] {
  /**
  * @return z^-1 times x
  */
  def leftDivide(z: T, x: T): T;
}

object Semiring {

  def apply[W:Semiring]() = implicitly[Semiring[W]];

  implicit val booleanSemiring = new Semiring[Boolean] {
    def plus(t1: Boolean, t2: Boolean) = t1 || t2;
    def times(t1:Boolean, t2: Boolean) = t1 && t2;
    def closure(t: Boolean) = true;
    val one = true;
    val zero = false;
  }

  object Probability {
    implicit val semiring = new ProbSemiring;
    class ProbSemiring extends WLDSemiring[Double] {
      def plus(t1: Double, t2: Double) = t1 + t2;
      def times(t1: Double, t2: Double) = t1 * t2;
      def leftDivide(t1: Double, t2: Double) = t2 / t1;

      override def closeTo(x: Double, y: Double) = {
        if(x == y) true
        else if(x == 0) math.abs(y)  < 1E-10;
        else math.abs( (x-y)/x)  < 1E-8;
      }

      def closure(t: Double) = {
        if(t < 1 && t >= 0) 1 / (1-t);
        else if(t < 0) error("Closure arg must be in [0,1), not "  +t);
        else Double.PositiveInfinity;
      }
      val one = 1.0;
      val zero = 0.0;
    }
  }
  
  implicit val doubleIsDivSemiring = new DoubleSemi;
  
  class DoubleSemi extends WLDSemiring[Double] {
    def plus(t1: Double, t2: Double) = t1 + t2;
    def times(t1: Double, t2: Double) = t1 * t2;
    def leftDivide(t1: Double, t2: Double) = t2 / t1;
    def closure(t: Double) = if(t >= 1) Double.PositiveInfinity else 1/(1-t);
    val one = 1.0
    val zero = 0.0;
  }

  /**
   * Provides access to the tropical algebra. The implicit is segregated because it conflicts with numericIsSemiring
   */
  object Tropical {
    implicit val doubleIsTropical:WLDSemiring[Double] = new WLDSemiring[Double] {
      def plus(t1: Double, t2: Double) = t1 min t2;
      def leftDivide(t1: Double, t2: Double) = t2 - t1;
      def times(t1: Double, t2: Double) = t1 + t2;
      override def closeTo(x: Double, y: Double) = {
        if(x == y) true
        else if(x == 0) math.abs(y)  < 1E-10;
        else math.abs( (x-y)/x)  < 1E-8;
      }
      def closure(t: Double) = if(t >= 0.0) 0.0 else Double.NegativeInfinity;
      val one = 0.0;
      val zero = Double.PositiveInfinity;
    }
  }

  /**
   * Provides access to the viterbi semiring. The implicit is segregated because it conflicts with numericIsSemiring
   */
  object Viterbi {
    implicit val doubleIsViterbi:WLDSemiring[Double] = new WLDSemiring[Double] {
      def plus(t1: Double, t2: Double) = t1 max t2;
      def leftDivide(t1: Double, t2: Double) = t2 - t1;
      def times(t1: Double, t2: Double) = t1 + t2;
      override def closeTo(x: Double, y: Double) = {
        if(x == y) true
        else if(x == 0) math.abs(y)  < 1E-10;
        else math.abs( (x-y)/x)  < 1E-8;
      }
      def closure(t: Double) = if(t <= 0.0) 0.0 else Double.NegativeInfinity;
      val one = 0.0;
      val zero = Double.NegativeInfinity;
    }
  }
  
  /**
   * Provides access to the logspace algebra. The implicit is segregated because it conflicts with numericIsSemiring
   */
  object LogSpace {
    implicit val doubleIsLogSpace:WLDSemiring[Double] = new WLDSemiring[Double] {
      def plus(t1: Double, t2: Double) = logSum(t1,t2);
      //override def sum(t: Iterable[Double]) =
      def leftDivide(t1: Double, t2: Double) = t2 - t1;
      def times(t1: Double, t2: Double) = if(t1 == Double.NegativeInfinity || t2 == Double.NegativeInfinity) Double.NegativeInfinity else t1 + t2;
      val one = 0.0;
      val zero = Double.NegativeInfinity;
      override def closeTo(x: Double, y: Double) = {
        if(x == y) true
        else if(x.abs < 1E-7) math.abs(y)  < 1E-7;
        else math.abs( (x-y)/x)  < 1E-5;
      }
      /**
      * p =&gt; 1/(1-p)
      * becomes
      * t =&lt; log(1/(1-exp(t))) = -log(1-exp(t));
      */ 
      def closure(t: Double) = {
        val t_* = -math.log(1 - math.exp(t));
        if(t_*.isNaN)
          throw new ArithmeticException("Cannot compute the log-closure of a quantity > 0, but got: " + t);
        t_*
      }

      /**
       * Sums together things in log space.
       * @return log(exp(a) + exp(b))
       */
      def logSum(a : Double, b : Double) = {
        import math._
        if(a == Double.NegativeInfinity) b
        else if (b == Double.NegativeInfinity) a
        else if(a < b) b + log(1 + exp(a-b))
        else a + log(1+exp(b-a));
      }

    }

  }
  
}
