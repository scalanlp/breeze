package scalanlp.stats;

/**
 * A trait for monadic sampling. Provides support for use in for-comprehensions
 * @author(dlwh)
 */
trait Rand[T] { outer : Rand[T] =>
  /**
   * Gets one sample from the distribution. Equivalent to sample()
   */
  def get() : T

  /**
   * Gets one sample from the distribution. Equivalent to get()
   */
  def sample() = get();

  /**
   * Gets n samples from the distribution. 
   */
  def sample(n : Int) = List.tabulate(n,x => get);

  /**
   * Converts a random sampler of one type to a random sampler of another type.
   * Examples:
   * randInt(10).flatMap(x => randInt(3 * x.asInstanceOf[Int]) gives a Rand[Int] in the range [0,30]
   * Equivalently, for(x &lt;- randInt(10); y &lt;- randInt(30 *x)) yield y;
   * 
   * @param f the transform to apply to the sampled value.
   *
   */
  def flatMap[E](f : T => Rand[E] ) =  {
    new Rand[E] {
      def get = f(outer.get).get
    }
  }

  /**
   * Converts a random sampler of one type to a random sampler of another type.
   * Examples:
   * uniform.map(_*2) gives a Rand[Double] in the range [0,2]
   * Equivalently, for(x &lt;- uniform) yield 2*x;
   * 
   * @param f the transform to apply to the sampled value.
   *
   */
  def map[E](f : T=>E) =  { 
    new Rand[E] {
      def get = f(outer.get);
    }
  }

  /**
   * Samples one element and qpplies the provided function to it.
   * Despite the name, the function is applied once. Sample usage:
   * <pre> for(x &lt;- Rand.uniform) { println(x) } </pre>
   * 
   * @param f the function to be applied
   */
  def foreach(f : T=>Unit) = f(get);

  def filter(p: T=>Boolean) = condition(p);

  // Not the most efficient implementation efver, but meh.
  def condition(p : T => Boolean) = new Rand[T] {
    def get() = {
      var x = get;
      while(!p(x)) {
        x = get;
      }
      x
    }
  }

}

/**
 * Provides a number of random generators.
 */
object Rand {
  /**
   * Chooses an element from a collection. 
   */
  def choose[T](c :Collection[T]) = new Rand[T] { 
    def get() = {
      val sz = uniform.get * c.size;
      val elems = c.elements;
      var i = 1;
      var e = elems.next;
      while(i < sz) {
        e = elems.next;
        i += 1;
      }
      e
    }
  }

  private val r = new java.util.Random;

  /**
   * The trivial random generator: always returns itself
   */
  def always[T](t : T) = new Rand[T] {
    def get = t;
  }

  /**
   * Uniformly samples in [0,1]
   */
  val uniform = new Rand[Double] {
    def get = r.nextDouble;
  }

  /**
   * Uniformly samples an integer in [0,MAX_INT]
   */
  val randInt = new Rand[Int] {
    def get = r.nextInt;
  }

  /**
   * Uniformly samples an integer in [0,n]
   */
  def randInt(n : Int) = new Rand[Int] {
    def get = r.nextInt(n);
  }

  /**
   * Samples a gaussian with 0 mean and 1 std
   */
  val gaussian :Rand[Double] = new Rand[Double] {
    def get = r.nextGaussian;
  }

  /**
   * Samples a gaussian with m mean and s std
   */
  def gaussian(m : Double, s : Double) :Rand[Double] = new Rand[Double] {
    def get = m + s * gaussian.get
  }

}
