package scalanlp.stats;
import scala.collection.mutable.ArrayBuffer;

/**
 * A trait for monadic sampling. Provides support for use in for-comprehensions
 * @author(dlwh)
 */
trait Rand[+T] { outer : Rand[T] =>
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
   * An infinitely long iterator that samples repeatedly from the Rand
   * @return an iterator that repeatedly samples
   */
  def samples:Iterator[T] = new Iterator[T] {
    def hasNext() = true;
    def next() = get();
  }

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

  // Not the most efficient implementation ever, but meh.
  def condition(p : T => Boolean) = new Rand[T] {
    def get() = {
      var x = outer.get;
      while(!p(x)) {
        x = outer.get;
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

  def choose[T](c : Seq[T]) = Rand.randInt(c.size).map( c(_));


  /**
   * The trivial random generator: always returns the argument
   */
  def always[T](t : T) = new Rand[T] {
    def get = t;
  }

  
  /**
  * Simply reevaluate the body every time get is called
  */ 
  def fromBody[T](f : =>T) = new Rand[T] {
    def get = f;
  }

  /**
  * Convert a Collection of Rand[T] into a Rand[Collection[T]]
  */
  def promote[U](col : Collection[Rand[U]]) = fromBody(col.map(_.get));
  /**
  * Convert an Iterable of Rand[T] into a Rand[Iterable[T]]
  */
  def promote[U](col : Iterable[Rand[U]]) = fromBody(col.map(_.get));
  /**
  * Convert a List of Rand[T] into a Rand[List[T]]
  */
  def promote[U](col : List[Rand[U]]) = fromBody(col.map(_.get));
  /**
  * Convert an Array of Rand[T] into a Rand[Array[T]]
  */
  def promote[U](col : Array[Rand[U]]) = fromBody(col.map(_.get));

  def promote[T1,T2](t : (Rand[T1],Rand[T2])) = fromBody( (t._1.get,t._2.get));
  def promote[T1,T2,T3](t : (Rand[T1],Rand[T2],Rand[T3])) = fromBody( (t._1.get,t._2.get,t._3.get));
  def promote[T1,T2,T3,T4](t : (Rand[T1],Rand[T2],Rand[T3],Rand[T4])) = 
    fromBody( (t._1.get,t._2.get,t._3.get,t._4.get));

  // Time-seeded random generators should be shared.
  private val r = new java.util.Random;
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

  /**
   * Implements the Knuth shuffle
   */
  def permutation(n : Int) = new Rand[RandomAccessSeq[Int]] {
    def get = {
      val arr = new ArrayBuffer[Int]();
      arr ++= (0 until n);
      var i = n;
      while(i > 1) {
        val k = r.nextInt(i);
        i -= 1;
        val tmp = arr(i);
        arr(i) = arr(k);
        arr(k) = tmp;
      }
      arr;
    }
  }

}
