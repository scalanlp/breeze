package scalanlp.stats.distributions;

/*
 Copyright 2009 David Hall, Daniel Ramage
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/

import scala.collection.mutable.ArrayBuffer
import scalanlp.stats.random.{MersenneTwister, RandomGenerator}

/**
 * A trait for monadic distributions. Provides support for use in for-comprehensions
 * @author(dlwh)
 */
trait Rand[+T] { outer =>
  /**
   * Gets one sample from the distribution. Equivalent to sample()
   */
  def draw() : T
  
  def get() = draw();

  // Overridden by filter/map/flatmap for monadic invocations
  protected def drawOpt():Option[T] = Some(draw());

  /**
   * Gets one sample from the distribution. Equivalent to get()
   */
  def sample() = get();

  /**
   * Gets n samples from the distribution. 
   */
  def sample(n : Int) = IndexedSeq.fill(n)(draw());

  /**
   * An infinitely long iterator that samples repeatedly from the Rand
   * @return an iterator that repeatedly samples
   */
  def samples:Iterator[T] = new Iterator[T] {
    def hasNext = true;
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
  def flatMap[E](f : T => Rand[E] ):Rand[E] = new Rand[E] {
    def draw() = Iterator.continually(f(outer.draw()).drawOpt()).find(_.nonEmpty).get.get;
    override def drawOpt() = outer.drawOpt().flatMap(t => f(t).drawOpt())
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
  def map[E](f : T=>E):Rand[E] =  {
    new Rand[E] {
      def draw() = f(outer.get());
      override def drawOpt() = outer.drawOpt().map(f);
    }
  }

  /**
   * Samples one element and qpplies the provided function to it.
   * Despite the name, the function is applied once. Sample usage:
   * <pre> for(x &lt;- Rand.uniform) { println(x) } </pre>
   * 
   * @param f the function to be applied
   */
  def foreach(f : T=>Unit) = f(get());

  def filter(p: T=>Boolean) = condition(p);

  def withFilter(p: T=>Boolean) = condition(p);

  // Not the most efficient implementation ever, but meh.
  def condition(p : T => Boolean):Rand[T] = new Rand[T] {
    def draw() = {
      var x = outer.get();
      while(!p(x)) {
        x = outer.get();
      }
      x
    }

    override def drawOpt() = {
      Some(outer.get()).filter(p);
    }
  }

}

/**
* Provides standard combinators and such to use
* to compose new Rands.
*/ 
class RandBasis(r: RandomGenerator) {
  /**
   * Chooses an element from a collection. 
   */
  def choose[T](c: Iterable[T]) = new Rand[T] { 
    def draw() = {
      val sz = uniform.get * c.size;
      val elems = c.iterator;
      var i = 1;
      var e = elems.next();
      while(i < sz) {
        e = elems.next();
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
    def draw = t;
  }

  
  /**
  * Simply reevaluate the body every time get is called
  */ 
  def fromBody[T](f : =>T) = new Rand[T] {
    def draw = f;
  }

  /**
  * Convert a Collection of Rand[T] into a Rand[Collection[T]]
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

  /**
  * Convert an Seq of Rand[T] into a Rand[Seq[T]]
  */
  def promote[U](col : Seq[Rand[U]]) = fromBody(col.map(_.get));

  def promote[T1,T2](t : (Rand[T1],Rand[T2])) = fromBody( (t._1.get,t._2.get));
  def promote[T1,T2,T3](t : (Rand[T1],Rand[T2],Rand[T3])) = fromBody( (t._1.get,t._2.get,t._3.get));
  def promote[T1,T2,T3,T4](t : (Rand[T1],Rand[T2],Rand[T3],Rand[T4])) = 
    fromBody( (t._1.get,t._2.get,t._3.get,t._4.get));

  /**
   * Uniformly samples in [0,1]
   */
  val uniform = new Rand[Double] {
    def draw = r.nextDouble;
  }

  /**
   * Uniformly samples an integer in [0,MAX_INT]
   */
  val randInt = new Rand[Int] {
    def draw = r.nextInt;
  }

  /**
   * Uniformly samples an integer in [0,n)
   */
  def randInt(n : Int) = new Rand[Int] {
    def draw = r.nextInt(n);
  }

  /**
   * Uniformly samples an integer in [n,m)
   */
  def randInt(n : Int, m: Int) = new Rand[Int] {
    def draw = r.nextInt(m-n)+n;
  }

  /**
   * Samples a gaussian with 0 mean and 1 std
   */
  val gaussian :Rand[Double] = new Rand[Double] {
    def draw = r.nextGaussian;
  }

  /**
   * Samples a gaussian with m mean and s std
   */
  def gaussian(m : Double, s : Double): Rand[Double] = new Rand[Double] {
    def draw = m + s * gaussian.get
  }

  /**
   * Implements the Knuth shuffle of numbers from 0 to n.
   */
  def permutation(n : Int) = new Rand[IndexedSeq[Int]] {
    def draw = {
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

  /**
   * Knuth shuffle of a subset of size n from a set
   */
  def subsetsOfSize(set: IndexedSeq[Int], n: Int) = new Rand[IndexedSeq[Int]] {
    def draw = {
      val arr = set.toArray
      var i = 0
      while( i < n.min(set.size)) {
        val k = r.nextInt(set.size-i) + i
        val temp = arr(i)
        arr(i) = arr(k)
        arr(k) = temp
        i+=1
      }
      arr.take(n)
    }
  }
}

/**
 * Provides a number of random generators.
 */
object Rand extends RandBasis(new MersenneTwister());
