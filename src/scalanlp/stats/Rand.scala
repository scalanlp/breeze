package scalanlp.stats;

trait Rand[T] {
  def get() : T

  def sample(n : Int) = List.tabulate(n,x => get);

  def flatMap[E](f : T => Rand[E] ) =  {
    def x = f(get());
    new Rand[E] {
      def get = x.get;
    }
  }

  def map[E](f : T=>E) =  { 
    def x = f(get());
    new Rand[E] {
      def get = x;
    }
  }

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

object Rand {
  def choose[T](c :Collection[T]) = { d : Double =>
    val sz = d * c.size;
    val elems = c.elements;
    var i = 1;
    var e = elems.next;
    while(i < sz) {
      e = elems.next;
      i += 1;
    }
    e
  }

  def always[T](t : T) = new Rand[T] {
    def get = t;
  }

  val uniform = new Rand[Double] {
    private val r = new Random;
    def get = r.nextDouble;
  }

  val randInt = new Rand[Int] {
    private val r = new Random;
    def get = r.nextInt;
  }

  def randInt(n : Int) = new Rand[Int] {
    private val r = new Random;
    def get = r.nextInt(n);
  }

  val gaussian :Rand[Double] = new Rand[Double] {
    private val r = new java.util.Random;
    def get = r.nextGaussian;
  }

  def gaussian(m : Double, s : Double) :Rand[Double] = new Rand[Double] {
    def get = m + s * gaussian.get
  }

}




