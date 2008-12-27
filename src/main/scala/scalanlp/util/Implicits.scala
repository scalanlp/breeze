package scalanlp.util;

/**
* Useful implicit conversions that Scala forgot.
* 
* @author(dlwh)
*/
object Implicits {
  
  //
  // Exploding arrays
  //
    
  implicit def explode2[E](s : Array[E]) : (E,E) = {
    assert(s.length == 2)
    return (s(0),s(1))
  }
  implicit def explode3[E](s : Array[E]) : (E,E,E) = {
    assert(s.length == 3)
    return (s(0),s(1),s(2))
  }
  implicit def explode4[E](s : Array[E]) : (E,E,E,E) = {
    assert(s.length == 4)
    return (s(0),s(1),s(2),s(3))
  }
  implicit def explode5[E](s : Array[E]) : (E,E,E,E,E) = {
    assert(s.length == 5)
    return (s(0),s(1),s(2),s(3),s(4))
  }
  implicit def explode6[E](s : Array[E]) : (E,E,E,E,E,E) = {
    assert(s.length == 6)
    return (s(0),s(1),s(2),s(3),s(4),s(5))
  }
  
  //
  // Extra convenience methods on Scala builtins
  //
  
  class ListExtra[T](list:List[T]) {
    def tails = new Seq[List[T]] {
      def length = list.length;
      def apply(i : Int) = list.drop(i);
      def elements = new Iterator[List[T]] {
        private var n = list;
        def next = {
          val ret = n;
          n = ret.tail;
          ret
        }

        def hasNext = (n!=Nil)
      }
    }
  }

  implicit def listExtras[T](list : List[T]) = new ListExtra(list);

  class StringExtras(s : String) {
    /**
    * Implements Levenshtein Distance. 
    */
    def editDistance(s2 : String) = {
      if(s.length == 0) s2.length;
      else if(s2.length ==0) s.length;
      else {
        val d = new Array[Array[Int]](s.length+1,s2.length+1);
        for(i <- 0 to s.length)
          d(i)(0) = i;
        for(i <- 0 to s2.length)
          d(0)(i) = i;
        for(i <- 1 to s.length;
          j <- 1 to s2.length) {
            val cost = if(s(i-1) == s2(j-1)) 0 else 1;
            d(i)(j) = Math.min( d(i-1)(j)+1, Math.min(d(i)(j-1)+1,d(i-1)(j-1) + cost));
        }
        d(s.length)(s2.length);
      }
    }
  }
  implicit def stringExtras(s : String) = new StringExtras(s);

  implicit def tExtras[T<:AnyRef](t : T) = new {
    def ?:[U>:T](u: =>U) = if(t eq null) u else t;
    def intern() = Interner(t).asInstanceOf[T];
  }
}
