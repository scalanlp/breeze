package scalanlp.util;

/**
* Useful implicit conversions that Scala forgot.
*/
object Implicits {
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
}
