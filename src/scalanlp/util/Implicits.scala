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
}
