package scalanlp.util;

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

import scala.collection.mutable.ArrayBuffer;


/**
* This class breaks the compiler. Taking it out to see what breaks.
* Useful implicit conversions and other utilities.
* 
* @author dlwh 
object Implicits extends Asserts {
  
  //
  // Extra convenience methods on Scala builtins
  //
  
  class ListExtra[T](list:List[T]) {
    def tails = new Seq[List[T]] {
      def length = list.length;
      def apply(i : Int) = list.drop(i);
      def iterator = new Iterator[List[T]] {
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

  implicit def anyExtras[T](a:T) = new AnyExtras[T](a);

  class AnyExtras[T](x: T) {
    /**
    * Unfold creates a list starting from a seed value. It's meant to be the 
    * opposite of List.foldr in that if there is an "inverse" of f,
    * that has some stopping criterion, then we have something like
    * list reduceRight ( f) unfoldr inversef == list
    */
    def unfoldr[R](f: T=>Option[(R,T)]): Seq[R] = {
      var result = new ArrayBuffer[R];
      var current = f(x);
      while( current != None ) {
        val next = current.asInstanceOf[Some[(R,T)]].get._2;
        val r = current.asInstanceOf[Some[(R,T)]].get._1;
        result += r;
        current = f(next);
      }
      result.reverse;
    }
  }

  implicit def doubleExtras(d: Double) = new {
    def =~=(e: Double) = d ==e ||  Math.abs(d - e)/d < 1E-4;
  }

}

*/