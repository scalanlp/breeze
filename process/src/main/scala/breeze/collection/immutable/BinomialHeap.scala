package breeze.collection.immutable;

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






import collection.mutable.{Builder, GrowingBuilder, AddingBuilder}
import collection.{GenTraversableOnce, IterableLike}
import collection.generic.{CanBuildFrom, Growable, Addable}
import breeze.util.Iterators
;

/**
 * From Okasaki's Functional Data Structures. Represents a functional heap
 *
 * @author dlwh
 */
class BinomialHeap[T<%Ordered[T]] extends Iterable[T] with IterableLike[T,BinomialHeap[T]] with Serializable {
  import BinomialHeap._;
  protected val trees: List[Node[T]] = Nil;
  override val size = 0;

  def +(x : T) = mkHeap(insertTree(Node(0,x,Nil),trees),size+1);
  private def insertTree(n : Node[T], t : List[Node[T]]) : List[Node[T]] = {
    if (t.isEmpty) List(n);
    else if(n.rank < t.head.rank) n :: t;
    else insertTree( n.link(t.head),t.tail);
  }

  def ++(other : BinomialHeap[T]) = mkHeap(merge(trees,other.trees,Nil),size + other.size);
  // TODO: make somewhat tail recursive
  private def merge(l1 : List[Node[T]], l2 : List[Node[T]], acc : List[Node[T]]) : List[Node[T]] = (l1,l2) match {
    case (Nil,l2) => acc.reverse ++ l2;
    case (l1,Nil) => acc.reverse ++ l1
    case (n1 :: r1 , n2 :: r2 )  =>
      if(n1.rank < n2.rank) merge(r1,l2, n1 :: acc)
      else if (n2.rank < n1.rank) merge(l1,r2, n2 :: acc);
      else insertTree(n1 link n2, merge(r1,r2,acc));
  }

  def min = get.get;

  protected override def newBuilder = new Builder[T,BinomialHeap[T]] {
    var heap = BinomialHeap.empty[T]

    def result() = heap;

    def clear() = heap = BinomialHeap.empty[T]

    def +=(elem: T) =  {heap += elem; this}
  }

  lazy val get = if(trees.isEmpty) None else Some(findMin(trees));
  private def findMin(trees : List[Node[T]]): T = {
    trees match {
      case (t :: Nil) => t.x
      case (t :: ts) =>
        val x = t.x;
        val y = findMin(ts);
        if(x < y) x else y;
      case _ => throw new IllegalArgumentException("Shouldn't get Nil!");
    }
  }

  def delMin() = {
    if(trees.isEmpty) this;
    else {
      def getMin(t : List[Node[T]]) : (Node[T],List[Node[T]]) = t match {
        case (n :: Nil) => (n,Nil);
        case (n :: ts) => {
          val (n2,ts2) = getMin(ts);
          if(n.x <= n2.x) (n,ts) else (n2,n :: ts2);
        }
        case _ => throw new IllegalArgumentException("Shouldn't get Nil!");
      }
      val (Node(_,x,t1),t2) = getMin(trees);
      merge(t1.reverse,t2,Nil);
      mkHeap (merge(t1.reverse,t2,Nil), size -1);
    }
  }

  private val comp = {(x : T, y :T) => x compare y}
  def iterator :Iterator[T] = Iterators.merge( (trees map treeIterator):_*)(comp)

  private def treeIterator(n : Node[T]) : Iterator[T] = {
    Iterators.merge((Iterator.single(n.x) :: (n.children map treeIterator)):_*)(comp);
  }

  override def toString() = iterator.mkString("Heap(",",",")");
}

object BinomialHeap {
  protected case class Node[T<%Ordered[T]](rank : Int, x : T, children: List[Node[T]]) {
    def link(n :Node[T]) = {
      if(x <= n.x) Node(rank+1,x,n :: children) else Node(rank+1,n.x,this :: n.children)
    }
  }

  def empty[T<%Ordered[T]] : BinomialHeap[T] = new BinomialHeap[T] {
    override val trees = Nil;
  }

  private def mkHeap[T<%Ordered[T]](ns : List[Node[T]],sz : Int) = new BinomialHeap[T] {
    override val trees = ns;
    override val size = sz;
  }

  def apply[T<%Ordered[T]](t:T*):BinomialHeap[T] = empty[T] ++ t;

  implicit def cbfForBinomialHeap[T<:B,B<%Ordered[B]]: CanBuildFrom[BinomialHeap[T],B,BinomialHeap[B]] = new CanBuildFrom[BinomialHeap[T],B,BinomialHeap[B]]{
    def apply():Builder[B,BinomialHeap[B]] = {
      empty[B].newBuilder;
    }

    def apply(from: BinomialHeap[T]) = apply()
  }
}
