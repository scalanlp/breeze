package scalanlp.collection.immutable;

import  scala.collection.mutable.Stack; // For iterator

/**
* From Okasaki's Functional Data Structures. Represents a functional heap
*/
@serializable
class BinomialHeap[T<%Ordered[T]] {
  import BinomialHeap._;
  protected val trees: List[Node[T]] = Nil;

  def +(x : T) = mkHeap(insertTree(Node(0,x,Nil),trees));
  private def insertTree(n : Node[T], t : List[Node[T]]) : List[Node[T]] = {
    println(n + " " + t);
    if (t.isEmpty) List(n);
    else if(n.rank < t.head.rank) n :: t;
    else insertTree( n.link(t.head),t.tail);
  }

  def ++(other : BinomialHeap[T]) = mkHeap(merge(trees,other.trees));
  // TODO: make tail recursive
  private def merge(l1 : List[Node[T]], l2 : List[Node[T]]) : List[Node[T]] = (l1,l2) match {
    case (Nil,l2) => l2;
    case (l1,Nil) => l1
    case (n1 :: r1 , n2 :: r2 )  => 
      if(n1.rank < n2.rank) n1 :: merge(r1,l2)
      else if (n2.rank < n1.rank) n2 :: merge(l1,r2);
      else insertTree(n1 link n2, merge(r1,r2));
  }

  def ++(other: Iterator[T]): BinomialHeap[T] = other.foldLeft(this)(_+_);
  def ++(other: Iterable[T]): BinomialHeap[T] = this ++ other.elements

  lazy val min = if(trees.isEmpty) None else Some(findMin(trees));
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

  def deleteMin() = {
    if(trees.isEmpty) this;
    else mkHeap {
      def getMin(t : List[Node[T]]) : (Node[T],List[Node[T]]) = t match {
        case (n :: Nil) => (n,Nil);
        case (n :: ts) => {
          val (n2,ts2) = getMin(ts);
          if(n.x <= n2.x) (n,ts) else (n2,n :: ts2);
        }
        case _ => throw new IllegalArgumentException("Shouldn't get Nil!");
      }
      val (Node(_,x,t1),t2) = getMin(trees);
      merge(t1.reverse,t2);
    }
  }

  def debug = trees.toString;

  def elements = new Iterator[T] { 
    private var state = trees.elements;
    def next : T = {
      val n = state.next;
      n.children match {
        case Nil => n.x; 
        case s => state = state ++ s.elements; n.x;
      }
    }
    def hasNext = state.hasNext;
  }

  override def toString() = elements.mkString("Heap(",",",")");
}

object BinomialHeap {
  private case class Node[T<%Ordered[T]](rank : Int, x : T, children: List[Node[T]]) {
    def link(n :Node[T]) = {
      if(x <= n.x) Node(rank+1,x,n::children) else Node(rank+1,n.x,this :: n.children)
    }
  }

  def empty[T<%Ordered[T]] : BinomialHeap[T] = new BinomialHeap[T] {
    override val trees = Nil;
  }

  private def mkHeap[T<%Ordered[T]](ns : List[Node[T]]) = new BinomialHeap[T] {
    override val trees = ns;
  }
}
