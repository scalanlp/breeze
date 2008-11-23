package scalanlp.collection.immutable;

import util.ScalaQL;

/**
* From Okasaki's Functional Data Structures. Represents a functional heap
*/
@serializable
class BinomialHeap[T<%Ordered[T]] extends Collection[T] {
  import BinomialHeap._;
  protected val trees: List[Node[T]] = Nil;
  val size = 0;

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

  def ++(other: Iterator[T]): BinomialHeap[T] = other.foldLeft(this)(_+_);
  def ++(other: Iterable[T]): BinomialHeap[T] = this ++ other.elements

  def min = get.get;

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
  def elements :Iterator[T] = ScalaQL.merge( (trees map treeIterator):_*)(comp) 

  private def treeIterator(n : Node[T]) : Iterator[T] = {
    ScalaQL.merge((Iterator.single(n.x) :: (n.children map treeIterator)):_*)(comp);
  }

  override def toString() = elements.mkString("Heap(",",",")");
}

object BinomialHeap {
  private case class Node[T<%Ordered[T]](rank : Int, x : T, children: List[Node[T]]) {
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

  def apply[T<%Ordered[T]](t:T*) = empty[T] ++ t;
}
