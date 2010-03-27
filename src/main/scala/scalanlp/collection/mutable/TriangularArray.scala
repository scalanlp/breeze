package scalanlp.collection.mutable

import scala.collection.mutable.Seq

class TriangularArray[T:ClassManifest](dim: Int, fill: =>T) { outer =>
  private val numElems = dim * (dim+1) / 2
  private val data = Array.fill(numElems)(fill);

  @inline
  private def index(r: Int, c: Int) = {
    if(r <= c) require(r <= c, "row must be less than column!");
    (c * (c-1) /2 + r);
  }

  def update(r: Int, c: Int, t: T) { data(index(r,c))  = t }

  def apply(r: Int, c: Int)  = data(index(r,c));

  def apply(r: Int) = slice(r);


  private def slice(r: Int):Seq[T] = new Seq[T] {
    def apply(c: Int) = outer.apply(r,c);
    def update(c: Int, t: T) = outer.update(r,c,t);
    def length = (dim - r);
    def iterator = Iterator.range(r,dim).map(apply _ );
  }

  def iterator = Iterator.range(0,numElems) map slice;
  def foreach(f: T=>Unit) { data foreach f }

  override def toString = {
    val buffer = new StringBuilder();
    for ( r <- 0 until dim )  {
      val columns = for(c <- 0 until dim) yield {
        if(c < r) "----" else apply(r,c).toString
      }
      buffer ++= columns.mkString("[",", ","]\n");
    }
    buffer.toString();
  }
}
