package breeze.collection.mutable
/*
 Copyright 2010 David Hall, Daniel Ramage

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/



import scala.collection.mutable.Seq

/**
 * A TriangularArray is a jagged 2-d array where for every row r,
 * we have an array of size dim - r.
 *
 * These are useful for parse charts.
 *
 * @author dlwh
 * @param dim: The size of the array
 * @param fill: what initial value should we put in the array
 */
final class TriangularArray[T:ClassManifest](dim: Int, fill: (Int,Int)=>T) { outer =>
  import TriangularArray._

  def this(dim: Int, fill: => T) = this(dim, (_,_)=>fill)
  private def numElems = dim * (dim+1) / 2
  val data = new Array[T](numElems)
  for( c <- 0 until dim; r <- 0 to c) {
     data(index(r,c)) = fill(r,c)
  }

  def update(r: Int, c: Int, t: T) { data(index(r,c))  = t }

  @inline
  def apply(r: Int, c: Int)  = data(index(r,c))

  @inline
  def apply(r: Int) = slice(r)

  private def slice(r: Int):Seq[T] = new Seq[T] {
    def apply(c: Int) = outer.apply(r,c)
    def update(c: Int, t: T) = outer.update(r,c,t)
    def length = (dim - r)
    def iterator = Iterator.range(r,dim).map(apply _ )
  }

  def iterator = Iterator.range(0,numElems) map slice
  def foreach(f: T=>Unit) { data foreach f }

  override def toString = {
    val buffer = new StringBuilder()
    for ( r <- 0 until dim )  {
      val columns = for(c <- 0 until dim) yield {
        if(c <= r) "----" else apply(r,c).toString
      }
      buffer ++= columns.mkString("[",", ","]\n")
    }
    buffer.toString()
  }
}

object TriangularArray {
  @inline
  def index(r: Int, c: Int) = {
    if(r > c) require(r <= c, "row must be less than column!")
    (c * (c+1) /2 + r)
  }

  def raw[T:ClassManifest](dim: Int, fill: =>T) = {
    val numElems = arraySize(dim)
    val data = Array.fill[T](numElems)(fill)
    data
  }

  def arraySize(dim: Int): Int = {
    dim * (dim + 1) / 2
  }
}