package breeze.linalg.support

import breeze.macros.arityize
import breeze.math.Complex
import scala.reflect.api.TypeTags.TypeTag

//import scala.reflect.ClassTag
import scala.reflect.runtime.universe.

/*
 Copyright 2012 Daniel Ramage

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

/**
 * Marker trait for a row literal used in Matrix construction.
 *
 * @author dramage
 */
trait LiteralRow[R, @specialized V] {
  def foreach[X](row : R, fn : ((Int,V) => Unit))
  def length(row : R) : Int

  //used for tuple row construction, with widening of type Complex=5 > Double=4 > Float=3 > Long=2 > Int=1
  def maxElementType(row: R): TypeTag[_]
}

object LiteralRow {
  implicit def array[V] : LiteralRow[Array[V],V] = new LiteralRow[Array[V],V] {
    def foreach[X](arr : Array[V], fn : ((Int,V) => Unit)) = {
      for (i <- 0 until arr.length) {
        fn(i, arr(i))
      }
    }

    def length(arr : Array[V]) = arr.length

    def maxElementType(arr : Array[V]) = elementTypeClassTag(elementTypeInteger(arr(0)))

  }

  implicit def vLiteral[V<:AnyVal] : LiteralRow[V,V] = new LiteralRow[V,V] {
    def foreach[X](tup : V, fn : ((Int,V) => Unit)) = {
      fn(0, tup)
    }

    def length(tup : V) = 1

    def maxElementType(tup : V) = elementTypeClassTag(elementTypeInteger(tup))

  }

  //Following used for the (0, 1, 2, 3).v style of DenseVector initialization
  @arityize(22)
  implicit def tuple[V] : LiteralRow[Tuple[V @arityize.repeat] @arityize.relative(tuple),V] =
    new LiteralRow[Tuple[V @arityize.repeat] @arityize.relative(tuple),V] {
      def foreach[X](tup : Tuple[V @arityize.repeat] @arityize.relative(tuple),
                     fn : ((Int,V) => X)) = {
        for( (v, i) <- tup.productIterator.zipWithIndex) {
          fn(i, v.asInstanceOf[V])
        }
      }

      def length(tup : Tuple[V @arityize.repeat] @arityize.relative(tuple)) = __order__

      def maxElementType(tup : Tuple[V @arityize.repeat] @arityize.relative(tuple)) = elementTypeInteger(tup._1)
    }

  //Following used for the (0, 1, 2.5, 3).v style of DenseVector initialization
  // takes TupleNN[Any] to widen all elements to lowest common type Complex > Double > Float > Long > Int
  // using V<:AnyVal would exclude construction of DenseVector[Complex]'s
  @arityize(22)
  implicit def tuple[Any] : LiteralRow[Tuple[Any @arityize.repeat] @arityize.relative(tuple), Any] =
    new LiteralRow[Tuple[Any @arityize.repeat] @arityize.relative(tuple),V] {

      def foreach[X](tup : Tuple[Any @arityize.repeat] @arityize.relative(tuple),
                     fn : ((Int, X) => Unit))(implicit tag: TypeTag[X]) = {
        tag match {
          case typeTag[Complex] =>
        }
        if( Complex.isInstanceOf[X] ){
          for( (v, i) <- tup.productIterator.zipWithIndex) {
            v match {
              case v0: Complex => fn(i, v0)
              case v0 => Complex(v0.asInstanceOf[Double], 0d)
            }
          }
        } else {
          for( (v, i) <- tup.productIterator.zipWithIndex) {
            fn(i, v.asInstanceOf[X])
          }
        }
      }

      def length(tup : Tuple[Any @arityize.repeat] @arityize.relative(tuple)) = __order__

      def maxElementType(tup : Tuple[Any @arityize.repeat] @arityize.relative(tuple)) = {
        var maxTypeInt = elementTypeInteger(tup._1)
        //var allSame = true //whether all types in the tuple are the same (eg for dealing with Complex)
        //cycle through the tuple once, to find the widest variable type
        for( (v, i) <- tup.productIterator.zipWithIndex.drop(1)) {
          val typeInt = elementTypeInteger(v)
          if( typeInt > maxTypeInt ){
            maxTypeInt = typeInt
            //allSame = false
          }
        }

        elementTypeClassTag(maxTypeInt)
      }

    }

  //used to find the maximum element type
  private def elementTypeInteger(x: Any): Int = x match {
    case x0: Complex => 5
    case x0: Double => 4
    case x0: Float => 3
    case x0: Long => 2
    case x0: Int => 1
    case x0 => throw new IllegalArgumentException("Tried to initialize DenseVector with non-standard element of type " + x0.getClass.getName )
  }

  //get a ClassTag once the maximum element type is found
  private def elementTypeClassTag(typeInt: Int): TypeTag[_] = typeInt match {
    case 5 => scala.reflect.runtime.universe.typeTag[Complex]
    case 4 => scala.reflect.runtime.universe.typeTag[Double]
    case 3 => scala.reflect.runtime.universe.typeTag[Float]
    case 2 => scala.reflect.runtime.universe.typeTag[Long]
    case 1 => scala.reflect.runtime.universe.typeTag[Int]
    case x => throw new IllegalArgumentException("Type integer " + x +" is not valid" )
  }

}
