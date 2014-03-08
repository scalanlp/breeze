package breeze.linalg.support

import breeze.macros.arityize

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
  def foreach[X](row : R, fn : ((Int,V) => X))
  def length(row : R) : Int
}

object LiteralRow {
  implicit def array[V] : LiteralRow[Array[V],V] = new LiteralRow[Array[V],V] {
    def foreach[X](arr : Array[V], fn : ((Int,V) => X)) = {
      for (i <- 0 until arr.length) {
        fn(i, arr(i))
      }
    }

    def length(arr : Array[V]) = arr.length
  }

  implicit def vLiteral[V<:AnyVal] : LiteralRow[V,V] = new LiteralRow[V,V] {
    def foreach[X](tup : V, fn : ((Int,V) => X)) = {
      fn(0, tup)
    }

    def length(tup : V) = 1
  }

//  @arityize(22)
//  implicit def tuple[V] : LiteralRow[Tuple[V @arityize.repeat] @arityize.relative(tuple),V] = new LiteralRow[Tuple[V @arityize.repeat] @arityize.relative(tuple),V] {
//    def foreach[X](tup : Tuple[V @arityize.repeat] @arityize.relative(tuple), fn : ((Int,V) => X)) = {
//      for( (v, i) <- tup.productIterator.zipWithIndex) {
//        fn(i, v.asInstanceOf[V])
//      }
//    }
//
//    def length(tup : Tuple[V @arityize.repeat] @arityize.relative(tuple)) = __order__
//  }
}
