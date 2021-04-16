package breeze.linalg.support

import breeze.linalg.DenseVector

/*
 Copyright 2012 Daniel Ramage, 2021 David Hall

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
  def foreach[X](row: R, fn: (Int, V) => X): Unit
  def length(row: R): Int
}

object LiteralRow {
  implicit def array[V]: LiteralRow[Array[V], V] = new LiteralRow[Array[V], V] {
    def foreach[X](arr: Array[V], fn: (Int, V) => X): Unit = {
      for (i <- 0 until arr.length) {
        fn(i, arr(i))
      }
    }

    def length(arr: Array[V]) = arr.length
  }

  implicit def dv[V]: LiteralRow[DenseVector[V], V] = new LiteralRow[DenseVector[V], V] {
    def foreach[X](arr: DenseVector[V], fn: (Int, V) => X): Unit = {
      for (i <- 0 until arr.length) {
        fn(i, arr(i))
      }
    }

    def length(arr: DenseVector[V]) = arr.length
  }

  implicit def seq[V, S](implicit ev: S <:< Seq[V]): LiteralRow[S, V] = new LiteralRow[S, V] {
    def foreach[X](arr: S, fn: ((Int, V) => X)): Unit = {
      for (i <- 0 until arr.length) {
        fn(i, arr(i))
      }
    }

    def length(arr: S) = arr.length
  }

  implicit def vLiteral[V <: AnyVal]: LiteralRow[V, V] = new LiteralRow[V, V] {
    def foreach[X](tup: V, fn: ((Int, V) => X)) = {
      fn(0, tup)
    }

    def length(tup: V) = 1
  }

  // TODO: replace this with appropriate inline defs.
  implicit def tuple2[V]: LiteralRow[(V, V), V] =
    new LiteralRow[(V, V), V] {
        def foreach[X](tup: (V, V), fn: ((Int, V) => X)) = {
          fn(0, tup._1)
          fn(1, tup._2)
        }
        def length(tup: Tuple[V]) = 2
    }

  implicit def tuple3[V]: LiteralRow[(V, V, V), V] =
    new LiteralRow[(V, V, V), V] {
      def foreach[X](tup: (V, V, V), fn: ((Int, V) => X)) = {
        fn(0, tup._1)
        fn(1, tup._2)
        fn(2, tup._2)
      }
      def length(tup: Tuple[V]) = 3
    }

  implicit def tuple4[V]: LiteralRow[(V, V, V, V), V] =
    new LiteralRow[(V, V, V, V), V] {
      def foreach[X](tup: (V, V, V, V), fn: ((Int, V) => X)) = {
        fn(0, tup._1)
        fn(1, tup._2)
        fn(2, tup._2)
        fn(3, tup._4)
      }
      def length(tup: Tuple[V]) = 4
    }

  implicit def tuple5[V]: LiteralRow[(V, V, V, V, V), V] =
    new LiteralRow[(V, V, V, V, V), V] {
      def foreach[X](tup: (V, V, V, V, V), fn: ((Int, V) => X)) = {
        fn(0, tup._1)
        fn(1, tup._2)
        fn(2, tup._2)
        fn(3, tup._4)
        fn(4, tup._5)
      }
      def length(tup: Tuple[V]) = 5
    }

  implicit def tuple6[V]: LiteralRow[(V, V, V, V, V, V), V] =
    new LiteralRow[(V, V, V, V, V, V), V] {
      def foreach[X](tup: (V, V, V, V, V, V), fn: ((Int, V) => X)) = {
        fn(0, tup._1)
        fn(1, tup._2)
        fn(2, tup._2)
        fn(3, tup._4)
        fn(4, tup._5)
        fn(5, tup._6)
      }
      def length(tup: Tuple[V]) = 6
    }

  implicit def tuple7[V]: LiteralRow[(V, V, V, V, V, V, V), V] =
    new LiteralRow[(V, V, V, V, V, V, V), V] {
      def foreach[X](tup: (V, V, V, V, V, V, V), fn: ((Int, V) => X)) = {
        fn(0, tup._1)
        fn(1, tup._2)
        fn(2, tup._2)
        fn(3, tup._4)
        fn(4, tup._5)
        fn(5, tup._6)
        fn(6, tup._7)
      }
      def length(tup: Tuple[V]) = 7
    }

  implicit def tuple8[V]: LiteralRow[(V, V, V, V, V, V, V, V), V] =
    new LiteralRow[(V, V, V, V, V, V, V, V), V] {
      def foreach[X](tup: (V, V, V, V, V, V, V, V), fn: ((Int, V) => X)) = {
        fn(0, tup._1)
        fn(1, tup._2)
        fn(2, tup._2)
        fn(3, tup._4)
        fn(4, tup._5)
        fn(5, tup._6)
        fn(6, tup._7)
        fn(7, tup._8)
      }
      def length(tup: Tuple[V]) = 8
    }

  implicit def tuple9[V]: LiteralRow[(V, V, V, V, V, V, V, V, V), V] =
    new LiteralRow[(V, V, V, V, V, V, V, V, V), V] {
      def foreach[X](tup: (V, V, V, V, V, V, V, V, V), fn: ((Int, V) => X)) = {
        fn(0, tup._1)
        fn(1, tup._2)
        fn(2, tup._2)
        fn(3, tup._4)
        fn(4, tup._5)
        fn(5, tup._6)
        fn(6, tup._7)
        fn(7, tup._8)
        fn(8, tup._9)
      }
      def length(tup: Tuple[V]) = 9
    }

  implicit def tuple10[V]: LiteralRow[(V, V, V, V, V, V, V, V, V, V), V] =
    new LiteralRow[(V, V, V, V, V, V, V, V, V, V), V] {
      def foreach[X](tup: (V, V, V, V, V, V, V, V, V, V), fn: ((Int, V) => X)) = {
        fn(0, tup._1)
        fn(1, tup._2)
        fn(2, tup._2)
        fn(3, tup._4)
        fn(4, tup._5)
        fn(5, tup._6)
        fn(6, tup._7)
        fn(7, tup._8)
        fn(8, tup._9)
        fn(9, tup._10)
      }
      def length(tup: Tuple[V]) = 10
    }

  implicit def tuple11[V]: LiteralRow[(V, V, V, V, V, V, V, V, V, V, V), V] =
    new LiteralRow[(V, V, V, V, V, V, V, V, V, V, V), V] {
      def foreach[X](tup: (V, V, V, V, V, V, V, V, V, V, V), fn: ((Int, V) => X)) = {
        fn(0, tup._1)
        fn(1, tup._2)
        fn(2, tup._2)
        fn(3, tup._4)
        fn(4, tup._5)
        fn(5, tup._6)
        fn(6, tup._7)
        fn(7, tup._8)
        fn(8, tup._9)
        fn(9, tup._10)
        fn(10, tup._11)
      }
      def length(tup: Tuple[V]) = 11
    }

  implicit def tuple12[V]: LiteralRow[(V, V, V, V, V, V, V, V, V, V, V, V), V] =
    new LiteralRow[(V, V, V, V, V, V, V, V, V, V, V, V), V] {
      def foreach[X](tup: (V, V, V, V, V, V, V, V, V, V, V, V), fn: ((Int, V) => X)) = {
        fn(0, tup._1)
        fn(1, tup._2)
        fn(2, tup._2)
        fn(3, tup._4)
        fn(4, tup._5)
        fn(5, tup._6)
        fn(6, tup._7)
        fn(7, tup._8)
        fn(8, tup._9)
        fn(9, tup._10)
        fn(10, tup._11)
        fn(11, tup._12)
      }
      def length(tup: Tuple[V]) = 12
    }

  implicit def tuple13[V]: LiteralRow[(V, V, V, V, V, V, V, V, V, V, V, V, V), V] =
    new LiteralRow[(V, V, V, V, V, V, V, V, V, V, V, V, V), V] {
      def foreach[X](tup: (V, V, V, V, V, V, V, V, V, V, V, V, V), fn: ((Int, V) => X)) = {
        fn(0, tup._1)
        fn(1, tup._2)
        fn(2, tup._2)
        fn(3, tup._4)
        fn(4, tup._5)
        fn(5, tup._6)
        fn(6, tup._7)
        fn(7, tup._8)
        fn(8, tup._9)
        fn(9, tup._10)
        fn(10, tup._11)
        fn(11, tup._12)
        fn(12, tup._13)
      }
      def length(tup: Tuple[V]) = 13
    }

  implicit def tuple14[V]: LiteralRow[(V, V, V, V, V, V, V, V, V, V, V, V, V, V), V] =
    new LiteralRow[(V, V, V, V, V, V, V, V, V, V, V, V, V, V), V] {
      def foreach[X](tup: (V, V, V, V, V, V, V, V, V, V, V, V, V, V), fn: ((Int, V) => X)) = {
        fn(0, tup._1)
        fn(1, tup._2)
        fn(2, tup._2)
        fn(3, tup._4)
        fn(4, tup._5)
        fn(5, tup._6)
        fn(6, tup._7)
        fn(7, tup._8)
        fn(8, tup._9)
        fn(9, tup._10)
        fn(10, tup._11)
        fn(11, tup._12)
        fn(12, tup._13)
        fn(13, tup._14)
      }
      def length(tup: Tuple[V]) = 14
    }

  implicit def tuple15[V]: LiteralRow[(V, V, V, V, V, V, V, V, V, V, V, V, V, V, V), V] =
    new LiteralRow[(V, V, V, V, V, V, V, V, V, V, V, V, V, V, V), V] {
      def foreach[X](tup: (V, V, V, V, V, V, V, V, V, V, V, V, V, V, V), fn: ((Int, V) => X)) = {
        fn(0, tup._1)
        fn(1, tup._2)
        fn(2, tup._2)
        fn(3, tup._4)
        fn(4, tup._5)
        fn(5, tup._6)
        fn(6, tup._7)
        fn(7, tup._8)
        fn(8, tup._9)
        fn(9, tup._10)
        fn(10, tup._11)
        fn(11, tup._12)
        fn(12, tup._13)
        fn(13, tup._14)
        fn(14, tup._15)
      }
      def length(tup: Tuple[V]) = 15
    }

  implicit def tuple16[V]: LiteralRow[(V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V), V] =
    new LiteralRow[(V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V), V] {
      def foreach[X](tup: (V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V), fn: ((Int, V) => X)) = {
        fn(0, tup._1)
        fn(1, tup._2)
        fn(2, tup._2)
        fn(3, tup._4)
        fn(4, tup._5)
        fn(5, tup._6)
        fn(6, tup._7)
        fn(7, tup._8)
        fn(8, tup._9)
        fn(9, tup._10)
        fn(10, tup._11)
        fn(11, tup._12)
        fn(12, tup._13)
        fn(13, tup._14)
        fn(14, tup._15)
        fn(15, tup._16)
      }
      def length(tup: Tuple[V]) = 16
    }

  implicit def tuple17[V]: LiteralRow[(V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V), V] =
    new LiteralRow[(V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V), V] {
      def foreach[X](tup: (V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V), fn: ((Int, V) => X)) = {
        fn(0, tup._1)
        fn(1, tup._2)
        fn(2, tup._2)
        fn(3, tup._4)
        fn(4, tup._5)
        fn(5, tup._6)
        fn(6, tup._7)
        fn(7, tup._8)
        fn(8, tup._9)
        fn(9, tup._10)
        fn(10, tup._11)
        fn(11, tup._12)
        fn(12, tup._13)
        fn(13, tup._14)
        fn(14, tup._15)
        fn(15, tup._16)
        fn(16, tup._17)
      }
      def length(tup: Tuple[V]) = 17
    }

  implicit def tuple18[V]: LiteralRow[(V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V), V] =
    new LiteralRow[(V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V), V] {
      def foreach[X](tup: (V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V), fn: ((Int, V) => X)) = {
        fn(0, tup._1)
        fn(1, tup._2)
        fn(2, tup._2)
        fn(3, tup._4)
        fn(4, tup._5)
        fn(5, tup._6)
        fn(6, tup._7)
        fn(7, tup._8)
        fn(8, tup._9)
        fn(9, tup._10)
        fn(10, tup._11)
        fn(11, tup._12)
        fn(12, tup._13)
        fn(13, tup._14)
        fn(14, tup._15)
        fn(15, tup._16)
        fn(16, tup._17)
        fn(17, tup._18)
      }
      def length(tup: Tuple[V]) = 18
    }

  implicit def tuple19[V]: LiteralRow[(V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V), V] =
    new LiteralRow[(V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V), V] {
      def foreach[X](tup: (V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V), fn: ((Int, V) => X)) = {
        fn(0, tup._1)
        fn(1, tup._2)
        fn(2, tup._2)
        fn(3, tup._4)
        fn(4, tup._5)
        fn(5, tup._6)
        fn(6, tup._7)
        fn(7, tup._8)
        fn(8, tup._9)
        fn(9, tup._10)
        fn(10, tup._11)
        fn(11, tup._12)
        fn(12, tup._13)
        fn(13, tup._14)
        fn(14, tup._15)
        fn(15, tup._16)
        fn(16, tup._17)
        fn(17, tup._18)
        fn(18, tup._19)
      }
      def length(tup: Tuple[V]) = 19
    }

  implicit def tuple20[V]: LiteralRow[(V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V), V] =
    new LiteralRow[(V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V), V] {
      def foreach[X](tup: (V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V), fn: ((Int, V) => X)) = {
        fn(0, tup._1)
        fn(1, tup._2)
        fn(2, tup._2)
        fn(3, tup._4)
        fn(4, tup._5)
        fn(5, tup._6)
        fn(6, tup._7)
        fn(7, tup._8)
        fn(8, tup._9)
        fn(9, tup._10)
        fn(10, tup._11)
        fn(11, tup._12)
        fn(12, tup._13)
        fn(13, tup._14)
        fn(14, tup._15)
        fn(15, tup._16)
        fn(16, tup._17)
        fn(17, tup._18)
        fn(18, tup._19)
        fn(19, tup._20)
      }
      def length(tup: Tuple[V]) = 20
    }

  implicit def tuple21[V]: LiteralRow[(V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V), V] =
    new LiteralRow[(V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V), V] {
      def foreach[X](tup: (V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V), fn: ((Int, V) => X)) = {
        fn(0, tup._1)
        fn(1, tup._2)
        fn(2, tup._2)
        fn(3, tup._4)
        fn(4, tup._5)
        fn(5, tup._6)
        fn(6, tup._7)
        fn(7, tup._8)
        fn(8, tup._9)
        fn(9, tup._10)
        fn(10, tup._11)
        fn(11, tup._12)
        fn(12, tup._13)
        fn(13, tup._14)
        fn(14, tup._15)
        fn(15, tup._16)
        fn(16, tup._17)
        fn(17, tup._18)
        fn(18, tup._19)
        fn(19, tup._20)
        fn(20, tup._21)
      }
      def length(tup: Tuple[V]) = 21
    }

  implicit def tuple22[V]: LiteralRow[(V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V), V] =
    new LiteralRow[(V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V), V] {
      def foreach[X](tup: (V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V, V), fn: ((Int, V) => X)) = {
        fn(0, tup._1)
        fn(1, tup._2)
        fn(2, tup._3)
        fn(3, tup._4)
        fn(4, tup._5)
        fn(5, tup._6)
        fn(6, tup._7)
        fn(7, tup._8)
        fn(8, tup._9)
        fn(9, tup._10)
        fn(10, tup._11)
        fn(11, tup._12)
        fn(12, tup._13)
        fn(13, tup._14)
        fn(14, tup._15)
        fn(15, tup._16)
        fn(16, tup._17)
        fn(17, tup._18)
        fn(18, tup._19)
        fn(19, tup._20)
        fn(20, tup._21)
        fn(21, tup._22)
      }
      def length(tup: Tuple[V]) = 22
    }



}
