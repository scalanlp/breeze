package breeze.linalg.support
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
trait LiteralRow[-R, @specialized V] {
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

  implicit def tuple2[V] : LiteralRow[Tuple2[V,V],V] = new LiteralRow[Tuple2[V,V],V] {
    def foreach[X](tup : Tuple2[V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1)
      fn(1, tup._2)
    }

    def length(tup : Tuple2[V,V]) = 2
  }


  implicit def tuple3[V] : LiteralRow[Tuple3[V,V,V],V] = new LiteralRow[Tuple3[V,V,V],V] {
    def foreach[X](tup : Tuple3[V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1)
      fn(1, tup._2)
      fn(2, tup._3)
    }

    def length(tup : Tuple3[V,V,V]) = 3
  }


  implicit def tuple4[V] : LiteralRow[Tuple4[V,V,V,V],V] = new LiteralRow[Tuple4[V,V,V,V],V] {
    def foreach[X](tup : Tuple4[V,V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1)
      fn(1, tup._2)
      fn(2, tup._3)
      fn(3, tup._4)
    }

    def length(tup : Tuple4[V,V,V,V]) = 4
  }


  implicit def tuple5[V] : LiteralRow[Tuple5[V,V,V,V,V],V] = new LiteralRow[Tuple5[V,V,V,V,V],V] {
    def foreach[X](tup : Tuple5[V,V,V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1)
      fn(1, tup._2)
      fn(2, tup._3)
      fn(3, tup._4)
      fn(4, tup._5)
    }

    def length(tup : Tuple5[V,V,V,V,V]) = 5
  }


  implicit def tuple6[V] : LiteralRow[Tuple6[V,V,V,V,V,V],V] = new LiteralRow[Tuple6[V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple6[V,V,V,V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1)
      fn(1, tup._2)
      fn(2, tup._3)
      fn(3, tup._4)
      fn(4, tup._5)
      fn(5, tup._6)
    }

    def length(tup : Tuple6[V,V,V,V,V,V]) = 6
  }


  implicit def tuple7[V] : LiteralRow[Tuple7[V,V,V,V,V,V,V],V] = new LiteralRow[Tuple7[V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple7[V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1)
      fn(1, tup._2)
      fn(2, tup._3)
      fn(3, tup._4)
      fn(4, tup._5)
      fn(5, tup._6)
      fn(6, tup._7)
    }

    def length(tup : Tuple7[V,V,V,V,V,V,V]) = 7
  }


  implicit def tuple8[V] : LiteralRow[Tuple8[V,V,V,V,V,V,V,V],V] = new LiteralRow[Tuple8[V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple8[V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1)
      fn(1, tup._2)
      fn(2, tup._3)
      fn(3, tup._4)
      fn(4, tup._5)
      fn(5, tup._6)
      fn(6, tup._7)
      fn(7, tup._8)
    }

    def length(tup : Tuple8[V,V,V,V,V,V,V,V]) = 8
  }


  implicit def tuple9[V] : LiteralRow[Tuple9[V,V,V,V,V,V,V,V,V],V] = new LiteralRow[Tuple9[V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple9[V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
      fn(0, tup._1)
      fn(1, tup._2)
      fn(2, tup._3)
      fn(3, tup._4)
      fn(4, tup._5)
      fn(5, tup._6)
      fn(6, tup._7)
      fn(7, tup._8)
      fn(8, tup._9)
    }

    def length(tup : Tuple9[V,V,V,V,V,V,V,V,V]) = 9
  }


  implicit def tuple10[V] : LiteralRow[Tuple10[V,V,V,V,V,V,V,V,V,V],V] = new LiteralRow[Tuple10[V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple10[V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
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
    }

    def length(tup : Tuple10[V,V,V,V,V,V,V,V,V,V]) = 10
  }


  implicit def tuple11[V] : LiteralRow[Tuple11[V,V,V,V,V,V,V,V,V,V,V],V] = new LiteralRow[Tuple11[V,V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple11[V,V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
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
    }

    def length(tup : Tuple11[V,V,V,V,V,V,V,V,V,V,V]) = 11
  }


  implicit def tuple12[V] : LiteralRow[Tuple12[V,V,V,V,V,V,V,V,V,V,V,V],V] = new LiteralRow[Tuple12[V,V,V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple12[V,V,V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
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
    }

    def length(tup : Tuple12[V,V,V,V,V,V,V,V,V,V,V,V]) = 12
  }


  implicit def tuple13[V] : LiteralRow[Tuple13[V,V,V,V,V,V,V,V,V,V,V,V,V],V] = new LiteralRow[Tuple13[V,V,V,V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple13[V,V,V,V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
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
    }

    def length(tup : Tuple13[V,V,V,V,V,V,V,V,V,V,V,V,V]) = 13
  }


  implicit def tuple14[V] : LiteralRow[Tuple14[V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] = new LiteralRow[Tuple14[V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple14[V,V,V,V,V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
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
    }

    def length(tup : Tuple14[V,V,V,V,V,V,V,V,V,V,V,V,V,V]) = 14
  }


  implicit def tuple15[V] : LiteralRow[Tuple15[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] = new LiteralRow[Tuple15[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple15[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
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
    }

    def length(tup : Tuple15[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V]) = 15
  }


  implicit def tuple16[V] : LiteralRow[Tuple16[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] = new LiteralRow[Tuple16[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple16[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
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
    }

    def length(tup : Tuple16[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V]) = 16
  }


  implicit def tuple17[V] : LiteralRow[Tuple17[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] = new LiteralRow[Tuple17[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple17[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
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
    }

    def length(tup : Tuple17[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V]) = 17
  }


  implicit def tuple18[V] : LiteralRow[Tuple18[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] = new LiteralRow[Tuple18[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple18[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
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
    }

    def length(tup : Tuple18[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V]) = 18
  }


  implicit def tuple19[V] : LiteralRow[Tuple19[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] = new LiteralRow[Tuple19[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple19[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
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
    }

    def length(tup : Tuple19[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V]) = 19
  }


  implicit def tuple20[V] : LiteralRow[Tuple20[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] = new LiteralRow[Tuple20[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple20[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
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
    }

    def length(tup : Tuple20[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V]) = 20
  }


  implicit def tuple21[V] : LiteralRow[Tuple21[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] = new LiteralRow[Tuple21[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple21[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
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
    }

    def length(tup : Tuple21[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V]) = 21
  }


  implicit def tuple22[V] : LiteralRow[Tuple22[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] = new LiteralRow[Tuple22[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V],V] {
    def foreach[X](tup : Tuple22[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V], fn : ((Int,V) => X)) = {
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

    def length(tup : Tuple22[V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V,V]) = 22
  }
}
