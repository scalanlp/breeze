package breeze.numerics

/*
 Copyright 2012 David Hall

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

/**
 *
 * @author dlwh
 */
object IntMath {

  /**
   * Computes base to the exp'th power for Ints by repeated squaring.
   * @param base
   * @param exp
   * @return
   */
  def ipow(base: Int, exp: Int): Int = {
    var b = base
    if (exp < 0 && base != 1) 0
    else {
      var e = exp
      var result = 1
      while (e != 0) {
        if ((e & 1) != 0)
          result *= b
        e >>= 1
        b *= b
      }

      result
    }
  }

  /**
   * Computes base to the exp'th power for Longs by repeated squaring.
   * @param base
   * @param exp
   * @return
   */
  def ipow(base: Long, exp: Long): Long = {
    var b = base
    if (exp < 0 && base != 1) 0
    else {
      var e = exp
      var result: Long = 1
      while (e != 0) {
        if ((e & 1) != 0)
          result *= b
        e >>= 1
        b *= b
      }

      result
    }
  }
}
