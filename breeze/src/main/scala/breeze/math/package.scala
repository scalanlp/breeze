package breeze

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
package object math {
  val i = Complex.i

  class RichField(value : Double) {
    def + (c : Complex) : Complex = Complex(value,0) + c
    def - (c : Complex) : Complex = Complex(value,0) - c
    def * (c : Complex) : Complex = Complex(value,0) * c
    def / (c : Complex) : Complex = Complex(value,0) / c
  }

  implicit def richInt(value : Int) =
    new RichField(value)

  implicit def richLong(value : Long) =
    new RichField(value)

  implicit def richFloat(value : Float) =
    new RichField(value)

  implicit def richDouble(value : Double) =
    new RichField(value)
}

