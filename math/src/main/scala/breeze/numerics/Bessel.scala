package breeze.numerics

import breeze.generic.UFunc

/*
Copyright 2012 David Hall

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
 * Implementations of the Bessel functions, based on Numerical Recipes
 *
 * @author dlwh
 */
object Bessel {

  object i0 extends UFunc {
    implicit object ImplInt extends Impl[Int, Double] {
      def apply(x: Int): Double = ImplDouble(x.toDouble)
    }
    implicit object ImplDouble extends Impl[Double, Double] {

      def apply(x: Double): Double = {
        val ax = x.abs
        if (ax < 15.0) {
          val y = x * x
          polyval(i0p, y) / polyval(i0q, 225.0 - y)
        } else {
          val z = 1.0 - 15.0 / ax
          exp(ax) * polyval(i0pp, z) / (polyval(i0qq, z) * sqrt(ax))
        }
      }
    }
  }

  object i1 extends UFunc {
    def apply(x: Double): Double = {
      var y: Double = Double.NaN
      var z = x.abs

      if (z <= 8.0) {
        y = (z / 2.0) - 2.0
        z = chbevl(y, A_i1, 29) * z * math.exp(z)
      } else {
        z = math.exp(z) * chbevl(32.0 / z - 2.0, B_i1, 25) / math.sqrt(z)
      }
      if (x < 0.0)
        z = -z
      z
    }

  }

  private def chbevl(x: Double, coef: Array[Double], N: Int) = {
    var b0 = 0.0
    var b1 = 0.0
    var b2 = 0.0

    var p = 0
    var i = N - 1

    b0 = coef(p)
    p += 1
    b1 = 0.0
    i = N - 1

    do {
      b2 = b1
      b1 = b0
      b0 = x * b1 - b2 + coef(p)
      p += 1
      i -= 1
    } while (i > 0)

    (0.5 * (b0 - b2))
  }

  private val i0p = Array(9.999999999999997e-1, 2.466405579426905e-1, 1.478980363444585e-2, 3.826993559940360e-4,
    5.395676869878828e-6, 4.700912200921704e-8, 2.733894920915608e-10, 1.115830108455192e-12, 3.301093025084127e-15,
    7.209167098020555e-18, 1.166898488777214e-20, 1.378948246502109e-23, 1.124884061857506e-26, 5.498556929587117e-30)
  private val i0q =
    Array(4.463598170691436e-1, 1.702205745042606e-3, 2.792125684538934e-6, 2.369902034785866e-9, 8.965900179621208e-13)
  private val i0pp =
    Array(1.192273748120670e-1, 1.947452015979746e-1, 7.629241821600588e-2, 8.474903580801549e-3, 2.023821945835647e-4)
  private val i0qq = Array(2.962898424533095e-1, 4.866115913196384e-1, 1.938352806477617e-1, 2.261671093400046e-2,
    6.450448095075585e-4, 1.529835782400450e-6)

  private val A_i1 = Array(
    2.77791411276104639959e-18, -2.11142121435816608115e-17, 1.55363195773620046921e-16, -1.10559694773538630805e-15,
    7.60068429473540693410e-15, -5.04218550472791168711e-14, 3.22379336594557470981e-13, -1.98397439776494371520e-12,
    1.17361862988909016308e-11, -6.66348972350202774223e-11, 3.62559028155211703701e-10, -1.88724975172282928790e-9,
    9.38153738649577178388e-9, -4.44505912879632808065e-8, 2.00329475355213526229e-7, -8.56872026469545474066e-7,
    3.47025130813767847674e-6, -1.32731636560394358279e-5, 4.78156510755005422638e-5, -1.61760815825896745588e-4,
    5.12285956168575772895e-4, -1.51357245063125314899e-3, 4.15642294431288815669e-3, -1.05640848946261981558e-2,
    2.47264490306265168283e-2, -5.29459812080949914269e-2, 1.02643658689847095384e-1, -1.76416518357834055153e-1,
    2.52587186443633654823e-1
  )

  private val B_i1 = Array(
    7.51729631084210481353e-18, 4.41434832307170791151e-18, -4.65030536848935832153e-17, -3.20952592199342395980e-17,
    2.96262899764595013876e-16, 3.30820231092092828324e-16, -1.88035477551078244854e-15, -3.81440307243700780478e-15,
    1.04202769841288027642e-14, 4.27244001671195135429e-14, -2.10154184277266431302e-14, -4.08355111109219731823e-13,
    -7.19855177624590851209e-13, 2.03562854414708950722e-12, 1.41258074366137813316e-11, 3.25260358301548823856e-11,
    -1.89749581235054123450e-11, -5.58974346219658380687e-10, -3.83538038596423702205e-9, -2.63146884688951950684e-8,
    -2.51223623787020892529e-7, -3.88256480887769039346e-6, -1.10588938762623716291e-4, -9.76109749136146840777e-3,
    7.78576235018280120474e-1
  )

}
