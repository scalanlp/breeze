package breeze.linalg
/*
 Copyright 2016 @author claydonkey (Anthony Campbell)

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
import breeze.math._
import scala.annotation.tailrec
import breeze.generic.UFunc
import DenseMatrix.canMapValues
import breeze.numerics._

/*
 * The Householder linear transformation. Describes a reflection about a plane or hyperplane containing the origin.
 * The reflection hyperplane can be defined by a unit vector v (a vector with length 1) which is orthogonal to the hyperplane. The reflection of a point x about this hyperplane is:
 * x-2<x,v> v=x-2v( v^H * x)
 * where v is given as a column unit vector with Hermitian transpose vH. This is a linear transformation given by the Householder matrix:
 * P=I-2vv^H, where I is the identity matrix.
 *
 * Widely used for tridiagonalization of symmetric matrices and for transforming non-symmetric matrices to a Hessenberg form.
 * (https://en.wikipedia.org/wiki/Householder_transformation)
 */
object householder extends UFunc {

  private val EPSILON: Double = 2.22045e-016
  private def isMuchSmallerThan(x: Double, y: Double) = { abs(x) <= abs(y) * EPSILON }
  /* takes a Matrix and optional computes a householder Transform. Creates householder class for further householder transformations. The result is stored in matrixH */
  // implicit def DMT_DVC_B_Cast_Impl_H[T](implicit cast: T => Double): Impl2[DenseMatrix[T], DenseVector[Complex], Householder] = {
  //   new Impl2[DenseMatrix[T], DenseVector[Complex], Householder] {
  //   def apply(M: DenseMatrix[T], tau: DenseVector[Complex]): Householder = {
  //    import DenseMatrix.canMapValues
  //    DMD_DVC_B_IMPL_H(M.mapValues(cast), tau)
  //  }
  //  }
  // }
  //
  //
  /* takes a Matrix and optional computes a householder Transform. Creates householder class for further householder transformations. The result is stored in matrixH */
  implicit def DMT_Cast_Impl_H[T](implicit cast: T => Double): Impl[DenseMatrix[T], Householder] = {
    new Impl[DenseMatrix[T], Householder] {
      def apply(M: DenseMatrix[T]): Householder = {
        import DenseMatrix.canMapValues
        DMD_IMPL_H(M.mapValues(cast))
      }
    }
  }

  /* takes a Matrix and optional computes a householder Transform. Creates householder class for further householder transformations. The result is stored in matrixH */
  implicit object DMD_IMPL_H extends Impl[DenseMatrix[Double], Householder] {
    def apply(M: DenseMatrix[Double]): Householder = {
      new Householder(M.mapValues(Complex(_, 0.0)))
    }
  }

  /* takes a Matrix and optional computes a householder Transform. Creates householder class for further householder transformations. The result is stored in matrixH */
  implicit object DMC_IMPL_H extends Impl[DenseMatrix[Complex], Householder] {
    def apply(M: DenseMatrix[Complex]): Householder = {
      new Householder(M)
    }
  }

  /* takes a Matrix and optional computes a householder Transform. Creates householder class for further householder transformations. The result is stored in matrixH */
  implicit def DMT_B_Cast_Impl_H[T](implicit cast: T => Double): Impl2[DenseMatrix[T], Int, Householder] = {
    new Impl2[DenseMatrix[T], Int, Householder] {
      def apply(M: DenseMatrix[T], shift: Int): Householder = {
        import DenseMatrix.canMapValues
        DMD_B_IMPL_H(M.mapValues(cast), shift)
      }
    }
  }

  /* takes a Matrix and optional computes a householder Transform. Creates householder class for further householder transformations. The result is stored in matrixH */
  implicit object DMD_B_IMPL_H extends Impl2[DenseMatrix[Double], Int, Householder] {
    def apply(M: DenseMatrix[Double], shift: Int): Householder = {
      new Householder(M.mapValues(Complex(_, 0.0)), shift)
    }
  }

  /* takes a Matrix and optional computes a householder Transform. Creates householder class for further householder transformations. The result is stored in matrixH */
  //implicit object DMD_DVC_B_IMPL_H extends Impl2[DenseMatrix[Double], DenseVector[Complex], Householder] {
  //    def apply(M: DenseMatrix[Double], tau: DenseVector[Complex]): Householder = {
  //    new Householder(M.mapValues(Complex(_, 0.0)), tau)
  //  }
  // }

  /* takes a Matrix and optional computes a householder Transform. Creates householder class for further householder transformations. The result is stored in matrixH */
  // implicit object DMC_DVC_B_IMPL_H extends Impl2[DenseMatrix[Complex], DenseVector[Complex], Householder] {
  //   def apply(M: DenseMatrix[Complex], tau: DenseVector[Complex]): Householder = {
  //    new Householder(M, tau)
  //  }
  //}

  /* takes a Matrix and optional computes a householder Transform. Creates householder class for further householder transformations. The result is stored in matrixH */
  implicit object DMC_B_IMPL_H extends Impl2[DenseMatrix[Complex], Int, Householder] {
    def apply(M: DenseMatrix[Complex], shift: Int): Householder = {
      new Householder(M, shift)
    }
  }

  class Householder(val M: DenseMatrix[Complex], val tau: DenseVector[Complex], val essential: Array[DenseVector[Complex]]) {

    var cShift = -1
    def this(M: DenseMatrix[Complex], tau: DenseVector[Complex]) = this(M, tau, Array.ofDim[DenseVector[Complex]](M.cols - 1))
    def this(M: DenseMatrix[Complex], shift: Int) = {

      this(M, DenseVector.zeros[Complex](M.cols - 1), Array.ofDim[DenseVector[Complex]](M.cols - 1))
      cShift = shift
      makeHouseholder(cShift)
    }

    def this(M: DenseMatrix[Complex]) = this(M, 0)

    val size: Int = M.cols - 1
    val beta: Array[Double] = Array.ofDim[Double](size)
    val matrixH = M.copy

    def generate() = {
      for (icnt <- 0 to matrixH.rows - 2) {
        if (!(icnt == 0)) makeHouseholder(icnt)
        applyHouseholderOnTheLeft()
        applyHouseholderOnTheRight()
      };
      this
    }

    def getShift() = cShift
    def setShift(value: Int) = { cShift = value }

    private def conj(n: DenseMatrix[Complex]): DenseMatrix[Complex] = (n.mapValues(c => (c.conjugate)))
    /*
     *  4 x 4 example
     *    x    x    x    x
     *    x    [ Row0 ]
     *    [e]    [  bott   ]
     *    [e]    [   bott  ]
     *  compute A = H A H'
     */
    def makeHouseholder(shift: Int) = {
      cShift = shift;
      essential(cShift) = matrixH((cShift + 2) to matrixH.rows - 1, shift)
      val eNorm = if (essential(cShift).length == 0) 0.0 else sum(essential(cShift).map(x => scala.math.pow(x.real, 2) + scala.math.pow(x.imag, 2))) // Does Complex component need squaring?

      val c0 = matrixH(cShift + 1, cShift);
      (eNorm, c0.imag) match {
        case (0, 0) =>
          beta(cShift) = c0.real
          tau(cShift) = Complex(0, 0)
        case _ =>
          val c0norm = scala.math.pow(c0.real, 2) + scala.math.pow(c0.imag, 2)
          beta(cShift) = if (c0.real >= 0) -Math.sqrt(c0norm + eNorm) else Math.sqrt(c0norm + eNorm)
          essential(cShift) = (essential(cShift) / (c0 - beta(cShift)))
          tau(shift) = ((beta(cShift) - c0) / beta(cShift)).conjugate
      }
      matrixH((cShift + 1), cShift) = Complex(beta(cShift), 0)
      matrixH((cShift + 2) until matrixH.rows, cShift) := essential(cShift)

    }
    /**
     * Apply the elementary reflector H given by
     *  H = I - tau v v^*
     * with
     * v^T = [1 essential^T]
     * from the left to a vector or matrix.
     *  4 x 4 example
     *    x    x    x    x
     *    x    r0  r0  r0
     *    e     Bottom
     *    e     Bottom
     */
    def applyHouseholderOnTheLeft(shift: Int, matShift: Boolean = true): Householder = {
      cShift = shift
      val matshift = if (matShift) cShift else -1

      if (matrixH.rows == 1) {
        matrixH(0, 0) = Complex(1.0, 0.0) - tau(cShift)
      } else {
        try {
          val ess = essential(cShift)
          var r0 = matrixH((matshift + 1), (matshift + 1) to matrixH.cols - 1).t
          var bottom = matrixH((matshift + 2) to matrixH.rows - 1, (matshift + 1) to matrixH.cols - 1)
          val tmp = (ess.t * bottom) + conj(r0.t)
          r0 -= (tmp * tau(cShift)).toDenseVector
          bottom -= (ess * tmp.toDenseMatrix) * tau(cShift)
        } catch { case e: Exception => }
      };
      this
    }

    def applyHouseholderOnTheLeft(): Householder = applyHouseholderOnTheLeft(cShift)

    /*
     * Apply the elementary reflector H given by
     *  H = I - tau v v^*
     * with
     *  v^T = [1 essential^T]
     * from the right to a vector or matrix.
     *  4 x 4 example
     *    x    c0    Right
     *    x    c0    Right
     *    e    c0    Right
     *    e    c0    Right
     */
    def applyHouseholderOnTheRight(shift: Int): Householder = {
      cShift = shift
      try {
        val ess = essential(cShift)
        //    if (ess.length != 0) {
        var c1 = matrixH(::, cShift + 1).toDenseMatrix
        var right = matrixH(::, (cShift + 2) to matrixH.cols - 1)
        val essTrans = essential(cShift).t
        val tmp2 = conj((essTrans * right.t) + conj(c1))
        matrixH(0 until matrixH.cols, (cShift + 1)) -= (tmp2.toDenseMatrix * tau(cShift).conjugate).toDenseVector
        right -= conj(tmp2.t * conj(essTrans) * tau(cShift))

        //   } else { return this }
      } catch { case e: Exception => }
      this
    }
    def applyHouseholderOnTheRight(): Householder = applyHouseholderOnTheRight(cShift)
    //householderTransformation shifted 1 with size -1
    /*  4 x 4 example of the form
     *  1    0    0     0   ^  ------- shift
     *   0    1    0    0   v
     *   0    0     x    x
     *   0    0     x    x
     */
    def P(shift: Int): DenseMatrix[Complex] = {
      var M = DenseMatrix.eye[Complex](matrixH.cols)
      for (k <- (matrixH.rows - 2) to 0 by -1) {
        val corner = M((k + 1) to (M.cols) - 1, (k + 1) to (M.cols - 1))
        try {
          corner :=
            {
              new Householder(corner, tau.mapValues(i => Complex(i.real, -1 * i.imag)), essential) {
                applyHouseholderOnTheLeft(k, false)
              }.matrixH
            }
        } catch { case e: Exception => }
      };
      M
    }

    def P: DenseMatrix[Complex] = if (!(cShift == -1)) P(cShift) else matrixH

    //householderTransformation shifted 1 with size -1
    /*  4 x 4 example of the form
     *  1    0    0     0   ^  ------- shift (wrapper)
     *   0    1    0    0   v
     *   0    0     x    x
     *   0    0     x    x
     */
    def PD(shift: Int): DenseMatrix[Complex] = {

      val hMatrix = matrixH.mapValues(_.real)
      val ctau = tau.mapValues(_.real)

      if ((hMatrix.rows - shift) != ctau.length)
        throw new MatrixNotSquareException // change to correct exception

      val matHS = hMatrix(shift to (hMatrix.cols - 1), 0 to (hMatrix.rows - shift - 1))
      val I = DenseMatrix.eye[Double](matHS.cols)
      val hhMv = (upperTriangular(matHS.t) *:* -(I - 1.0)) +:+ I

      val sum2 = (new ((Int, Int, DenseMatrix[Double]) => DenseMatrix[Double]) {
        @tailrec def apply(from: Int, to: Int, s: DenseMatrix[Double]): DenseMatrix[Double] = {
          if (from == to) return s;
          apply(from + 1, to, s * -(ctau(from) * hhMv(from, ::).t * hhMv(from, ::).t.toDenseMatrix - I))
        }
      })(0, matHS.cols - 1, I)

      if (shift != 0) {
        val wrapper = DenseMatrix.zeros[Double](hMatrix.cols, hMatrix.cols)
        wrapper(shift to wrapper.cols - 1, shift to wrapper.cols - 1) := sum2
        for (cnt <- 0 to shift - 1) {
          wrapper(cnt, cnt) = 1.0;
        }
        wrapper.mapValues(Complex(_, 0.0))
      } else
        sum2.mapValues(Complex(_, 0.0))
    }

    def PD: DenseMatrix[Complex] = if (!(cShift == -1)) PD(cShift) else matrixH
  }
  //householderTransformation shifted 1 with size -1
  /*  4 x 4 example of the form
   *  1    0    0     0   ^  ------- shift (wrapper)
   *   0    1    0    0   v
   *   0    0     x    x
   *   0    0     x    x
   */
  /*
   def householderTransformationD(House: Householder, shift: Int): DenseMatrix[Double] = {

   val hMatrix = House.matrixH.mapValues(_.real)
   val tau = House.tau.mapValues(_.real)

   if ((hMatrix.rows - shift) != tau.length)
   throw new MatrixNotSquareException // change to correct exception

   val matHS = hMatrix(shift to (hMatrix.cols - 1), 0 to (hMatrix.rows - shift - 1))

   val I = DenseMatrix.eye[Double](matHS.cols)
   val hhMv = (upperTriangular(matHS.t) *:* -(I - 1.0)) +:+ I
   var cnt = 0

   val sum2 = (new ((Int, Int, DenseMatrix[Double]) => DenseMatrix[Double]) {
   @tailrec def apply(from: Int, to: Int, s: DenseMatrix[Double]): DenseMatrix[Double] = {
   if (from == to) return s;
   apply(from + 1, to, s * -(tau(from) * hhMv(from, ::).t * hhMv(from, ::).t.toDenseMatrix - I))
   }
   })(0, matHS.cols - 1, I)

   if (shift != 0) {
   val wrapper = DenseMatrix.zeros[Double](hMatrix.cols, hMatrix.cols)
   wrapper(shift to wrapper.cols - 1, shift to wrapper.cols - 1) := sum2
   for (cnt <- 0 to shift - 1) {
   wrapper(cnt, cnt) = 1.0;
   }
   wrapper
   } else

   sum2
   }
   //householderTransformation shifted 1 with size -1
   /*  4 x 4 example of the form
    *  1    0    0     0   ^  ------- shift (wrapper)
    *   0    1    0    0   v
    *   0    0     x    x
    *   0    0     x    x
    */
   def householderTransformationC(house: Householder, shift: Int): DenseMatrix[Complex] = {

   var M = DenseMatrix.eye[Complex](house.matrixH.cols)
   var k = 0
   for (k <- (house.matrixH.rows - 2) to 0 by -1) {
   val kshift = house.matrixH.rows - k - shift
   val corner = M((k + 1) to (M.cols) - 1, (k + 1) to (M.cols - 1))
   try {
   corner :=
   {
   new Householder(corner, house.tau.mapValues(i => Complex(i.real, -1 * i.imag)), house.essential) {
   applyHouseholderOnTheLeft(k, false)
   }.matrixH
   }
   } catch { case e: Exception => }
   }
   M
   }
   */

  def triDiagonalize(M: DenseMatrix[Complex]) =
    {
      var A = M.copy
      for (icnt <- 0 to M.rows - 2) {
        var P: DenseMatrix[Complex] = householder(A, icnt).P
        A = P * A * P
      };
      A
    }
}

