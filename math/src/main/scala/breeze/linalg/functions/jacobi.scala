package breeze.linalg
/*
 Copyright 2016 @claydonkey (Anthony Campbell)

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
import breeze.numerics._
import breeze.math._
import breeze.linalg.Helper._
import breeze.generic.UFunc

/* This class represents a Jacobi or Givens rotation.
 * This is a 2D rotation in the plane \c J of angle \f$ \theta \f$ defined by
 * its cosine \c c and sine \c s as follow:
 * \f$ J = \left ( \begin{array}{cc} c & \overline s \\ -s  & \overline c \end{array} \right ) \f$
 *
 * You can apply the respective counter-clockwise rotation to a column vector \c v by
 * applying its adjoint on the left: \f$ v = J^* v \f$
 */

object jacobi extends UFunc {

  implicit object DMC_IMPL_Jacobi  extends Impl[DenseMatrix[Complex],Jacobi] {
    def apply(M: DenseMatrix[Complex]):Jacobi = {
      new Jacobi(M)
    }
  }
   class Jacobi(val M: DenseMatrix[Complex]) {

    def rotateL(jrot: jRotation) = jacobi.applyRotationinPlane(M(0, ::).t, M(1, ::).t, new jRotation(conj(jrot.m_c), -(jrot.m_s), jrot.rot))
    def rotateR(jrot: jRotation) = jacobi.applyRotationinPlane(M(::, 0), M(::, 1), new jRotation((jrot.m_c), -conj(jrot.m_s), jrot.rot))

    //Need to fix these...
   // def rotateL(jrot: jRotation) = jacobi.getRotationLeft(M(0, ::).t, M(1, ::).t, new jRotation(conj(jrot.m_c), -(jrot.m_s), jrot.rot))
    //def rotateR(jrot: jRotation) = jacobi.getRotationRight(M(::, 0), M(::, 1), new jRotation((jrot.m_c), -conj(jrot.m_s), jrot.rot))
  }
/*
   def getRotationLeft(_x: DenseVector[Complex], _y: DenseVector[Complex], j: jRotation) = {
    val x1 = DenseVector.tabulate[Complex](_x.length) { (i) => j.m_c * _x(i) + conj(j.m_s) * _y(i) }
    val y1 = DenseVector.tabulate[Complex](_y.length) { (i) => -j.m_s * _x(i) + conj(j.m_c) * _y(i) }
    DenseMatrix.vertcat(x1.t, y1.t)
  }

   def getRotationRight(_x: DenseVector[Complex], _y: DenseVector[Complex], j: jRotation) = {
    val x1 = DenseVector.tabulate[Complex](_x.length) { (i) => j.m_c * _x(i) + conj(j.m_s) * _y(i) }
    val y1 = DenseVector.tabulate[Complex](_y.length) { (i) => -j.m_s * _x(i) + conj(j.m_c) * _y(i) }
    DenseVector.horzcat(x1, y1)
  }
*/
  private  def applyRotationinPlane(_x: DenseVector[Complex], _y: DenseVector[Complex], j: jRotation) = {

    if (j.m_c == 1 && j.m_s == 0)
      DenseMatrix.zeros[Complex](_x.size, 2)

    val x1 = DenseVector.tabulate[Complex](_x.length) { (i) => j.m_c * _x(i) + conj(j.m_s) * _y(i) }
    val y1 = DenseVector.tabulate[Complex](_y.length) { (i) => -j.m_s * _x(i) + conj(j.m_c) * _y(i) }
    val res = DenseMatrix.vertcat(x1.t, y1.t)
    val res1 = DenseVector.horzcat(x1, y1)

    val i = 0

    for (i <- 0 to _x.length - 1) {
      val xi = _x(i)
      val yi = _y(i)
      _x(i) = j.m_c * xi + conj(j.m_s) * yi
      _y(i) = -j.m_s * xi + conj(j.m_c) * yi
    }
  }

  /*This function implements the continuous Givens rotation
 *generation algorithm found in Anderson (2000),
 *Discontinuous Plane Rotations and the Symmetric Eigenvalue Problem.
 *LAPACK Working Note 150, University of Tennessee, UT-CS-00-454, December 4, 2000. */

  def makeGivens(p: Complex, q: Complex) = {

    (p, q) match {
      case (_, Complex(0.0, 0.0)) =>
        val m_c = if (p.real < 0) Complex(-1.0, 0.0) else Complex(1.0, 0.0)
        val m_s = Complex(0.0, 0.0)
        val r = m_c * p;

        new jRotation(m_c, m_s, r)
      case (Complex(0.0, 0.0), _) =>
        val m_c = Complex(0.0, 0.0)
        val m_s = -q / abs(q)
        val r = Complex(abs(q), 0.0)

        new jRotation(m_c, m_s, r)
      case _ =>
        val p1 = norm1(p)
        val q1 = norm1(q)
        if (p1 >= q1) {
          val ps = p / p1
          val p2 = abs2(ps)
          val qs = q / p1
          val q2 = abs2(qs)

          var u = pow(1.0 + (q2 / p2), 0.5)
          if (p.real < 0)
            u = -u

          val m_c = Complex(1.0, 0) / u
          val m_s = -qs * conj(ps) * (m_c / p2)
          val r = p * u

          new jRotation(m_c, m_s, r)
        } else {

          val p2 = abs2(p / q1)
          val qs = q / q1
          val q2 = abs2(qs);

          var u = q1 * pow((p2 + q2), 0.5)

          if (p.real < 0)
            u = -u

          val p1 = abs(p)
          val ps2 = p / p1
          val m_c = Complex(p1 / u, 0.0)
          val m_s = -conj(ps2) * (q / u)
          val r = ps2 * u
          new jRotation(m_c, m_s, r)
        }
    }
  }

  class jRotation(val m_c: Complex, val m_s: Complex, val rot: Complex) {}
}