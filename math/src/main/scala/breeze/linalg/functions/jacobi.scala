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
import breeze.linalg.jacobi.Givens.GivensType
import breeze.math._
import breeze.generic.UFunc
import DenseMatrix.canMapValues
import breeze.numerics._

/* This class represents a Jacobi or Givens rotation.
 * This is a 2D rotation in the plane \c J of angle \f$ \theta \f$ defined by
 * its cosine \c c and sine \c s as follow:
 * \f$ J = \left ( \begin{array}{cc} c & \overline s \\ -s  & \overline c \end{array} \right ) \f$
 *
 * You can apply the respective counter-clockwise rotation to a column vector \c v by
 * applying its adjoint on the left: \f$ v = J^* v \f$
 */
object jacobi extends UFunc {

  implicit def DMT_Cast_Impl_J[T](implicit cast: T => Double): Impl[DenseMatrix[T], Jacobi] = {
    new Impl[DenseMatrix[T], Jacobi] {
      def apply(M: DenseMatrix[T]): Jacobi = {
        import DenseMatrix.canMapValues
        DMD_IMPL_J(M.mapValues(cast))
      }
    }
  }

  implicit object DMC_IMPL_J extends Impl[DenseMatrix[Complex], Jacobi] {
    def apply(M: DenseMatrix[Complex]): Jacobi =
      {
        new Jacobi(M)
      }
  }

  implicit object DMD_IMPL_J extends Impl[DenseMatrix[Double], Jacobi] {
    def apply(M: DenseMatrix[Double]): Jacobi = {
      new Jacobi(M.mapValues(Complex(_, 0.0)))
    }
  }

  class Jacobi(val M: DenseMatrix[Complex]) {

    var cGivens: Givens = new Givens(null, null, null)
    var vPos: Int = 0
    var hPos: Int = 0
    var typeRot: GivensType.EnumVal = GivensType.Undefined

    //mutating (speedier)
    def rotateMutateL(g: Givens) = { cGivens = new Givens((g.m_c).conjugate, -(g.m_s), g.rot); jacobi.applyRotationinPlane(M(0, ::).t, M(1, ::).t, cGivens) }
    def rotateMutateR(g: Givens) = { cGivens = new Givens((g.m_c), -(g.m_s).conjugate, g.rot); jacobi.applyRotationinPlane(M(::, 0), M(::, 1), cGivens) }

    //non-mutating
    def rotateL(g: Givens, vpos: Int = 0, hpos: Int = 0) = { typeRot = GivensType.Left; vPos = vpos; hPos = hpos; cGivens = g; getGivens(cGivens, vPos, hPos) * M }
    def rotateR(g: Givens, vpos: Int = 0, hpos: Int = 0) = { typeRot = GivensType.Right; vPos = vpos; hPos = hpos; cGivens = g; getGivens(cGivens, vPos, hPos) * M }

    //non-mutating using Givens (p,q)
    def rotateL(p: Complex, q: Complex, vpos: Int, hpos: Int): DenseMatrix[Complex] = rotateL(Givens(p, q), vpos, hpos)
    def rotateR(p: Complex, q: Complex, vpos: Int, hpos: Int): DenseMatrix[Complex] = rotateR(Givens(p, q), vpos, hpos)

    /* Method fo Givens QR decomposition

     hPos defines the column where the Givens is extracted from
     vPos defines both the diag the Givens rotation is placed, and the row the Givens is extracted from

     RotateR
     Example 1:

     [x x x x]
     [x x x x]
     [i x x x]
     [j x x x]   <= index = (0,0)

     Gives Givens:
     [1 0 0 0]
     [0 1 0 0]
     [0 0 c -s]
     [0 0 s c]

     RotateR
     Example 2:

     [x i x x]
     [x j x x]
     [x x x x]
     [x x x x]   <= index = (2,1)

     Gives Givens:
     [c -s 0 0]
     [s c 0 0]
     [0 0 1  0]
     [0 0 0 1]
     */
    def rotateL(vpos: Int, hpos: Int): DenseMatrix[Complex] = { typeRot = GivensType.Left; vPos = vpos; hPos = hpos; cGivens = Givens(M(vPos, hPos), M(vPos + 1, hPos)); getGivens(cGivens, vPos, hPos) * M }
    def rotateR(vpos: Int, hpos: Int): DenseMatrix[Complex] = { typeRot = GivensType.Right; vPos = vpos; hPos = hpos; cGivens = Givens(M(M.rows - (vPos + 2), hPos), M(M.rows - (vPos + 1), hPos)); getGivens(cGivens, vPos, hPos) * M }
    /*returns tuple of Matrix A transformed and Givens Rotation Matrix (useful for QR decomp) */
    def rotate2L(vpos: Int, hpos: Int): (DenseMatrix[Complex], DenseMatrix[Complex]) = { typeRot = GivensType.Left; vPos = vpos; hPos = hpos; cGivens = Givens(M(vPos, hPos), M(vPos + 1, hPos)); (getGivens(cGivens, vPos, hPos) * M, getGivens(cGivens, vPos, hPos)) }
    /*returns tuple of Matrix A transformed and Givens Rotation Matrix (useful for QR decomp) */
    def rotate2R(vpos: Int, hpos: Int): (DenseMatrix[Complex], DenseMatrix[Complex]) = { typeRot = GivensType.Right; vPos = vpos; hPos = hpos; cGivens = Givens(M(M.rows - (vPos + 2), hPos), M(M.rows - (vpos + 1), hPos)); (getGivens(cGivens, vPos, hPos) * M, getGivens(cGivens, vPos, hPos)) }

    //semi static method
    /* returns the Givens Matrix G used in Rotation */
    def getGivens(g: Givens, vpos: Int, hpos: Int): DenseMatrix[Complex] = {

      vPos = vpos
      hPos = hpos
      cGivens = g

      typeRot match {
        case GivensType.Right =>
          val m = DenseMatrix.eye[Complex](M.rows)
          m((m.cols - (vPos + 2)) to (m.cols - (vPos + 1)), (m.cols - (vPos + 2)) to (m.cols - (vPos + 1))) := g.G
          m
        case GivensType.Left =>
          val m = DenseMatrix.eye[Complex](M.cols)
          m(vPos to (vPos + 1), vPos to (vPos + 1)) := g.G
          m
        case _ => throw new IllegalArgumentException("Rotation Type not defined")
      }
    }

    def getGivens(vpos: Int, hpos: Int): DenseMatrix[Complex] = getGivens(cGivens, vpos, hpos)
    def getGivens: DenseMatrix[Complex] = getGivens(cGivens, vPos, hPos)
  }

  private def applyRotationinPlane(_x: DenseVector[Complex], _y: DenseVector[Complex], g: Givens) = {

    if (g.m_c == 1 && g.m_s == 0)
      DenseMatrix.zeros[Complex](_x.size, 2)

    val x1 = DenseVector.tabulate[Complex](_x.length) { (i) => g.m_c * _x(i) + g.m_s.conjugate * _y(i) }
    val y1 = DenseVector.tabulate[Complex](_y.length) { (i) => -g.m_s * _x(i) + g.m_c.conjugate * _y(i) }
    val res = DenseMatrix.vertcat(x1.t, y1.t)
    val res1 = DenseVector.horzcat(x1, y1)

    val i = 0

    for (i <- 0 to _x.length - 1) {
      val xi = _x(i)
      val yi = _y(i)
      _x(i) = g.m_c * xi + g.m_s.conjugate * yi
      _y(i) = -g.m_s * xi + g.m_c.conjugate * yi
    }
  }

  case class Givens(val m_c: Complex, val m_s: Complex, val rot: Complex) {

    def G = DenseMatrix((m_c, -m_s.conjugate), (m_s, m_c))
    def y = rot
    def planerot: (DenseMatrix[Complex], Complex) = (G, rot)
  }
  object Givens {
    object GivensType {
      sealed trait EnumVal
      case object Undefined extends EnumVal
      case object Right extends EnumVal
      case object Left extends EnumVal
      val givensType = Seq(Right, Left)
    }

    def apply(p: Int, q: Int): Givens = this(Complex(p, 0.0), Complex(q, 0.0))
    def apply(p: Double, q: Double): Givens = this(Complex(p, 0.0), Complex(q, 0.0))
    /*This function implements the continuous Givens rotation
 *generation algorithm found in Anderson (2000),
 *Discontinuous Plane Rotations and the Symmetric Eigenvalue Problem.
 *LAPACK Working Note 150, University of Tennessee, UT-CS-00-454, December 4, 2000. */
    def apply(p: Complex, q: Complex): Givens = {

      (p, q) match {
        case (_, Complex(0.0, 0.0)) =>
          val m_c = if (p.real < 0) Complex(-1.0, 0.0) else Complex(1.0, 0.0)
          val m_s = Complex(0.0, 0.0)
          val r = m_c * p;
          new Givens(m_c, m_s, r)

        case (Complex(0.0, 0.0), _) =>
          val m_c = Complex(0.0, 0.0)
          val m_s = -q / abs(q)
          val r = Complex(abs(q), 0.0)

          new Givens(m_c, m_s, r)
        case _ =>
          val p1 = p.norm1
          val q1 = q.norm1
          if (p1 >= q1) {
            val ps = p / p1
            val p2 = ps.abs2
            val qs = q / p1
            val q2 = qs.abs2

            var u = pow(1.0 + (q2 / p2), 0.5)
            if (p.real < 0)
              u = -u

            val m_c = Complex(1.0, 0) / u
            val m_s = -qs * ps.conjugate * (m_c / p2)
            val r = p * u

            new Givens(m_c, m_s, r)
          } else {

            val p2 = (p / q1).abs2
            val qs = q / q1
            val q2 = (qs).abs2

            var u = q1 * pow((p2 + q2), 0.5)

            if (p.real < 0)
              u = -u

            val p1 = abs(p)
            val ps2 = p / p1
            val m_c = Complex(p1 / u, 0.0)
            val m_s = -ps2.conjugate * (q / u)
            val r = ps2 * u
            new Givens(m_c, m_s, r)
          }
      }
    }
  }
}
