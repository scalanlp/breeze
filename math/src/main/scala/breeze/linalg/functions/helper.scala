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

import breeze.numerics._
import breeze.math._
import DenseMatrix.canMapValues
import DenseMatrix._
import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import reflect.runtime.universe._
import scala.io._
import java.text.DecimalFormat
import scala.Console._

object GlobalConsts {

  implicit class MutableInt(var value: Int) {
    def inc() = { value += 1 }
  }
  object printType extends Enumeration {
    type printType = Value
    val REAL, IMAG, BOTH = Value
  }
  def function(s: MutableInt): Boolean = {
    s.inc() // parentheses here to denote that method has side effects
    true
  }
  var Off = false
  var showHouseholder = false
  var showCompute2x2 = false
  var matnum1 = MutableInt(0)
  var matnum2 = MutableInt(0)
  var showSchur = false
  var showCalculator = false
  var showComplex = false
  var showTitles = false
  var showLines = false
  var fileOutput = false
  var showRevertSchur = false
  var showIMinusT = false
  //  val formatter = new DecimalFormat("#0.###E0")
  var formatter = new DecimalFormat("#0.######")
  val printEnabled = Array(Off, showRevertSchur, showCalculator, showCompute2x2, showIMinusT, false, showHouseholder, showSchur) //0,1 for debugging last part
  val EPSILON: Double = 2.22045e-016
  var currentPrintType = printType.BOTH
  var file = new File("schurBT.dat")
  var bw: Option[BufferedWriter] = if (fileOutput == true) { Some(new BufferedWriter(new FileWriter(file))) } else { None }

}

object Helper {
  import GlobalConsts._

  def abs2(n: Complex): Double = { (n.real * n.real) + (n.imag * n.imag) }
  def conj(n: Complex) = { Complex(n.real, -n.imag) }
  def norm1(n: Complex): Double = { abs(n.real) + abs(n.imag) }
  def biggest(M: DenseMatrix[Complex]) = norm1(sum(M(::, *)).t.reduceLeft((x, y) => if (norm1(x) > norm1(y)) x else y))
  val M_PI = 3.14159265358979323846
  def sinh2(c: Complex) = { Complex(sinh(c.real) * cos(c.imag), cosh(c.real) * sin(c.imag)) }
  def isMuchSmallerThan(x: Double, y: Double) = { abs(x) <= abs(y) * EPSILON }
  implicit def Y3[A1, A2, A3, B](f: ((A1, A2, A3) => B) => ((A1, A2, A3) => B)): (A1, A2, A3) => B = f(Y3(f))(_, _, _)
  type fType[A] = (Int, DenseMatrix[Complex]) => A
  def Y[A, B](f: (A => B) => (A => B)): A => B = f(Y(f))(_)
  def atan2h(x: Complex, y: Complex): Complex = { val z = x / y; if ((y == 0) || (abs2(z) > pow(GlobalConsts.EPSILON, 0.5))) (0.5) * log((y + x) / (y - x)) else z + z * z * z / 3 }

  //this is not used  ...  for inverting Upper Triangle form
  def invU(M: DenseMatrix[Complex]): DenseMatrix[Complex] =
    { // All upper must be non zero
      // Ax = b -> LUx = b. Then y is defined to be Ux
      val n = M.cols
      val U = DenseMatrix.zeros[Complex](n, n)
      // Backward solve Ux = y
      var k = 0
      var j = 0
      var i = 0
      for (j <- (n - 1) to 0 by -1) {
        U(j, j) = 1.0 / M(j, j)
        for (i <- (j - 1) to 0 by -1) {
          for (k <- (i + 1) to j)
            U(i, j) -= 1 / M(i, i) * M(i, k) * U(k, j)
        }
      }
      U
    }
  def UTadj(A: DenseMatrix[Complex]) = invU(A) * diag(A).reduce(_ * _)
  def Tadj(A: DenseMatrix[Complex]) = A.t.mapValues(i => Complex(i.real, -i.imag))

  def printcount2(name: String) = {

    function(matnum1)
    val count = matnum1.value
    "************************************************************** " + count + " *** " + name + "**************************************************************\n"
  }
  implicit def enrichString2(stuff: String) =
    new {
      def showTitle(name: String) = { if (showTitles) printcount2(name) + stuff else stuff }
    }

  implicit def enrichString(stuff: String) =
    new {
      def oneLiner = { if (showLines) stuff.filter(_ >= ' ') else stuff }
    }

  def output(str: String) = { if (fileOutput) bw.get.write(str) else print(str) }
  def adder(x: Double) = { if (x > 0) { "+" } else { "" } }
  def debugPrint[T: TypeTag](M: T, name: String = "", loglevel: Int = 0) =

    typeTag[T].tpe match {
      case b if b =:= typeOf[DenseMatrix[Double]] => if (printEnabled(loglevel)) {
        currentPrintType match {

          case _ => output(("" + M.asInstanceOf[DenseMatrix[Double]].mapValues { (x) => formatter.format(x) }).oneLiner.showTitle(name) + "\n")
        }

      }
      case b if b =:= typeOf[DenseVector[Double]] => if (printEnabled(loglevel)) {
        currentPrintType match {

          case _ => output(("" + M.asInstanceOf[DenseVector[Double]].mapValues { (x) => formatter.format(x) }).oneLiner.showTitle(name) + "\n")
        }
      }

      case b if b =:= typeOf[DenseVector[Complex]] => if (printEnabled(loglevel)) {
        currentPrintType match {
          case printType.REAL => output(("" + M.asInstanceOf[DenseVector[Complex]].mapValues { (x) => formatter.format(x.real) }).oneLiner.showTitle(name) + "\n")
          case printType.IMAG => output(("" + M.asInstanceOf[DenseVector[Complex]].mapValues { (x) => formatter.format(x.imag) }).oneLiner.showTitle(name) + "\n")
          case _ => output(("" + M.asInstanceOf[DenseVector[Complex]].mapValues { (x) => formatter.format(x.real) + "," + formatter.format(x.imag) }).oneLiner.showTitle(name) + "\n")
        }
      }
      case b if b =:= typeOf[DenseMatrix[Complex]] => if (printEnabled(loglevel)) {
        currentPrintType match {
          case printType.REAL => output(("" + M.asInstanceOf[DenseMatrix[Complex]].mapValues { (x) => formatter.format(x.real) }).oneLiner.showTitle(name) + "\n")
          case printType.IMAG => output(("" + M.asInstanceOf[DenseMatrix[Complex]].mapValues { (x) => formatter.format(x.imag) }).oneLiner.showTitle(name) + "\n")
          case _ => output(("" + M.asInstanceOf[DenseMatrix[Complex]].mapValues { (x) => "(" + formatter.format(x.real) + "," + formatter.format(x.imag) + ")" }).oneLiner.showTitle(name) + "\n")
        }
      }

      case b if b =:= typeOf[Array[Double]] => if (printEnabled(loglevel)) {
        currentPrintType match {
          case _ => output("" + M.asInstanceOf[Array[Double]].deep.mkString("\n").oneLiner.showTitle(name) + "\n")
        }
      }

      case b if b =:= typeOf[Array[DenseVector[Complex]]] => if (printEnabled(loglevel)) {
        currentPrintType match {
          case _ => output("" + M.asInstanceOf[Array[DenseVector[Complex]]].deep.mkString("\n").oneLiner.showTitle(name) + "\n")
        }
      }
      case _ => if (printEnabled(loglevel)) {
        currentPrintType match {
          case printType.REAL => output(M.toString.oneLiner.showTitle(name) + "\n")
          case printType.IMAG => output(M.toString.oneLiner.showTitle(name) + "\n")
          case _ => output(M.toString.oneLiner.showTitle(name) + "\n")
        }
      }
    }
}
