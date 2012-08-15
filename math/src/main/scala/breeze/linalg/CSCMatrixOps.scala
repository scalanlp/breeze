package breeze.linalg
import java.util._
import breeze.linalg.operators._
import breeze.linalg.support._
import breeze.numerics._

/** This is an auto-generated trait providing operators for CSCMatrix */
trait CSCMatrixOps_Double { this: CSCMatrix.type =>

  class canMulM_V_Double private[linalg] () extends BinaryRegistry[CSCMatrix[Double], Vector[Double], breeze.linalg.operators.OpMulMatrix, Vector[Double]] {
    override def bindingMissing(a: CSCMatrix[Double], b: Vector[Double]) = {
      
      val res = DenseVector.zeros[Double](a.rows)
      var c = 0
      while(c < a.cols) {
        var rr = a.colPtrs(c)
        val rrlast = a.colPtrs(c+1)
        while (rr < rrlast) {
          val r = a.rowIndices(rr)
          res(r) += a.data(rr) * b(c)
          rr += 1
        }
        c += 1
      }

      res
    }
  };
  val canMulM_V_Double = new canMulM_V_Double()
  implicit def canMulM_V_Double_def[A <: CSCMatrix[Double], B <: Vector[Double]]:BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, Vector[Double]] = (
    canMulM_V_Double.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, Vector[Double]]]
  )
    
}

/** This is an auto-generated trait providing operators for CSCMatrix */
trait CSCMatrixOps_Float { this: CSCMatrix.type =>

  class canMulM_V_Float private[linalg] () extends BinaryRegistry[CSCMatrix[Float], Vector[Float], breeze.linalg.operators.OpMulMatrix, Vector[Float]] {
    override def bindingMissing(a: CSCMatrix[Float], b: Vector[Float]) = {
      
      val res = DenseVector.zeros[Float](a.rows)
      var c = 0
      while(c < a.cols) {
        var rr = a.colPtrs(c)
        val rrlast = a.colPtrs(c+1)
        while (rr < rrlast) {
          val r = a.rowIndices(rr)
          res(r) += a.data(rr) * b(c)
          rr += 1
        }
        c += 1
      }

      res
    }
  };
  val canMulM_V_Float = new canMulM_V_Float()
  implicit def canMulM_V_Float_def[A <: CSCMatrix[Float], B <: Vector[Float]]:BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, Vector[Float]] = (
    canMulM_V_Float.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, Vector[Float]]]
  )
    
}

/** This is an auto-generated trait providing operators for CSCMatrix */
trait CSCMatrixOps_Int { this: CSCMatrix.type =>

  class canMulM_V_Int private[linalg] () extends BinaryRegistry[CSCMatrix[Int], Vector[Int], breeze.linalg.operators.OpMulMatrix, Vector[Int]] {
    override def bindingMissing(a: CSCMatrix[Int], b: Vector[Int]) = {
      
      val res = DenseVector.zeros[Int](a.rows)
      var c = 0
      while(c < a.cols) {
        var rr = a.colPtrs(c)
        val rrlast = a.colPtrs(c+1)
        while (rr < rrlast) {
          val r = a.rowIndices(rr)
          res(r) += a.data(rr) * b(c)
          rr += 1
        }
        c += 1
      }

      res
    }
  };
  val canMulM_V_Int = new canMulM_V_Int()
  implicit def canMulM_V_Int_def[A <: CSCMatrix[Int], B <: Vector[Int]]:BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, Vector[Int]] = (
    canMulM_V_Int.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, Vector[Int]]]
  )
    
}
