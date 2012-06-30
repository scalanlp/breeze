package breeze.linalg
import java.util._
import breeze.linalg.operators._
import breeze.linalg.support._
import breeze.numerics._

/** This is an auto-generated trait providing operators for CSCMatrix */
trait CSCMatrixOps_Double { this: CSCMatrix.type =>

  implicit val canMulM_V_Double: BinaryOp[CSCMatrix[Double], Vector[Double], breeze.linalg.operators.OpMulMatrix, Vector[Double]] = {
    new BinaryOp[CSCMatrix[Double], Vector[Double], breeze.linalg.operators.OpMulMatrix, Vector[Double]] {
      def apply(a: CSCMatrix[Double], b: Vector[Double]) = {
        
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
    }
  }

}

/** This is an auto-generated trait providing operators for CSCMatrix */
trait CSCMatrixOps_Float { this: CSCMatrix.type =>

  implicit val canMulM_V_Float: BinaryOp[CSCMatrix[Float], Vector[Float], breeze.linalg.operators.OpMulMatrix, Vector[Float]] = {
    new BinaryOp[CSCMatrix[Float], Vector[Float], breeze.linalg.operators.OpMulMatrix, Vector[Float]] {
      def apply(a: CSCMatrix[Float], b: Vector[Float]) = {
        
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
    }
  }

}

/** This is an auto-generated trait providing operators for CSCMatrix */
trait CSCMatrixOps_Int { this: CSCMatrix.type =>

  implicit val canMulM_V_Int: BinaryOp[CSCMatrix[Int], Vector[Int], breeze.linalg.operators.OpMulMatrix, Vector[Int]] = {
    new BinaryOp[CSCMatrix[Int], Vector[Int], breeze.linalg.operators.OpMulMatrix, Vector[Int]] {
      def apply(a: CSCMatrix[Int], b: Vector[Int]) = {
        
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
    }
  }

}
