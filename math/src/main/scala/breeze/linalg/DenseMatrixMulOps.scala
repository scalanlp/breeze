package breeze.linalg
import java.util._
import breeze.linalg.operators._
import breeze.linalg.support._
import breeze.numerics._

/** This is an auto-generated trait providing multiplication for DenseMatrix */
trait DenseMatrixMultOps_Double extends DenseMatrixOps_Double { this: DenseMatrix.type =>

  class canMulM_V_Double private[linalg] () extends BinaryRegistry[DenseMatrix[Double], Vector[Double], breeze.linalg.operators.OpMulMatrix, DenseVector[Double]] {
    override def bindingMissing(a: DenseMatrix[Double], b: Vector[Double]) = {
      
      // TODO: this could probably be much faster?
      require(a.cols == b.length)
      val res = DenseVector.zeros[Double](a.rows)
      var c = 0
      while(c < a.cols) {
        var r = 0
        while (r < a.rows) {
          val v = a(r, c)
          res(r) += v * b(c)
          r += 1
        }
        c += 1
      }

      res                                                               
    }
  }; implicit val canMulM_V_Double = new canMulM_V_Double()


  class canMulM_M_Double private[linalg] () extends BinaryRegistry[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpMulMatrix, DenseMatrix[Double]] {
    override def bindingMissing(a: DenseMatrix[Double], b: Matrix[Double]) = {
      
      // TODO: this could probably be much faster
      val res = DenseMatrix.zeros[Double](a.rows, b.cols)
      require(a.cols == b.rows)
      var c = 0
      while(c < a.cols) {
        var r = 0
        while (r < a.rows) {
          val v = a(r, c)
          var j = 0
          while(j < b.cols) {
            res(r, j) += v * b(c, j)
            j += 1
          }
          r += 1
        }
        c += 1
      }

      res                                                               
    }
  }; implicit val canMulM_M_Double = new canMulM_M_Double()

}

/** This is an auto-generated trait providing multiplication for DenseMatrix */
trait DenseMatrixMultOps_Float extends DenseMatrixOps_Float { this: DenseMatrix.type =>

  class canMulM_V_Float private[linalg] () extends BinaryRegistry[DenseMatrix[Float], Vector[Float], breeze.linalg.operators.OpMulMatrix, DenseVector[Float]] {
    override def bindingMissing(a: DenseMatrix[Float], b: Vector[Float]) = {
      
      // TODO: this could probably be much faster?
      require(a.cols == b.length)
      val res = DenseVector.zeros[Float](a.rows)
      var c = 0
      while(c < a.cols) {
        var r = 0
        while (r < a.rows) {
          val v = a(r, c)
          res(r) += v * b(c)
          r += 1
        }
        c += 1
      }

      res                                                               
    }
  }; implicit val canMulM_V_Float = new canMulM_V_Float()


  class canMulM_M_Float private[linalg] () extends BinaryRegistry[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpMulMatrix, DenseMatrix[Float]] {
    override def bindingMissing(a: DenseMatrix[Float], b: Matrix[Float]) = {
      
      // TODO: this could probably be much faster
      val res = DenseMatrix.zeros[Float](a.rows, b.cols)
      require(a.cols == b.rows)
      var c = 0
      while(c < a.cols) {
        var r = 0
        while (r < a.rows) {
          val v = a(r, c)
          var j = 0
          while(j < b.cols) {
            res(r, j) += v * b(c, j)
            j += 1
          }
          r += 1
        }
        c += 1
      }

      res                                                               
    }
  }; implicit val canMulM_M_Float = new canMulM_M_Float()

}

/** This is an auto-generated trait providing multiplication for DenseMatrix */
trait DenseMatrixMultOps_Int extends DenseMatrixOps_Int { this: DenseMatrix.type =>

  class canMulM_V_Int private[linalg] () extends BinaryRegistry[DenseMatrix[Int], Vector[Int], breeze.linalg.operators.OpMulMatrix, DenseVector[Int]] {
    override def bindingMissing(a: DenseMatrix[Int], b: Vector[Int]) = {
      
      // TODO: this could probably be much faster?
      require(a.cols == b.length)
      val res = DenseVector.zeros[Int](a.rows)
      var c = 0
      while(c < a.cols) {
        var r = 0
        while (r < a.rows) {
          val v = a(r, c)
          res(r) += v * b(c)
          r += 1
        }
        c += 1
      }

      res                                                               
    }
  }; implicit val canMulM_V_Int = new canMulM_V_Int()


  class canMulM_M_Int private[linalg] () extends BinaryRegistry[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpMulMatrix, DenseMatrix[Int]] {
    override def bindingMissing(a: DenseMatrix[Int], b: Matrix[Int]) = {
      
      // TODO: this could probably be much faster
      val res = DenseMatrix.zeros[Int](a.rows, b.cols)
      require(a.cols == b.rows)
      var c = 0
      while(c < a.cols) {
        var r = 0
        while (r < a.rows) {
          val v = a(r, c)
          var j = 0
          while(j < b.cols) {
            res(r, j) += v * b(c, j)
            j += 1
          }
          r += 1
        }
        c += 1
      }

      res                                                               
    }
  }; implicit val canMulM_M_Int = new canMulM_M_Int()

}
