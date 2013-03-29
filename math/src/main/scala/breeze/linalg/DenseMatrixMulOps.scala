package breeze.linalg
import java.util._
import breeze.linalg.operators._
import breeze.linalg.support._
import breeze.math.Complex
import breeze.math.Complex._
import breeze.numerics._
import breeze.storage.DefaultArrayValue._

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
  };
  val canMulM_V_Double = new canMulM_V_Double()
  implicit def canMulM_V_Double_def[A <: DenseMatrix[Double], B <: Vector[Double]]:BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, DenseVector[Double]] = (
    canMulM_V_Double.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, DenseVector[Double]]]
  )
    

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
  };
  val canMulM_M_Double = new canMulM_M_Double()
  implicit def canMulM_M_Double_def[A <: DenseMatrix[Double], B <: Matrix[Double]]:BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, DenseMatrix[Double]] = (
    canMulM_M_Double.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, DenseMatrix[Double]]]
  )
    
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
  };
  val canMulM_V_Float = new canMulM_V_Float()
  implicit def canMulM_V_Float_def[A <: DenseMatrix[Float], B <: Vector[Float]]:BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, DenseVector[Float]] = (
    canMulM_V_Float.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, DenseVector[Float]]]
  )
    

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
  };
  val canMulM_M_Float = new canMulM_M_Float()
  implicit def canMulM_M_Float_def[A <: DenseMatrix[Float], B <: Matrix[Float]]:BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, DenseMatrix[Float]] = (
    canMulM_M_Float.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, DenseMatrix[Float]]]
  )
    
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
  };
  val canMulM_V_Int = new canMulM_V_Int()
  implicit def canMulM_V_Int_def[A <: DenseMatrix[Int], B <: Vector[Int]]:BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, DenseVector[Int]] = (
    canMulM_V_Int.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, DenseVector[Int]]]
  )
    

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
  };
  val canMulM_M_Int = new canMulM_M_Int()
  implicit def canMulM_M_Int_def[A <: DenseMatrix[Int], B <: Matrix[Int]]:BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, DenseMatrix[Int]] = (
    canMulM_M_Int.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, DenseMatrix[Int]]]
  )
    
}

/** This is an auto-generated trait providing multiplication for DenseMatrix */
trait DenseMatrixMultOps_Complex extends DenseMatrixOps_Complex { this: DenseMatrix.type =>

  class canMulM_V_Complex private[linalg] () extends BinaryRegistry[DenseMatrix[Complex], Vector[Complex], breeze.linalg.operators.OpMulMatrix, DenseVector[Complex]] {
    override def bindingMissing(a: DenseMatrix[Complex], b: Vector[Complex]) = {
      
      // TODO: this could probably be much faster?
      require(a.cols == b.length)
      val res = DenseVector.zeros[Complex](a.rows)
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
  };
  val canMulM_V_Complex = new canMulM_V_Complex()
  implicit def canMulM_V_Complex_def[A <: DenseMatrix[Complex], B <: Vector[Complex]]:BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, DenseVector[Complex]] = (
    canMulM_V_Complex.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, DenseVector[Complex]]]
  )
    

  class canMulM_M_Complex private[linalg] () extends BinaryRegistry[DenseMatrix[Complex], Matrix[Complex], breeze.linalg.operators.OpMulMatrix, DenseMatrix[Complex]] {
    override def bindingMissing(a: DenseMatrix[Complex], b: Matrix[Complex]) = {
      
      // TODO: this could probably be much faster
      val res = DenseMatrix.zeros[Complex](a.rows, b.cols)
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
  };
  val canMulM_M_Complex = new canMulM_M_Complex()
  implicit def canMulM_M_Complex_def[A <: DenseMatrix[Complex], B <: Matrix[Complex]]:BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, DenseMatrix[Complex]] = (
    canMulM_M_Complex.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, DenseMatrix[Complex]]]
  )
    
}
