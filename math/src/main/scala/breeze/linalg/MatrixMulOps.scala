package breeze.linalg
import java.util._
import breeze.linalg.operators._
import breeze.linalg.support._
import breeze.math.Complex
import breeze.math.Complex._
import breeze.numerics._

/** This is an auto-generated trait providing multiplication for Matrix */
trait MatrixMultOps_Double { this: Matrix.type =>

  class canMulM_V_Double private[linalg] () extends BinaryRegistry[Matrix[Double], Vector[Double], breeze.linalg.operators.OpMulMatrix, Vector[Double]] {
    override def bindingMissing(a: Matrix[Double], b: Vector[Double]) = {
      
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
  implicit def canMulM_V_Double_def[A <: Matrix[Double], B <: Vector[Double]]:BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, Vector[Double]] = (
    canMulM_V_Double.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, Vector[Double]]]
  )
    

  class canMulM_M_Double private[linalg] () extends BinaryRegistry[Matrix[Double], Matrix[Double], breeze.linalg.operators.OpMulMatrix, Matrix[Double]] {
    override def bindingMissing(a: Matrix[Double], b: Matrix[Double]) = {
      
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
  implicit def canMulM_M_Double_def[A <: Matrix[Double], B <: Matrix[Double]]:BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, Matrix[Double]] = (
    canMulM_M_Double.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, Matrix[Double]]]
  )
    
}

/** This is an auto-generated trait providing multiplication for Matrix */
trait MatrixMultOps_Float { this: Matrix.type =>

  class canMulM_V_Float private[linalg] () extends BinaryRegistry[Matrix[Float], Vector[Float], breeze.linalg.operators.OpMulMatrix, Vector[Float]] {
    override def bindingMissing(a: Matrix[Float], b: Vector[Float]) = {
      
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
  implicit def canMulM_V_Float_def[A <: Matrix[Float], B <: Vector[Float]]:BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, Vector[Float]] = (
    canMulM_V_Float.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, Vector[Float]]]
  )
    

  class canMulM_M_Float private[linalg] () extends BinaryRegistry[Matrix[Float], Matrix[Float], breeze.linalg.operators.OpMulMatrix, Matrix[Float]] {
    override def bindingMissing(a: Matrix[Float], b: Matrix[Float]) = {
      
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
  implicit def canMulM_M_Float_def[A <: Matrix[Float], B <: Matrix[Float]]:BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, Matrix[Float]] = (
    canMulM_M_Float.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, Matrix[Float]]]
  )
    
}

/** This is an auto-generated trait providing multiplication for Matrix */
trait MatrixMultOps_Int { this: Matrix.type =>

  class canMulM_V_Int private[linalg] () extends BinaryRegistry[Matrix[Int], Vector[Int], breeze.linalg.operators.OpMulMatrix, Vector[Int]] {
    override def bindingMissing(a: Matrix[Int], b: Vector[Int]) = {
      
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
  implicit def canMulM_V_Int_def[A <: Matrix[Int], B <: Vector[Int]]:BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, Vector[Int]] = (
    canMulM_V_Int.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, Vector[Int]]]
  )
    

  class canMulM_M_Int private[linalg] () extends BinaryRegistry[Matrix[Int], Matrix[Int], breeze.linalg.operators.OpMulMatrix, Matrix[Int]] {
    override def bindingMissing(a: Matrix[Int], b: Matrix[Int]) = {
      
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
  implicit def canMulM_M_Int_def[A <: Matrix[Int], B <: Matrix[Int]]:BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, Matrix[Int]] = (
    canMulM_M_Int.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, Matrix[Int]]]
  )
    
}

/** This is an auto-generated trait providing multiplication for Matrix */
trait MatrixMultOps_Complex { this: Matrix.type =>

  class canMulM_V_Complex private[linalg] () extends BinaryRegistry[Matrix[Complex], Vector[Complex], breeze.linalg.operators.OpMulMatrix, Vector[Complex]] {
    override def bindingMissing(a: Matrix[Complex], b: Vector[Complex]) = {
      
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
  implicit def canMulM_V_Complex_def[A <: Matrix[Complex], B <: Vector[Complex]]:BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, Vector[Complex]] = (
    canMulM_V_Complex.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, Vector[Complex]]]
  )
    

  class canMulM_M_Complex private[linalg] () extends BinaryRegistry[Matrix[Complex], Matrix[Complex], breeze.linalg.operators.OpMulMatrix, Matrix[Complex]] {
    override def bindingMissing(a: Matrix[Complex], b: Matrix[Complex]) = {
      
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
  implicit def canMulM_M_Complex_def[A <: Matrix[Complex], B <: Matrix[Complex]]:BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, Matrix[Complex]] = (
    canMulM_M_Complex.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulMatrix, Matrix[Complex]]]
  )
    
}
