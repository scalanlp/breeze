package breeze.linalg
import breeze.linalg.operators._
import breeze.linalg.support._
import breeze.numerics._
/** This is an auto-generated trait providing operators for DenseMatrix. */
trait DenseMatrixOps_Double { this: DenseMatrix.type =>

  def pureFromUpdate_Double[Other,Op<:OpType](op: BinaryUpdateOp[DenseMatrix[Double], Other, Op])(implicit copy: CanCopy[DenseMatrix[Double]]):BinaryOp[DenseMatrix[Double], Other, Op, DenseMatrix[Double]] = {
    new BinaryOp[DenseMatrix[Double], Other, Op, DenseMatrix[Double]] {
      override def apply(a : DenseMatrix[Double], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }
        

  class canSetInto_DV_S_Double private[linalg] () extends BinaryUpdateOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpSet] {
    def apply(a: DenseMatrix[Double], b: Double) {
      
    val ad = a.data
    var c = 0
    while(c < a.cols) {
       var r = 0
       while(r < a.rows) {
         ad(a.linearIndex(r, c)) = b
         r += 1
       }
       c += 1
    }
    
    }
  }; implicit val canSetInto_DV_S_Double = new canSetInto_DV_S_Double ()


  implicit val canSet_DV_S_Double: BinaryOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpSet, DenseMatrix[Double]] = pureFromUpdate_Double(canSetInto_DV_S_Double)


  class canSetInto_DV_V_Double private[linalg] () extends BinaryUpdateOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpSet] {
    def apply(a: DenseMatrix[Double], b: Matrix[Double]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = b(r,c)
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canSetInto_DV_V_Double = new canSetInto_DV_V_Double ()


  implicit val canSet_DV_V_Double: BinaryOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpSet, DenseMatrix[Double]] = pureFromUpdate_Double(canSetInto_DV_V_Double)


  class canDivInto_DV_DV_Double private[linalg] () extends BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpDiv] {
    def apply(a: DenseMatrix[Double], b: DenseMatrix[Double]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) / bd(b.linearIndex(r,c))
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canDivInto_DV_DV_Double = new canDivInto_DV_DV_Double ()


  implicit val canDiv_DV_DV_Double: BinaryOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpDiv, DenseMatrix[Double]] = pureFromUpdate_Double(canDivInto_DV_DV_Double)


  class canDivInto_DV_S_Double private[linalg] () extends BinaryUpdateOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpDiv] {
    def apply(a: DenseMatrix[Double], b: Double) {
      
    val ad = a.data
    var c = 0
    while(c < a.cols) {
       var r = 0
       while(r < a.rows) {
         ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) / b
         r += 1
       }
       c += 1
    }
    
    }
  }; implicit val canDivInto_DV_S_Double = new canDivInto_DV_S_Double ()


  implicit val canDiv_DV_S_Double: BinaryOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpDiv, DenseMatrix[Double]] = pureFromUpdate_Double(canDivInto_DV_S_Double)


  class canDivInto_DV_V_Double private[linalg] () extends BinaryUpdateOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpDiv] {
    def apply(a: DenseMatrix[Double], b: Matrix[Double]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) / b(r,c)
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canDivInto_DV_V_Double = new canDivInto_DV_V_Double ()


  implicit val canDiv_DV_V_Double: BinaryOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpDiv, DenseMatrix[Double]] = pureFromUpdate_Double(canDivInto_DV_V_Double)


  class canPowInto_DV_DV_Double private[linalg] () extends BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpPow] {
    def apply(a: DenseMatrix[Double], b: DenseMatrix[Double]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = scala.math.pow(ad(a.linearIndex(r,c)), bd(b.linearIndex(r,c)))
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canPowInto_DV_DV_Double = new canPowInto_DV_DV_Double ()


  implicit val canPow_DV_DV_Double: BinaryOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpPow, DenseMatrix[Double]] = pureFromUpdate_Double(canPowInto_DV_DV_Double)


  class canPowInto_DV_S_Double private[linalg] () extends BinaryUpdateOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpPow] {
    def apply(a: DenseMatrix[Double], b: Double) {
      
    val ad = a.data
    var c = 0
    while(c < a.cols) {
       var r = 0
       while(r < a.rows) {
         ad(a.linearIndex(r, c)) = scala.math.pow(ad(a.linearIndex(r,c)), b)
         r += 1
       }
       c += 1
    }
    
    }
  }; implicit val canPowInto_DV_S_Double = new canPowInto_DV_S_Double ()


  implicit val canPow_DV_S_Double: BinaryOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpPow, DenseMatrix[Double]] = pureFromUpdate_Double(canPowInto_DV_S_Double)


  class canPowInto_DV_V_Double private[linalg] () extends BinaryUpdateOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpPow] {
    def apply(a: DenseMatrix[Double], b: Matrix[Double]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = scala.math.pow(ad(a.linearIndex(r,c)), b(r,c))
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canPowInto_DV_V_Double = new canPowInto_DV_V_Double ()


  implicit val canPow_DV_V_Double: BinaryOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpPow, DenseMatrix[Double]] = pureFromUpdate_Double(canPowInto_DV_V_Double)


  class canModInto_DV_DV_Double private[linalg] () extends BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpMod] {
    def apply(a: DenseMatrix[Double], b: DenseMatrix[Double]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) % bd(b.linearIndex(r,c))
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canModInto_DV_DV_Double = new canModInto_DV_DV_Double ()


  implicit val canMod_DV_DV_Double: BinaryOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpMod, DenseMatrix[Double]] = pureFromUpdate_Double(canModInto_DV_DV_Double)


  class canModInto_DV_S_Double private[linalg] () extends BinaryUpdateOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpMod] {
    def apply(a: DenseMatrix[Double], b: Double) {
      
    val ad = a.data
    var c = 0
    while(c < a.cols) {
       var r = 0
       while(r < a.rows) {
         ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) % b
         r += 1
       }
       c += 1
    }
    
    }
  }; implicit val canModInto_DV_S_Double = new canModInto_DV_S_Double ()


  implicit val canMod_DV_S_Double: BinaryOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpMod, DenseMatrix[Double]] = pureFromUpdate_Double(canModInto_DV_S_Double)


  class canModInto_DV_V_Double private[linalg] () extends BinaryUpdateOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpMod] {
    def apply(a: DenseMatrix[Double], b: Matrix[Double]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) % b(r,c)
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canModInto_DV_V_Double = new canModInto_DV_V_Double ()


  implicit val canMod_DV_V_Double: BinaryOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpMod, DenseMatrix[Double]] = pureFromUpdate_Double(canModInto_DV_V_Double)


  class canAddInto_DV_S_Double private[linalg] () extends BinaryUpdateOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpAdd] {
    def apply(a: DenseMatrix[Double], b: Double) {
      
    val ad = a.data
    var c = 0
    while(c < a.cols) {
       var r = 0
       while(r < a.rows) {
         ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) + b
         r += 1
       }
       c += 1
    }
    
    }
  }; implicit val canAddInto_DV_S_Double = new canAddInto_DV_S_Double ()


  implicit val canAdd_DV_S_Double: BinaryOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpAdd, DenseMatrix[Double]] = pureFromUpdate_Double(canAddInto_DV_S_Double)


  class canAddInto_DV_V_Double private[linalg] () extends BinaryUpdateOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpAdd] {
    def apply(a: DenseMatrix[Double], b: Matrix[Double]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) + b(r,c)
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canAddInto_DV_V_Double = new canAddInto_DV_V_Double ()


  implicit val canAdd_DV_V_Double: BinaryOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpAdd, DenseMatrix[Double]] = pureFromUpdate_Double(canAddInto_DV_V_Double)


  class canMulScalarInto_DV_DV_Double private[linalg] () extends BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpMulScalar] {
    def apply(a: DenseMatrix[Double], b: DenseMatrix[Double]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) * bd(b.linearIndex(r,c))
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canMulScalarInto_DV_DV_Double = new canMulScalarInto_DV_DV_Double ()


  implicit val canMulScalar_DV_DV_Double: BinaryOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpMulScalar, DenseMatrix[Double]] = pureFromUpdate_Double(canMulScalarInto_DV_DV_Double)


  class canMulScalarInto_DV_V_Double private[linalg] () extends BinaryUpdateOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpMulScalar] {
    def apply(a: DenseMatrix[Double], b: Matrix[Double]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) * b(r,c)
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canMulScalarInto_DV_V_Double = new canMulScalarInto_DV_V_Double ()


  implicit val canMulScalar_DV_V_Double: BinaryOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpMulScalar, DenseMatrix[Double]] = pureFromUpdate_Double(canMulScalarInto_DV_V_Double)


  class canSubInto_DV_S_Double private[linalg] () extends BinaryUpdateOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpSub] {
    def apply(a: DenseMatrix[Double], b: Double) {
      
    val ad = a.data
    var c = 0
    while(c < a.cols) {
       var r = 0
       while(r < a.rows) {
         ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) - b
         r += 1
       }
       c += 1
    }
    
    }
  }; implicit val canSubInto_DV_S_Double = new canSubInto_DV_S_Double ()


  implicit val canSub_DV_S_Double: BinaryOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpSub, DenseMatrix[Double]] = pureFromUpdate_Double(canSubInto_DV_S_Double)


  class canSubInto_DV_V_Double private[linalg] () extends BinaryUpdateOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpSub] {
    def apply(a: DenseMatrix[Double], b: Matrix[Double]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) - b(r,c)
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canSubInto_DV_V_Double = new canSubInto_DV_V_Double ()


  implicit val canSub_DV_V_Double: BinaryOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpSub, DenseMatrix[Double]] = pureFromUpdate_Double(canSubInto_DV_V_Double)

}
/** This is an auto-generated trait providing operators for DenseMatrix. */
trait DenseMatrixOps_Float { this: DenseMatrix.type =>

  def pureFromUpdate_Float[Other,Op<:OpType](op: BinaryUpdateOp[DenseMatrix[Float], Other, Op])(implicit copy: CanCopy[DenseMatrix[Float]]):BinaryOp[DenseMatrix[Float], Other, Op, DenseMatrix[Float]] = {
    new BinaryOp[DenseMatrix[Float], Other, Op, DenseMatrix[Float]] {
      override def apply(a : DenseMatrix[Float], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }
        

  class canSetInto_DV_DV_Float private[linalg] () extends BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpSet] {
    def apply(a: DenseMatrix[Float], b: DenseMatrix[Float]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = bd(b.linearIndex(r,c))
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canSetInto_DV_DV_Float = new canSetInto_DV_DV_Float ()


  implicit val canSet_DV_DV_Float: BinaryOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpSet, DenseMatrix[Float]] = pureFromUpdate_Float(canSetInto_DV_DV_Float)


  class canSetInto_DV_S_Float private[linalg] () extends BinaryUpdateOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpSet] {
    def apply(a: DenseMatrix[Float], b: Float) {
      
    val ad = a.data
    var c = 0
    while(c < a.cols) {
       var r = 0
       while(r < a.rows) {
         ad(a.linearIndex(r, c)) = b
         r += 1
       }
       c += 1
    }
    
    }
  }; implicit val canSetInto_DV_S_Float = new canSetInto_DV_S_Float ()


  implicit val canSet_DV_S_Float: BinaryOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpSet, DenseMatrix[Float]] = pureFromUpdate_Float(canSetInto_DV_S_Float)


  class canSetInto_DV_V_Float private[linalg] () extends BinaryUpdateOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpSet] {
    def apply(a: DenseMatrix[Float], b: Matrix[Float]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = b(r,c)
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canSetInto_DV_V_Float = new canSetInto_DV_V_Float ()


  implicit val canSet_DV_V_Float: BinaryOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpSet, DenseMatrix[Float]] = pureFromUpdate_Float(canSetInto_DV_V_Float)


  class canDivInto_DV_DV_Float private[linalg] () extends BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpDiv] {
    def apply(a: DenseMatrix[Float], b: DenseMatrix[Float]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) / bd(b.linearIndex(r,c))
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canDivInto_DV_DV_Float = new canDivInto_DV_DV_Float ()


  implicit val canDiv_DV_DV_Float: BinaryOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpDiv, DenseMatrix[Float]] = pureFromUpdate_Float(canDivInto_DV_DV_Float)


  class canDivInto_DV_S_Float private[linalg] () extends BinaryUpdateOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpDiv] {
    def apply(a: DenseMatrix[Float], b: Float) {
      
    val ad = a.data
    var c = 0
    while(c < a.cols) {
       var r = 0
       while(r < a.rows) {
         ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) / b
         r += 1
       }
       c += 1
    }
    
    }
  }; implicit val canDivInto_DV_S_Float = new canDivInto_DV_S_Float ()


  implicit val canDiv_DV_S_Float: BinaryOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpDiv, DenseMatrix[Float]] = pureFromUpdate_Float(canDivInto_DV_S_Float)


  class canDivInto_DV_V_Float private[linalg] () extends BinaryUpdateOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpDiv] {
    def apply(a: DenseMatrix[Float], b: Matrix[Float]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) / b(r,c)
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canDivInto_DV_V_Float = new canDivInto_DV_V_Float ()


  implicit val canDiv_DV_V_Float: BinaryOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpDiv, DenseMatrix[Float]] = pureFromUpdate_Float(canDivInto_DV_V_Float)


  class canPowInto_DV_DV_Float private[linalg] () extends BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpPow] {
    def apply(a: DenseMatrix[Float], b: DenseMatrix[Float]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = scala.math.pow(ad(a.linearIndex(r,c)), bd(b.linearIndex(r,c))).toFloat
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canPowInto_DV_DV_Float = new canPowInto_DV_DV_Float ()


  implicit val canPow_DV_DV_Float: BinaryOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpPow, DenseMatrix[Float]] = pureFromUpdate_Float(canPowInto_DV_DV_Float)


  class canPowInto_DV_S_Float private[linalg] () extends BinaryUpdateOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpPow] {
    def apply(a: DenseMatrix[Float], b: Float) {
      
    val ad = a.data
    var c = 0
    while(c < a.cols) {
       var r = 0
       while(r < a.rows) {
         ad(a.linearIndex(r, c)) = scala.math.pow(ad(a.linearIndex(r,c)), b).toFloat
         r += 1
       }
       c += 1
    }
    
    }
  }; implicit val canPowInto_DV_S_Float = new canPowInto_DV_S_Float ()


  implicit val canPow_DV_S_Float: BinaryOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpPow, DenseMatrix[Float]] = pureFromUpdate_Float(canPowInto_DV_S_Float)


  class canPowInto_DV_V_Float private[linalg] () extends BinaryUpdateOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpPow] {
    def apply(a: DenseMatrix[Float], b: Matrix[Float]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = scala.math.pow(ad(a.linearIndex(r,c)), b(r,c)).toFloat
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canPowInto_DV_V_Float = new canPowInto_DV_V_Float ()


  implicit val canPow_DV_V_Float: BinaryOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpPow, DenseMatrix[Float]] = pureFromUpdate_Float(canPowInto_DV_V_Float)


  class canModInto_DV_DV_Float private[linalg] () extends BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpMod] {
    def apply(a: DenseMatrix[Float], b: DenseMatrix[Float]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) % bd(b.linearIndex(r,c))
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canModInto_DV_DV_Float = new canModInto_DV_DV_Float ()


  implicit val canMod_DV_DV_Float: BinaryOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpMod, DenseMatrix[Float]] = pureFromUpdate_Float(canModInto_DV_DV_Float)


  class canModInto_DV_S_Float private[linalg] () extends BinaryUpdateOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpMod] {
    def apply(a: DenseMatrix[Float], b: Float) {
      
    val ad = a.data
    var c = 0
    while(c < a.cols) {
       var r = 0
       while(r < a.rows) {
         ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) % b
         r += 1
       }
       c += 1
    }
    
    }
  }; implicit val canModInto_DV_S_Float = new canModInto_DV_S_Float ()


  implicit val canMod_DV_S_Float: BinaryOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpMod, DenseMatrix[Float]] = pureFromUpdate_Float(canModInto_DV_S_Float)


  class canModInto_DV_V_Float private[linalg] () extends BinaryUpdateOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpMod] {
    def apply(a: DenseMatrix[Float], b: Matrix[Float]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) % b(r,c)
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canModInto_DV_V_Float = new canModInto_DV_V_Float ()


  implicit val canMod_DV_V_Float: BinaryOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpMod, DenseMatrix[Float]] = pureFromUpdate_Float(canModInto_DV_V_Float)


  class canAddInto_DV_DV_Float private[linalg] () extends BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpAdd] {
    def apply(a: DenseMatrix[Float], b: DenseMatrix[Float]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) + bd(b.linearIndex(r,c))
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canAddInto_DV_DV_Float = new canAddInto_DV_DV_Float ()


  implicit val canAdd_DV_DV_Float: BinaryOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpAdd, DenseMatrix[Float]] = pureFromUpdate_Float(canAddInto_DV_DV_Float)


  class canAddInto_DV_S_Float private[linalg] () extends BinaryUpdateOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpAdd] {
    def apply(a: DenseMatrix[Float], b: Float) {
      
    val ad = a.data
    var c = 0
    while(c < a.cols) {
       var r = 0
       while(r < a.rows) {
         ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) + b
         r += 1
       }
       c += 1
    }
    
    }
  }; implicit val canAddInto_DV_S_Float = new canAddInto_DV_S_Float ()


  implicit val canAdd_DV_S_Float: BinaryOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpAdd, DenseMatrix[Float]] = pureFromUpdate_Float(canAddInto_DV_S_Float)


  class canAddInto_DV_V_Float private[linalg] () extends BinaryUpdateOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpAdd] {
    def apply(a: DenseMatrix[Float], b: Matrix[Float]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) + b(r,c)
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canAddInto_DV_V_Float = new canAddInto_DV_V_Float ()


  implicit val canAdd_DV_V_Float: BinaryOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpAdd, DenseMatrix[Float]] = pureFromUpdate_Float(canAddInto_DV_V_Float)


  class canMulScalarInto_DV_DV_Float private[linalg] () extends BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpMulScalar] {
    def apply(a: DenseMatrix[Float], b: DenseMatrix[Float]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) * bd(b.linearIndex(r,c))
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canMulScalarInto_DV_DV_Float = new canMulScalarInto_DV_DV_Float ()


  implicit val canMulScalar_DV_DV_Float: BinaryOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpMulScalar, DenseMatrix[Float]] = pureFromUpdate_Float(canMulScalarInto_DV_DV_Float)


  class canMulScalarInto_DV_S_Float private[linalg] () extends BinaryUpdateOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpMulScalar] {
    def apply(a: DenseMatrix[Float], b: Float) {
      
    val ad = a.data
    var c = 0
    while(c < a.cols) {
       var r = 0
       while(r < a.rows) {
         ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) * b
         r += 1
       }
       c += 1
    }
    
    }
  }; implicit val canMulScalarInto_DV_S_Float = new canMulScalarInto_DV_S_Float ()


  implicit val canMulScalar_DV_S_Float: BinaryOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpMulScalar, DenseMatrix[Float]] = pureFromUpdate_Float(canMulScalarInto_DV_S_Float)


  class canMulScalarInto_DV_V_Float private[linalg] () extends BinaryUpdateOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpMulScalar] {
    def apply(a: DenseMatrix[Float], b: Matrix[Float]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) * b(r,c)
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canMulScalarInto_DV_V_Float = new canMulScalarInto_DV_V_Float ()


  implicit val canMulScalar_DV_V_Float: BinaryOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpMulScalar, DenseMatrix[Float]] = pureFromUpdate_Float(canMulScalarInto_DV_V_Float)


  class canSubInto_DV_DV_Float private[linalg] () extends BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpSub] {
    def apply(a: DenseMatrix[Float], b: DenseMatrix[Float]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) - bd(b.linearIndex(r,c))
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canSubInto_DV_DV_Float = new canSubInto_DV_DV_Float ()


  implicit val canSub_DV_DV_Float: BinaryOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpSub, DenseMatrix[Float]] = pureFromUpdate_Float(canSubInto_DV_DV_Float)


  class canSubInto_DV_S_Float private[linalg] () extends BinaryUpdateOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpSub] {
    def apply(a: DenseMatrix[Float], b: Float) {
      
    val ad = a.data
    var c = 0
    while(c < a.cols) {
       var r = 0
       while(r < a.rows) {
         ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) - b
         r += 1
       }
       c += 1
    }
    
    }
  }; implicit val canSubInto_DV_S_Float = new canSubInto_DV_S_Float ()


  implicit val canSub_DV_S_Float: BinaryOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpSub, DenseMatrix[Float]] = pureFromUpdate_Float(canSubInto_DV_S_Float)


  class canSubInto_DV_V_Float private[linalg] () extends BinaryUpdateOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpSub] {
    def apply(a: DenseMatrix[Float], b: Matrix[Float]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) - b(r,c)
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canSubInto_DV_V_Float = new canSubInto_DV_V_Float ()


  implicit val canSub_DV_V_Float: BinaryOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpSub, DenseMatrix[Float]] = pureFromUpdate_Float(canSubInto_DV_V_Float)

}
/** This is an auto-generated trait providing operators for DenseMatrix. */
trait DenseMatrixOps_Int { this: DenseMatrix.type =>

  def pureFromUpdate_Int[Other,Op<:OpType](op: BinaryUpdateOp[DenseMatrix[Int], Other, Op])(implicit copy: CanCopy[DenseMatrix[Int]]):BinaryOp[DenseMatrix[Int], Other, Op, DenseMatrix[Int]] = {
    new BinaryOp[DenseMatrix[Int], Other, Op, DenseMatrix[Int]] {
      override def apply(a : DenseMatrix[Int], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }
        

  class canSetInto_DV_DV_Int private[linalg] () extends BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpSet] {
    def apply(a: DenseMatrix[Int], b: DenseMatrix[Int]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = bd(b.linearIndex(r,c))
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canSetInto_DV_DV_Int = new canSetInto_DV_DV_Int ()


  implicit val canSet_DV_DV_Int: BinaryOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpSet, DenseMatrix[Int]] = pureFromUpdate_Int(canSetInto_DV_DV_Int)


  class canSetInto_DV_S_Int private[linalg] () extends BinaryUpdateOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpSet] {
    def apply(a: DenseMatrix[Int], b: Int) {
      
    val ad = a.data
    var c = 0
    while(c < a.cols) {
       var r = 0
       while(r < a.rows) {
         ad(a.linearIndex(r, c)) = b
         r += 1
       }
       c += 1
    }
    
    }
  }; implicit val canSetInto_DV_S_Int = new canSetInto_DV_S_Int ()


  implicit val canSet_DV_S_Int: BinaryOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpSet, DenseMatrix[Int]] = pureFromUpdate_Int(canSetInto_DV_S_Int)


  class canSetInto_DV_V_Int private[linalg] () extends BinaryUpdateOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpSet] {
    def apply(a: DenseMatrix[Int], b: Matrix[Int]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = b(r,c)
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canSetInto_DV_V_Int = new canSetInto_DV_V_Int ()


  implicit val canSet_DV_V_Int: BinaryOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpSet, DenseMatrix[Int]] = pureFromUpdate_Int(canSetInto_DV_V_Int)


  class canDivInto_DV_DV_Int private[linalg] () extends BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpDiv] {
    def apply(a: DenseMatrix[Int], b: DenseMatrix[Int]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) / bd(b.linearIndex(r,c))
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canDivInto_DV_DV_Int = new canDivInto_DV_DV_Int ()


  implicit val canDiv_DV_DV_Int: BinaryOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpDiv, DenseMatrix[Int]] = pureFromUpdate_Int(canDivInto_DV_DV_Int)


  class canDivInto_DV_S_Int private[linalg] () extends BinaryUpdateOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpDiv] {
    def apply(a: DenseMatrix[Int], b: Int) {
      
    val ad = a.data
    var c = 0
    while(c < a.cols) {
       var r = 0
       while(r < a.rows) {
         ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) / b
         r += 1
       }
       c += 1
    }
    
    }
  }; implicit val canDivInto_DV_S_Int = new canDivInto_DV_S_Int ()


  implicit val canDiv_DV_S_Int: BinaryOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpDiv, DenseMatrix[Int]] = pureFromUpdate_Int(canDivInto_DV_S_Int)


  class canDivInto_DV_V_Int private[linalg] () extends BinaryUpdateOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpDiv] {
    def apply(a: DenseMatrix[Int], b: Matrix[Int]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) / b(r,c)
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canDivInto_DV_V_Int = new canDivInto_DV_V_Int ()


  implicit val canDiv_DV_V_Int: BinaryOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpDiv, DenseMatrix[Int]] = pureFromUpdate_Int(canDivInto_DV_V_Int)


  class canPowInto_DV_DV_Int private[linalg] () extends BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpPow] {
    def apply(a: DenseMatrix[Int], b: DenseMatrix[Int]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = IntMath.ipow(ad(a.linearIndex(r,c)), bd(b.linearIndex(r,c)))
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canPowInto_DV_DV_Int = new canPowInto_DV_DV_Int ()


  implicit val canPow_DV_DV_Int: BinaryOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpPow, DenseMatrix[Int]] = pureFromUpdate_Int(canPowInto_DV_DV_Int)


  class canPowInto_DV_S_Int private[linalg] () extends BinaryUpdateOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpPow] {
    def apply(a: DenseMatrix[Int], b: Int) {
      
    val ad = a.data
    var c = 0
    while(c < a.cols) {
       var r = 0
       while(r < a.rows) {
         ad(a.linearIndex(r, c)) = IntMath.ipow(ad(a.linearIndex(r,c)), b)
         r += 1
       }
       c += 1
    }
    
    }
  }; implicit val canPowInto_DV_S_Int = new canPowInto_DV_S_Int ()


  implicit val canPow_DV_S_Int: BinaryOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpPow, DenseMatrix[Int]] = pureFromUpdate_Int(canPowInto_DV_S_Int)


  class canPowInto_DV_V_Int private[linalg] () extends BinaryUpdateOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpPow] {
    def apply(a: DenseMatrix[Int], b: Matrix[Int]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = IntMath.ipow(ad(a.linearIndex(r,c)), b(r,c))
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canPowInto_DV_V_Int = new canPowInto_DV_V_Int ()


  implicit val canPow_DV_V_Int: BinaryOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpPow, DenseMatrix[Int]] = pureFromUpdate_Int(canPowInto_DV_V_Int)


  class canModInto_DV_DV_Int private[linalg] () extends BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpMod] {
    def apply(a: DenseMatrix[Int], b: DenseMatrix[Int]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) % bd(b.linearIndex(r,c))
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canModInto_DV_DV_Int = new canModInto_DV_DV_Int ()


  implicit val canMod_DV_DV_Int: BinaryOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpMod, DenseMatrix[Int]] = pureFromUpdate_Int(canModInto_DV_DV_Int)


  class canModInto_DV_S_Int private[linalg] () extends BinaryUpdateOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpMod] {
    def apply(a: DenseMatrix[Int], b: Int) {
      
    val ad = a.data
    var c = 0
    while(c < a.cols) {
       var r = 0
       while(r < a.rows) {
         ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) % b
         r += 1
       }
       c += 1
    }
    
    }
  }; implicit val canModInto_DV_S_Int = new canModInto_DV_S_Int ()


  implicit val canMod_DV_S_Int: BinaryOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpMod, DenseMatrix[Int]] = pureFromUpdate_Int(canModInto_DV_S_Int)


  class canModInto_DV_V_Int private[linalg] () extends BinaryUpdateOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpMod] {
    def apply(a: DenseMatrix[Int], b: Matrix[Int]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) % b(r,c)
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canModInto_DV_V_Int = new canModInto_DV_V_Int ()


  implicit val canMod_DV_V_Int: BinaryOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpMod, DenseMatrix[Int]] = pureFromUpdate_Int(canModInto_DV_V_Int)


  class canAddInto_DV_DV_Int private[linalg] () extends BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpAdd] {
    def apply(a: DenseMatrix[Int], b: DenseMatrix[Int]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) + bd(b.linearIndex(r,c))
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canAddInto_DV_DV_Int = new canAddInto_DV_DV_Int ()


  implicit val canAdd_DV_DV_Int: BinaryOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpAdd, DenseMatrix[Int]] = pureFromUpdate_Int(canAddInto_DV_DV_Int)


  class canAddInto_DV_S_Int private[linalg] () extends BinaryUpdateOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpAdd] {
    def apply(a: DenseMatrix[Int], b: Int) {
      
    val ad = a.data
    var c = 0
    while(c < a.cols) {
       var r = 0
       while(r < a.rows) {
         ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) + b
         r += 1
       }
       c += 1
    }
    
    }
  }; implicit val canAddInto_DV_S_Int = new canAddInto_DV_S_Int ()


  implicit val canAdd_DV_S_Int: BinaryOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpAdd, DenseMatrix[Int]] = pureFromUpdate_Int(canAddInto_DV_S_Int)


  class canAddInto_DV_V_Int private[linalg] () extends BinaryUpdateOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpAdd] {
    def apply(a: DenseMatrix[Int], b: Matrix[Int]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) + b(r,c)
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canAddInto_DV_V_Int = new canAddInto_DV_V_Int ()


  implicit val canAdd_DV_V_Int: BinaryOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpAdd, DenseMatrix[Int]] = pureFromUpdate_Int(canAddInto_DV_V_Int)


  class canMulScalarInto_DV_DV_Int private[linalg] () extends BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpMulScalar] {
    def apply(a: DenseMatrix[Int], b: DenseMatrix[Int]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) * bd(b.linearIndex(r,c))
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canMulScalarInto_DV_DV_Int = new canMulScalarInto_DV_DV_Int ()


  implicit val canMulScalar_DV_DV_Int: BinaryOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpMulScalar, DenseMatrix[Int]] = pureFromUpdate_Int(canMulScalarInto_DV_DV_Int)


  class canMulScalarInto_DV_S_Int private[linalg] () extends BinaryUpdateOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpMulScalar] {
    def apply(a: DenseMatrix[Int], b: Int) {
      
    val ad = a.data
    var c = 0
    while(c < a.cols) {
       var r = 0
       while(r < a.rows) {
         ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) * b
         r += 1
       }
       c += 1
    }
    
    }
  }; implicit val canMulScalarInto_DV_S_Int = new canMulScalarInto_DV_S_Int ()


  implicit val canMulScalar_DV_S_Int: BinaryOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpMulScalar, DenseMatrix[Int]] = pureFromUpdate_Int(canMulScalarInto_DV_S_Int)


  class canMulScalarInto_DV_V_Int private[linalg] () extends BinaryUpdateOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpMulScalar] {
    def apply(a: DenseMatrix[Int], b: Matrix[Int]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) * b(r,c)
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canMulScalarInto_DV_V_Int = new canMulScalarInto_DV_V_Int ()


  implicit val canMulScalar_DV_V_Int: BinaryOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpMulScalar, DenseMatrix[Int]] = pureFromUpdate_Int(canMulScalarInto_DV_V_Int)


  class canSubInto_DV_DV_Int private[linalg] () extends BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpSub] {
    def apply(a: DenseMatrix[Int], b: DenseMatrix[Int]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        val bd = b.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) - bd(b.linearIndex(r,c))
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canSubInto_DV_DV_Int = new canSubInto_DV_DV_Int ()


  implicit val canSub_DV_DV_Int: BinaryOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpSub, DenseMatrix[Int]] = pureFromUpdate_Int(canSubInto_DV_DV_Int)


  class canSubInto_DV_S_Int private[linalg] () extends BinaryUpdateOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpSub] {
    def apply(a: DenseMatrix[Int], b: Int) {
      
    val ad = a.data
    var c = 0
    while(c < a.cols) {
       var r = 0
       while(r < a.rows) {
         ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) - b
         r += 1
       }
       c += 1
    }
    
    }
  }; implicit val canSubInto_DV_S_Int = new canSubInto_DV_S_Int ()


  implicit val canSub_DV_S_Int: BinaryOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpSub, DenseMatrix[Int]] = pureFromUpdate_Int(canSubInto_DV_S_Int)


  class canSubInto_DV_V_Int private[linalg] () extends BinaryUpdateOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpSub] {
    def apply(a: DenseMatrix[Int], b: Matrix[Int]) {
      
        require(a.rows == b.rows, "Matrices must have same number of rows!")
        require(a.cols == b.cols, "Matrices must have same number of cols!")
        val ad = a.data
        var c = 0
        while(c < a.cols) {
          var r = 0
          while(r < a.rows) {
            ad(a.linearIndex(r, c)) = ad(a.linearIndex(r,c)) - b(r,c)
            r += 1
          }
          c += 1
        }
    
    }
  }; implicit val canSubInto_DV_V_Int = new canSubInto_DV_V_Int ()


  implicit val canSub_DV_V_Int: BinaryOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpSub, DenseMatrix[Int]] = pureFromUpdate_Int(canSubInto_DV_V_Int)

}
