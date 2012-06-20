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
        

  implicit val canPowInto_DV_DV_Double: BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpPow] = {
    new BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpPow] {
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
    }
  }


  implicit val canPow_DV_DV_Double: BinaryOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpPow, DenseMatrix[Double]] = pureFromUpdate_Double(canPowInto_DV_DV_Double)


  implicit val canPowInto_DV_S_Double: BinaryUpdateOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpPow] = {
    new BinaryUpdateOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpPow] {
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
    }
  }


  implicit val canPow_DV_S_Double: BinaryOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpPow, DenseMatrix[Double]] = pureFromUpdate_Double(canPowInto_DV_S_Double)


  implicit val canPowInto_DV_V_Double: BinaryUpdateOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpPow] = {
    new BinaryUpdateOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpPow] {
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
    }
  }


  implicit val canPow_DV_V_Double: BinaryOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpPow, DenseMatrix[Double]] = pureFromUpdate_Double(canPowInto_DV_V_Double)


  implicit val canModInto_DV_DV_Double: BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpMod] = {
    new BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpMod] {
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
    }
  }


  implicit val canMod_DV_DV_Double: BinaryOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpMod, DenseMatrix[Double]] = pureFromUpdate_Double(canModInto_DV_DV_Double)


  implicit val canModInto_DV_S_Double: BinaryUpdateOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpMod] = {
    new BinaryUpdateOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpMod] {
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
    }
  }


  implicit val canMod_DV_S_Double: BinaryOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpMod, DenseMatrix[Double]] = pureFromUpdate_Double(canModInto_DV_S_Double)


  implicit val canModInto_DV_V_Double: BinaryUpdateOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpMod] = {
    new BinaryUpdateOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpMod] {
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
    }
  }


  implicit val canMod_DV_V_Double: BinaryOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpMod, DenseMatrix[Double]] = pureFromUpdate_Double(canModInto_DV_V_Double)


  implicit val canSetInto_DV_DV_Double: BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpSet] = {
    new BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpSet] {
      def apply(a: DenseMatrix[Double], b: DenseMatrix[Double]) {
        
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
    }
  }


  implicit val canSet_DV_DV_Double: BinaryOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpSet, DenseMatrix[Double]] = pureFromUpdate_Double(canSetInto_DV_DV_Double)


  implicit val canSetInto_DV_S_Double: BinaryUpdateOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpSet] = {
    new BinaryUpdateOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpSet] {
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
    }
  }


  implicit val canSet_DV_S_Double: BinaryOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpSet, DenseMatrix[Double]] = pureFromUpdate_Double(canSetInto_DV_S_Double)


  implicit val canSetInto_DV_V_Double: BinaryUpdateOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpSet] = {
    new BinaryUpdateOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpSet] {
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
    }
  }


  implicit val canSet_DV_V_Double: BinaryOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpSet, DenseMatrix[Double]] = pureFromUpdate_Double(canSetInto_DV_V_Double)


  implicit val canSubInto_DV_DV_Double: BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpSub] = {
    new BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpSub] {
      def apply(a: DenseMatrix[Double], b: DenseMatrix[Double]) {
        
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
    }
  }


  implicit val canSub_DV_DV_Double: BinaryOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpSub, DenseMatrix[Double]] = pureFromUpdate_Double(canSubInto_DV_DV_Double)


  implicit val canSubInto_DV_S_Double: BinaryUpdateOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpSub] = {
    new BinaryUpdateOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpSub] {
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
    }
  }


  implicit val canSub_DV_S_Double: BinaryOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpSub, DenseMatrix[Double]] = pureFromUpdate_Double(canSubInto_DV_S_Double)


  implicit val canSubInto_DV_V_Double: BinaryUpdateOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpSub] = {
    new BinaryUpdateOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpSub] {
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
    }
  }


  implicit val canSub_DV_V_Double: BinaryOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpSub, DenseMatrix[Double]] = pureFromUpdate_Double(canSubInto_DV_V_Double)


  implicit val canDivInto_DV_DV_Double: BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpDiv] = {
    new BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpDiv] {
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
    }
  }


  implicit val canDiv_DV_DV_Double: BinaryOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpDiv, DenseMatrix[Double]] = pureFromUpdate_Double(canDivInto_DV_DV_Double)


  implicit val canDivInto_DV_S_Double: BinaryUpdateOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpDiv] = {
    new BinaryUpdateOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpDiv] {
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
    }
  }


  implicit val canDiv_DV_S_Double: BinaryOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpDiv, DenseMatrix[Double]] = pureFromUpdate_Double(canDivInto_DV_S_Double)


  implicit val canDivInto_DV_V_Double: BinaryUpdateOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpDiv] = {
    new BinaryUpdateOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpDiv] {
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
    }
  }


  implicit val canDiv_DV_V_Double: BinaryOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpDiv, DenseMatrix[Double]] = pureFromUpdate_Double(canDivInto_DV_V_Double)


  implicit val canAddInto_DV_DV_Double: BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpAdd] = {
    new BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpAdd] {
      def apply(a: DenseMatrix[Double], b: DenseMatrix[Double]) {
        
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
    }
  }


  implicit val canAdd_DV_DV_Double: BinaryOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpAdd, DenseMatrix[Double]] = pureFromUpdate_Double(canAddInto_DV_DV_Double)


  implicit val canAddInto_DV_S_Double: BinaryUpdateOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpAdd] = {
    new BinaryUpdateOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpAdd] {
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
    }
  }


  implicit val canAdd_DV_S_Double: BinaryOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpAdd, DenseMatrix[Double]] = pureFromUpdate_Double(canAddInto_DV_S_Double)


  implicit val canAddInto_DV_V_Double: BinaryUpdateOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpAdd] = {
    new BinaryUpdateOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpAdd] {
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
    }
  }


  implicit val canAdd_DV_V_Double: BinaryOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpAdd, DenseMatrix[Double]] = pureFromUpdate_Double(canAddInto_DV_V_Double)


  implicit val canMulScalarInto_DV_DV_Double: BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpMulScalar] = {
    new BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpMulScalar] {
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
    }
  }


  implicit val canMulScalar_DV_DV_Double: BinaryOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpMulScalar, DenseMatrix[Double]] = pureFromUpdate_Double(canMulScalarInto_DV_DV_Double)


  implicit val canMulScalarInto_DV_S_Double: BinaryUpdateOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpMulScalar] = {
    new BinaryUpdateOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpMulScalar] {
      def apply(a: DenseMatrix[Double], b: Double) {
        
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
    }
  }


  implicit val canMulScalar_DV_S_Double: BinaryOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpMulScalar, DenseMatrix[Double]] = pureFromUpdate_Double(canMulScalarInto_DV_S_Double)


  implicit val canMulScalarInto_DV_V_Double: BinaryUpdateOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpMulScalar] = {
    new BinaryUpdateOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpMulScalar] {
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
    }
  }


  implicit val canMulScalar_DV_V_Double: BinaryOp[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpMulScalar, DenseMatrix[Double]] = pureFromUpdate_Double(canMulScalarInto_DV_V_Double)

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
        

  implicit val canPowInto_DV_DV_Float: BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpPow] = {
    new BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpPow] {
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
    }
  }


  implicit val canPow_DV_DV_Float: BinaryOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpPow, DenseMatrix[Float]] = pureFromUpdate_Float(canPowInto_DV_DV_Float)


  implicit val canPowInto_DV_S_Float: BinaryUpdateOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpPow] = {
    new BinaryUpdateOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpPow] {
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
    }
  }


  implicit val canPow_DV_S_Float: BinaryOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpPow, DenseMatrix[Float]] = pureFromUpdate_Float(canPowInto_DV_S_Float)


  implicit val canPowInto_DV_V_Float: BinaryUpdateOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpPow] = {
    new BinaryUpdateOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpPow] {
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
    }
  }


  implicit val canPow_DV_V_Float: BinaryOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpPow, DenseMatrix[Float]] = pureFromUpdate_Float(canPowInto_DV_V_Float)


  implicit val canModInto_DV_DV_Float: BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpMod] = {
    new BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpMod] {
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
    }
  }


  implicit val canMod_DV_DV_Float: BinaryOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpMod, DenseMatrix[Float]] = pureFromUpdate_Float(canModInto_DV_DV_Float)


  implicit val canModInto_DV_S_Float: BinaryUpdateOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpMod] = {
    new BinaryUpdateOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpMod] {
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
    }
  }


  implicit val canMod_DV_S_Float: BinaryOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpMod, DenseMatrix[Float]] = pureFromUpdate_Float(canModInto_DV_S_Float)


  implicit val canModInto_DV_V_Float: BinaryUpdateOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpMod] = {
    new BinaryUpdateOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpMod] {
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
    }
  }


  implicit val canMod_DV_V_Float: BinaryOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpMod, DenseMatrix[Float]] = pureFromUpdate_Float(canModInto_DV_V_Float)


  implicit val canSetInto_DV_DV_Float: BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpSet] = {
    new BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpSet] {
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
    }
  }


  implicit val canSet_DV_DV_Float: BinaryOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpSet, DenseMatrix[Float]] = pureFromUpdate_Float(canSetInto_DV_DV_Float)


  implicit val canSetInto_DV_S_Float: BinaryUpdateOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpSet] = {
    new BinaryUpdateOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpSet] {
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
    }
  }


  implicit val canSet_DV_S_Float: BinaryOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpSet, DenseMatrix[Float]] = pureFromUpdate_Float(canSetInto_DV_S_Float)


  implicit val canSetInto_DV_V_Float: BinaryUpdateOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpSet] = {
    new BinaryUpdateOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpSet] {
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
    }
  }


  implicit val canSet_DV_V_Float: BinaryOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpSet, DenseMatrix[Float]] = pureFromUpdate_Float(canSetInto_DV_V_Float)


  implicit val canSubInto_DV_DV_Float: BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpSub] = {
    new BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpSub] {
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
    }
  }


  implicit val canSub_DV_DV_Float: BinaryOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpSub, DenseMatrix[Float]] = pureFromUpdate_Float(canSubInto_DV_DV_Float)


  implicit val canSubInto_DV_S_Float: BinaryUpdateOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpSub] = {
    new BinaryUpdateOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpSub] {
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
    }
  }


  implicit val canSub_DV_S_Float: BinaryOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpSub, DenseMatrix[Float]] = pureFromUpdate_Float(canSubInto_DV_S_Float)


  implicit val canSubInto_DV_V_Float: BinaryUpdateOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpSub] = {
    new BinaryUpdateOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpSub] {
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
    }
  }


  implicit val canSub_DV_V_Float: BinaryOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpSub, DenseMatrix[Float]] = pureFromUpdate_Float(canSubInto_DV_V_Float)


  implicit val canDivInto_DV_DV_Float: BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpDiv] = {
    new BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpDiv] {
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
    }
  }


  implicit val canDiv_DV_DV_Float: BinaryOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpDiv, DenseMatrix[Float]] = pureFromUpdate_Float(canDivInto_DV_DV_Float)


  implicit val canDivInto_DV_S_Float: BinaryUpdateOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpDiv] = {
    new BinaryUpdateOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpDiv] {
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
    }
  }


  implicit val canDiv_DV_S_Float: BinaryOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpDiv, DenseMatrix[Float]] = pureFromUpdate_Float(canDivInto_DV_S_Float)


  implicit val canDivInto_DV_V_Float: BinaryUpdateOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpDiv] = {
    new BinaryUpdateOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpDiv] {
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
    }
  }


  implicit val canDiv_DV_V_Float: BinaryOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpDiv, DenseMatrix[Float]] = pureFromUpdate_Float(canDivInto_DV_V_Float)


  implicit val canAddInto_DV_DV_Float: BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpAdd] = {
    new BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpAdd] {
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
    }
  }


  implicit val canAdd_DV_DV_Float: BinaryOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpAdd, DenseMatrix[Float]] = pureFromUpdate_Float(canAddInto_DV_DV_Float)


  implicit val canAddInto_DV_S_Float: BinaryUpdateOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpAdd] = {
    new BinaryUpdateOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpAdd] {
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
    }
  }


  implicit val canAdd_DV_S_Float: BinaryOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpAdd, DenseMatrix[Float]] = pureFromUpdate_Float(canAddInto_DV_S_Float)


  implicit val canAddInto_DV_V_Float: BinaryUpdateOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpAdd] = {
    new BinaryUpdateOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpAdd] {
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
    }
  }


  implicit val canAdd_DV_V_Float: BinaryOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpAdd, DenseMatrix[Float]] = pureFromUpdate_Float(canAddInto_DV_V_Float)


  implicit val canMulScalarInto_DV_DV_Float: BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpMulScalar] = {
    new BinaryUpdateOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpMulScalar] {
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
    }
  }


  implicit val canMulScalar_DV_DV_Float: BinaryOp[DenseMatrix[Float], DenseMatrix[Float], breeze.linalg.operators.OpMulScalar, DenseMatrix[Float]] = pureFromUpdate_Float(canMulScalarInto_DV_DV_Float)


  implicit val canMulScalarInto_DV_S_Float: BinaryUpdateOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpMulScalar] = {
    new BinaryUpdateOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpMulScalar] {
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
    }
  }


  implicit val canMulScalar_DV_S_Float: BinaryOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpMulScalar, DenseMatrix[Float]] = pureFromUpdate_Float(canMulScalarInto_DV_S_Float)


  implicit val canMulScalarInto_DV_V_Float: BinaryUpdateOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpMulScalar] = {
    new BinaryUpdateOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpMulScalar] {
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
    }
  }


  implicit val canMulScalar_DV_V_Float: BinaryOp[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpMulScalar, DenseMatrix[Float]] = pureFromUpdate_Float(canMulScalarInto_DV_V_Float)

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
        

  implicit val canPowInto_DV_DV_Int: BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpPow] = {
    new BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpPow] {
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
    }
  }


  implicit val canPow_DV_DV_Int: BinaryOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpPow, DenseMatrix[Int]] = pureFromUpdate_Int(canPowInto_DV_DV_Int)


  implicit val canPowInto_DV_S_Int: BinaryUpdateOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpPow] = {
    new BinaryUpdateOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpPow] {
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
    }
  }


  implicit val canPow_DV_S_Int: BinaryOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpPow, DenseMatrix[Int]] = pureFromUpdate_Int(canPowInto_DV_S_Int)


  implicit val canPowInto_DV_V_Int: BinaryUpdateOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpPow] = {
    new BinaryUpdateOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpPow] {
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
    }
  }


  implicit val canPow_DV_V_Int: BinaryOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpPow, DenseMatrix[Int]] = pureFromUpdate_Int(canPowInto_DV_V_Int)


  implicit val canModInto_DV_DV_Int: BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpMod] = {
    new BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpMod] {
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
    }
  }


  implicit val canMod_DV_DV_Int: BinaryOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpMod, DenseMatrix[Int]] = pureFromUpdate_Int(canModInto_DV_DV_Int)


  implicit val canModInto_DV_S_Int: BinaryUpdateOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpMod] = {
    new BinaryUpdateOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpMod] {
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
    }
  }


  implicit val canMod_DV_S_Int: BinaryOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpMod, DenseMatrix[Int]] = pureFromUpdate_Int(canModInto_DV_S_Int)


  implicit val canModInto_DV_V_Int: BinaryUpdateOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpMod] = {
    new BinaryUpdateOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpMod] {
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
    }
  }


  implicit val canMod_DV_V_Int: BinaryOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpMod, DenseMatrix[Int]] = pureFromUpdate_Int(canModInto_DV_V_Int)


  implicit val canSetInto_DV_DV_Int: BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpSet] = {
    new BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpSet] {
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
    }
  }


  implicit val canSet_DV_DV_Int: BinaryOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpSet, DenseMatrix[Int]] = pureFromUpdate_Int(canSetInto_DV_DV_Int)


  implicit val canSetInto_DV_S_Int: BinaryUpdateOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpSet] = {
    new BinaryUpdateOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpSet] {
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
    }
  }


  implicit val canSet_DV_S_Int: BinaryOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpSet, DenseMatrix[Int]] = pureFromUpdate_Int(canSetInto_DV_S_Int)


  implicit val canSetInto_DV_V_Int: BinaryUpdateOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpSet] = {
    new BinaryUpdateOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpSet] {
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
    }
  }


  implicit val canSet_DV_V_Int: BinaryOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpSet, DenseMatrix[Int]] = pureFromUpdate_Int(canSetInto_DV_V_Int)


  implicit val canSubInto_DV_DV_Int: BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpSub] = {
    new BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpSub] {
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
    }
  }


  implicit val canSub_DV_DV_Int: BinaryOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpSub, DenseMatrix[Int]] = pureFromUpdate_Int(canSubInto_DV_DV_Int)


  implicit val canSubInto_DV_S_Int: BinaryUpdateOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpSub] = {
    new BinaryUpdateOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpSub] {
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
    }
  }


  implicit val canSub_DV_S_Int: BinaryOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpSub, DenseMatrix[Int]] = pureFromUpdate_Int(canSubInto_DV_S_Int)


  implicit val canSubInto_DV_V_Int: BinaryUpdateOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpSub] = {
    new BinaryUpdateOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpSub] {
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
    }
  }


  implicit val canSub_DV_V_Int: BinaryOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpSub, DenseMatrix[Int]] = pureFromUpdate_Int(canSubInto_DV_V_Int)


  implicit val canDivInto_DV_DV_Int: BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpDiv] = {
    new BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpDiv] {
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
    }
  }


  implicit val canDiv_DV_DV_Int: BinaryOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpDiv, DenseMatrix[Int]] = pureFromUpdate_Int(canDivInto_DV_DV_Int)


  implicit val canDivInto_DV_S_Int: BinaryUpdateOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpDiv] = {
    new BinaryUpdateOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpDiv] {
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
    }
  }


  implicit val canDiv_DV_S_Int: BinaryOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpDiv, DenseMatrix[Int]] = pureFromUpdate_Int(canDivInto_DV_S_Int)


  implicit val canDivInto_DV_V_Int: BinaryUpdateOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpDiv] = {
    new BinaryUpdateOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpDiv] {
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
    }
  }


  implicit val canDiv_DV_V_Int: BinaryOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpDiv, DenseMatrix[Int]] = pureFromUpdate_Int(canDivInto_DV_V_Int)


  implicit val canAddInto_DV_DV_Int: BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpAdd] = {
    new BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpAdd] {
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
    }
  }


  implicit val canAdd_DV_DV_Int: BinaryOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpAdd, DenseMatrix[Int]] = pureFromUpdate_Int(canAddInto_DV_DV_Int)


  implicit val canAddInto_DV_S_Int: BinaryUpdateOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpAdd] = {
    new BinaryUpdateOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpAdd] {
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
    }
  }


  implicit val canAdd_DV_S_Int: BinaryOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpAdd, DenseMatrix[Int]] = pureFromUpdate_Int(canAddInto_DV_S_Int)


  implicit val canAddInto_DV_V_Int: BinaryUpdateOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpAdd] = {
    new BinaryUpdateOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpAdd] {
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
    }
  }


  implicit val canAdd_DV_V_Int: BinaryOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpAdd, DenseMatrix[Int]] = pureFromUpdate_Int(canAddInto_DV_V_Int)


  implicit val canMulScalarInto_DV_DV_Int: BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpMulScalar] = {
    new BinaryUpdateOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpMulScalar] {
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
    }
  }


  implicit val canMulScalar_DV_DV_Int: BinaryOp[DenseMatrix[Int], DenseMatrix[Int], breeze.linalg.operators.OpMulScalar, DenseMatrix[Int]] = pureFromUpdate_Int(canMulScalarInto_DV_DV_Int)


  implicit val canMulScalarInto_DV_S_Int: BinaryUpdateOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpMulScalar] = {
    new BinaryUpdateOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpMulScalar] {
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
    }
  }


  implicit val canMulScalar_DV_S_Int: BinaryOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpMulScalar, DenseMatrix[Int]] = pureFromUpdate_Int(canMulScalarInto_DV_S_Int)


  implicit val canMulScalarInto_DV_V_Int: BinaryUpdateOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpMulScalar] = {
    new BinaryUpdateOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpMulScalar] {
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
    }
  }


  implicit val canMulScalar_DV_V_Int: BinaryOp[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpMulScalar, DenseMatrix[Int]] = pureFromUpdate_Int(canMulScalarInto_DV_V_Int)

}
