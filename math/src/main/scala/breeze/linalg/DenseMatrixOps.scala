package breeze.linalg
import breeze.linalg.operators._
import breeze.linalg.support._
import breeze.numerics._
/** This is an auto-generated trait providing operators for DenseMatrix. */
trait DenseMatrixOps_Double extends DenseMatrixOps_Double_Generic { this: DenseMatrix.type =>

       def pureFromUpdate_Double[Other,Op<:OpType](op: BinaryUpdateOp[DenseMatrix[Double], Other, Op])(implicit copy: CanCopy[DenseMatrix[Double]]):BinaryOp[DenseMatrix[Double], Other, Op, DenseMatrix[Double]] = {
         new BinaryOp[DenseMatrix[Double], Other, Op, DenseMatrix[Double]] {
           override def apply(a : DenseMatrix[Double], b : Other) = {
             val c = copy(a)
             op(c, b)
             c
           }
         }
       }

  class canAddInto_DV_DV_Double private[linalg] () extends BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpAdd] {
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
  implicit val canAddInto_DV_DV_Double = new canAddInto_DV_DV_Double ()
    

  implicit val canAdd_DV_DV_Double: BinaryOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpAdd, DenseMatrix[Double]] = pureFromUpdate_Double(canAddInto_DV_DV_Double)


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
  }
  implicit val canAddInto_DV_S_Double = new canAddInto_DV_S_Double ()
    

  implicit val canAdd_DV_S_Double: BinaryOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpAdd, DenseMatrix[Double]] = pureFromUpdate_Double(canAddInto_DV_S_Double)


  class canSubInto_DV_DV_Double private[linalg] () extends BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpSub] {
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
  implicit val canSubInto_DV_DV_Double = new canSubInto_DV_DV_Double ()
    

  implicit val canSub_DV_DV_Double: BinaryOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpSub, DenseMatrix[Double]] = pureFromUpdate_Double(canSubInto_DV_DV_Double)


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
  }
  implicit val canSubInto_DV_S_Double = new canSubInto_DV_S_Double ()
    

  implicit val canSub_DV_S_Double: BinaryOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpSub, DenseMatrix[Double]] = pureFromUpdate_Double(canSubInto_DV_S_Double)


  class canMulMatrixInto_DV_S_Double private[linalg] () extends BinaryUpdateOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpMulMatrix] {
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
  implicit val canMulMatrixInto_DV_S_Double = new canMulMatrixInto_DV_S_Double ()
    

  implicit val canMulMatrix_DV_S_Double: BinaryOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpMulMatrix, DenseMatrix[Double]] = pureFromUpdate_Double(canMulMatrixInto_DV_S_Double)


  class canSetInto_DV_DV_Double private[linalg] () extends BinaryUpdateOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpSet] {
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
  implicit val canSetInto_DV_DV_Double = new canSetInto_DV_DV_Double ()
    

  implicit val canSet_DV_DV_Double: BinaryOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpSet, DenseMatrix[Double]] = pureFromUpdate_Double(canSetInto_DV_DV_Double)


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
  }
  implicit val canSetInto_DV_S_Double = new canSetInto_DV_S_Double ()
    

  implicit val canSet_DV_S_Double: BinaryOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpSet, DenseMatrix[Double]] = pureFromUpdate_Double(canSetInto_DV_S_Double)


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
  }
  implicit val canDivInto_DV_DV_Double = new canDivInto_DV_DV_Double ()
    

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
  }
  implicit val canDivInto_DV_S_Double = new canDivInto_DV_S_Double ()
    

  implicit val canDiv_DV_S_Double: BinaryOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpDiv, DenseMatrix[Double]] = pureFromUpdate_Double(canDivInto_DV_S_Double)


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
  }
  implicit val canMulScalarInto_DV_DV_Double = new canMulScalarInto_DV_DV_Double ()
    

  implicit val canMulScalar_DV_DV_Double: BinaryOp[DenseMatrix[Double], DenseMatrix[Double], breeze.linalg.operators.OpMulScalar, DenseMatrix[Double]] = pureFromUpdate_Double(canMulScalarInto_DV_DV_Double)


  class canMulScalarInto_DV_S_Double private[linalg] () extends BinaryUpdateOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpMulScalar] {
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
  implicit val canMulScalarInto_DV_S_Double = new canMulScalarInto_DV_S_Double ()
    

  implicit val canMulScalar_DV_S_Double: BinaryOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpMulScalar, DenseMatrix[Double]] = pureFromUpdate_Double(canMulScalarInto_DV_S_Double)


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
  }
  implicit val canPowInto_DV_DV_Double = new canPowInto_DV_DV_Double ()
    

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
  }
  implicit val canPowInto_DV_S_Double = new canPowInto_DV_S_Double ()
    

  implicit val canPow_DV_S_Double: BinaryOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpPow, DenseMatrix[Double]] = pureFromUpdate_Double(canPowInto_DV_S_Double)


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
  }
  implicit val canModInto_DV_DV_Double = new canModInto_DV_DV_Double ()
    

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
  }
  implicit val canModInto_DV_S_Double = new canModInto_DV_S_Double ()
    

  implicit val canMod_DV_S_Double: BinaryOp[DenseMatrix[Double], Double, breeze.linalg.operators.OpMod, DenseMatrix[Double]] = pureFromUpdate_Double(canModInto_DV_S_Double)

}
/** This is an auto-generated trait providing operators for DenseMatrix. */
trait DenseMatrixOps_Double_Generic extends LowPriorityDenseMatrix{ this: DenseMatrix.type =>

    def pureRegistryFromUpdate_Double[Other,Op<:OpType](op: BinaryUpdateRegistry[DenseMatrix[Double], Other, Op])(implicit copy: CanCopy[DenseMatrix[Double]]):BinaryRegistry[DenseMatrix[Double], Other, Op, DenseMatrix[Double]] = {
      new BinaryRegistry[DenseMatrix[Double], Other, Op, DenseMatrix[Double]] {
        override def bindingMissing(a : DenseMatrix[Double], b : Other) = {
          val c = copy(a)
          op(c, b)
          c
        }
      }
    }
        

  class canAddInto_DV_V_Double private[linalg] () extends BinaryUpdateRegistry[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: DenseMatrix[Double], b: Matrix[Double]) {
      
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
  val canAddInto_DV_V_Double = new canAddInto_DV_V_Double ()
  implicit def canAddInto_DV_V_Double_def[A <: DenseMatrix[Double], B <: Matrix[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd] = (
    canAddInto_DV_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd]]
  )

  val canAdd_DV_V_Double: BinaryRegistry[DenseMatrix[Double], Matrix[Double], OpAdd, DenseMatrix[Double]] = pureRegistryFromUpdate_Double(canAddInto_DV_V_Double)
  implicit def canAdd_DV_V_Double_def[A <: DenseMatrix[Double], B <: Matrix[Double]]:BinaryOp[A, B, OpAdd, DenseMatrix[Double]] = pureRegistryFromUpdate_Double(canAddInto_DV_V_Double).asInstanceOf[BinaryOp[A, B, OpAdd, DenseMatrix[Double]]]
    


  class canSubInto_DV_V_Double private[linalg] () extends BinaryUpdateRegistry[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: DenseMatrix[Double], b: Matrix[Double]) {
      
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
  val canSubInto_DV_V_Double = new canSubInto_DV_V_Double ()
  implicit def canSubInto_DV_V_Double_def[A <: DenseMatrix[Double], B <: Matrix[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub] = (
    canSubInto_DV_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub]]
  )

  val canSub_DV_V_Double: BinaryRegistry[DenseMatrix[Double], Matrix[Double], OpSub, DenseMatrix[Double]] = pureRegistryFromUpdate_Double(canSubInto_DV_V_Double)
  implicit def canSub_DV_V_Double_def[A <: DenseMatrix[Double], B <: Matrix[Double]]:BinaryOp[A, B, OpSub, DenseMatrix[Double]] = pureRegistryFromUpdate_Double(canSubInto_DV_V_Double).asInstanceOf[BinaryOp[A, B, OpSub, DenseMatrix[Double]]]
    


  class canSetInto_DV_V_Double private[linalg] () extends BinaryUpdateRegistry[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: DenseMatrix[Double], b: Matrix[Double]) {
      
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
  val canSetInto_DV_V_Double = new canSetInto_DV_V_Double ()
  implicit def canSetInto_DV_V_Double_def[A <: DenseMatrix[Double], B <: Matrix[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet] = (
    canSetInto_DV_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet]]
  )

  val canSet_DV_V_Double: BinaryRegistry[DenseMatrix[Double], Matrix[Double], OpSet, DenseMatrix[Double]] = pureRegistryFromUpdate_Double(canSetInto_DV_V_Double)
  implicit def canSet_DV_V_Double_def[A <: DenseMatrix[Double], B <: Matrix[Double]]:BinaryOp[A, B, OpSet, DenseMatrix[Double]] = pureRegistryFromUpdate_Double(canSetInto_DV_V_Double).asInstanceOf[BinaryOp[A, B, OpSet, DenseMatrix[Double]]]
    


  class canDivInto_DV_V_Double private[linalg] () extends BinaryUpdateRegistry[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: DenseMatrix[Double], b: Matrix[Double]) {
      
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
  val canDivInto_DV_V_Double = new canDivInto_DV_V_Double ()
  implicit def canDivInto_DV_V_Double_def[A <: DenseMatrix[Double], B <: Matrix[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv] = (
    canDivInto_DV_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv]]
  )

  val canDiv_DV_V_Double: BinaryRegistry[DenseMatrix[Double], Matrix[Double], OpDiv, DenseMatrix[Double]] = pureRegistryFromUpdate_Double(canDivInto_DV_V_Double)
  implicit def canDiv_DV_V_Double_def[A <: DenseMatrix[Double], B <: Matrix[Double]]:BinaryOp[A, B, OpDiv, DenseMatrix[Double]] = pureRegistryFromUpdate_Double(canDivInto_DV_V_Double).asInstanceOf[BinaryOp[A, B, OpDiv, DenseMatrix[Double]]]
    


  class canMulScalarInto_DV_V_Double private[linalg] () extends BinaryUpdateRegistry[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: DenseMatrix[Double], b: Matrix[Double]) {
      
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
  val canMulScalarInto_DV_V_Double = new canMulScalarInto_DV_V_Double ()
  implicit def canMulScalarInto_DV_V_Double_def[A <: DenseMatrix[Double], B <: Matrix[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar] = (
    canMulScalarInto_DV_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar]]
  )

  val canMulScalar_DV_V_Double: BinaryRegistry[DenseMatrix[Double], Matrix[Double], OpMulScalar, DenseMatrix[Double]] = pureRegistryFromUpdate_Double(canMulScalarInto_DV_V_Double)
  implicit def canMulScalar_DV_V_Double_def[A <: DenseMatrix[Double], B <: Matrix[Double]]:BinaryOp[A, B, OpMulScalar, DenseMatrix[Double]] = pureRegistryFromUpdate_Double(canMulScalarInto_DV_V_Double).asInstanceOf[BinaryOp[A, B, OpMulScalar, DenseMatrix[Double]]]
    


  class canPowInto_DV_V_Double private[linalg] () extends BinaryUpdateRegistry[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: DenseMatrix[Double], b: Matrix[Double]) {
      
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
  val canPowInto_DV_V_Double = new canPowInto_DV_V_Double ()
  implicit def canPowInto_DV_V_Double_def[A <: DenseMatrix[Double], B <: Matrix[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow] = (
    canPowInto_DV_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow]]
  )

  val canPow_DV_V_Double: BinaryRegistry[DenseMatrix[Double], Matrix[Double], OpPow, DenseMatrix[Double]] = pureRegistryFromUpdate_Double(canPowInto_DV_V_Double)
  implicit def canPow_DV_V_Double_def[A <: DenseMatrix[Double], B <: Matrix[Double]]:BinaryOp[A, B, OpPow, DenseMatrix[Double]] = pureRegistryFromUpdate_Double(canPowInto_DV_V_Double).asInstanceOf[BinaryOp[A, B, OpPow, DenseMatrix[Double]]]
    


  class canModInto_DV_V_Double private[linalg] () extends BinaryUpdateRegistry[DenseMatrix[Double], Matrix[Double], breeze.linalg.operators.OpMod] {
    override def bindingMissing(a: DenseMatrix[Double], b: Matrix[Double]) {
      
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
  val canModInto_DV_V_Double = new canModInto_DV_V_Double ()
  implicit def canModInto_DV_V_Double_def[A <: DenseMatrix[Double], B <: Matrix[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod] = (
    canModInto_DV_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod]]
  )

  val canMod_DV_V_Double: BinaryRegistry[DenseMatrix[Double], Matrix[Double], OpMod, DenseMatrix[Double]] = pureRegistryFromUpdate_Double(canModInto_DV_V_Double)
  implicit def canMod_DV_V_Double_def[A <: DenseMatrix[Double], B <: Matrix[Double]]:BinaryOp[A, B, OpMod, DenseMatrix[Double]] = pureRegistryFromUpdate_Double(canModInto_DV_V_Double).asInstanceOf[BinaryOp[A, B, OpMod, DenseMatrix[Double]]]
    

}
/** This is an auto-generated trait providing operators for DenseMatrix. */
trait DenseMatrixOps_Float extends DenseMatrixOps_Float_Generic { this: DenseMatrix.type =>

       def pureFromUpdate_Float[Other,Op<:OpType](op: BinaryUpdateOp[DenseMatrix[Float], Other, Op])(implicit copy: CanCopy[DenseMatrix[Float]]):BinaryOp[DenseMatrix[Float], Other, Op, DenseMatrix[Float]] = {
         new BinaryOp[DenseMatrix[Float], Other, Op, DenseMatrix[Float]] {
           override def apply(a : DenseMatrix[Float], b : Other) = {
             val c = copy(a)
             op(c, b)
             c
           }
         }
       }

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
  }
  implicit val canAddInto_DV_DV_Float = new canAddInto_DV_DV_Float ()
    

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
  }
  implicit val canAddInto_DV_S_Float = new canAddInto_DV_S_Float ()
    

  implicit val canAdd_DV_S_Float: BinaryOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpAdd, DenseMatrix[Float]] = pureFromUpdate_Float(canAddInto_DV_S_Float)


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
  }
  implicit val canSubInto_DV_DV_Float = new canSubInto_DV_DV_Float ()
    

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
  }
  implicit val canSubInto_DV_S_Float = new canSubInto_DV_S_Float ()
    

  implicit val canSub_DV_S_Float: BinaryOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpSub, DenseMatrix[Float]] = pureFromUpdate_Float(canSubInto_DV_S_Float)


  class canMulMatrixInto_DV_S_Float private[linalg] () extends BinaryUpdateOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpMulMatrix] {
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
  implicit val canMulMatrixInto_DV_S_Float = new canMulMatrixInto_DV_S_Float ()
    

  implicit val canMulMatrix_DV_S_Float: BinaryOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpMulMatrix, DenseMatrix[Float]] = pureFromUpdate_Float(canMulMatrixInto_DV_S_Float)


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
  }
  implicit val canSetInto_DV_DV_Float = new canSetInto_DV_DV_Float ()
    

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
  }
  implicit val canSetInto_DV_S_Float = new canSetInto_DV_S_Float ()
    

  implicit val canSet_DV_S_Float: BinaryOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpSet, DenseMatrix[Float]] = pureFromUpdate_Float(canSetInto_DV_S_Float)


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
  }
  implicit val canDivInto_DV_DV_Float = new canDivInto_DV_DV_Float ()
    

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
  }
  implicit val canDivInto_DV_S_Float = new canDivInto_DV_S_Float ()
    

  implicit val canDiv_DV_S_Float: BinaryOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpDiv, DenseMatrix[Float]] = pureFromUpdate_Float(canDivInto_DV_S_Float)


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
  }
  implicit val canMulScalarInto_DV_DV_Float = new canMulScalarInto_DV_DV_Float ()
    

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
  }
  implicit val canMulScalarInto_DV_S_Float = new canMulScalarInto_DV_S_Float ()
    

  implicit val canMulScalar_DV_S_Float: BinaryOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpMulScalar, DenseMatrix[Float]] = pureFromUpdate_Float(canMulScalarInto_DV_S_Float)


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
  }
  implicit val canPowInto_DV_DV_Float = new canPowInto_DV_DV_Float ()
    

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
  }
  implicit val canPowInto_DV_S_Float = new canPowInto_DV_S_Float ()
    

  implicit val canPow_DV_S_Float: BinaryOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpPow, DenseMatrix[Float]] = pureFromUpdate_Float(canPowInto_DV_S_Float)


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
  }
  implicit val canModInto_DV_DV_Float = new canModInto_DV_DV_Float ()
    

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
  }
  implicit val canModInto_DV_S_Float = new canModInto_DV_S_Float ()
    

  implicit val canMod_DV_S_Float: BinaryOp[DenseMatrix[Float], Float, breeze.linalg.operators.OpMod, DenseMatrix[Float]] = pureFromUpdate_Float(canModInto_DV_S_Float)

}
/** This is an auto-generated trait providing operators for DenseMatrix. */
trait DenseMatrixOps_Float_Generic extends LowPriorityDenseMatrix{ this: DenseMatrix.type =>

    def pureRegistryFromUpdate_Float[Other,Op<:OpType](op: BinaryUpdateRegistry[DenseMatrix[Float], Other, Op])(implicit copy: CanCopy[DenseMatrix[Float]]):BinaryRegistry[DenseMatrix[Float], Other, Op, DenseMatrix[Float]] = {
      new BinaryRegistry[DenseMatrix[Float], Other, Op, DenseMatrix[Float]] {
        override def bindingMissing(a : DenseMatrix[Float], b : Other) = {
          val c = copy(a)
          op(c, b)
          c
        }
      }
    }
        

  class canAddInto_DV_V_Float private[linalg] () extends BinaryUpdateRegistry[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: DenseMatrix[Float], b: Matrix[Float]) {
      
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
  val canAddInto_DV_V_Float = new canAddInto_DV_V_Float ()
  implicit def canAddInto_DV_V_Float_def[A <: DenseMatrix[Float], B <: Matrix[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd] = (
    canAddInto_DV_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd]]
  )

  val canAdd_DV_V_Float: BinaryRegistry[DenseMatrix[Float], Matrix[Float], OpAdd, DenseMatrix[Float]] = pureRegistryFromUpdate_Float(canAddInto_DV_V_Float)
  implicit def canAdd_DV_V_Float_def[A <: DenseMatrix[Float], B <: Matrix[Float]]:BinaryOp[A, B, OpAdd, DenseMatrix[Float]] = pureRegistryFromUpdate_Float(canAddInto_DV_V_Float).asInstanceOf[BinaryOp[A, B, OpAdd, DenseMatrix[Float]]]
    


  class canSubInto_DV_V_Float private[linalg] () extends BinaryUpdateRegistry[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: DenseMatrix[Float], b: Matrix[Float]) {
      
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
  val canSubInto_DV_V_Float = new canSubInto_DV_V_Float ()
  implicit def canSubInto_DV_V_Float_def[A <: DenseMatrix[Float], B <: Matrix[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub] = (
    canSubInto_DV_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub]]
  )

  val canSub_DV_V_Float: BinaryRegistry[DenseMatrix[Float], Matrix[Float], OpSub, DenseMatrix[Float]] = pureRegistryFromUpdate_Float(canSubInto_DV_V_Float)
  implicit def canSub_DV_V_Float_def[A <: DenseMatrix[Float], B <: Matrix[Float]]:BinaryOp[A, B, OpSub, DenseMatrix[Float]] = pureRegistryFromUpdate_Float(canSubInto_DV_V_Float).asInstanceOf[BinaryOp[A, B, OpSub, DenseMatrix[Float]]]
    


  class canSetInto_DV_V_Float private[linalg] () extends BinaryUpdateRegistry[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: DenseMatrix[Float], b: Matrix[Float]) {
      
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
  val canSetInto_DV_V_Float = new canSetInto_DV_V_Float ()
  implicit def canSetInto_DV_V_Float_def[A <: DenseMatrix[Float], B <: Matrix[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet] = (
    canSetInto_DV_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet]]
  )

  val canSet_DV_V_Float: BinaryRegistry[DenseMatrix[Float], Matrix[Float], OpSet, DenseMatrix[Float]] = pureRegistryFromUpdate_Float(canSetInto_DV_V_Float)
  implicit def canSet_DV_V_Float_def[A <: DenseMatrix[Float], B <: Matrix[Float]]:BinaryOp[A, B, OpSet, DenseMatrix[Float]] = pureRegistryFromUpdate_Float(canSetInto_DV_V_Float).asInstanceOf[BinaryOp[A, B, OpSet, DenseMatrix[Float]]]
    


  class canDivInto_DV_V_Float private[linalg] () extends BinaryUpdateRegistry[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: DenseMatrix[Float], b: Matrix[Float]) {
      
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
  val canDivInto_DV_V_Float = new canDivInto_DV_V_Float ()
  implicit def canDivInto_DV_V_Float_def[A <: DenseMatrix[Float], B <: Matrix[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv] = (
    canDivInto_DV_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv]]
  )

  val canDiv_DV_V_Float: BinaryRegistry[DenseMatrix[Float], Matrix[Float], OpDiv, DenseMatrix[Float]] = pureRegistryFromUpdate_Float(canDivInto_DV_V_Float)
  implicit def canDiv_DV_V_Float_def[A <: DenseMatrix[Float], B <: Matrix[Float]]:BinaryOp[A, B, OpDiv, DenseMatrix[Float]] = pureRegistryFromUpdate_Float(canDivInto_DV_V_Float).asInstanceOf[BinaryOp[A, B, OpDiv, DenseMatrix[Float]]]
    


  class canMulScalarInto_DV_V_Float private[linalg] () extends BinaryUpdateRegistry[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: DenseMatrix[Float], b: Matrix[Float]) {
      
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
  val canMulScalarInto_DV_V_Float = new canMulScalarInto_DV_V_Float ()
  implicit def canMulScalarInto_DV_V_Float_def[A <: DenseMatrix[Float], B <: Matrix[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar] = (
    canMulScalarInto_DV_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar]]
  )

  val canMulScalar_DV_V_Float: BinaryRegistry[DenseMatrix[Float], Matrix[Float], OpMulScalar, DenseMatrix[Float]] = pureRegistryFromUpdate_Float(canMulScalarInto_DV_V_Float)
  implicit def canMulScalar_DV_V_Float_def[A <: DenseMatrix[Float], B <: Matrix[Float]]:BinaryOp[A, B, OpMulScalar, DenseMatrix[Float]] = pureRegistryFromUpdate_Float(canMulScalarInto_DV_V_Float).asInstanceOf[BinaryOp[A, B, OpMulScalar, DenseMatrix[Float]]]
    


  class canPowInto_DV_V_Float private[linalg] () extends BinaryUpdateRegistry[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: DenseMatrix[Float], b: Matrix[Float]) {
      
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
  val canPowInto_DV_V_Float = new canPowInto_DV_V_Float ()
  implicit def canPowInto_DV_V_Float_def[A <: DenseMatrix[Float], B <: Matrix[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow] = (
    canPowInto_DV_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow]]
  )

  val canPow_DV_V_Float: BinaryRegistry[DenseMatrix[Float], Matrix[Float], OpPow, DenseMatrix[Float]] = pureRegistryFromUpdate_Float(canPowInto_DV_V_Float)
  implicit def canPow_DV_V_Float_def[A <: DenseMatrix[Float], B <: Matrix[Float]]:BinaryOp[A, B, OpPow, DenseMatrix[Float]] = pureRegistryFromUpdate_Float(canPowInto_DV_V_Float).asInstanceOf[BinaryOp[A, B, OpPow, DenseMatrix[Float]]]
    


  class canModInto_DV_V_Float private[linalg] () extends BinaryUpdateRegistry[DenseMatrix[Float], Matrix[Float], breeze.linalg.operators.OpMod] {
    override def bindingMissing(a: DenseMatrix[Float], b: Matrix[Float]) {
      
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
  val canModInto_DV_V_Float = new canModInto_DV_V_Float ()
  implicit def canModInto_DV_V_Float_def[A <: DenseMatrix[Float], B <: Matrix[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod] = (
    canModInto_DV_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod]]
  )

  val canMod_DV_V_Float: BinaryRegistry[DenseMatrix[Float], Matrix[Float], OpMod, DenseMatrix[Float]] = pureRegistryFromUpdate_Float(canModInto_DV_V_Float)
  implicit def canMod_DV_V_Float_def[A <: DenseMatrix[Float], B <: Matrix[Float]]:BinaryOp[A, B, OpMod, DenseMatrix[Float]] = pureRegistryFromUpdate_Float(canModInto_DV_V_Float).asInstanceOf[BinaryOp[A, B, OpMod, DenseMatrix[Float]]]
    

}
/** This is an auto-generated trait providing operators for DenseMatrix. */
trait DenseMatrixOps_Int extends DenseMatrixOps_Int_Generic { this: DenseMatrix.type =>

       def pureFromUpdate_Int[Other,Op<:OpType](op: BinaryUpdateOp[DenseMatrix[Int], Other, Op])(implicit copy: CanCopy[DenseMatrix[Int]]):BinaryOp[DenseMatrix[Int], Other, Op, DenseMatrix[Int]] = {
         new BinaryOp[DenseMatrix[Int], Other, Op, DenseMatrix[Int]] {
           override def apply(a : DenseMatrix[Int], b : Other) = {
             val c = copy(a)
             op(c, b)
             c
           }
         }
       }

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
  }
  implicit val canAddInto_DV_DV_Int = new canAddInto_DV_DV_Int ()
    

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
  }
  implicit val canAddInto_DV_S_Int = new canAddInto_DV_S_Int ()
    

  implicit val canAdd_DV_S_Int: BinaryOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpAdd, DenseMatrix[Int]] = pureFromUpdate_Int(canAddInto_DV_S_Int)


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
  }
  implicit val canSubInto_DV_DV_Int = new canSubInto_DV_DV_Int ()
    

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
  }
  implicit val canSubInto_DV_S_Int = new canSubInto_DV_S_Int ()
    

  implicit val canSub_DV_S_Int: BinaryOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpSub, DenseMatrix[Int]] = pureFromUpdate_Int(canSubInto_DV_S_Int)


  class canMulMatrixInto_DV_S_Int private[linalg] () extends BinaryUpdateOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpMulMatrix] {
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
  implicit val canMulMatrixInto_DV_S_Int = new canMulMatrixInto_DV_S_Int ()
    

  implicit val canMulMatrix_DV_S_Int: BinaryOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpMulMatrix, DenseMatrix[Int]] = pureFromUpdate_Int(canMulMatrixInto_DV_S_Int)


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
  }
  implicit val canSetInto_DV_DV_Int = new canSetInto_DV_DV_Int ()
    

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
  }
  implicit val canSetInto_DV_S_Int = new canSetInto_DV_S_Int ()
    

  implicit val canSet_DV_S_Int: BinaryOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpSet, DenseMatrix[Int]] = pureFromUpdate_Int(canSetInto_DV_S_Int)


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
  }
  implicit val canDivInto_DV_DV_Int = new canDivInto_DV_DV_Int ()
    

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
  }
  implicit val canDivInto_DV_S_Int = new canDivInto_DV_S_Int ()
    

  implicit val canDiv_DV_S_Int: BinaryOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpDiv, DenseMatrix[Int]] = pureFromUpdate_Int(canDivInto_DV_S_Int)


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
  }
  implicit val canMulScalarInto_DV_DV_Int = new canMulScalarInto_DV_DV_Int ()
    

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
  }
  implicit val canMulScalarInto_DV_S_Int = new canMulScalarInto_DV_S_Int ()
    

  implicit val canMulScalar_DV_S_Int: BinaryOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpMulScalar, DenseMatrix[Int]] = pureFromUpdate_Int(canMulScalarInto_DV_S_Int)


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
  }
  implicit val canPowInto_DV_DV_Int = new canPowInto_DV_DV_Int ()
    

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
  }
  implicit val canPowInto_DV_S_Int = new canPowInto_DV_S_Int ()
    

  implicit val canPow_DV_S_Int: BinaryOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpPow, DenseMatrix[Int]] = pureFromUpdate_Int(canPowInto_DV_S_Int)


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
  }
  implicit val canModInto_DV_DV_Int = new canModInto_DV_DV_Int ()
    

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
  }
  implicit val canModInto_DV_S_Int = new canModInto_DV_S_Int ()
    

  implicit val canMod_DV_S_Int: BinaryOp[DenseMatrix[Int], Int, breeze.linalg.operators.OpMod, DenseMatrix[Int]] = pureFromUpdate_Int(canModInto_DV_S_Int)

}
/** This is an auto-generated trait providing operators for DenseMatrix. */
trait DenseMatrixOps_Int_Generic extends LowPriorityDenseMatrix{ this: DenseMatrix.type =>

    def pureRegistryFromUpdate_Int[Other,Op<:OpType](op: BinaryUpdateRegistry[DenseMatrix[Int], Other, Op])(implicit copy: CanCopy[DenseMatrix[Int]]):BinaryRegistry[DenseMatrix[Int], Other, Op, DenseMatrix[Int]] = {
      new BinaryRegistry[DenseMatrix[Int], Other, Op, DenseMatrix[Int]] {
        override def bindingMissing(a : DenseMatrix[Int], b : Other) = {
          val c = copy(a)
          op(c, b)
          c
        }
      }
    }
        

  class canAddInto_DV_V_Int private[linalg] () extends BinaryUpdateRegistry[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: DenseMatrix[Int], b: Matrix[Int]) {
      
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
  val canAddInto_DV_V_Int = new canAddInto_DV_V_Int ()
  implicit def canAddInto_DV_V_Int_def[A <: DenseMatrix[Int], B <: Matrix[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd] = (
    canAddInto_DV_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd]]
  )

  val canAdd_DV_V_Int: BinaryRegistry[DenseMatrix[Int], Matrix[Int], OpAdd, DenseMatrix[Int]] = pureRegistryFromUpdate_Int(canAddInto_DV_V_Int)
  implicit def canAdd_DV_V_Int_def[A <: DenseMatrix[Int], B <: Matrix[Int]]:BinaryOp[A, B, OpAdd, DenseMatrix[Int]] = pureRegistryFromUpdate_Int(canAddInto_DV_V_Int).asInstanceOf[BinaryOp[A, B, OpAdd, DenseMatrix[Int]]]
    


  class canSubInto_DV_V_Int private[linalg] () extends BinaryUpdateRegistry[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: DenseMatrix[Int], b: Matrix[Int]) {
      
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
  val canSubInto_DV_V_Int = new canSubInto_DV_V_Int ()
  implicit def canSubInto_DV_V_Int_def[A <: DenseMatrix[Int], B <: Matrix[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub] = (
    canSubInto_DV_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub]]
  )

  val canSub_DV_V_Int: BinaryRegistry[DenseMatrix[Int], Matrix[Int], OpSub, DenseMatrix[Int]] = pureRegistryFromUpdate_Int(canSubInto_DV_V_Int)
  implicit def canSub_DV_V_Int_def[A <: DenseMatrix[Int], B <: Matrix[Int]]:BinaryOp[A, B, OpSub, DenseMatrix[Int]] = pureRegistryFromUpdate_Int(canSubInto_DV_V_Int).asInstanceOf[BinaryOp[A, B, OpSub, DenseMatrix[Int]]]
    


  class canSetInto_DV_V_Int private[linalg] () extends BinaryUpdateRegistry[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: DenseMatrix[Int], b: Matrix[Int]) {
      
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
  val canSetInto_DV_V_Int = new canSetInto_DV_V_Int ()
  implicit def canSetInto_DV_V_Int_def[A <: DenseMatrix[Int], B <: Matrix[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet] = (
    canSetInto_DV_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet]]
  )

  val canSet_DV_V_Int: BinaryRegistry[DenseMatrix[Int], Matrix[Int], OpSet, DenseMatrix[Int]] = pureRegistryFromUpdate_Int(canSetInto_DV_V_Int)
  implicit def canSet_DV_V_Int_def[A <: DenseMatrix[Int], B <: Matrix[Int]]:BinaryOp[A, B, OpSet, DenseMatrix[Int]] = pureRegistryFromUpdate_Int(canSetInto_DV_V_Int).asInstanceOf[BinaryOp[A, B, OpSet, DenseMatrix[Int]]]
    


  class canDivInto_DV_V_Int private[linalg] () extends BinaryUpdateRegistry[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: DenseMatrix[Int], b: Matrix[Int]) {
      
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
  val canDivInto_DV_V_Int = new canDivInto_DV_V_Int ()
  implicit def canDivInto_DV_V_Int_def[A <: DenseMatrix[Int], B <: Matrix[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv] = (
    canDivInto_DV_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv]]
  )

  val canDiv_DV_V_Int: BinaryRegistry[DenseMatrix[Int], Matrix[Int], OpDiv, DenseMatrix[Int]] = pureRegistryFromUpdate_Int(canDivInto_DV_V_Int)
  implicit def canDiv_DV_V_Int_def[A <: DenseMatrix[Int], B <: Matrix[Int]]:BinaryOp[A, B, OpDiv, DenseMatrix[Int]] = pureRegistryFromUpdate_Int(canDivInto_DV_V_Int).asInstanceOf[BinaryOp[A, B, OpDiv, DenseMatrix[Int]]]
    


  class canMulScalarInto_DV_V_Int private[linalg] () extends BinaryUpdateRegistry[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: DenseMatrix[Int], b: Matrix[Int]) {
      
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
  val canMulScalarInto_DV_V_Int = new canMulScalarInto_DV_V_Int ()
  implicit def canMulScalarInto_DV_V_Int_def[A <: DenseMatrix[Int], B <: Matrix[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar] = (
    canMulScalarInto_DV_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar]]
  )

  val canMulScalar_DV_V_Int: BinaryRegistry[DenseMatrix[Int], Matrix[Int], OpMulScalar, DenseMatrix[Int]] = pureRegistryFromUpdate_Int(canMulScalarInto_DV_V_Int)
  implicit def canMulScalar_DV_V_Int_def[A <: DenseMatrix[Int], B <: Matrix[Int]]:BinaryOp[A, B, OpMulScalar, DenseMatrix[Int]] = pureRegistryFromUpdate_Int(canMulScalarInto_DV_V_Int).asInstanceOf[BinaryOp[A, B, OpMulScalar, DenseMatrix[Int]]]
    


  class canPowInto_DV_V_Int private[linalg] () extends BinaryUpdateRegistry[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: DenseMatrix[Int], b: Matrix[Int]) {
      
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
  val canPowInto_DV_V_Int = new canPowInto_DV_V_Int ()
  implicit def canPowInto_DV_V_Int_def[A <: DenseMatrix[Int], B <: Matrix[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow] = (
    canPowInto_DV_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow]]
  )

  val canPow_DV_V_Int: BinaryRegistry[DenseMatrix[Int], Matrix[Int], OpPow, DenseMatrix[Int]] = pureRegistryFromUpdate_Int(canPowInto_DV_V_Int)
  implicit def canPow_DV_V_Int_def[A <: DenseMatrix[Int], B <: Matrix[Int]]:BinaryOp[A, B, OpPow, DenseMatrix[Int]] = pureRegistryFromUpdate_Int(canPowInto_DV_V_Int).asInstanceOf[BinaryOp[A, B, OpPow, DenseMatrix[Int]]]
    


  class canModInto_DV_V_Int private[linalg] () extends BinaryUpdateRegistry[DenseMatrix[Int], Matrix[Int], breeze.linalg.operators.OpMod] {
    override def bindingMissing(a: DenseMatrix[Int], b: Matrix[Int]) {
      
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
  val canModInto_DV_V_Int = new canModInto_DV_V_Int ()
  implicit def canModInto_DV_V_Int_def[A <: DenseMatrix[Int], B <: Matrix[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod] = (
    canModInto_DV_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod]]
  )

  val canMod_DV_V_Int: BinaryRegistry[DenseMatrix[Int], Matrix[Int], OpMod, DenseMatrix[Int]] = pureRegistryFromUpdate_Int(canModInto_DV_V_Int)
  implicit def canMod_DV_V_Int_def[A <: DenseMatrix[Int], B <: Matrix[Int]]:BinaryOp[A, B, OpMod, DenseMatrix[Int]] = pureRegistryFromUpdate_Int(canModInto_DV_V_Int).asInstanceOf[BinaryOp[A, B, OpMod, DenseMatrix[Int]]]
    

}
