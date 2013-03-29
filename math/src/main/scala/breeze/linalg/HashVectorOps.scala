package breeze.linalg
import breeze.linalg.operators._
import breeze.linalg.support._
import breeze.math.Complex
import breeze.math.Complex._
import breeze.numerics._
/** This is an auto-generated trait providing operators for HashVector. */
trait HashVectorOps_Double { this: HashVector.type =>

  def pureFromUpdate_Double[Other,Op<:OpType](op: BinaryUpdateOp[HashVector[Double], Other, Op])(implicit copy: CanCopy[HashVector[Double]]):BinaryRegistry[HashVector[Double], Other, Op, HashVector[Double]] = {
    new BinaryRegistry[HashVector[Double], Other, Op, HashVector[Double]] {
      override def bindingMissing(a : HashVector[Double], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }
        

  class canSubInto_V_V_Double private[linalg] () extends BinaryUpdateRegistry[HashVector[Double], HashVector[Double], breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: HashVector[Double], b: HashVector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) - v
        }
        
    }
  }
  val canSubInto_V_V_Double = new canSubInto_V_V_Double ()
  implicit def canSubInto_V_V_Double_def[A <: HashVector[Double], B <: HashVector[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub] = (
    canSubInto_V_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub]]
  )

  implicit val canSub_V_V_Double: BinaryRegistry[HashVector[Double], HashVector[Double], OpSub, HashVector[Double]] = pureFromUpdate_Double(canSubInto_V_V_Double)


  class canSubInto_V_S_Double private[linalg] () extends BinaryUpdateRegistry[HashVector[Double], Double, breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: HashVector[Double], b: Double) {
      
        if(!true || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v - b
          }
        
    }
  }
  val canSubInto_V_S_Double = new canSubInto_V_S_Double ()
  implicit def canSubInto_V_S_Double_def[A <: HashVector[Double], B <: Double]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub] = (
    canSubInto_V_S_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub]]
  )

  implicit val canSub_V_S_Double: BinaryRegistry[HashVector[Double], Double, OpSub, HashVector[Double]] = pureFromUpdate_Double(canSubInto_V_S_Double)


  class canModInto_V_V_Double private[linalg] () extends BinaryUpdateRegistry[HashVector[Double], HashVector[Double], breeze.linalg.operators.OpMod] {
    override def bindingMissing(a: HashVector[Double], b: HashVector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) % v
        }
        
    }
  }
  val canModInto_V_V_Double = new canModInto_V_V_Double ()
  implicit def canModInto_V_V_Double_def[A <: HashVector[Double], B <: HashVector[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod] = (
    canModInto_V_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod]]
  )

  implicit val canMod_V_V_Double: BinaryRegistry[HashVector[Double], HashVector[Double], OpMod, HashVector[Double]] = pureFromUpdate_Double(canModInto_V_V_Double)


  class canModInto_V_S_Double private[linalg] () extends BinaryUpdateRegistry[HashVector[Double], Double, breeze.linalg.operators.OpMod] {
    override def bindingMissing(a: HashVector[Double], b: Double) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v % b
          }
        
    }
  }
  val canModInto_V_S_Double = new canModInto_V_S_Double ()
  implicit def canModInto_V_S_Double_def[A <: HashVector[Double], B <: Double]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod] = (
    canModInto_V_S_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod]]
  )

  implicit val canMod_V_S_Double: BinaryRegistry[HashVector[Double], Double, OpMod, HashVector[Double]] = pureFromUpdate_Double(canModInto_V_S_Double)


  class canAddInto_V_V_Double private[linalg] () extends BinaryUpdateRegistry[HashVector[Double], HashVector[Double], breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: HashVector[Double], b: HashVector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) + v
        }
        
    }
  }
  val canAddInto_V_V_Double = new canAddInto_V_V_Double ()
  implicit def canAddInto_V_V_Double_def[A <: HashVector[Double], B <: HashVector[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd] = (
    canAddInto_V_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd]]
  )

  implicit val canAdd_V_V_Double: BinaryRegistry[HashVector[Double], HashVector[Double], OpAdd, HashVector[Double]] = pureFromUpdate_Double(canAddInto_V_V_Double)


  class canAddInto_V_S_Double private[linalg] () extends BinaryUpdateRegistry[HashVector[Double], Double, breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: HashVector[Double], b: Double) {
      
        if(!true || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v + b
          }
        
    }
  }
  val canAddInto_V_S_Double = new canAddInto_V_S_Double ()
  implicit def canAddInto_V_S_Double_def[A <: HashVector[Double], B <: Double]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd] = (
    canAddInto_V_S_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd]]
  )

  implicit val canAdd_V_S_Double: BinaryRegistry[HashVector[Double], Double, OpAdd, HashVector[Double]] = pureFromUpdate_Double(canAddInto_V_S_Double)


  class canPowInto_V_V_Double private[linalg] () extends BinaryUpdateRegistry[HashVector[Double], HashVector[Double], breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: HashVector[Double], b: HashVector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = scala.math.pow(a(i), v)
        }
        
    }
  }
  val canPowInto_V_V_Double = new canPowInto_V_V_Double ()
  implicit def canPowInto_V_V_Double_def[A <: HashVector[Double], B <: HashVector[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow] = (
    canPowInto_V_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow]]
  )

  implicit val canPow_V_V_Double: BinaryRegistry[HashVector[Double], HashVector[Double], OpPow, HashVector[Double]] = pureFromUpdate_Double(canPowInto_V_V_Double)


  class canPowInto_V_S_Double private[linalg] () extends BinaryUpdateRegistry[HashVector[Double], Double, breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: HashVector[Double], b: Double) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = scala.math.pow(v, b)
          }
        
    }
  }
  val canPowInto_V_S_Double = new canPowInto_V_S_Double ()
  implicit def canPowInto_V_S_Double_def[A <: HashVector[Double], B <: Double]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow] = (
    canPowInto_V_S_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow]]
  )

  implicit val canPow_V_S_Double: BinaryRegistry[HashVector[Double], Double, OpPow, HashVector[Double]] = pureFromUpdate_Double(canPowInto_V_S_Double)


  class canMulScalarInto_V_V_Double private[linalg] () extends BinaryUpdateRegistry[HashVector[Double], HashVector[Double], breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: HashVector[Double], b: HashVector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) * v
        }
        
    }
  }
  val canMulScalarInto_V_V_Double = new canMulScalarInto_V_V_Double ()
  implicit def canMulScalarInto_V_V_Double_def[A <: HashVector[Double], B <: HashVector[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar] = (
    canMulScalarInto_V_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar]]
  )

  implicit val canMulScalar_V_V_Double: BinaryRegistry[HashVector[Double], HashVector[Double], OpMulScalar, HashVector[Double]] = pureFromUpdate_Double(canMulScalarInto_V_V_Double)


  class canMulScalarInto_V_S_Double private[linalg] () extends BinaryUpdateRegistry[HashVector[Double], Double, breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: HashVector[Double], b: Double) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v * b
          }
        
    }
  }
  val canMulScalarInto_V_S_Double = new canMulScalarInto_V_S_Double ()
  implicit def canMulScalarInto_V_S_Double_def[A <: HashVector[Double], B <: Double]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar] = (
    canMulScalarInto_V_S_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar]]
  )

  implicit val canMulScalar_V_S_Double: BinaryRegistry[HashVector[Double], Double, OpMulScalar, HashVector[Double]] = pureFromUpdate_Double(canMulScalarInto_V_S_Double)


  class canDivInto_V_V_Double private[linalg] () extends BinaryUpdateRegistry[HashVector[Double], HashVector[Double], breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: HashVector[Double], b: HashVector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) / v
        }
        
    }
  }
  val canDivInto_V_V_Double = new canDivInto_V_V_Double ()
  implicit def canDivInto_V_V_Double_def[A <: HashVector[Double], B <: HashVector[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv] = (
    canDivInto_V_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv]]
  )

  implicit val canDiv_V_V_Double: BinaryRegistry[HashVector[Double], HashVector[Double], OpDiv, HashVector[Double]] = pureFromUpdate_Double(canDivInto_V_V_Double)


  class canDivInto_V_S_Double private[linalg] () extends BinaryUpdateRegistry[HashVector[Double], Double, breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: HashVector[Double], b: Double) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v / b
          }
        
    }
  }
  val canDivInto_V_S_Double = new canDivInto_V_S_Double ()
  implicit def canDivInto_V_S_Double_def[A <: HashVector[Double], B <: Double]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv] = (
    canDivInto_V_S_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv]]
  )

  implicit val canDiv_V_S_Double: BinaryRegistry[HashVector[Double], Double, OpDiv, HashVector[Double]] = pureFromUpdate_Double(canDivInto_V_S_Double)


  class canMulMatrixInto_V_S_Double private[linalg] () extends BinaryUpdateRegistry[HashVector[Double], Double, breeze.linalg.operators.OpMulMatrix] {
    override def bindingMissing(a: HashVector[Double], b: Double) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v * b
          }
        
    }
  }
  val canMulMatrixInto_V_S_Double = new canMulMatrixInto_V_S_Double ()
  implicit def canMulMatrixInto_V_S_Double_def[A <: HashVector[Double], B <: Double]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulMatrix] = (
    canMulMatrixInto_V_S_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulMatrix]]
  )

  implicit val canMulMatrix_V_S_Double: BinaryRegistry[HashVector[Double], Double, OpMulMatrix, HashVector[Double]] = pureFromUpdate_Double(canMulMatrixInto_V_S_Double)


  class canSetInto_V_V_Double private[linalg] () extends BinaryUpdateRegistry[HashVector[Double], HashVector[Double], breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: HashVector[Double], b: HashVector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = v
        }
        
    }
  }
  val canSetInto_V_V_Double = new canSetInto_V_V_Double ()
  implicit def canSetInto_V_V_Double_def[A <: HashVector[Double], B <: HashVector[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet] = (
    canSetInto_V_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet]]
  )

  implicit val canSet_V_V_Double: BinaryRegistry[HashVector[Double], HashVector[Double], OpSet, HashVector[Double]] = pureFromUpdate_Double(canSetInto_V_V_Double)


  class canSetInto_V_S_Double private[linalg] () extends BinaryUpdateRegistry[HashVector[Double], Double, breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: HashVector[Double], b: Double) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = b
          }
        
    }
  }
  val canSetInto_V_S_Double = new canSetInto_V_S_Double ()
  implicit def canSetInto_V_S_Double_def[A <: HashVector[Double], B <: Double]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet] = (
    canSetInto_V_S_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet]]
  )

  implicit val canSet_V_S_Double: BinaryRegistry[HashVector[Double], Double, OpSet, HashVector[Double]] = pureFromUpdate_Double(canSetInto_V_S_Double)


  class canAxpy_SV_SV_Double private[linalg] () extends CanAxpy[Double, HashVector[Double], HashVector[Double]] {
    def apply(s: Double, b: HashVector[Double], a: HashVector[Double]) {
      if(s == 0) return;
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i, v) <- b.activeIterator) {
          a(i) += v * s
        }
        
    }
  }
  implicit val canAxpy_SV_SV_Double = new canAxpy_SV_SV_Double ()
    

  class canDotProductV_Double private[linalg] () extends BinaryRegistry[HashVector[Double], HashVector[Double], breeze.linalg.operators.OpMulInner, Double] {
    override def bindingMissing(a: HashVector[Double], b: HashVector[Double]) = {
      require(b.length == a.length, "Vectors must be the same length!")

       var result: Double = 0

         for( (i, v) <- b.activeIterator) {
           result += a(i) * v
         }
         result
    }
  };
  val canDotProductV_Double = new canDotProductV_Double()
  implicit def canDotProductV_Double_def[A <: HashVector[Double], B <: HashVector[Double]]:BinaryOp[A, B, breeze.linalg.operators.OpMulInner, Double] = (
    canDotProductV_Double.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulInner, Double]]
  )
    
}
/** This is an auto-generated trait providing operators for HashVector. */
trait HashVectorOps_Float { this: HashVector.type =>

  def pureFromUpdate_Float[Other,Op<:OpType](op: BinaryUpdateOp[HashVector[Float], Other, Op])(implicit copy: CanCopy[HashVector[Float]]):BinaryRegistry[HashVector[Float], Other, Op, HashVector[Float]] = {
    new BinaryRegistry[HashVector[Float], Other, Op, HashVector[Float]] {
      override def bindingMissing(a : HashVector[Float], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }
        

  class canSubInto_V_V_Float private[linalg] () extends BinaryUpdateRegistry[HashVector[Float], HashVector[Float], breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: HashVector[Float], b: HashVector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) - v
        }
        
    }
  }
  val canSubInto_V_V_Float = new canSubInto_V_V_Float ()
  implicit def canSubInto_V_V_Float_def[A <: HashVector[Float], B <: HashVector[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub] = (
    canSubInto_V_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub]]
  )

  implicit val canSub_V_V_Float: BinaryRegistry[HashVector[Float], HashVector[Float], OpSub, HashVector[Float]] = pureFromUpdate_Float(canSubInto_V_V_Float)


  class canSubInto_V_S_Float private[linalg] () extends BinaryUpdateRegistry[HashVector[Float], Float, breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: HashVector[Float], b: Float) {
      
        if(!true || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v - b
          }
        
    }
  }
  val canSubInto_V_S_Float = new canSubInto_V_S_Float ()
  implicit def canSubInto_V_S_Float_def[A <: HashVector[Float], B <: Float]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub] = (
    canSubInto_V_S_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub]]
  )

  implicit val canSub_V_S_Float: BinaryRegistry[HashVector[Float], Float, OpSub, HashVector[Float]] = pureFromUpdate_Float(canSubInto_V_S_Float)


  class canModInto_V_V_Float private[linalg] () extends BinaryUpdateRegistry[HashVector[Float], HashVector[Float], breeze.linalg.operators.OpMod] {
    override def bindingMissing(a: HashVector[Float], b: HashVector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) % v
        }
        
    }
  }
  val canModInto_V_V_Float = new canModInto_V_V_Float ()
  implicit def canModInto_V_V_Float_def[A <: HashVector[Float], B <: HashVector[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod] = (
    canModInto_V_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod]]
  )

  implicit val canMod_V_V_Float: BinaryRegistry[HashVector[Float], HashVector[Float], OpMod, HashVector[Float]] = pureFromUpdate_Float(canModInto_V_V_Float)


  class canModInto_V_S_Float private[linalg] () extends BinaryUpdateRegistry[HashVector[Float], Float, breeze.linalg.operators.OpMod] {
    override def bindingMissing(a: HashVector[Float], b: Float) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v % b
          }
        
    }
  }
  val canModInto_V_S_Float = new canModInto_V_S_Float ()
  implicit def canModInto_V_S_Float_def[A <: HashVector[Float], B <: Float]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod] = (
    canModInto_V_S_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod]]
  )

  implicit val canMod_V_S_Float: BinaryRegistry[HashVector[Float], Float, OpMod, HashVector[Float]] = pureFromUpdate_Float(canModInto_V_S_Float)


  class canAddInto_V_V_Float private[linalg] () extends BinaryUpdateRegistry[HashVector[Float], HashVector[Float], breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: HashVector[Float], b: HashVector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) + v
        }
        
    }
  }
  val canAddInto_V_V_Float = new canAddInto_V_V_Float ()
  implicit def canAddInto_V_V_Float_def[A <: HashVector[Float], B <: HashVector[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd] = (
    canAddInto_V_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd]]
  )

  implicit val canAdd_V_V_Float: BinaryRegistry[HashVector[Float], HashVector[Float], OpAdd, HashVector[Float]] = pureFromUpdate_Float(canAddInto_V_V_Float)


  class canAddInto_V_S_Float private[linalg] () extends BinaryUpdateRegistry[HashVector[Float], Float, breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: HashVector[Float], b: Float) {
      
        if(!true || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v + b
          }
        
    }
  }
  val canAddInto_V_S_Float = new canAddInto_V_S_Float ()
  implicit def canAddInto_V_S_Float_def[A <: HashVector[Float], B <: Float]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd] = (
    canAddInto_V_S_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd]]
  )

  implicit val canAdd_V_S_Float: BinaryRegistry[HashVector[Float], Float, OpAdd, HashVector[Float]] = pureFromUpdate_Float(canAddInto_V_S_Float)


  class canPowInto_V_V_Float private[linalg] () extends BinaryUpdateRegistry[HashVector[Float], HashVector[Float], breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: HashVector[Float], b: HashVector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = scala.math.pow(a(i), v).toFloat
        }
        
    }
  }
  val canPowInto_V_V_Float = new canPowInto_V_V_Float ()
  implicit def canPowInto_V_V_Float_def[A <: HashVector[Float], B <: HashVector[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow] = (
    canPowInto_V_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow]]
  )

  implicit val canPow_V_V_Float: BinaryRegistry[HashVector[Float], HashVector[Float], OpPow, HashVector[Float]] = pureFromUpdate_Float(canPowInto_V_V_Float)


  class canPowInto_V_S_Float private[linalg] () extends BinaryUpdateRegistry[HashVector[Float], Float, breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: HashVector[Float], b: Float) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = scala.math.pow(v, b).toFloat
          }
        
    }
  }
  val canPowInto_V_S_Float = new canPowInto_V_S_Float ()
  implicit def canPowInto_V_S_Float_def[A <: HashVector[Float], B <: Float]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow] = (
    canPowInto_V_S_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow]]
  )

  implicit val canPow_V_S_Float: BinaryRegistry[HashVector[Float], Float, OpPow, HashVector[Float]] = pureFromUpdate_Float(canPowInto_V_S_Float)


  class canMulScalarInto_V_V_Float private[linalg] () extends BinaryUpdateRegistry[HashVector[Float], HashVector[Float], breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: HashVector[Float], b: HashVector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) * v
        }
        
    }
  }
  val canMulScalarInto_V_V_Float = new canMulScalarInto_V_V_Float ()
  implicit def canMulScalarInto_V_V_Float_def[A <: HashVector[Float], B <: HashVector[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar] = (
    canMulScalarInto_V_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar]]
  )

  implicit val canMulScalar_V_V_Float: BinaryRegistry[HashVector[Float], HashVector[Float], OpMulScalar, HashVector[Float]] = pureFromUpdate_Float(canMulScalarInto_V_V_Float)


  class canMulScalarInto_V_S_Float private[linalg] () extends BinaryUpdateRegistry[HashVector[Float], Float, breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: HashVector[Float], b: Float) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v * b
          }
        
    }
  }
  val canMulScalarInto_V_S_Float = new canMulScalarInto_V_S_Float ()
  implicit def canMulScalarInto_V_S_Float_def[A <: HashVector[Float], B <: Float]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar] = (
    canMulScalarInto_V_S_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar]]
  )

  implicit val canMulScalar_V_S_Float: BinaryRegistry[HashVector[Float], Float, OpMulScalar, HashVector[Float]] = pureFromUpdate_Float(canMulScalarInto_V_S_Float)


  class canDivInto_V_V_Float private[linalg] () extends BinaryUpdateRegistry[HashVector[Float], HashVector[Float], breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: HashVector[Float], b: HashVector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) / v
        }
        
    }
  }
  val canDivInto_V_V_Float = new canDivInto_V_V_Float ()
  implicit def canDivInto_V_V_Float_def[A <: HashVector[Float], B <: HashVector[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv] = (
    canDivInto_V_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv]]
  )

  implicit val canDiv_V_V_Float: BinaryRegistry[HashVector[Float], HashVector[Float], OpDiv, HashVector[Float]] = pureFromUpdate_Float(canDivInto_V_V_Float)


  class canDivInto_V_S_Float private[linalg] () extends BinaryUpdateRegistry[HashVector[Float], Float, breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: HashVector[Float], b: Float) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v / b
          }
        
    }
  }
  val canDivInto_V_S_Float = new canDivInto_V_S_Float ()
  implicit def canDivInto_V_S_Float_def[A <: HashVector[Float], B <: Float]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv] = (
    canDivInto_V_S_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv]]
  )

  implicit val canDiv_V_S_Float: BinaryRegistry[HashVector[Float], Float, OpDiv, HashVector[Float]] = pureFromUpdate_Float(canDivInto_V_S_Float)


  class canMulMatrixInto_V_S_Float private[linalg] () extends BinaryUpdateRegistry[HashVector[Float], Float, breeze.linalg.operators.OpMulMatrix] {
    override def bindingMissing(a: HashVector[Float], b: Float) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v * b
          }
        
    }
  }
  val canMulMatrixInto_V_S_Float = new canMulMatrixInto_V_S_Float ()
  implicit def canMulMatrixInto_V_S_Float_def[A <: HashVector[Float], B <: Float]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulMatrix] = (
    canMulMatrixInto_V_S_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulMatrix]]
  )

  implicit val canMulMatrix_V_S_Float: BinaryRegistry[HashVector[Float], Float, OpMulMatrix, HashVector[Float]] = pureFromUpdate_Float(canMulMatrixInto_V_S_Float)


  class canSetInto_V_V_Float private[linalg] () extends BinaryUpdateRegistry[HashVector[Float], HashVector[Float], breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: HashVector[Float], b: HashVector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = v
        }
        
    }
  }
  val canSetInto_V_V_Float = new canSetInto_V_V_Float ()
  implicit def canSetInto_V_V_Float_def[A <: HashVector[Float], B <: HashVector[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet] = (
    canSetInto_V_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet]]
  )

  implicit val canSet_V_V_Float: BinaryRegistry[HashVector[Float], HashVector[Float], OpSet, HashVector[Float]] = pureFromUpdate_Float(canSetInto_V_V_Float)


  class canSetInto_V_S_Float private[linalg] () extends BinaryUpdateRegistry[HashVector[Float], Float, breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: HashVector[Float], b: Float) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = b
          }
        
    }
  }
  val canSetInto_V_S_Float = new canSetInto_V_S_Float ()
  implicit def canSetInto_V_S_Float_def[A <: HashVector[Float], B <: Float]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet] = (
    canSetInto_V_S_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet]]
  )

  implicit val canSet_V_S_Float: BinaryRegistry[HashVector[Float], Float, OpSet, HashVector[Float]] = pureFromUpdate_Float(canSetInto_V_S_Float)


  class canAxpy_SV_SV_Float private[linalg] () extends CanAxpy[Float, HashVector[Float], HashVector[Float]] {
    def apply(s: Float, b: HashVector[Float], a: HashVector[Float]) {
      if(s == 0) return;
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i, v) <- b.activeIterator) {
          a(i) += v * s
        }
        
    }
  }
  implicit val canAxpy_SV_SV_Float = new canAxpy_SV_SV_Float ()
    

  class canDotProductV_Float private[linalg] () extends BinaryRegistry[HashVector[Float], HashVector[Float], breeze.linalg.operators.OpMulInner, Float] {
    override def bindingMissing(a: HashVector[Float], b: HashVector[Float]) = {
      require(b.length == a.length, "Vectors must be the same length!")

       var result: Float = 0

         for( (i, v) <- b.activeIterator) {
           result += a(i) * v
         }
         result
    }
  };
  val canDotProductV_Float = new canDotProductV_Float()
  implicit def canDotProductV_Float_def[A <: HashVector[Float], B <: HashVector[Float]]:BinaryOp[A, B, breeze.linalg.operators.OpMulInner, Float] = (
    canDotProductV_Float.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulInner, Float]]
  )
    
}
/** This is an auto-generated trait providing operators for HashVector. */
trait HashVectorOps_Int { this: HashVector.type =>

  def pureFromUpdate_Int[Other,Op<:OpType](op: BinaryUpdateOp[HashVector[Int], Other, Op])(implicit copy: CanCopy[HashVector[Int]]):BinaryRegistry[HashVector[Int], Other, Op, HashVector[Int]] = {
    new BinaryRegistry[HashVector[Int], Other, Op, HashVector[Int]] {
      override def bindingMissing(a : HashVector[Int], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }
        

  class canSubInto_V_V_Int private[linalg] () extends BinaryUpdateRegistry[HashVector[Int], HashVector[Int], breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: HashVector[Int], b: HashVector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) - v
        }
        
    }
  }
  val canSubInto_V_V_Int = new canSubInto_V_V_Int ()
  implicit def canSubInto_V_V_Int_def[A <: HashVector[Int], B <: HashVector[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub] = (
    canSubInto_V_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub]]
  )

  implicit val canSub_V_V_Int: BinaryRegistry[HashVector[Int], HashVector[Int], OpSub, HashVector[Int]] = pureFromUpdate_Int(canSubInto_V_V_Int)


  class canSubInto_V_S_Int private[linalg] () extends BinaryUpdateRegistry[HashVector[Int], Int, breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: HashVector[Int], b: Int) {
      
        if(!true || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v - b
          }
        
    }
  }
  val canSubInto_V_S_Int = new canSubInto_V_S_Int ()
  implicit def canSubInto_V_S_Int_def[A <: HashVector[Int], B <: Int]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub] = (
    canSubInto_V_S_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub]]
  )

  implicit val canSub_V_S_Int: BinaryRegistry[HashVector[Int], Int, OpSub, HashVector[Int]] = pureFromUpdate_Int(canSubInto_V_S_Int)


  class canModInto_V_V_Int private[linalg] () extends BinaryUpdateRegistry[HashVector[Int], HashVector[Int], breeze.linalg.operators.OpMod] {
    override def bindingMissing(a: HashVector[Int], b: HashVector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) % v
        }
        
    }
  }
  val canModInto_V_V_Int = new canModInto_V_V_Int ()
  implicit def canModInto_V_V_Int_def[A <: HashVector[Int], B <: HashVector[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod] = (
    canModInto_V_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod]]
  )

  implicit val canMod_V_V_Int: BinaryRegistry[HashVector[Int], HashVector[Int], OpMod, HashVector[Int]] = pureFromUpdate_Int(canModInto_V_V_Int)


  class canModInto_V_S_Int private[linalg] () extends BinaryUpdateRegistry[HashVector[Int], Int, breeze.linalg.operators.OpMod] {
    override def bindingMissing(a: HashVector[Int], b: Int) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v % b
          }
        
    }
  }
  val canModInto_V_S_Int = new canModInto_V_S_Int ()
  implicit def canModInto_V_S_Int_def[A <: HashVector[Int], B <: Int]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod] = (
    canModInto_V_S_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod]]
  )

  implicit val canMod_V_S_Int: BinaryRegistry[HashVector[Int], Int, OpMod, HashVector[Int]] = pureFromUpdate_Int(canModInto_V_S_Int)


  class canAddInto_V_V_Int private[linalg] () extends BinaryUpdateRegistry[HashVector[Int], HashVector[Int], breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: HashVector[Int], b: HashVector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) + v
        }
        
    }
  }
  val canAddInto_V_V_Int = new canAddInto_V_V_Int ()
  implicit def canAddInto_V_V_Int_def[A <: HashVector[Int], B <: HashVector[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd] = (
    canAddInto_V_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd]]
  )

  implicit val canAdd_V_V_Int: BinaryRegistry[HashVector[Int], HashVector[Int], OpAdd, HashVector[Int]] = pureFromUpdate_Int(canAddInto_V_V_Int)


  class canAddInto_V_S_Int private[linalg] () extends BinaryUpdateRegistry[HashVector[Int], Int, breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: HashVector[Int], b: Int) {
      
        if(!true || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v + b
          }
        
    }
  }
  val canAddInto_V_S_Int = new canAddInto_V_S_Int ()
  implicit def canAddInto_V_S_Int_def[A <: HashVector[Int], B <: Int]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd] = (
    canAddInto_V_S_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd]]
  )

  implicit val canAdd_V_S_Int: BinaryRegistry[HashVector[Int], Int, OpAdd, HashVector[Int]] = pureFromUpdate_Int(canAddInto_V_S_Int)


  class canPowInto_V_V_Int private[linalg] () extends BinaryUpdateRegistry[HashVector[Int], HashVector[Int], breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: HashVector[Int], b: HashVector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = IntMath.ipow(a(i), v)
        }
        
    }
  }
  val canPowInto_V_V_Int = new canPowInto_V_V_Int ()
  implicit def canPowInto_V_V_Int_def[A <: HashVector[Int], B <: HashVector[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow] = (
    canPowInto_V_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow]]
  )

  implicit val canPow_V_V_Int: BinaryRegistry[HashVector[Int], HashVector[Int], OpPow, HashVector[Int]] = pureFromUpdate_Int(canPowInto_V_V_Int)


  class canPowInto_V_S_Int private[linalg] () extends BinaryUpdateRegistry[HashVector[Int], Int, breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: HashVector[Int], b: Int) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = IntMath.ipow(v, b)
          }
        
    }
  }
  val canPowInto_V_S_Int = new canPowInto_V_S_Int ()
  implicit def canPowInto_V_S_Int_def[A <: HashVector[Int], B <: Int]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow] = (
    canPowInto_V_S_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow]]
  )

  implicit val canPow_V_S_Int: BinaryRegistry[HashVector[Int], Int, OpPow, HashVector[Int]] = pureFromUpdate_Int(canPowInto_V_S_Int)


  class canMulScalarInto_V_V_Int private[linalg] () extends BinaryUpdateRegistry[HashVector[Int], HashVector[Int], breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: HashVector[Int], b: HashVector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) * v
        }
        
    }
  }
  val canMulScalarInto_V_V_Int = new canMulScalarInto_V_V_Int ()
  implicit def canMulScalarInto_V_V_Int_def[A <: HashVector[Int], B <: HashVector[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar] = (
    canMulScalarInto_V_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar]]
  )

  implicit val canMulScalar_V_V_Int: BinaryRegistry[HashVector[Int], HashVector[Int], OpMulScalar, HashVector[Int]] = pureFromUpdate_Int(canMulScalarInto_V_V_Int)


  class canMulScalarInto_V_S_Int private[linalg] () extends BinaryUpdateRegistry[HashVector[Int], Int, breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: HashVector[Int], b: Int) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v * b
          }
        
    }
  }
  val canMulScalarInto_V_S_Int = new canMulScalarInto_V_S_Int ()
  implicit def canMulScalarInto_V_S_Int_def[A <: HashVector[Int], B <: Int]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar] = (
    canMulScalarInto_V_S_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar]]
  )

  implicit val canMulScalar_V_S_Int: BinaryRegistry[HashVector[Int], Int, OpMulScalar, HashVector[Int]] = pureFromUpdate_Int(canMulScalarInto_V_S_Int)


  class canDivInto_V_V_Int private[linalg] () extends BinaryUpdateRegistry[HashVector[Int], HashVector[Int], breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: HashVector[Int], b: HashVector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) / v
        }
        
    }
  }
  val canDivInto_V_V_Int = new canDivInto_V_V_Int ()
  implicit def canDivInto_V_V_Int_def[A <: HashVector[Int], B <: HashVector[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv] = (
    canDivInto_V_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv]]
  )

  implicit val canDiv_V_V_Int: BinaryRegistry[HashVector[Int], HashVector[Int], OpDiv, HashVector[Int]] = pureFromUpdate_Int(canDivInto_V_V_Int)


  class canDivInto_V_S_Int private[linalg] () extends BinaryUpdateRegistry[HashVector[Int], Int, breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: HashVector[Int], b: Int) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v / b
          }
        
    }
  }
  val canDivInto_V_S_Int = new canDivInto_V_S_Int ()
  implicit def canDivInto_V_S_Int_def[A <: HashVector[Int], B <: Int]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv] = (
    canDivInto_V_S_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv]]
  )

  implicit val canDiv_V_S_Int: BinaryRegistry[HashVector[Int], Int, OpDiv, HashVector[Int]] = pureFromUpdate_Int(canDivInto_V_S_Int)


  class canMulMatrixInto_V_S_Int private[linalg] () extends BinaryUpdateRegistry[HashVector[Int], Int, breeze.linalg.operators.OpMulMatrix] {
    override def bindingMissing(a: HashVector[Int], b: Int) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v * b
          }
        
    }
  }
  val canMulMatrixInto_V_S_Int = new canMulMatrixInto_V_S_Int ()
  implicit def canMulMatrixInto_V_S_Int_def[A <: HashVector[Int], B <: Int]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulMatrix] = (
    canMulMatrixInto_V_S_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulMatrix]]
  )

  implicit val canMulMatrix_V_S_Int: BinaryRegistry[HashVector[Int], Int, OpMulMatrix, HashVector[Int]] = pureFromUpdate_Int(canMulMatrixInto_V_S_Int)


  class canSetInto_V_V_Int private[linalg] () extends BinaryUpdateRegistry[HashVector[Int], HashVector[Int], breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: HashVector[Int], b: HashVector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = v
        }
        
    }
  }
  val canSetInto_V_V_Int = new canSetInto_V_V_Int ()
  implicit def canSetInto_V_V_Int_def[A <: HashVector[Int], B <: HashVector[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet] = (
    canSetInto_V_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet]]
  )

  implicit val canSet_V_V_Int: BinaryRegistry[HashVector[Int], HashVector[Int], OpSet, HashVector[Int]] = pureFromUpdate_Int(canSetInto_V_V_Int)


  class canSetInto_V_S_Int private[linalg] () extends BinaryUpdateRegistry[HashVector[Int], Int, breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: HashVector[Int], b: Int) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = b
          }
        
    }
  }
  val canSetInto_V_S_Int = new canSetInto_V_S_Int ()
  implicit def canSetInto_V_S_Int_def[A <: HashVector[Int], B <: Int]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet] = (
    canSetInto_V_S_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet]]
  )

  implicit val canSet_V_S_Int: BinaryRegistry[HashVector[Int], Int, OpSet, HashVector[Int]] = pureFromUpdate_Int(canSetInto_V_S_Int)


  class canAxpy_SV_SV_Int private[linalg] () extends CanAxpy[Int, HashVector[Int], HashVector[Int]] {
    def apply(s: Int, b: HashVector[Int], a: HashVector[Int]) {
      if(s == 0) return;
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i, v) <- b.activeIterator) {
          a(i) += v * s
        }
        
    }
  }
  implicit val canAxpy_SV_SV_Int = new canAxpy_SV_SV_Int ()
    

  class canDotProductV_Int private[linalg] () extends BinaryRegistry[HashVector[Int], HashVector[Int], breeze.linalg.operators.OpMulInner, Int] {
    override def bindingMissing(a: HashVector[Int], b: HashVector[Int]) = {
      require(b.length == a.length, "Vectors must be the same length!")

       var result: Int = 0

         for( (i, v) <- b.activeIterator) {
           result += a(i) * v
         }
         result
    }
  };
  val canDotProductV_Int = new canDotProductV_Int()
  implicit def canDotProductV_Int_def[A <: HashVector[Int], B <: HashVector[Int]]:BinaryOp[A, B, breeze.linalg.operators.OpMulInner, Int] = (
    canDotProductV_Int.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulInner, Int]]
  )
    
}
/** This is an auto-generated trait providing operators for HashVector. */
trait HashVectorOps_Complex { this: HashVector.type =>

  def pureFromUpdate_Complex[Other,Op<:OpType](op: BinaryUpdateOp[HashVector[Complex], Other, Op])(implicit copy: CanCopy[HashVector[Complex]]):BinaryRegistry[HashVector[Complex], Other, Op, HashVector[Complex]] = {
    new BinaryRegistry[HashVector[Complex], Other, Op, HashVector[Complex]] {
      override def bindingMissing(a : HashVector[Complex], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }
        

  class canSubInto_V_V_Complex private[linalg] () extends BinaryUpdateRegistry[HashVector[Complex], HashVector[Complex], breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: HashVector[Complex], b: HashVector[Complex]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) - v
        }
        
    }
  }
  val canSubInto_V_V_Complex = new canSubInto_V_V_Complex ()
  implicit def canSubInto_V_V_Complex_def[A <: HashVector[Complex], B <: HashVector[Complex]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub] = (
    canSubInto_V_V_Complex.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub]]
  )

  implicit val canSub_V_V_Complex: BinaryRegistry[HashVector[Complex], HashVector[Complex], OpSub, HashVector[Complex]] = pureFromUpdate_Complex(canSubInto_V_V_Complex)


  class canSubInto_V_S_Complex private[linalg] () extends BinaryUpdateRegistry[HashVector[Complex], Complex, breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: HashVector[Complex], b: Complex) {
      
        if(!true || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v - b
          }
        
    }
  }
  val canSubInto_V_S_Complex = new canSubInto_V_S_Complex ()
  implicit def canSubInto_V_S_Complex_def[A <: HashVector[Complex], B <: Complex]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub] = (
    canSubInto_V_S_Complex.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub]]
  )

  implicit val canSub_V_S_Complex: BinaryRegistry[HashVector[Complex], Complex, OpSub, HashVector[Complex]] = pureFromUpdate_Complex(canSubInto_V_S_Complex)


  class canAddInto_V_V_Complex private[linalg] () extends BinaryUpdateRegistry[HashVector[Complex], HashVector[Complex], breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: HashVector[Complex], b: HashVector[Complex]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) + v
        }
        
    }
  }
  val canAddInto_V_V_Complex = new canAddInto_V_V_Complex ()
  implicit def canAddInto_V_V_Complex_def[A <: HashVector[Complex], B <: HashVector[Complex]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd] = (
    canAddInto_V_V_Complex.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd]]
  )

  implicit val canAdd_V_V_Complex: BinaryRegistry[HashVector[Complex], HashVector[Complex], OpAdd, HashVector[Complex]] = pureFromUpdate_Complex(canAddInto_V_V_Complex)


  class canAddInto_V_S_Complex private[linalg] () extends BinaryUpdateRegistry[HashVector[Complex], Complex, breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: HashVector[Complex], b: Complex) {
      
        if(!true || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v + b
          }
        
    }
  }
  val canAddInto_V_S_Complex = new canAddInto_V_S_Complex ()
  implicit def canAddInto_V_S_Complex_def[A <: HashVector[Complex], B <: Complex]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd] = (
    canAddInto_V_S_Complex.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd]]
  )

  implicit val canAdd_V_S_Complex: BinaryRegistry[HashVector[Complex], Complex, OpAdd, HashVector[Complex]] = pureFromUpdate_Complex(canAddInto_V_S_Complex)


  class canPowInto_V_V_Complex private[linalg] () extends BinaryUpdateRegistry[HashVector[Complex], HashVector[Complex], breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: HashVector[Complex], b: HashVector[Complex]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i).pow(v)
        }
        
    }
  }
  val canPowInto_V_V_Complex = new canPowInto_V_V_Complex ()
  implicit def canPowInto_V_V_Complex_def[A <: HashVector[Complex], B <: HashVector[Complex]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow] = (
    canPowInto_V_V_Complex.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow]]
  )

  implicit val canPow_V_V_Complex: BinaryRegistry[HashVector[Complex], HashVector[Complex], OpPow, HashVector[Complex]] = pureFromUpdate_Complex(canPowInto_V_V_Complex)


  class canPowInto_V_S_Complex private[linalg] () extends BinaryUpdateRegistry[HashVector[Complex], Complex, breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: HashVector[Complex], b: Complex) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v.pow(b)
          }
        
    }
  }
  val canPowInto_V_S_Complex = new canPowInto_V_S_Complex ()
  implicit def canPowInto_V_S_Complex_def[A <: HashVector[Complex], B <: Complex]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow] = (
    canPowInto_V_S_Complex.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow]]
  )

  implicit val canPow_V_S_Complex: BinaryRegistry[HashVector[Complex], Complex, OpPow, HashVector[Complex]] = pureFromUpdate_Complex(canPowInto_V_S_Complex)


  class canMulScalarInto_V_V_Complex private[linalg] () extends BinaryUpdateRegistry[HashVector[Complex], HashVector[Complex], breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: HashVector[Complex], b: HashVector[Complex]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) * v
        }
        
    }
  }
  val canMulScalarInto_V_V_Complex = new canMulScalarInto_V_V_Complex ()
  implicit def canMulScalarInto_V_V_Complex_def[A <: HashVector[Complex], B <: HashVector[Complex]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar] = (
    canMulScalarInto_V_V_Complex.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar]]
  )

  implicit val canMulScalar_V_V_Complex: BinaryRegistry[HashVector[Complex], HashVector[Complex], OpMulScalar, HashVector[Complex]] = pureFromUpdate_Complex(canMulScalarInto_V_V_Complex)


  class canMulScalarInto_V_S_Complex private[linalg] () extends BinaryUpdateRegistry[HashVector[Complex], Complex, breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: HashVector[Complex], b: Complex) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v * b
          }
        
    }
  }
  val canMulScalarInto_V_S_Complex = new canMulScalarInto_V_S_Complex ()
  implicit def canMulScalarInto_V_S_Complex_def[A <: HashVector[Complex], B <: Complex]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar] = (
    canMulScalarInto_V_S_Complex.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar]]
  )

  implicit val canMulScalar_V_S_Complex: BinaryRegistry[HashVector[Complex], Complex, OpMulScalar, HashVector[Complex]] = pureFromUpdate_Complex(canMulScalarInto_V_S_Complex)


  class canDivInto_V_V_Complex private[linalg] () extends BinaryUpdateRegistry[HashVector[Complex], HashVector[Complex], breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: HashVector[Complex], b: HashVector[Complex]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) / v
        }
        
    }
  }
  val canDivInto_V_V_Complex = new canDivInto_V_V_Complex ()
  implicit def canDivInto_V_V_Complex_def[A <: HashVector[Complex], B <: HashVector[Complex]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv] = (
    canDivInto_V_V_Complex.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv]]
  )

  implicit val canDiv_V_V_Complex: BinaryRegistry[HashVector[Complex], HashVector[Complex], OpDiv, HashVector[Complex]] = pureFromUpdate_Complex(canDivInto_V_V_Complex)


  class canDivInto_V_S_Complex private[linalg] () extends BinaryUpdateRegistry[HashVector[Complex], Complex, breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: HashVector[Complex], b: Complex) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v / b
          }
        
    }
  }
  val canDivInto_V_S_Complex = new canDivInto_V_S_Complex ()
  implicit def canDivInto_V_S_Complex_def[A <: HashVector[Complex], B <: Complex]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv] = (
    canDivInto_V_S_Complex.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv]]
  )

  implicit val canDiv_V_S_Complex: BinaryRegistry[HashVector[Complex], Complex, OpDiv, HashVector[Complex]] = pureFromUpdate_Complex(canDivInto_V_S_Complex)


  class canMulMatrixInto_V_S_Complex private[linalg] () extends BinaryUpdateRegistry[HashVector[Complex], Complex, breeze.linalg.operators.OpMulMatrix] {
    override def bindingMissing(a: HashVector[Complex], b: Complex) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v * b
          }
        
    }
  }
  val canMulMatrixInto_V_S_Complex = new canMulMatrixInto_V_S_Complex ()
  implicit def canMulMatrixInto_V_S_Complex_def[A <: HashVector[Complex], B <: Complex]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulMatrix] = (
    canMulMatrixInto_V_S_Complex.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulMatrix]]
  )

  implicit val canMulMatrix_V_S_Complex: BinaryRegistry[HashVector[Complex], Complex, OpMulMatrix, HashVector[Complex]] = pureFromUpdate_Complex(canMulMatrixInto_V_S_Complex)


  class canSetInto_V_V_Complex private[linalg] () extends BinaryUpdateRegistry[HashVector[Complex], HashVector[Complex], breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: HashVector[Complex], b: HashVector[Complex]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = v
        }
        
    }
  }
  val canSetInto_V_V_Complex = new canSetInto_V_V_Complex ()
  implicit def canSetInto_V_V_Complex_def[A <: HashVector[Complex], B <: HashVector[Complex]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet] = (
    canSetInto_V_V_Complex.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet]]
  )

  implicit val canSet_V_V_Complex: BinaryRegistry[HashVector[Complex], HashVector[Complex], OpSet, HashVector[Complex]] = pureFromUpdate_Complex(canSetInto_V_V_Complex)


  class canSetInto_V_S_Complex private[linalg] () extends BinaryUpdateRegistry[HashVector[Complex], Complex, breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: HashVector[Complex], b: Complex) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = b
          }
        
    }
  }
  val canSetInto_V_S_Complex = new canSetInto_V_S_Complex ()
  implicit def canSetInto_V_S_Complex_def[A <: HashVector[Complex], B <: Complex]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet] = (
    canSetInto_V_S_Complex.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet]]
  )

  implicit val canSet_V_S_Complex: BinaryRegistry[HashVector[Complex], Complex, OpSet, HashVector[Complex]] = pureFromUpdate_Complex(canSetInto_V_S_Complex)


  class canAxpy_SV_SV_Complex private[linalg] () extends CanAxpy[Complex, HashVector[Complex], HashVector[Complex]] {
    def apply(s: Complex, b: HashVector[Complex], a: HashVector[Complex]) {
      if(s == 0) return;
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i, v) <- b.activeIterator) {
          a(i) += v * s
        }
        
    }
  }
  implicit val canAxpy_SV_SV_Complex = new canAxpy_SV_SV_Complex ()
    

  class canDotProductV_Complex private[linalg] () extends BinaryRegistry[HashVector[Complex], HashVector[Complex], breeze.linalg.operators.OpMulInner, Complex] {
    override def bindingMissing(a: HashVector[Complex], b: HashVector[Complex]) = {
      require(b.length == a.length, "Vectors must be the same length!")

       var result: Complex = Complex(0, 0)

         for( (i, v) <- b.activeIterator) {
           result += a(i) * v
         }
         result
    }
  };
  val canDotProductV_Complex = new canDotProductV_Complex()
  implicit def canDotProductV_Complex_def[A <: HashVector[Complex], B <: HashVector[Complex]]:BinaryOp[A, B, breeze.linalg.operators.OpMulInner, Complex] = (
    canDotProductV_Complex.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulInner, Complex]]
  )
    
}
