package breeze.linalg
import breeze.linalg.operators._
import breeze.linalg.support._
import breeze.numerics._
/** This is an auto-generated trait providing operators for Vector. */
trait VectorOps_Double { this: Vector.type =>

  def pureFromUpdate_Double[Other,Op<:OpType](op: BinaryUpdateOp[Vector[Double], Other, Op])(implicit copy: CanCopy[Vector[Double]]):BinaryRegistry[Vector[Double], Other, Op, Vector[Double]] = {
    new BinaryRegistry[Vector[Double], Other, Op, Vector[Double]] {
      override def bindingMissing(a : Vector[Double], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }
        

  class canAddInto_V_V_Double private[linalg] () extends BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: Vector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) + v
        }
        
    }
  }
  val canAddInto_V_V_Double = new canAddInto_V_V_Double ()
  implicit def canAddInto_V_V_Double_def[A <: Vector[Double], B <: Vector[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd] = (
    canAddInto_V_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd]]
  )

  implicit val canAdd_V_V_Double: BinaryRegistry[Vector[Double], Vector[Double], OpAdd, Vector[Double]] = pureFromUpdate_Double(canAddInto_V_V_Double)


  class canAddInto_V_S_Double private[linalg] () extends BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: Vector[Double], b: Double) {
      
        if(!true || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v + b
          }
        
    }
  }
  val canAddInto_V_S_Double = new canAddInto_V_S_Double ()
  implicit def canAddInto_V_S_Double_def[A <: Vector[Double], B <: Double]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd] = (
    canAddInto_V_S_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd]]
  )

  implicit val canAdd_V_S_Double: BinaryRegistry[Vector[Double], Double, OpAdd, Vector[Double]] = pureFromUpdate_Double(canAddInto_V_S_Double)


  class canSubInto_V_V_Double private[linalg] () extends BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: Vector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) - v
        }
        
    }
  }
  val canSubInto_V_V_Double = new canSubInto_V_V_Double ()
  implicit def canSubInto_V_V_Double_def[A <: Vector[Double], B <: Vector[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub] = (
    canSubInto_V_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub]]
  )

  implicit val canSub_V_V_Double: BinaryRegistry[Vector[Double], Vector[Double], OpSub, Vector[Double]] = pureFromUpdate_Double(canSubInto_V_V_Double)


  class canSubInto_V_S_Double private[linalg] () extends BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: Vector[Double], b: Double) {
      
        if(!true || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v - b
          }
        
    }
  }
  val canSubInto_V_S_Double = new canSubInto_V_S_Double ()
  implicit def canSubInto_V_S_Double_def[A <: Vector[Double], B <: Double]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub] = (
    canSubInto_V_S_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub]]
  )

  implicit val canSub_V_S_Double: BinaryRegistry[Vector[Double], Double, OpSub, Vector[Double]] = pureFromUpdate_Double(canSubInto_V_S_Double)


  class canMulMatrixInto_V_S_Double private[linalg] () extends BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpMulMatrix] {
    override def bindingMissing(a: Vector[Double], b: Double) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v * b
          }
        
    }
  }
  val canMulMatrixInto_V_S_Double = new canMulMatrixInto_V_S_Double ()
  implicit def canMulMatrixInto_V_S_Double_def[A <: Vector[Double], B <: Double]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulMatrix] = (
    canMulMatrixInto_V_S_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulMatrix]]
  )

  implicit val canMulMatrix_V_S_Double: BinaryRegistry[Vector[Double], Double, OpMulMatrix, Vector[Double]] = pureFromUpdate_Double(canMulMatrixInto_V_S_Double)


  class canSetInto_V_V_Double private[linalg] () extends BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: Vector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = v
        }
        
    }
  }
  val canSetInto_V_V_Double = new canSetInto_V_V_Double ()
  implicit def canSetInto_V_V_Double_def[A <: Vector[Double], B <: Vector[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet] = (
    canSetInto_V_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet]]
  )

  implicit val canSet_V_V_Double: BinaryRegistry[Vector[Double], Vector[Double], OpSet, Vector[Double]] = pureFromUpdate_Double(canSetInto_V_V_Double)


  class canSetInto_V_S_Double private[linalg] () extends BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: Vector[Double], b: Double) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = b
          }
        
    }
  }
  val canSetInto_V_S_Double = new canSetInto_V_S_Double ()
  implicit def canSetInto_V_S_Double_def[A <: Vector[Double], B <: Double]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet] = (
    canSetInto_V_S_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet]]
  )

  implicit val canSet_V_S_Double: BinaryRegistry[Vector[Double], Double, OpSet, Vector[Double]] = pureFromUpdate_Double(canSetInto_V_S_Double)


  class canDivInto_V_V_Double private[linalg] () extends BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: Vector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) / v
        }
        
    }
  }
  val canDivInto_V_V_Double = new canDivInto_V_V_Double ()
  implicit def canDivInto_V_V_Double_def[A <: Vector[Double], B <: Vector[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv] = (
    canDivInto_V_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv]]
  )

  implicit val canDiv_V_V_Double: BinaryRegistry[Vector[Double], Vector[Double], OpDiv, Vector[Double]] = pureFromUpdate_Double(canDivInto_V_V_Double)


  class canDivInto_V_S_Double private[linalg] () extends BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: Vector[Double], b: Double) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v / b
          }
        
    }
  }
  val canDivInto_V_S_Double = new canDivInto_V_S_Double ()
  implicit def canDivInto_V_S_Double_def[A <: Vector[Double], B <: Double]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv] = (
    canDivInto_V_S_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv]]
  )

  implicit val canDiv_V_S_Double: BinaryRegistry[Vector[Double], Double, OpDiv, Vector[Double]] = pureFromUpdate_Double(canDivInto_V_S_Double)


  class canMulScalarInto_V_V_Double private[linalg] () extends BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: Vector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) * v
        }
        
    }
  }
  val canMulScalarInto_V_V_Double = new canMulScalarInto_V_V_Double ()
  implicit def canMulScalarInto_V_V_Double_def[A <: Vector[Double], B <: Vector[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar] = (
    canMulScalarInto_V_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar]]
  )

  implicit val canMulScalar_V_V_Double: BinaryRegistry[Vector[Double], Vector[Double], OpMulScalar, Vector[Double]] = pureFromUpdate_Double(canMulScalarInto_V_V_Double)


  class canMulScalarInto_V_S_Double private[linalg] () extends BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: Vector[Double], b: Double) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v * b
          }
        
    }
  }
  val canMulScalarInto_V_S_Double = new canMulScalarInto_V_S_Double ()
  implicit def canMulScalarInto_V_S_Double_def[A <: Vector[Double], B <: Double]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar] = (
    canMulScalarInto_V_S_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar]]
  )

  implicit val canMulScalar_V_S_Double: BinaryRegistry[Vector[Double], Double, OpMulScalar, Vector[Double]] = pureFromUpdate_Double(canMulScalarInto_V_S_Double)


  class canPowInto_V_V_Double private[linalg] () extends BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: Vector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = scala.math.pow(a(i), v)
        }
        
    }
  }
  val canPowInto_V_V_Double = new canPowInto_V_V_Double ()
  implicit def canPowInto_V_V_Double_def[A <: Vector[Double], B <: Vector[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow] = (
    canPowInto_V_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow]]
  )

  implicit val canPow_V_V_Double: BinaryRegistry[Vector[Double], Vector[Double], OpPow, Vector[Double]] = pureFromUpdate_Double(canPowInto_V_V_Double)


  class canPowInto_V_S_Double private[linalg] () extends BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: Vector[Double], b: Double) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = scala.math.pow(v, b)
          }
        
    }
  }
  val canPowInto_V_S_Double = new canPowInto_V_S_Double ()
  implicit def canPowInto_V_S_Double_def[A <: Vector[Double], B <: Double]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow] = (
    canPowInto_V_S_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow]]
  )

  implicit val canPow_V_S_Double: BinaryRegistry[Vector[Double], Double, OpPow, Vector[Double]] = pureFromUpdate_Double(canPowInto_V_S_Double)


  class canModInto_V_V_Double private[linalg] () extends BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpMod] {
    override def bindingMissing(a: Vector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) % v
        }
        
    }
  }
  val canModInto_V_V_Double = new canModInto_V_V_Double ()
  implicit def canModInto_V_V_Double_def[A <: Vector[Double], B <: Vector[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod] = (
    canModInto_V_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod]]
  )

  implicit val canMod_V_V_Double: BinaryRegistry[Vector[Double], Vector[Double], OpMod, Vector[Double]] = pureFromUpdate_Double(canModInto_V_V_Double)


  class canModInto_V_S_Double private[linalg] () extends BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpMod] {
    override def bindingMissing(a: Vector[Double], b: Double) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v % b
          }
        
    }
  }
  val canModInto_V_S_Double = new canModInto_V_S_Double ()
  implicit def canModInto_V_S_Double_def[A <: Vector[Double], B <: Double]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod] = (
    canModInto_V_S_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod]]
  )

  implicit val canMod_V_S_Double: BinaryRegistry[Vector[Double], Double, OpMod, Vector[Double]] = pureFromUpdate_Double(canModInto_V_S_Double)


  class canDotProductV_Double private[linalg] () extends BinaryRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpMulInner, Double] {
    override def bindingMissing(a: Vector[Double], b: Vector[Double]) = {
      require(b.length == a.length, "Vectors must be the same length!")

       var result: Double = 0

         for( (i, v) <- b.activeIterator) {
           result += a(i) * v
         }
         result
    }
  };
  val canDotProductV_Double = new canDotProductV_Double()
  implicit def canDotProductV_Double_def[A <: Vector[Double], B <: Vector[Double]]:BinaryOp[A, B, breeze.linalg.operators.OpMulInner, Double] = (
    canDotProductV_Double.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulInner, Double]]
  )
    
}
/** This is an auto-generated trait providing operators for Vector. */
trait VectorOps_Float { this: Vector.type =>

  def pureFromUpdate_Float[Other,Op<:OpType](op: BinaryUpdateOp[Vector[Float], Other, Op])(implicit copy: CanCopy[Vector[Float]]):BinaryRegistry[Vector[Float], Other, Op, Vector[Float]] = {
    new BinaryRegistry[Vector[Float], Other, Op, Vector[Float]] {
      override def bindingMissing(a : Vector[Float], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }
        

  class canAddInto_V_V_Float private[linalg] () extends BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: Vector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) + v
        }
        
    }
  }
  val canAddInto_V_V_Float = new canAddInto_V_V_Float ()
  implicit def canAddInto_V_V_Float_def[A <: Vector[Float], B <: Vector[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd] = (
    canAddInto_V_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd]]
  )

  implicit val canAdd_V_V_Float: BinaryRegistry[Vector[Float], Vector[Float], OpAdd, Vector[Float]] = pureFromUpdate_Float(canAddInto_V_V_Float)


  class canAddInto_V_S_Float private[linalg] () extends BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: Vector[Float], b: Float) {
      
        if(!true || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v + b
          }
        
    }
  }
  val canAddInto_V_S_Float = new canAddInto_V_S_Float ()
  implicit def canAddInto_V_S_Float_def[A <: Vector[Float], B <: Float]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd] = (
    canAddInto_V_S_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd]]
  )

  implicit val canAdd_V_S_Float: BinaryRegistry[Vector[Float], Float, OpAdd, Vector[Float]] = pureFromUpdate_Float(canAddInto_V_S_Float)


  class canSubInto_V_V_Float private[linalg] () extends BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: Vector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) - v
        }
        
    }
  }
  val canSubInto_V_V_Float = new canSubInto_V_V_Float ()
  implicit def canSubInto_V_V_Float_def[A <: Vector[Float], B <: Vector[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub] = (
    canSubInto_V_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub]]
  )

  implicit val canSub_V_V_Float: BinaryRegistry[Vector[Float], Vector[Float], OpSub, Vector[Float]] = pureFromUpdate_Float(canSubInto_V_V_Float)


  class canSubInto_V_S_Float private[linalg] () extends BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: Vector[Float], b: Float) {
      
        if(!true || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v - b
          }
        
    }
  }
  val canSubInto_V_S_Float = new canSubInto_V_S_Float ()
  implicit def canSubInto_V_S_Float_def[A <: Vector[Float], B <: Float]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub] = (
    canSubInto_V_S_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub]]
  )

  implicit val canSub_V_S_Float: BinaryRegistry[Vector[Float], Float, OpSub, Vector[Float]] = pureFromUpdate_Float(canSubInto_V_S_Float)


  class canMulMatrixInto_V_S_Float private[linalg] () extends BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpMulMatrix] {
    override def bindingMissing(a: Vector[Float], b: Float) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v * b
          }
        
    }
  }
  val canMulMatrixInto_V_S_Float = new canMulMatrixInto_V_S_Float ()
  implicit def canMulMatrixInto_V_S_Float_def[A <: Vector[Float], B <: Float]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulMatrix] = (
    canMulMatrixInto_V_S_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulMatrix]]
  )

  implicit val canMulMatrix_V_S_Float: BinaryRegistry[Vector[Float], Float, OpMulMatrix, Vector[Float]] = pureFromUpdate_Float(canMulMatrixInto_V_S_Float)


  class canSetInto_V_V_Float private[linalg] () extends BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: Vector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = v
        }
        
    }
  }
  val canSetInto_V_V_Float = new canSetInto_V_V_Float ()
  implicit def canSetInto_V_V_Float_def[A <: Vector[Float], B <: Vector[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet] = (
    canSetInto_V_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet]]
  )

  implicit val canSet_V_V_Float: BinaryRegistry[Vector[Float], Vector[Float], OpSet, Vector[Float]] = pureFromUpdate_Float(canSetInto_V_V_Float)


  class canSetInto_V_S_Float private[linalg] () extends BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: Vector[Float], b: Float) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = b
          }
        
    }
  }
  val canSetInto_V_S_Float = new canSetInto_V_S_Float ()
  implicit def canSetInto_V_S_Float_def[A <: Vector[Float], B <: Float]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet] = (
    canSetInto_V_S_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet]]
  )

  implicit val canSet_V_S_Float: BinaryRegistry[Vector[Float], Float, OpSet, Vector[Float]] = pureFromUpdate_Float(canSetInto_V_S_Float)


  class canDivInto_V_V_Float private[linalg] () extends BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: Vector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) / v
        }
        
    }
  }
  val canDivInto_V_V_Float = new canDivInto_V_V_Float ()
  implicit def canDivInto_V_V_Float_def[A <: Vector[Float], B <: Vector[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv] = (
    canDivInto_V_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv]]
  )

  implicit val canDiv_V_V_Float: BinaryRegistry[Vector[Float], Vector[Float], OpDiv, Vector[Float]] = pureFromUpdate_Float(canDivInto_V_V_Float)


  class canDivInto_V_S_Float private[linalg] () extends BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: Vector[Float], b: Float) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v / b
          }
        
    }
  }
  val canDivInto_V_S_Float = new canDivInto_V_S_Float ()
  implicit def canDivInto_V_S_Float_def[A <: Vector[Float], B <: Float]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv] = (
    canDivInto_V_S_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv]]
  )

  implicit val canDiv_V_S_Float: BinaryRegistry[Vector[Float], Float, OpDiv, Vector[Float]] = pureFromUpdate_Float(canDivInto_V_S_Float)


  class canMulScalarInto_V_V_Float private[linalg] () extends BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: Vector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) * v
        }
        
    }
  }
  val canMulScalarInto_V_V_Float = new canMulScalarInto_V_V_Float ()
  implicit def canMulScalarInto_V_V_Float_def[A <: Vector[Float], B <: Vector[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar] = (
    canMulScalarInto_V_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar]]
  )

  implicit val canMulScalar_V_V_Float: BinaryRegistry[Vector[Float], Vector[Float], OpMulScalar, Vector[Float]] = pureFromUpdate_Float(canMulScalarInto_V_V_Float)


  class canMulScalarInto_V_S_Float private[linalg] () extends BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: Vector[Float], b: Float) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v * b
          }
        
    }
  }
  val canMulScalarInto_V_S_Float = new canMulScalarInto_V_S_Float ()
  implicit def canMulScalarInto_V_S_Float_def[A <: Vector[Float], B <: Float]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar] = (
    canMulScalarInto_V_S_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar]]
  )

  implicit val canMulScalar_V_S_Float: BinaryRegistry[Vector[Float], Float, OpMulScalar, Vector[Float]] = pureFromUpdate_Float(canMulScalarInto_V_S_Float)


  class canPowInto_V_V_Float private[linalg] () extends BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: Vector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = scala.math.pow(a(i), v).toFloat
        }
        
    }
  }
  val canPowInto_V_V_Float = new canPowInto_V_V_Float ()
  implicit def canPowInto_V_V_Float_def[A <: Vector[Float], B <: Vector[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow] = (
    canPowInto_V_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow]]
  )

  implicit val canPow_V_V_Float: BinaryRegistry[Vector[Float], Vector[Float], OpPow, Vector[Float]] = pureFromUpdate_Float(canPowInto_V_V_Float)


  class canPowInto_V_S_Float private[linalg] () extends BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: Vector[Float], b: Float) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = scala.math.pow(v, b).toFloat
          }
        
    }
  }
  val canPowInto_V_S_Float = new canPowInto_V_S_Float ()
  implicit def canPowInto_V_S_Float_def[A <: Vector[Float], B <: Float]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow] = (
    canPowInto_V_S_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow]]
  )

  implicit val canPow_V_S_Float: BinaryRegistry[Vector[Float], Float, OpPow, Vector[Float]] = pureFromUpdate_Float(canPowInto_V_S_Float)


  class canModInto_V_V_Float private[linalg] () extends BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpMod] {
    override def bindingMissing(a: Vector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) % v
        }
        
    }
  }
  val canModInto_V_V_Float = new canModInto_V_V_Float ()
  implicit def canModInto_V_V_Float_def[A <: Vector[Float], B <: Vector[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod] = (
    canModInto_V_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod]]
  )

  implicit val canMod_V_V_Float: BinaryRegistry[Vector[Float], Vector[Float], OpMod, Vector[Float]] = pureFromUpdate_Float(canModInto_V_V_Float)


  class canModInto_V_S_Float private[linalg] () extends BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpMod] {
    override def bindingMissing(a: Vector[Float], b: Float) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v % b
          }
        
    }
  }
  val canModInto_V_S_Float = new canModInto_V_S_Float ()
  implicit def canModInto_V_S_Float_def[A <: Vector[Float], B <: Float]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod] = (
    canModInto_V_S_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod]]
  )

  implicit val canMod_V_S_Float: BinaryRegistry[Vector[Float], Float, OpMod, Vector[Float]] = pureFromUpdate_Float(canModInto_V_S_Float)


  class canDotProductV_Float private[linalg] () extends BinaryRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpMulInner, Float] {
    override def bindingMissing(a: Vector[Float], b: Vector[Float]) = {
      require(b.length == a.length, "Vectors must be the same length!")

       var result: Float = 0

         for( (i, v) <- b.activeIterator) {
           result += a(i) * v
         }
         result
    }
  };
  val canDotProductV_Float = new canDotProductV_Float()
  implicit def canDotProductV_Float_def[A <: Vector[Float], B <: Vector[Float]]:BinaryOp[A, B, breeze.linalg.operators.OpMulInner, Float] = (
    canDotProductV_Float.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulInner, Float]]
  )
    
}
/** This is an auto-generated trait providing operators for Vector. */
trait VectorOps_Int { this: Vector.type =>

  def pureFromUpdate_Int[Other,Op<:OpType](op: BinaryUpdateOp[Vector[Int], Other, Op])(implicit copy: CanCopy[Vector[Int]]):BinaryRegistry[Vector[Int], Other, Op, Vector[Int]] = {
    new BinaryRegistry[Vector[Int], Other, Op, Vector[Int]] {
      override def bindingMissing(a : Vector[Int], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }
        

  class canAddInto_V_V_Int private[linalg] () extends BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: Vector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) + v
        }
        
    }
  }
  val canAddInto_V_V_Int = new canAddInto_V_V_Int ()
  implicit def canAddInto_V_V_Int_def[A <: Vector[Int], B <: Vector[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd] = (
    canAddInto_V_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd]]
  )

  implicit val canAdd_V_V_Int: BinaryRegistry[Vector[Int], Vector[Int], OpAdd, Vector[Int]] = pureFromUpdate_Int(canAddInto_V_V_Int)


  class canAddInto_V_S_Int private[linalg] () extends BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: Vector[Int], b: Int) {
      
        if(!true || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v + b
          }
        
    }
  }
  val canAddInto_V_S_Int = new canAddInto_V_S_Int ()
  implicit def canAddInto_V_S_Int_def[A <: Vector[Int], B <: Int]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd] = (
    canAddInto_V_S_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd]]
  )

  implicit val canAdd_V_S_Int: BinaryRegistry[Vector[Int], Int, OpAdd, Vector[Int]] = pureFromUpdate_Int(canAddInto_V_S_Int)


  class canSubInto_V_V_Int private[linalg] () extends BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: Vector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) - v
        }
        
    }
  }
  val canSubInto_V_V_Int = new canSubInto_V_V_Int ()
  implicit def canSubInto_V_V_Int_def[A <: Vector[Int], B <: Vector[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub] = (
    canSubInto_V_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub]]
  )

  implicit val canSub_V_V_Int: BinaryRegistry[Vector[Int], Vector[Int], OpSub, Vector[Int]] = pureFromUpdate_Int(canSubInto_V_V_Int)


  class canSubInto_V_S_Int private[linalg] () extends BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: Vector[Int], b: Int) {
      
        if(!true || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v - b
          }
        
    }
  }
  val canSubInto_V_S_Int = new canSubInto_V_S_Int ()
  implicit def canSubInto_V_S_Int_def[A <: Vector[Int], B <: Int]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub] = (
    canSubInto_V_S_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub]]
  )

  implicit val canSub_V_S_Int: BinaryRegistry[Vector[Int], Int, OpSub, Vector[Int]] = pureFromUpdate_Int(canSubInto_V_S_Int)


  class canMulMatrixInto_V_S_Int private[linalg] () extends BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpMulMatrix] {
    override def bindingMissing(a: Vector[Int], b: Int) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v * b
          }
        
    }
  }
  val canMulMatrixInto_V_S_Int = new canMulMatrixInto_V_S_Int ()
  implicit def canMulMatrixInto_V_S_Int_def[A <: Vector[Int], B <: Int]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulMatrix] = (
    canMulMatrixInto_V_S_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulMatrix]]
  )

  implicit val canMulMatrix_V_S_Int: BinaryRegistry[Vector[Int], Int, OpMulMatrix, Vector[Int]] = pureFromUpdate_Int(canMulMatrixInto_V_S_Int)


  class canSetInto_V_V_Int private[linalg] () extends BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: Vector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = v
        }
        
    }
  }
  val canSetInto_V_V_Int = new canSetInto_V_V_Int ()
  implicit def canSetInto_V_V_Int_def[A <: Vector[Int], B <: Vector[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet] = (
    canSetInto_V_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet]]
  )

  implicit val canSet_V_V_Int: BinaryRegistry[Vector[Int], Vector[Int], OpSet, Vector[Int]] = pureFromUpdate_Int(canSetInto_V_V_Int)


  class canSetInto_V_S_Int private[linalg] () extends BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: Vector[Int], b: Int) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = b
          }
        
    }
  }
  val canSetInto_V_S_Int = new canSetInto_V_S_Int ()
  implicit def canSetInto_V_S_Int_def[A <: Vector[Int], B <: Int]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet] = (
    canSetInto_V_S_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet]]
  )

  implicit val canSet_V_S_Int: BinaryRegistry[Vector[Int], Int, OpSet, Vector[Int]] = pureFromUpdate_Int(canSetInto_V_S_Int)


  class canDivInto_V_V_Int private[linalg] () extends BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: Vector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) / v
        }
        
    }
  }
  val canDivInto_V_V_Int = new canDivInto_V_V_Int ()
  implicit def canDivInto_V_V_Int_def[A <: Vector[Int], B <: Vector[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv] = (
    canDivInto_V_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv]]
  )

  implicit val canDiv_V_V_Int: BinaryRegistry[Vector[Int], Vector[Int], OpDiv, Vector[Int]] = pureFromUpdate_Int(canDivInto_V_V_Int)


  class canDivInto_V_S_Int private[linalg] () extends BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: Vector[Int], b: Int) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v / b
          }
        
    }
  }
  val canDivInto_V_S_Int = new canDivInto_V_S_Int ()
  implicit def canDivInto_V_S_Int_def[A <: Vector[Int], B <: Int]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv] = (
    canDivInto_V_S_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv]]
  )

  implicit val canDiv_V_S_Int: BinaryRegistry[Vector[Int], Int, OpDiv, Vector[Int]] = pureFromUpdate_Int(canDivInto_V_S_Int)


  class canMulScalarInto_V_V_Int private[linalg] () extends BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: Vector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) * v
        }
        
    }
  }
  val canMulScalarInto_V_V_Int = new canMulScalarInto_V_V_Int ()
  implicit def canMulScalarInto_V_V_Int_def[A <: Vector[Int], B <: Vector[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar] = (
    canMulScalarInto_V_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar]]
  )

  implicit val canMulScalar_V_V_Int: BinaryRegistry[Vector[Int], Vector[Int], OpMulScalar, Vector[Int]] = pureFromUpdate_Int(canMulScalarInto_V_V_Int)


  class canMulScalarInto_V_S_Int private[linalg] () extends BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: Vector[Int], b: Int) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v * b
          }
        
    }
  }
  val canMulScalarInto_V_S_Int = new canMulScalarInto_V_S_Int ()
  implicit def canMulScalarInto_V_S_Int_def[A <: Vector[Int], B <: Int]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar] = (
    canMulScalarInto_V_S_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar]]
  )

  implicit val canMulScalar_V_S_Int: BinaryRegistry[Vector[Int], Int, OpMulScalar, Vector[Int]] = pureFromUpdate_Int(canMulScalarInto_V_S_Int)


  class canPowInto_V_V_Int private[linalg] () extends BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: Vector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = IntMath.ipow(a(i), v)
        }
        
    }
  }
  val canPowInto_V_V_Int = new canPowInto_V_V_Int ()
  implicit def canPowInto_V_V_Int_def[A <: Vector[Int], B <: Vector[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow] = (
    canPowInto_V_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow]]
  )

  implicit val canPow_V_V_Int: BinaryRegistry[Vector[Int], Vector[Int], OpPow, Vector[Int]] = pureFromUpdate_Int(canPowInto_V_V_Int)


  class canPowInto_V_S_Int private[linalg] () extends BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: Vector[Int], b: Int) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = IntMath.ipow(v, b)
          }
        
    }
  }
  val canPowInto_V_S_Int = new canPowInto_V_S_Int ()
  implicit def canPowInto_V_S_Int_def[A <: Vector[Int], B <: Int]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow] = (
    canPowInto_V_S_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow]]
  )

  implicit val canPow_V_S_Int: BinaryRegistry[Vector[Int], Int, OpPow, Vector[Int]] = pureFromUpdate_Int(canPowInto_V_S_Int)


  class canModInto_V_V_Int private[linalg] () extends BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpMod] {
    override def bindingMissing(a: Vector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) % v
        }
        
    }
  }
  val canModInto_V_V_Int = new canModInto_V_V_Int ()
  implicit def canModInto_V_V_Int_def[A <: Vector[Int], B <: Vector[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod] = (
    canModInto_V_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod]]
  )

  implicit val canMod_V_V_Int: BinaryRegistry[Vector[Int], Vector[Int], OpMod, Vector[Int]] = pureFromUpdate_Int(canModInto_V_V_Int)


  class canModInto_V_S_Int private[linalg] () extends BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpMod] {
    override def bindingMissing(a: Vector[Int], b: Int) {
      
        if(!false || b != 0)
          for( (i,v) <- a.iterator) {
                a(i) = v % b
          }
        
    }
  }
  val canModInto_V_S_Int = new canModInto_V_S_Int ()
  implicit def canModInto_V_S_Int_def[A <: Vector[Int], B <: Int]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod] = (
    canModInto_V_S_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod]]
  )

  implicit val canMod_V_S_Int: BinaryRegistry[Vector[Int], Int, OpMod, Vector[Int]] = pureFromUpdate_Int(canModInto_V_S_Int)


  class canDotProductV_Int private[linalg] () extends BinaryRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpMulInner, Int] {
    override def bindingMissing(a: Vector[Int], b: Vector[Int]) = {
      require(b.length == a.length, "Vectors must be the same length!")

       var result: Int = 0

         for( (i, v) <- b.activeIterator) {
           result += a(i) * v
         }
         result
    }
  };
  val canDotProductV_Int = new canDotProductV_Int()
  implicit def canDotProductV_Int_def[A <: Vector[Int], B <: Vector[Int]]:BinaryOp[A, B, breeze.linalg.operators.OpMulInner, Int] = (
    canDotProductV_Int.asInstanceOf[BinaryOp[A, B, breeze.linalg.operators.OpMulInner, Int]]
  )
    
}
