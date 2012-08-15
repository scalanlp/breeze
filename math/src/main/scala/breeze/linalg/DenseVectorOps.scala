package breeze.linalg
import breeze.linalg.operators._
import breeze.linalg.support._
import breeze.numerics._
/** This is an auto-generated trait providing operators for DenseVector. */
trait DenseVectorOps_Double extends DenseVectorOps_Double_Generic { this: DenseVector.type =>

       def pureFromUpdate_Double[Other,Op<:OpType](op: BinaryUpdateOp[DenseVector[Double], Other, Op])(implicit copy: CanCopy[DenseVector[Double]]):BinaryOp[DenseVector[Double], Other, Op, DenseVector[Double]] = {
         new BinaryOp[DenseVector[Double], Other, Op, DenseVector[Double]] {
           override def apply(a : DenseVector[Double], b : Other) = {
             val c = copy(a)
             op(c, b)
             c
           }
         }
       }

  class canMulScalarInto_DV_DV_Double private[linalg] () extends BinaryUpdateOp[DenseVector[Double], DenseVector[Double], breeze.linalg.operators.OpMulScalar] {
    def apply(a: DenseVector[Double], b: DenseVector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset

        var i = 0
        while(i < a.length) {
          ad(aoff) = ad(aoff) * bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
        
    }
  }
  implicit val canMulScalarInto_DV_DV_Double = new canMulScalarInto_DV_DV_Double ()
    
  Vector.canMulScalarInto_V_V_Double.register(canMulScalarInto_DV_DV_Double)

  implicit val canMulScalar_DV_DV_Double: BinaryOp[DenseVector[Double], DenseVector[Double], breeze.linalg.operators.OpMulScalar, DenseVector[Double]] = pureFromUpdate_Double(canMulScalarInto_DV_DV_Double)
  Vector.canMulScalar_V_V_Double.register(canMulScalar_DV_DV_Double)


  class canModInto_DV_DV_Double private[linalg] () extends BinaryUpdateOp[DenseVector[Double], DenseVector[Double], breeze.linalg.operators.OpMod] {
    def apply(a: DenseVector[Double], b: DenseVector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset

        var i = 0
        while(i < a.length) {
          ad(aoff) = ad(aoff) % bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
        
    }
  }
  implicit val canModInto_DV_DV_Double = new canModInto_DV_DV_Double ()
    
  Vector.canModInto_V_V_Double.register(canModInto_DV_DV_Double)

  implicit val canMod_DV_DV_Double: BinaryOp[DenseVector[Double], DenseVector[Double], breeze.linalg.operators.OpMod, DenseVector[Double]] = pureFromUpdate_Double(canModInto_DV_DV_Double)
  Vector.canMod_V_V_Double.register(canMod_DV_DV_Double)


  class canModInto_DV_S_Double private[linalg] () extends BinaryUpdateOp[DenseVector[Double], Double, breeze.linalg.operators.OpMod] {
    def apply(a: DenseVector[Double], b: Double) {
      val ad = a.data

        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = ad(aoff) % b
          aoff += a.stride
          i += 1
        }
        
    }
  }
  implicit val canModInto_DV_S_Double = new canModInto_DV_S_Double ()
    
  Vector.canModInto_V_S_Double.register(canModInto_DV_S_Double)

  implicit val canMod_DV_S_Double: BinaryOp[DenseVector[Double], Double, breeze.linalg.operators.OpMod, DenseVector[Double]] = pureFromUpdate_Double(canModInto_DV_S_Double)
  Vector.canMod_V_S_Double.register(canMod_DV_S_Double)


  class canSetInto_DV_S_Double private[linalg] () extends BinaryUpdateOp[DenseVector[Double], Double, breeze.linalg.operators.OpSet] {
    def apply(a: DenseVector[Double], b: Double) {
      val ad = a.data

        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = b
          aoff += a.stride
          i += 1
        }
        
    }
  }
  implicit val canSetInto_DV_S_Double = new canSetInto_DV_S_Double ()
    
  Vector.canSetInto_V_S_Double.register(canSetInto_DV_S_Double)

  implicit val canSet_DV_S_Double: BinaryOp[DenseVector[Double], Double, breeze.linalg.operators.OpSet, DenseVector[Double]] = pureFromUpdate_Double(canSetInto_DV_S_Double)
  Vector.canSet_V_S_Double.register(canSet_DV_S_Double)


  class canSubInto_DV_S_Double private[linalg] () extends BinaryUpdateOp[DenseVector[Double], Double, breeze.linalg.operators.OpSub] {
    def apply(a: DenseVector[Double], b: Double) {
      val ad = a.data

        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = ad(aoff) - b
          aoff += a.stride
          i += 1
        }
        
    }
  }
  implicit val canSubInto_DV_S_Double = new canSubInto_DV_S_Double ()
    
  Vector.canSubInto_V_S_Double.register(canSubInto_DV_S_Double)

  implicit val canSub_DV_S_Double: BinaryOp[DenseVector[Double], Double, breeze.linalg.operators.OpSub, DenseVector[Double]] = pureFromUpdate_Double(canSubInto_DV_S_Double)
  Vector.canSub_V_S_Double.register(canSub_DV_S_Double)


  class canMulMatrixInto_DV_S_Double private[linalg] () extends BinaryUpdateOp[DenseVector[Double], Double, breeze.linalg.operators.OpMulMatrix] {
    def apply(a: DenseVector[Double], b: Double) {
      val ad = a.data

        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = ad(aoff) * b
          aoff += a.stride
          i += 1
        }
        
    }
  }
  implicit val canMulMatrixInto_DV_S_Double = new canMulMatrixInto_DV_S_Double ()
    
  Vector.canMulMatrixInto_V_S_Double.register(canMulMatrixInto_DV_S_Double)

  implicit val canMulMatrix_DV_S_Double: BinaryOp[DenseVector[Double], Double, breeze.linalg.operators.OpMulMatrix, DenseVector[Double]] = pureFromUpdate_Double(canMulMatrixInto_DV_S_Double)
  Vector.canMulMatrix_V_S_Double.register(canMulMatrix_DV_S_Double)


  class canDivInto_DV_DV_Double private[linalg] () extends BinaryUpdateOp[DenseVector[Double], DenseVector[Double], breeze.linalg.operators.OpDiv] {
    def apply(a: DenseVector[Double], b: DenseVector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset

        var i = 0
        while(i < a.length) {
          ad(aoff) = ad(aoff) / bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
        
    }
  }
  implicit val canDivInto_DV_DV_Double = new canDivInto_DV_DV_Double ()
    
  Vector.canDivInto_V_V_Double.register(canDivInto_DV_DV_Double)

  implicit val canDiv_DV_DV_Double: BinaryOp[DenseVector[Double], DenseVector[Double], breeze.linalg.operators.OpDiv, DenseVector[Double]] = pureFromUpdate_Double(canDivInto_DV_DV_Double)
  Vector.canDiv_V_V_Double.register(canDiv_DV_DV_Double)


  class canDivInto_DV_S_Double private[linalg] () extends BinaryUpdateOp[DenseVector[Double], Double, breeze.linalg.operators.OpDiv] {
    def apply(a: DenseVector[Double], b: Double) {
      val ad = a.data

        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = ad(aoff) / b
          aoff += a.stride
          i += 1
        }
        
    }
  }
  implicit val canDivInto_DV_S_Double = new canDivInto_DV_S_Double ()
    
  Vector.canDivInto_V_S_Double.register(canDivInto_DV_S_Double)

  implicit val canDiv_DV_S_Double: BinaryOp[DenseVector[Double], Double, breeze.linalg.operators.OpDiv, DenseVector[Double]] = pureFromUpdate_Double(canDivInto_DV_S_Double)
  Vector.canDiv_V_S_Double.register(canDiv_DV_S_Double)


  class canAddInto_DV_S_Double private[linalg] () extends BinaryUpdateOp[DenseVector[Double], Double, breeze.linalg.operators.OpAdd] {
    def apply(a: DenseVector[Double], b: Double) {
      val ad = a.data

        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = ad(aoff) + b
          aoff += a.stride
          i += 1
        }
        
    }
  }
  implicit val canAddInto_DV_S_Double = new canAddInto_DV_S_Double ()
    
  Vector.canAddInto_V_S_Double.register(canAddInto_DV_S_Double)

  implicit val canAdd_DV_S_Double: BinaryOp[DenseVector[Double], Double, breeze.linalg.operators.OpAdd, DenseVector[Double]] = pureFromUpdate_Double(canAddInto_DV_S_Double)
  Vector.canAdd_V_S_Double.register(canAdd_DV_S_Double)


  class canPowInto_DV_DV_Double private[linalg] () extends BinaryUpdateOp[DenseVector[Double], DenseVector[Double], breeze.linalg.operators.OpPow] {
    def apply(a: DenseVector[Double], b: DenseVector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset

        var i = 0
        while(i < a.length) {
          ad(aoff) = scala.math.pow(ad(aoff), bd(boff))
          aoff += a.stride
          boff += b.stride
          i += 1
        }
        
    }
  }
  implicit val canPowInto_DV_DV_Double = new canPowInto_DV_DV_Double ()
    
  Vector.canPowInto_V_V_Double.register(canPowInto_DV_DV_Double)

  implicit val canPow_DV_DV_Double: BinaryOp[DenseVector[Double], DenseVector[Double], breeze.linalg.operators.OpPow, DenseVector[Double]] = pureFromUpdate_Double(canPowInto_DV_DV_Double)
  Vector.canPow_V_V_Double.register(canPow_DV_DV_Double)


  class canPowInto_DV_S_Double private[linalg] () extends BinaryUpdateOp[DenseVector[Double], Double, breeze.linalg.operators.OpPow] {
    def apply(a: DenseVector[Double], b: Double) {
      val ad = a.data

        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = scala.math.pow(ad(aoff), b)
          aoff += a.stride
          i += 1
        }
        
    }
  }
  implicit val canPowInto_DV_S_Double = new canPowInto_DV_S_Double ()
    
  Vector.canPowInto_V_S_Double.register(canPowInto_DV_S_Double)

  implicit val canPow_DV_S_Double: BinaryOp[DenseVector[Double], Double, breeze.linalg.operators.OpPow, DenseVector[Double]] = pureFromUpdate_Double(canPowInto_DV_S_Double)
  Vector.canPow_V_S_Double.register(canPow_DV_S_Double)

}
/** This is an auto-generated trait providing operators for DenseVector. */
trait DenseVectorOps_Double_Generic extends AnyRef{ this: DenseVector.type =>

    def pureRegistryFromUpdate_Double[Other,Op<:OpType](op: BinaryUpdateRegistry[DenseVector[Double], Other, Op])(implicit copy: CanCopy[DenseVector[Double]]):BinaryRegistry[DenseVector[Double], Other, Op, DenseVector[Double]] = {
      new BinaryRegistry[DenseVector[Double], Other, Op, DenseVector[Double]] {
        override def bindingMissing(a : DenseVector[Double], b : Other) = {
          val c = copy(a)
          op(c, b)
          c
        }
      }
    }
        

  class canMulScalarInto_DV_V_Double private[linalg] () extends BinaryUpdateRegistry[DenseVector[Double], Vector[Double], breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: DenseVector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) * v
        }
        
    }
  }
  val canMulScalarInto_DV_V_Double = new canMulScalarInto_DV_V_Double ()
  implicit def canMulScalarInto_DV_V_Double_def[A <: DenseVector[Double], B <: Vector[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar] = (
    canMulScalarInto_DV_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar]]
  )
  Vector.canMulScalarInto_V_V_Double.register(canMulScalarInto_DV_V_Double)

  val canMulScalar_DV_V_Double: BinaryRegistry[DenseVector[Double], Vector[Double], OpMulScalar, DenseVector[Double]] = pureRegistryFromUpdate_Double(canMulScalarInto_DV_V_Double)
  implicit def canMulScalar_DV_V_Double_def[A <: DenseVector[Double], B <: Vector[Double]]:BinaryOp[A, B, OpMulScalar, DenseVector[Double]] = pureRegistryFromUpdate_Double(canMulScalarInto_DV_V_Double).asInstanceOf[BinaryOp[A, B, OpMulScalar, DenseVector[Double]]]
    
  Vector.canMulScalar_V_V_Double.register(canMulScalar_DV_V_Double)


  class canModInto_DV_V_Double private[linalg] () extends BinaryUpdateRegistry[DenseVector[Double], Vector[Double], breeze.linalg.operators.OpMod] {
    override def bindingMissing(a: DenseVector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) % v
        }
        
    }
  }
  val canModInto_DV_V_Double = new canModInto_DV_V_Double ()
  implicit def canModInto_DV_V_Double_def[A <: DenseVector[Double], B <: Vector[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod] = (
    canModInto_DV_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod]]
  )
  Vector.canModInto_V_V_Double.register(canModInto_DV_V_Double)

  val canMod_DV_V_Double: BinaryRegistry[DenseVector[Double], Vector[Double], OpMod, DenseVector[Double]] = pureRegistryFromUpdate_Double(canModInto_DV_V_Double)
  implicit def canMod_DV_V_Double_def[A <: DenseVector[Double], B <: Vector[Double]]:BinaryOp[A, B, OpMod, DenseVector[Double]] = pureRegistryFromUpdate_Double(canModInto_DV_V_Double).asInstanceOf[BinaryOp[A, B, OpMod, DenseVector[Double]]]
    
  Vector.canMod_V_V_Double.register(canMod_DV_V_Double)


  class canSetInto_DV_V_Double private[linalg] () extends BinaryUpdateRegistry[DenseVector[Double], Vector[Double], breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: DenseVector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = v
        }
        
    }
  }
  val canSetInto_DV_V_Double = new canSetInto_DV_V_Double ()
  implicit def canSetInto_DV_V_Double_def[A <: DenseVector[Double], B <: Vector[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet] = (
    canSetInto_DV_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet]]
  )
  Vector.canSetInto_V_V_Double.register(canSetInto_DV_V_Double)

  val canSet_DV_V_Double: BinaryRegistry[DenseVector[Double], Vector[Double], OpSet, DenseVector[Double]] = pureRegistryFromUpdate_Double(canSetInto_DV_V_Double)
  implicit def canSet_DV_V_Double_def[A <: DenseVector[Double], B <: Vector[Double]]:BinaryOp[A, B, OpSet, DenseVector[Double]] = pureRegistryFromUpdate_Double(canSetInto_DV_V_Double).asInstanceOf[BinaryOp[A, B, OpSet, DenseVector[Double]]]
    
  Vector.canSet_V_V_Double.register(canSet_DV_V_Double)


  class canSubInto_DV_V_Double private[linalg] () extends BinaryUpdateRegistry[DenseVector[Double], Vector[Double], breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: DenseVector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) - v
        }
        
    }
  }
  val canSubInto_DV_V_Double = new canSubInto_DV_V_Double ()
  implicit def canSubInto_DV_V_Double_def[A <: DenseVector[Double], B <: Vector[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub] = (
    canSubInto_DV_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub]]
  )
  Vector.canSubInto_V_V_Double.register(canSubInto_DV_V_Double)

  val canSub_DV_V_Double: BinaryRegistry[DenseVector[Double], Vector[Double], OpSub, DenseVector[Double]] = pureRegistryFromUpdate_Double(canSubInto_DV_V_Double)
  implicit def canSub_DV_V_Double_def[A <: DenseVector[Double], B <: Vector[Double]]:BinaryOp[A, B, OpSub, DenseVector[Double]] = pureRegistryFromUpdate_Double(canSubInto_DV_V_Double).asInstanceOf[BinaryOp[A, B, OpSub, DenseVector[Double]]]
    
  Vector.canSub_V_V_Double.register(canSub_DV_V_Double)


  class canDivInto_DV_V_Double private[linalg] () extends BinaryUpdateRegistry[DenseVector[Double], Vector[Double], breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: DenseVector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) / v
        }
        
    }
  }
  val canDivInto_DV_V_Double = new canDivInto_DV_V_Double ()
  implicit def canDivInto_DV_V_Double_def[A <: DenseVector[Double], B <: Vector[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv] = (
    canDivInto_DV_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv]]
  )
  Vector.canDivInto_V_V_Double.register(canDivInto_DV_V_Double)

  val canDiv_DV_V_Double: BinaryRegistry[DenseVector[Double], Vector[Double], OpDiv, DenseVector[Double]] = pureRegistryFromUpdate_Double(canDivInto_DV_V_Double)
  implicit def canDiv_DV_V_Double_def[A <: DenseVector[Double], B <: Vector[Double]]:BinaryOp[A, B, OpDiv, DenseVector[Double]] = pureRegistryFromUpdate_Double(canDivInto_DV_V_Double).asInstanceOf[BinaryOp[A, B, OpDiv, DenseVector[Double]]]
    
  Vector.canDiv_V_V_Double.register(canDiv_DV_V_Double)


  class canAddInto_DV_V_Double private[linalg] () extends BinaryUpdateRegistry[DenseVector[Double], Vector[Double], breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: DenseVector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) + v
        }
        
    }
  }
  val canAddInto_DV_V_Double = new canAddInto_DV_V_Double ()
  implicit def canAddInto_DV_V_Double_def[A <: DenseVector[Double], B <: Vector[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd] = (
    canAddInto_DV_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd]]
  )
  Vector.canAddInto_V_V_Double.register(canAddInto_DV_V_Double)

  val canAdd_DV_V_Double: BinaryRegistry[DenseVector[Double], Vector[Double], OpAdd, DenseVector[Double]] = pureRegistryFromUpdate_Double(canAddInto_DV_V_Double)
  implicit def canAdd_DV_V_Double_def[A <: DenseVector[Double], B <: Vector[Double]]:BinaryOp[A, B, OpAdd, DenseVector[Double]] = pureRegistryFromUpdate_Double(canAddInto_DV_V_Double).asInstanceOf[BinaryOp[A, B, OpAdd, DenseVector[Double]]]
    
  Vector.canAdd_V_V_Double.register(canAdd_DV_V_Double)


  class canPowInto_DV_V_Double private[linalg] () extends BinaryUpdateRegistry[DenseVector[Double], Vector[Double], breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: DenseVector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = scala.math.pow(a(i), v)
        }
        
    }
  }
  val canPowInto_DV_V_Double = new canPowInto_DV_V_Double ()
  implicit def canPowInto_DV_V_Double_def[A <: DenseVector[Double], B <: Vector[Double]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow] = (
    canPowInto_DV_V_Double.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow]]
  )
  Vector.canPowInto_V_V_Double.register(canPowInto_DV_V_Double)

  val canPow_DV_V_Double: BinaryRegistry[DenseVector[Double], Vector[Double], OpPow, DenseVector[Double]] = pureRegistryFromUpdate_Double(canPowInto_DV_V_Double)
  implicit def canPow_DV_V_Double_def[A <: DenseVector[Double], B <: Vector[Double]]:BinaryOp[A, B, OpPow, DenseVector[Double]] = pureRegistryFromUpdate_Double(canPowInto_DV_V_Double).asInstanceOf[BinaryOp[A, B, OpPow, DenseVector[Double]]]
    
  Vector.canPow_V_V_Double.register(canPow_DV_V_Double)

}
/** This is an auto-generated trait providing operators for DenseVector. */
trait DenseVectorOps_Float extends DenseVectorOps_Float_Generic { this: DenseVector.type =>

       def pureFromUpdate_Float[Other,Op<:OpType](op: BinaryUpdateOp[DenseVector[Float], Other, Op])(implicit copy: CanCopy[DenseVector[Float]]):BinaryOp[DenseVector[Float], Other, Op, DenseVector[Float]] = {
         new BinaryOp[DenseVector[Float], Other, Op, DenseVector[Float]] {
           override def apply(a : DenseVector[Float], b : Other) = {
             val c = copy(a)
             op(c, b)
             c
           }
         }
       }

  class canMulScalarInto_DV_DV_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], DenseVector[Float], breeze.linalg.operators.OpMulScalar] {
    def apply(a: DenseVector[Float], b: DenseVector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset

        var i = 0
        while(i < a.length) {
          ad(aoff) = ad(aoff) * bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
        
    }
  }
  implicit val canMulScalarInto_DV_DV_Float = new canMulScalarInto_DV_DV_Float ()
    
  Vector.canMulScalarInto_V_V_Float.register(canMulScalarInto_DV_DV_Float)

  implicit val canMulScalar_DV_DV_Float: BinaryOp[DenseVector[Float], DenseVector[Float], breeze.linalg.operators.OpMulScalar, DenseVector[Float]] = pureFromUpdate_Float(canMulScalarInto_DV_DV_Float)
  Vector.canMulScalar_V_V_Float.register(canMulScalar_DV_DV_Float)


  class canMulScalarInto_DV_S_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], Float, breeze.linalg.operators.OpMulScalar] {
    def apply(a: DenseVector[Float], b: Float) {
      val ad = a.data

        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = ad(aoff) * b
          aoff += a.stride
          i += 1
        }
        
    }
  }
  implicit val canMulScalarInto_DV_S_Float = new canMulScalarInto_DV_S_Float ()
    
  Vector.canMulScalarInto_V_S_Float.register(canMulScalarInto_DV_S_Float)

  implicit val canMulScalar_DV_S_Float: BinaryOp[DenseVector[Float], Float, breeze.linalg.operators.OpMulScalar, DenseVector[Float]] = pureFromUpdate_Float(canMulScalarInto_DV_S_Float)
  Vector.canMulScalar_V_S_Float.register(canMulScalar_DV_S_Float)


  class canModInto_DV_DV_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], DenseVector[Float], breeze.linalg.operators.OpMod] {
    def apply(a: DenseVector[Float], b: DenseVector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset

        var i = 0
        while(i < a.length) {
          ad(aoff) = ad(aoff) % bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
        
    }
  }
  implicit val canModInto_DV_DV_Float = new canModInto_DV_DV_Float ()
    
  Vector.canModInto_V_V_Float.register(canModInto_DV_DV_Float)

  implicit val canMod_DV_DV_Float: BinaryOp[DenseVector[Float], DenseVector[Float], breeze.linalg.operators.OpMod, DenseVector[Float]] = pureFromUpdate_Float(canModInto_DV_DV_Float)
  Vector.canMod_V_V_Float.register(canMod_DV_DV_Float)


  class canModInto_DV_S_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], Float, breeze.linalg.operators.OpMod] {
    def apply(a: DenseVector[Float], b: Float) {
      val ad = a.data

        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = ad(aoff) % b
          aoff += a.stride
          i += 1
        }
        
    }
  }
  implicit val canModInto_DV_S_Float = new canModInto_DV_S_Float ()
    
  Vector.canModInto_V_S_Float.register(canModInto_DV_S_Float)

  implicit val canMod_DV_S_Float: BinaryOp[DenseVector[Float], Float, breeze.linalg.operators.OpMod, DenseVector[Float]] = pureFromUpdate_Float(canModInto_DV_S_Float)
  Vector.canMod_V_S_Float.register(canMod_DV_S_Float)


  class canSetInto_DV_DV_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], DenseVector[Float], breeze.linalg.operators.OpSet] {
    def apply(a: DenseVector[Float], b: DenseVector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset

        var i = 0
        while(i < a.length) {
          ad(aoff) = bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
        
    }
  }
  implicit val canSetInto_DV_DV_Float = new canSetInto_DV_DV_Float ()
    
  Vector.canSetInto_V_V_Float.register(canSetInto_DV_DV_Float)

  implicit val canSet_DV_DV_Float: BinaryOp[DenseVector[Float], DenseVector[Float], breeze.linalg.operators.OpSet, DenseVector[Float]] = pureFromUpdate_Float(canSetInto_DV_DV_Float)
  Vector.canSet_V_V_Float.register(canSet_DV_DV_Float)


  class canSetInto_DV_S_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], Float, breeze.linalg.operators.OpSet] {
    def apply(a: DenseVector[Float], b: Float) {
      val ad = a.data

        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = b
          aoff += a.stride
          i += 1
        }
        
    }
  }
  implicit val canSetInto_DV_S_Float = new canSetInto_DV_S_Float ()
    
  Vector.canSetInto_V_S_Float.register(canSetInto_DV_S_Float)

  implicit val canSet_DV_S_Float: BinaryOp[DenseVector[Float], Float, breeze.linalg.operators.OpSet, DenseVector[Float]] = pureFromUpdate_Float(canSetInto_DV_S_Float)
  Vector.canSet_V_S_Float.register(canSet_DV_S_Float)


  class canSubInto_DV_DV_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], DenseVector[Float], breeze.linalg.operators.OpSub] {
    def apply(a: DenseVector[Float], b: DenseVector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset

        var i = 0
        while(i < a.length) {
          ad(aoff) = ad(aoff) - bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
        
    }
  }
  implicit val canSubInto_DV_DV_Float = new canSubInto_DV_DV_Float ()
    
  Vector.canSubInto_V_V_Float.register(canSubInto_DV_DV_Float)

  implicit val canSub_DV_DV_Float: BinaryOp[DenseVector[Float], DenseVector[Float], breeze.linalg.operators.OpSub, DenseVector[Float]] = pureFromUpdate_Float(canSubInto_DV_DV_Float)
  Vector.canSub_V_V_Float.register(canSub_DV_DV_Float)


  class canSubInto_DV_S_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], Float, breeze.linalg.operators.OpSub] {
    def apply(a: DenseVector[Float], b: Float) {
      val ad = a.data

        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = ad(aoff) - b
          aoff += a.stride
          i += 1
        }
        
    }
  }
  implicit val canSubInto_DV_S_Float = new canSubInto_DV_S_Float ()
    
  Vector.canSubInto_V_S_Float.register(canSubInto_DV_S_Float)

  implicit val canSub_DV_S_Float: BinaryOp[DenseVector[Float], Float, breeze.linalg.operators.OpSub, DenseVector[Float]] = pureFromUpdate_Float(canSubInto_DV_S_Float)
  Vector.canSub_V_S_Float.register(canSub_DV_S_Float)


  class canMulMatrixInto_DV_S_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], Float, breeze.linalg.operators.OpMulMatrix] {
    def apply(a: DenseVector[Float], b: Float) {
      val ad = a.data

        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = ad(aoff) * b
          aoff += a.stride
          i += 1
        }
        
    }
  }
  implicit val canMulMatrixInto_DV_S_Float = new canMulMatrixInto_DV_S_Float ()
    
  Vector.canMulMatrixInto_V_S_Float.register(canMulMatrixInto_DV_S_Float)

  implicit val canMulMatrix_DV_S_Float: BinaryOp[DenseVector[Float], Float, breeze.linalg.operators.OpMulMatrix, DenseVector[Float]] = pureFromUpdate_Float(canMulMatrixInto_DV_S_Float)
  Vector.canMulMatrix_V_S_Float.register(canMulMatrix_DV_S_Float)


  class canDivInto_DV_DV_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], DenseVector[Float], breeze.linalg.operators.OpDiv] {
    def apply(a: DenseVector[Float], b: DenseVector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset

        var i = 0
        while(i < a.length) {
          ad(aoff) = ad(aoff) / bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
        
    }
  }
  implicit val canDivInto_DV_DV_Float = new canDivInto_DV_DV_Float ()
    
  Vector.canDivInto_V_V_Float.register(canDivInto_DV_DV_Float)

  implicit val canDiv_DV_DV_Float: BinaryOp[DenseVector[Float], DenseVector[Float], breeze.linalg.operators.OpDiv, DenseVector[Float]] = pureFromUpdate_Float(canDivInto_DV_DV_Float)
  Vector.canDiv_V_V_Float.register(canDiv_DV_DV_Float)


  class canDivInto_DV_S_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], Float, breeze.linalg.operators.OpDiv] {
    def apply(a: DenseVector[Float], b: Float) {
      val ad = a.data

        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = ad(aoff) / b
          aoff += a.stride
          i += 1
        }
        
    }
  }
  implicit val canDivInto_DV_S_Float = new canDivInto_DV_S_Float ()
    
  Vector.canDivInto_V_S_Float.register(canDivInto_DV_S_Float)

  implicit val canDiv_DV_S_Float: BinaryOp[DenseVector[Float], Float, breeze.linalg.operators.OpDiv, DenseVector[Float]] = pureFromUpdate_Float(canDivInto_DV_S_Float)
  Vector.canDiv_V_S_Float.register(canDiv_DV_S_Float)


  class canAddInto_DV_DV_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], DenseVector[Float], breeze.linalg.operators.OpAdd] {
    def apply(a: DenseVector[Float], b: DenseVector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset

        var i = 0
        while(i < a.length) {
          ad(aoff) = ad(aoff) + bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
        
    }
  }
  implicit val canAddInto_DV_DV_Float = new canAddInto_DV_DV_Float ()
    
  Vector.canAddInto_V_V_Float.register(canAddInto_DV_DV_Float)

  implicit val canAdd_DV_DV_Float: BinaryOp[DenseVector[Float], DenseVector[Float], breeze.linalg.operators.OpAdd, DenseVector[Float]] = pureFromUpdate_Float(canAddInto_DV_DV_Float)
  Vector.canAdd_V_V_Float.register(canAdd_DV_DV_Float)


  class canAddInto_DV_S_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], Float, breeze.linalg.operators.OpAdd] {
    def apply(a: DenseVector[Float], b: Float) {
      val ad = a.data

        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = ad(aoff) + b
          aoff += a.stride
          i += 1
        }
        
    }
  }
  implicit val canAddInto_DV_S_Float = new canAddInto_DV_S_Float ()
    
  Vector.canAddInto_V_S_Float.register(canAddInto_DV_S_Float)

  implicit val canAdd_DV_S_Float: BinaryOp[DenseVector[Float], Float, breeze.linalg.operators.OpAdd, DenseVector[Float]] = pureFromUpdate_Float(canAddInto_DV_S_Float)
  Vector.canAdd_V_S_Float.register(canAdd_DV_S_Float)


  class canPowInto_DV_DV_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], DenseVector[Float], breeze.linalg.operators.OpPow] {
    def apply(a: DenseVector[Float], b: DenseVector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset

        var i = 0
        while(i < a.length) {
          ad(aoff) = scala.math.pow(ad(aoff), bd(boff)).toFloat
          aoff += a.stride
          boff += b.stride
          i += 1
        }
        
    }
  }
  implicit val canPowInto_DV_DV_Float = new canPowInto_DV_DV_Float ()
    
  Vector.canPowInto_V_V_Float.register(canPowInto_DV_DV_Float)

  implicit val canPow_DV_DV_Float: BinaryOp[DenseVector[Float], DenseVector[Float], breeze.linalg.operators.OpPow, DenseVector[Float]] = pureFromUpdate_Float(canPowInto_DV_DV_Float)
  Vector.canPow_V_V_Float.register(canPow_DV_DV_Float)


  class canPowInto_DV_S_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], Float, breeze.linalg.operators.OpPow] {
    def apply(a: DenseVector[Float], b: Float) {
      val ad = a.data

        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = scala.math.pow(ad(aoff), b).toFloat
          aoff += a.stride
          i += 1
        }
        
    }
  }
  implicit val canPowInto_DV_S_Float = new canPowInto_DV_S_Float ()
    
  Vector.canPowInto_V_S_Float.register(canPowInto_DV_S_Float)

  implicit val canPow_DV_S_Float: BinaryOp[DenseVector[Float], Float, breeze.linalg.operators.OpPow, DenseVector[Float]] = pureFromUpdate_Float(canPowInto_DV_S_Float)
  Vector.canPow_V_S_Float.register(canPow_DV_S_Float)

}
/** This is an auto-generated trait providing operators for DenseVector. */
trait DenseVectorOps_Float_Generic extends AnyRef{ this: DenseVector.type =>

    def pureRegistryFromUpdate_Float[Other,Op<:OpType](op: BinaryUpdateRegistry[DenseVector[Float], Other, Op])(implicit copy: CanCopy[DenseVector[Float]]):BinaryRegistry[DenseVector[Float], Other, Op, DenseVector[Float]] = {
      new BinaryRegistry[DenseVector[Float], Other, Op, DenseVector[Float]] {
        override def bindingMissing(a : DenseVector[Float], b : Other) = {
          val c = copy(a)
          op(c, b)
          c
        }
      }
    }
        

  class canMulScalarInto_DV_V_Float private[linalg] () extends BinaryUpdateRegistry[DenseVector[Float], Vector[Float], breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: DenseVector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) * v
        }
        
    }
  }
  val canMulScalarInto_DV_V_Float = new canMulScalarInto_DV_V_Float ()
  implicit def canMulScalarInto_DV_V_Float_def[A <: DenseVector[Float], B <: Vector[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar] = (
    canMulScalarInto_DV_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar]]
  )
  Vector.canMulScalarInto_V_V_Float.register(canMulScalarInto_DV_V_Float)

  val canMulScalar_DV_V_Float: BinaryRegistry[DenseVector[Float], Vector[Float], OpMulScalar, DenseVector[Float]] = pureRegistryFromUpdate_Float(canMulScalarInto_DV_V_Float)
  implicit def canMulScalar_DV_V_Float_def[A <: DenseVector[Float], B <: Vector[Float]]:BinaryOp[A, B, OpMulScalar, DenseVector[Float]] = pureRegistryFromUpdate_Float(canMulScalarInto_DV_V_Float).asInstanceOf[BinaryOp[A, B, OpMulScalar, DenseVector[Float]]]
    
  Vector.canMulScalar_V_V_Float.register(canMulScalar_DV_V_Float)


  class canModInto_DV_V_Float private[linalg] () extends BinaryUpdateRegistry[DenseVector[Float], Vector[Float], breeze.linalg.operators.OpMod] {
    override def bindingMissing(a: DenseVector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) % v
        }
        
    }
  }
  val canModInto_DV_V_Float = new canModInto_DV_V_Float ()
  implicit def canModInto_DV_V_Float_def[A <: DenseVector[Float], B <: Vector[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod] = (
    canModInto_DV_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod]]
  )
  Vector.canModInto_V_V_Float.register(canModInto_DV_V_Float)

  val canMod_DV_V_Float: BinaryRegistry[DenseVector[Float], Vector[Float], OpMod, DenseVector[Float]] = pureRegistryFromUpdate_Float(canModInto_DV_V_Float)
  implicit def canMod_DV_V_Float_def[A <: DenseVector[Float], B <: Vector[Float]]:BinaryOp[A, B, OpMod, DenseVector[Float]] = pureRegistryFromUpdate_Float(canModInto_DV_V_Float).asInstanceOf[BinaryOp[A, B, OpMod, DenseVector[Float]]]
    
  Vector.canMod_V_V_Float.register(canMod_DV_V_Float)


  class canSetInto_DV_V_Float private[linalg] () extends BinaryUpdateRegistry[DenseVector[Float], Vector[Float], breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: DenseVector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = v
        }
        
    }
  }
  val canSetInto_DV_V_Float = new canSetInto_DV_V_Float ()
  implicit def canSetInto_DV_V_Float_def[A <: DenseVector[Float], B <: Vector[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet] = (
    canSetInto_DV_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet]]
  )
  Vector.canSetInto_V_V_Float.register(canSetInto_DV_V_Float)

  val canSet_DV_V_Float: BinaryRegistry[DenseVector[Float], Vector[Float], OpSet, DenseVector[Float]] = pureRegistryFromUpdate_Float(canSetInto_DV_V_Float)
  implicit def canSet_DV_V_Float_def[A <: DenseVector[Float], B <: Vector[Float]]:BinaryOp[A, B, OpSet, DenseVector[Float]] = pureRegistryFromUpdate_Float(canSetInto_DV_V_Float).asInstanceOf[BinaryOp[A, B, OpSet, DenseVector[Float]]]
    
  Vector.canSet_V_V_Float.register(canSet_DV_V_Float)


  class canSubInto_DV_V_Float private[linalg] () extends BinaryUpdateRegistry[DenseVector[Float], Vector[Float], breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: DenseVector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) - v
        }
        
    }
  }
  val canSubInto_DV_V_Float = new canSubInto_DV_V_Float ()
  implicit def canSubInto_DV_V_Float_def[A <: DenseVector[Float], B <: Vector[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub] = (
    canSubInto_DV_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub]]
  )
  Vector.canSubInto_V_V_Float.register(canSubInto_DV_V_Float)

  val canSub_DV_V_Float: BinaryRegistry[DenseVector[Float], Vector[Float], OpSub, DenseVector[Float]] = pureRegistryFromUpdate_Float(canSubInto_DV_V_Float)
  implicit def canSub_DV_V_Float_def[A <: DenseVector[Float], B <: Vector[Float]]:BinaryOp[A, B, OpSub, DenseVector[Float]] = pureRegistryFromUpdate_Float(canSubInto_DV_V_Float).asInstanceOf[BinaryOp[A, B, OpSub, DenseVector[Float]]]
    
  Vector.canSub_V_V_Float.register(canSub_DV_V_Float)


  class canDivInto_DV_V_Float private[linalg] () extends BinaryUpdateRegistry[DenseVector[Float], Vector[Float], breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: DenseVector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) / v
        }
        
    }
  }
  val canDivInto_DV_V_Float = new canDivInto_DV_V_Float ()
  implicit def canDivInto_DV_V_Float_def[A <: DenseVector[Float], B <: Vector[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv] = (
    canDivInto_DV_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv]]
  )
  Vector.canDivInto_V_V_Float.register(canDivInto_DV_V_Float)

  val canDiv_DV_V_Float: BinaryRegistry[DenseVector[Float], Vector[Float], OpDiv, DenseVector[Float]] = pureRegistryFromUpdate_Float(canDivInto_DV_V_Float)
  implicit def canDiv_DV_V_Float_def[A <: DenseVector[Float], B <: Vector[Float]]:BinaryOp[A, B, OpDiv, DenseVector[Float]] = pureRegistryFromUpdate_Float(canDivInto_DV_V_Float).asInstanceOf[BinaryOp[A, B, OpDiv, DenseVector[Float]]]
    
  Vector.canDiv_V_V_Float.register(canDiv_DV_V_Float)


  class canAddInto_DV_V_Float private[linalg] () extends BinaryUpdateRegistry[DenseVector[Float], Vector[Float], breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: DenseVector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) + v
        }
        
    }
  }
  val canAddInto_DV_V_Float = new canAddInto_DV_V_Float ()
  implicit def canAddInto_DV_V_Float_def[A <: DenseVector[Float], B <: Vector[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd] = (
    canAddInto_DV_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd]]
  )
  Vector.canAddInto_V_V_Float.register(canAddInto_DV_V_Float)

  val canAdd_DV_V_Float: BinaryRegistry[DenseVector[Float], Vector[Float], OpAdd, DenseVector[Float]] = pureRegistryFromUpdate_Float(canAddInto_DV_V_Float)
  implicit def canAdd_DV_V_Float_def[A <: DenseVector[Float], B <: Vector[Float]]:BinaryOp[A, B, OpAdd, DenseVector[Float]] = pureRegistryFromUpdate_Float(canAddInto_DV_V_Float).asInstanceOf[BinaryOp[A, B, OpAdd, DenseVector[Float]]]
    
  Vector.canAdd_V_V_Float.register(canAdd_DV_V_Float)


  class canPowInto_DV_V_Float private[linalg] () extends BinaryUpdateRegistry[DenseVector[Float], Vector[Float], breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: DenseVector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = scala.math.pow(a(i), v).toFloat
        }
        
    }
  }
  val canPowInto_DV_V_Float = new canPowInto_DV_V_Float ()
  implicit def canPowInto_DV_V_Float_def[A <: DenseVector[Float], B <: Vector[Float]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow] = (
    canPowInto_DV_V_Float.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow]]
  )
  Vector.canPowInto_V_V_Float.register(canPowInto_DV_V_Float)

  val canPow_DV_V_Float: BinaryRegistry[DenseVector[Float], Vector[Float], OpPow, DenseVector[Float]] = pureRegistryFromUpdate_Float(canPowInto_DV_V_Float)
  implicit def canPow_DV_V_Float_def[A <: DenseVector[Float], B <: Vector[Float]]:BinaryOp[A, B, OpPow, DenseVector[Float]] = pureRegistryFromUpdate_Float(canPowInto_DV_V_Float).asInstanceOf[BinaryOp[A, B, OpPow, DenseVector[Float]]]
    
  Vector.canPow_V_V_Float.register(canPow_DV_V_Float)

}
/** This is an auto-generated trait providing operators for DenseVector. */
trait DenseVectorOps_Int extends DenseVectorOps_Int_Generic { this: DenseVector.type =>

       def pureFromUpdate_Int[Other,Op<:OpType](op: BinaryUpdateOp[DenseVector[Int], Other, Op])(implicit copy: CanCopy[DenseVector[Int]]):BinaryOp[DenseVector[Int], Other, Op, DenseVector[Int]] = {
         new BinaryOp[DenseVector[Int], Other, Op, DenseVector[Int]] {
           override def apply(a : DenseVector[Int], b : Other) = {
             val c = copy(a)
             op(c, b)
             c
           }
         }
       }

  class canMulScalarInto_DV_DV_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], DenseVector[Int], breeze.linalg.operators.OpMulScalar] {
    def apply(a: DenseVector[Int], b: DenseVector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset

        var i = 0
        while(i < a.length) {
          ad(aoff) = ad(aoff) * bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
        
    }
  }
  implicit val canMulScalarInto_DV_DV_Int = new canMulScalarInto_DV_DV_Int ()
    
  Vector.canMulScalarInto_V_V_Int.register(canMulScalarInto_DV_DV_Int)

  implicit val canMulScalar_DV_DV_Int: BinaryOp[DenseVector[Int], DenseVector[Int], breeze.linalg.operators.OpMulScalar, DenseVector[Int]] = pureFromUpdate_Int(canMulScalarInto_DV_DV_Int)
  Vector.canMulScalar_V_V_Int.register(canMulScalar_DV_DV_Int)


  class canMulScalarInto_DV_S_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], Int, breeze.linalg.operators.OpMulScalar] {
    def apply(a: DenseVector[Int], b: Int) {
      val ad = a.data

        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = ad(aoff) * b
          aoff += a.stride
          i += 1
        }
        
    }
  }
  implicit val canMulScalarInto_DV_S_Int = new canMulScalarInto_DV_S_Int ()
    
  Vector.canMulScalarInto_V_S_Int.register(canMulScalarInto_DV_S_Int)

  implicit val canMulScalar_DV_S_Int: BinaryOp[DenseVector[Int], Int, breeze.linalg.operators.OpMulScalar, DenseVector[Int]] = pureFromUpdate_Int(canMulScalarInto_DV_S_Int)
  Vector.canMulScalar_V_S_Int.register(canMulScalar_DV_S_Int)


  class canModInto_DV_DV_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], DenseVector[Int], breeze.linalg.operators.OpMod] {
    def apply(a: DenseVector[Int], b: DenseVector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset

        var i = 0
        while(i < a.length) {
          ad(aoff) = ad(aoff) % bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
        
    }
  }
  implicit val canModInto_DV_DV_Int = new canModInto_DV_DV_Int ()
    
  Vector.canModInto_V_V_Int.register(canModInto_DV_DV_Int)

  implicit val canMod_DV_DV_Int: BinaryOp[DenseVector[Int], DenseVector[Int], breeze.linalg.operators.OpMod, DenseVector[Int]] = pureFromUpdate_Int(canModInto_DV_DV_Int)
  Vector.canMod_V_V_Int.register(canMod_DV_DV_Int)


  class canModInto_DV_S_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], Int, breeze.linalg.operators.OpMod] {
    def apply(a: DenseVector[Int], b: Int) {
      val ad = a.data

        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = ad(aoff) % b
          aoff += a.stride
          i += 1
        }
        
    }
  }
  implicit val canModInto_DV_S_Int = new canModInto_DV_S_Int ()
    
  Vector.canModInto_V_S_Int.register(canModInto_DV_S_Int)

  implicit val canMod_DV_S_Int: BinaryOp[DenseVector[Int], Int, breeze.linalg.operators.OpMod, DenseVector[Int]] = pureFromUpdate_Int(canModInto_DV_S_Int)
  Vector.canMod_V_S_Int.register(canMod_DV_S_Int)


  class canSetInto_DV_DV_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], DenseVector[Int], breeze.linalg.operators.OpSet] {
    def apply(a: DenseVector[Int], b: DenseVector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset

        var i = 0
        while(i < a.length) {
          ad(aoff) = bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
        
    }
  }
  implicit val canSetInto_DV_DV_Int = new canSetInto_DV_DV_Int ()
    
  Vector.canSetInto_V_V_Int.register(canSetInto_DV_DV_Int)

  implicit val canSet_DV_DV_Int: BinaryOp[DenseVector[Int], DenseVector[Int], breeze.linalg.operators.OpSet, DenseVector[Int]] = pureFromUpdate_Int(canSetInto_DV_DV_Int)
  Vector.canSet_V_V_Int.register(canSet_DV_DV_Int)


  class canSetInto_DV_S_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], Int, breeze.linalg.operators.OpSet] {
    def apply(a: DenseVector[Int], b: Int) {
      val ad = a.data

        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = b
          aoff += a.stride
          i += 1
        }
        
    }
  }
  implicit val canSetInto_DV_S_Int = new canSetInto_DV_S_Int ()
    
  Vector.canSetInto_V_S_Int.register(canSetInto_DV_S_Int)

  implicit val canSet_DV_S_Int: BinaryOp[DenseVector[Int], Int, breeze.linalg.operators.OpSet, DenseVector[Int]] = pureFromUpdate_Int(canSetInto_DV_S_Int)
  Vector.canSet_V_S_Int.register(canSet_DV_S_Int)


  class canSubInto_DV_DV_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], DenseVector[Int], breeze.linalg.operators.OpSub] {
    def apply(a: DenseVector[Int], b: DenseVector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset

        var i = 0
        while(i < a.length) {
          ad(aoff) = ad(aoff) - bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
        
    }
  }
  implicit val canSubInto_DV_DV_Int = new canSubInto_DV_DV_Int ()
    
  Vector.canSubInto_V_V_Int.register(canSubInto_DV_DV_Int)

  implicit val canSub_DV_DV_Int: BinaryOp[DenseVector[Int], DenseVector[Int], breeze.linalg.operators.OpSub, DenseVector[Int]] = pureFromUpdate_Int(canSubInto_DV_DV_Int)
  Vector.canSub_V_V_Int.register(canSub_DV_DV_Int)


  class canSubInto_DV_S_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], Int, breeze.linalg.operators.OpSub] {
    def apply(a: DenseVector[Int], b: Int) {
      val ad = a.data

        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = ad(aoff) - b
          aoff += a.stride
          i += 1
        }
        
    }
  }
  implicit val canSubInto_DV_S_Int = new canSubInto_DV_S_Int ()
    
  Vector.canSubInto_V_S_Int.register(canSubInto_DV_S_Int)

  implicit val canSub_DV_S_Int: BinaryOp[DenseVector[Int], Int, breeze.linalg.operators.OpSub, DenseVector[Int]] = pureFromUpdate_Int(canSubInto_DV_S_Int)
  Vector.canSub_V_S_Int.register(canSub_DV_S_Int)


  class canMulMatrixInto_DV_S_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], Int, breeze.linalg.operators.OpMulMatrix] {
    def apply(a: DenseVector[Int], b: Int) {
      val ad = a.data

        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = ad(aoff) * b
          aoff += a.stride
          i += 1
        }
        
    }
  }
  implicit val canMulMatrixInto_DV_S_Int = new canMulMatrixInto_DV_S_Int ()
    
  Vector.canMulMatrixInto_V_S_Int.register(canMulMatrixInto_DV_S_Int)

  implicit val canMulMatrix_DV_S_Int: BinaryOp[DenseVector[Int], Int, breeze.linalg.operators.OpMulMatrix, DenseVector[Int]] = pureFromUpdate_Int(canMulMatrixInto_DV_S_Int)
  Vector.canMulMatrix_V_S_Int.register(canMulMatrix_DV_S_Int)


  class canDivInto_DV_DV_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], DenseVector[Int], breeze.linalg.operators.OpDiv] {
    def apply(a: DenseVector[Int], b: DenseVector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset

        var i = 0
        while(i < a.length) {
          ad(aoff) = ad(aoff) / bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
        
    }
  }
  implicit val canDivInto_DV_DV_Int = new canDivInto_DV_DV_Int ()
    
  Vector.canDivInto_V_V_Int.register(canDivInto_DV_DV_Int)

  implicit val canDiv_DV_DV_Int: BinaryOp[DenseVector[Int], DenseVector[Int], breeze.linalg.operators.OpDiv, DenseVector[Int]] = pureFromUpdate_Int(canDivInto_DV_DV_Int)
  Vector.canDiv_V_V_Int.register(canDiv_DV_DV_Int)


  class canDivInto_DV_S_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], Int, breeze.linalg.operators.OpDiv] {
    def apply(a: DenseVector[Int], b: Int) {
      val ad = a.data

        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = ad(aoff) / b
          aoff += a.stride
          i += 1
        }
        
    }
  }
  implicit val canDivInto_DV_S_Int = new canDivInto_DV_S_Int ()
    
  Vector.canDivInto_V_S_Int.register(canDivInto_DV_S_Int)

  implicit val canDiv_DV_S_Int: BinaryOp[DenseVector[Int], Int, breeze.linalg.operators.OpDiv, DenseVector[Int]] = pureFromUpdate_Int(canDivInto_DV_S_Int)
  Vector.canDiv_V_S_Int.register(canDiv_DV_S_Int)


  class canAddInto_DV_DV_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], DenseVector[Int], breeze.linalg.operators.OpAdd] {
    def apply(a: DenseVector[Int], b: DenseVector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset

        var i = 0
        while(i < a.length) {
          ad(aoff) = ad(aoff) + bd(boff)
          aoff += a.stride
          boff += b.stride
          i += 1
        }
        
    }
  }
  implicit val canAddInto_DV_DV_Int = new canAddInto_DV_DV_Int ()
    
  Vector.canAddInto_V_V_Int.register(canAddInto_DV_DV_Int)

  implicit val canAdd_DV_DV_Int: BinaryOp[DenseVector[Int], DenseVector[Int], breeze.linalg.operators.OpAdd, DenseVector[Int]] = pureFromUpdate_Int(canAddInto_DV_DV_Int)
  Vector.canAdd_V_V_Int.register(canAdd_DV_DV_Int)


  class canAddInto_DV_S_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], Int, breeze.linalg.operators.OpAdd] {
    def apply(a: DenseVector[Int], b: Int) {
      val ad = a.data

        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = ad(aoff) + b
          aoff += a.stride
          i += 1
        }
        
    }
  }
  implicit val canAddInto_DV_S_Int = new canAddInto_DV_S_Int ()
    
  Vector.canAddInto_V_S_Int.register(canAddInto_DV_S_Int)

  implicit val canAdd_DV_S_Int: BinaryOp[DenseVector[Int], Int, breeze.linalg.operators.OpAdd, DenseVector[Int]] = pureFromUpdate_Int(canAddInto_DV_S_Int)
  Vector.canAdd_V_S_Int.register(canAdd_DV_S_Int)


  class canPowInto_DV_DV_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], DenseVector[Int], breeze.linalg.operators.OpPow] {
    def apply(a: DenseVector[Int], b: DenseVector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val ad = a.data
        val bd = b.data
        var aoff = a.offset
        var boff = b.offset

        var i = 0
        while(i < a.length) {
          ad(aoff) = IntMath.ipow(ad(aoff), bd(boff))
          aoff += a.stride
          boff += b.stride
          i += 1
        }
        
    }
  }
  implicit val canPowInto_DV_DV_Int = new canPowInto_DV_DV_Int ()
    
  Vector.canPowInto_V_V_Int.register(canPowInto_DV_DV_Int)

  implicit val canPow_DV_DV_Int: BinaryOp[DenseVector[Int], DenseVector[Int], breeze.linalg.operators.OpPow, DenseVector[Int]] = pureFromUpdate_Int(canPowInto_DV_DV_Int)
  Vector.canPow_V_V_Int.register(canPow_DV_DV_Int)


  class canPowInto_DV_S_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], Int, breeze.linalg.operators.OpPow] {
    def apply(a: DenseVector[Int], b: Int) {
      val ad = a.data

        var i = 0
        var aoff = a.offset
        while(i < a.length) {
          ad(aoff) = IntMath.ipow(ad(aoff), b)
          aoff += a.stride
          i += 1
        }
        
    }
  }
  implicit val canPowInto_DV_S_Int = new canPowInto_DV_S_Int ()
    
  Vector.canPowInto_V_S_Int.register(canPowInto_DV_S_Int)

  implicit val canPow_DV_S_Int: BinaryOp[DenseVector[Int], Int, breeze.linalg.operators.OpPow, DenseVector[Int]] = pureFromUpdate_Int(canPowInto_DV_S_Int)
  Vector.canPow_V_S_Int.register(canPow_DV_S_Int)

}
/** This is an auto-generated trait providing operators for DenseVector. */
trait DenseVectorOps_Int_Generic extends AnyRef{ this: DenseVector.type =>

    def pureRegistryFromUpdate_Int[Other,Op<:OpType](op: BinaryUpdateRegistry[DenseVector[Int], Other, Op])(implicit copy: CanCopy[DenseVector[Int]]):BinaryRegistry[DenseVector[Int], Other, Op, DenseVector[Int]] = {
      new BinaryRegistry[DenseVector[Int], Other, Op, DenseVector[Int]] {
        override def bindingMissing(a : DenseVector[Int], b : Other) = {
          val c = copy(a)
          op(c, b)
          c
        }
      }
    }
        

  class canMulScalarInto_DV_V_Int private[linalg] () extends BinaryUpdateRegistry[DenseVector[Int], Vector[Int], breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: DenseVector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) * v
        }
        
    }
  }
  val canMulScalarInto_DV_V_Int = new canMulScalarInto_DV_V_Int ()
  implicit def canMulScalarInto_DV_V_Int_def[A <: DenseVector[Int], B <: Vector[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar] = (
    canMulScalarInto_DV_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMulScalar]]
  )
  Vector.canMulScalarInto_V_V_Int.register(canMulScalarInto_DV_V_Int)

  val canMulScalar_DV_V_Int: BinaryRegistry[DenseVector[Int], Vector[Int], OpMulScalar, DenseVector[Int]] = pureRegistryFromUpdate_Int(canMulScalarInto_DV_V_Int)
  implicit def canMulScalar_DV_V_Int_def[A <: DenseVector[Int], B <: Vector[Int]]:BinaryOp[A, B, OpMulScalar, DenseVector[Int]] = pureRegistryFromUpdate_Int(canMulScalarInto_DV_V_Int).asInstanceOf[BinaryOp[A, B, OpMulScalar, DenseVector[Int]]]
    
  Vector.canMulScalar_V_V_Int.register(canMulScalar_DV_V_Int)


  class canModInto_DV_V_Int private[linalg] () extends BinaryUpdateRegistry[DenseVector[Int], Vector[Int], breeze.linalg.operators.OpMod] {
    override def bindingMissing(a: DenseVector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) % v
        }
        
    }
  }
  val canModInto_DV_V_Int = new canModInto_DV_V_Int ()
  implicit def canModInto_DV_V_Int_def[A <: DenseVector[Int], B <: Vector[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod] = (
    canModInto_DV_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpMod]]
  )
  Vector.canModInto_V_V_Int.register(canModInto_DV_V_Int)

  val canMod_DV_V_Int: BinaryRegistry[DenseVector[Int], Vector[Int], OpMod, DenseVector[Int]] = pureRegistryFromUpdate_Int(canModInto_DV_V_Int)
  implicit def canMod_DV_V_Int_def[A <: DenseVector[Int], B <: Vector[Int]]:BinaryOp[A, B, OpMod, DenseVector[Int]] = pureRegistryFromUpdate_Int(canModInto_DV_V_Int).asInstanceOf[BinaryOp[A, B, OpMod, DenseVector[Int]]]
    
  Vector.canMod_V_V_Int.register(canMod_DV_V_Int)


  class canSetInto_DV_V_Int private[linalg] () extends BinaryUpdateRegistry[DenseVector[Int], Vector[Int], breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: DenseVector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = v
        }
        
    }
  }
  val canSetInto_DV_V_Int = new canSetInto_DV_V_Int ()
  implicit def canSetInto_DV_V_Int_def[A <: DenseVector[Int], B <: Vector[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet] = (
    canSetInto_DV_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSet]]
  )
  Vector.canSetInto_V_V_Int.register(canSetInto_DV_V_Int)

  val canSet_DV_V_Int: BinaryRegistry[DenseVector[Int], Vector[Int], OpSet, DenseVector[Int]] = pureRegistryFromUpdate_Int(canSetInto_DV_V_Int)
  implicit def canSet_DV_V_Int_def[A <: DenseVector[Int], B <: Vector[Int]]:BinaryOp[A, B, OpSet, DenseVector[Int]] = pureRegistryFromUpdate_Int(canSetInto_DV_V_Int).asInstanceOf[BinaryOp[A, B, OpSet, DenseVector[Int]]]
    
  Vector.canSet_V_V_Int.register(canSet_DV_V_Int)


  class canSubInto_DV_V_Int private[linalg] () extends BinaryUpdateRegistry[DenseVector[Int], Vector[Int], breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: DenseVector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) - v
        }
        
    }
  }
  val canSubInto_DV_V_Int = new canSubInto_DV_V_Int ()
  implicit def canSubInto_DV_V_Int_def[A <: DenseVector[Int], B <: Vector[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub] = (
    canSubInto_DV_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpSub]]
  )
  Vector.canSubInto_V_V_Int.register(canSubInto_DV_V_Int)

  val canSub_DV_V_Int: BinaryRegistry[DenseVector[Int], Vector[Int], OpSub, DenseVector[Int]] = pureRegistryFromUpdate_Int(canSubInto_DV_V_Int)
  implicit def canSub_DV_V_Int_def[A <: DenseVector[Int], B <: Vector[Int]]:BinaryOp[A, B, OpSub, DenseVector[Int]] = pureRegistryFromUpdate_Int(canSubInto_DV_V_Int).asInstanceOf[BinaryOp[A, B, OpSub, DenseVector[Int]]]
    
  Vector.canSub_V_V_Int.register(canSub_DV_V_Int)


  class canDivInto_DV_V_Int private[linalg] () extends BinaryUpdateRegistry[DenseVector[Int], Vector[Int], breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: DenseVector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) / v
        }
        
    }
  }
  val canDivInto_DV_V_Int = new canDivInto_DV_V_Int ()
  implicit def canDivInto_DV_V_Int_def[A <: DenseVector[Int], B <: Vector[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv] = (
    canDivInto_DV_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpDiv]]
  )
  Vector.canDivInto_V_V_Int.register(canDivInto_DV_V_Int)

  val canDiv_DV_V_Int: BinaryRegistry[DenseVector[Int], Vector[Int], OpDiv, DenseVector[Int]] = pureRegistryFromUpdate_Int(canDivInto_DV_V_Int)
  implicit def canDiv_DV_V_Int_def[A <: DenseVector[Int], B <: Vector[Int]]:BinaryOp[A, B, OpDiv, DenseVector[Int]] = pureRegistryFromUpdate_Int(canDivInto_DV_V_Int).asInstanceOf[BinaryOp[A, B, OpDiv, DenseVector[Int]]]
    
  Vector.canDiv_V_V_Int.register(canDiv_DV_V_Int)


  class canAddInto_DV_V_Int private[linalg] () extends BinaryUpdateRegistry[DenseVector[Int], Vector[Int], breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: DenseVector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) + v
        }
        
    }
  }
  val canAddInto_DV_V_Int = new canAddInto_DV_V_Int ()
  implicit def canAddInto_DV_V_Int_def[A <: DenseVector[Int], B <: Vector[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd] = (
    canAddInto_DV_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpAdd]]
  )
  Vector.canAddInto_V_V_Int.register(canAddInto_DV_V_Int)

  val canAdd_DV_V_Int: BinaryRegistry[DenseVector[Int], Vector[Int], OpAdd, DenseVector[Int]] = pureRegistryFromUpdate_Int(canAddInto_DV_V_Int)
  implicit def canAdd_DV_V_Int_def[A <: DenseVector[Int], B <: Vector[Int]]:BinaryOp[A, B, OpAdd, DenseVector[Int]] = pureRegistryFromUpdate_Int(canAddInto_DV_V_Int).asInstanceOf[BinaryOp[A, B, OpAdd, DenseVector[Int]]]
    
  Vector.canAdd_V_V_Int.register(canAdd_DV_V_Int)


  class canPowInto_DV_V_Int private[linalg] () extends BinaryUpdateRegistry[DenseVector[Int], Vector[Int], breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: DenseVector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = IntMath.ipow(a(i), v)
        }
        
    }
  }
  val canPowInto_DV_V_Int = new canPowInto_DV_V_Int ()
  implicit def canPowInto_DV_V_Int_def[A <: DenseVector[Int], B <: Vector[Int]]:BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow] = (
    canPowInto_DV_V_Int.asInstanceOf[BinaryUpdateOp[A, B, breeze.linalg.operators.OpPow]]
  )
  Vector.canPowInto_V_V_Int.register(canPowInto_DV_V_Int)

  val canPow_DV_V_Int: BinaryRegistry[DenseVector[Int], Vector[Int], OpPow, DenseVector[Int]] = pureRegistryFromUpdate_Int(canPowInto_DV_V_Int)
  implicit def canPow_DV_V_Int_def[A <: DenseVector[Int], B <: Vector[Int]]:BinaryOp[A, B, OpPow, DenseVector[Int]] = pureRegistryFromUpdate_Int(canPowInto_DV_V_Int).asInstanceOf[BinaryOp[A, B, OpPow, DenseVector[Int]]]
    
  Vector.canPow_V_V_Int.register(canPow_DV_V_Int)

}
