package breeze.linalg
import breeze.linalg.operators._
import breeze.linalg.support._
import breeze.numerics._
/** This is an auto-generated trait providing operators for DenseVector. */
trait DenseVectorOps_Double { this: DenseVector.type =>

  def pureFromUpdate_Double[Other,Op<:OpType](op: BinaryUpdateOp[DenseVector[Double], Other, Op])(implicit copy: CanCopy[DenseVector[Double]]):BinaryOp[DenseVector[Double], Other, Op, DenseVector[Double]] = {
    new BinaryOp[DenseVector[Double], Other, Op, DenseVector[Double]] {
      override def apply(a : DenseVector[Double], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }
        

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
  }; implicit val canDivInto_DV_DV_Double = new canDivInto_DV_DV_Double ()

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
  }; implicit val canDivInto_DV_S_Double = new canDivInto_DV_S_Double ()

  Vector.canDivInto_V_S_Double.register(canDivInto_DV_S_Double)

  implicit val canDiv_DV_S_Double: BinaryOp[DenseVector[Double], Double, breeze.linalg.operators.OpDiv, DenseVector[Double]] = pureFromUpdate_Double(canDivInto_DV_S_Double)
  Vector.canDiv_V_S_Double.register(canDiv_DV_S_Double)


  class canDivInto_DV_V_Double private[linalg] () extends BinaryUpdateOp[DenseVector[Double], Vector[Double], breeze.linalg.operators.OpDiv] {
    def apply(a: DenseVector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) / v
        }
        
    }
  }; implicit val canDivInto_DV_V_Double = new canDivInto_DV_V_Double ()

  Vector.canDivInto_V_V_Double.register(canDivInto_DV_V_Double)

  implicit val canDiv_DV_V_Double: BinaryOp[DenseVector[Double], Vector[Double], breeze.linalg.operators.OpDiv, DenseVector[Double]] = pureFromUpdate_Double(canDivInto_DV_V_Double)
  Vector.canDiv_V_V_Double.register(canDiv_DV_V_Double)


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
  }; implicit val canMulScalarInto_DV_DV_Double = new canMulScalarInto_DV_DV_Double ()

  Vector.canMulScalarInto_V_V_Double.register(canMulScalarInto_DV_DV_Double)

  implicit val canMulScalar_DV_DV_Double: BinaryOp[DenseVector[Double], DenseVector[Double], breeze.linalg.operators.OpMulScalar, DenseVector[Double]] = pureFromUpdate_Double(canMulScalarInto_DV_DV_Double)
  Vector.canMulScalar_V_V_Double.register(canMulScalar_DV_DV_Double)


  class canMulScalarInto_DV_V_Double private[linalg] () extends BinaryUpdateOp[DenseVector[Double], Vector[Double], breeze.linalg.operators.OpMulScalar] {
    def apply(a: DenseVector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) * v
        }
        
    }
  }; implicit val canMulScalarInto_DV_V_Double = new canMulScalarInto_DV_V_Double ()

  Vector.canMulScalarInto_V_V_Double.register(canMulScalarInto_DV_V_Double)

  implicit val canMulScalar_DV_V_Double: BinaryOp[DenseVector[Double], Vector[Double], breeze.linalg.operators.OpMulScalar, DenseVector[Double]] = pureFromUpdate_Double(canMulScalarInto_DV_V_Double)
  Vector.canMulScalar_V_V_Double.register(canMulScalar_DV_V_Double)


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
  }; implicit val canSetInto_DV_S_Double = new canSetInto_DV_S_Double ()

  Vector.canSetInto_V_S_Double.register(canSetInto_DV_S_Double)

  implicit val canSet_DV_S_Double: BinaryOp[DenseVector[Double], Double, breeze.linalg.operators.OpSet, DenseVector[Double]] = pureFromUpdate_Double(canSetInto_DV_S_Double)
  Vector.canSet_V_S_Double.register(canSet_DV_S_Double)


  class canSetInto_DV_V_Double private[linalg] () extends BinaryUpdateOp[DenseVector[Double], Vector[Double], breeze.linalg.operators.OpSet] {
    def apply(a: DenseVector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = v
        }
        
    }
  }; implicit val canSetInto_DV_V_Double = new canSetInto_DV_V_Double ()

  Vector.canSetInto_V_V_Double.register(canSetInto_DV_V_Double)

  implicit val canSet_DV_V_Double: BinaryOp[DenseVector[Double], Vector[Double], breeze.linalg.operators.OpSet, DenseVector[Double]] = pureFromUpdate_Double(canSetInto_DV_V_Double)
  Vector.canSet_V_V_Double.register(canSet_DV_V_Double)


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
  }; implicit val canModInto_DV_DV_Double = new canModInto_DV_DV_Double ()

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
  }; implicit val canModInto_DV_S_Double = new canModInto_DV_S_Double ()

  Vector.canModInto_V_S_Double.register(canModInto_DV_S_Double)

  implicit val canMod_DV_S_Double: BinaryOp[DenseVector[Double], Double, breeze.linalg.operators.OpMod, DenseVector[Double]] = pureFromUpdate_Double(canModInto_DV_S_Double)
  Vector.canMod_V_S_Double.register(canMod_DV_S_Double)


  class canModInto_DV_V_Double private[linalg] () extends BinaryUpdateOp[DenseVector[Double], Vector[Double], breeze.linalg.operators.OpMod] {
    def apply(a: DenseVector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) % v
        }
        
    }
  }; implicit val canModInto_DV_V_Double = new canModInto_DV_V_Double ()

  Vector.canModInto_V_V_Double.register(canModInto_DV_V_Double)

  implicit val canMod_DV_V_Double: BinaryOp[DenseVector[Double], Vector[Double], breeze.linalg.operators.OpMod, DenseVector[Double]] = pureFromUpdate_Double(canModInto_DV_V_Double)
  Vector.canMod_V_V_Double.register(canMod_DV_V_Double)


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
  }; implicit val canSubInto_DV_S_Double = new canSubInto_DV_S_Double ()

  Vector.canSubInto_V_S_Double.register(canSubInto_DV_S_Double)

  implicit val canSub_DV_S_Double: BinaryOp[DenseVector[Double], Double, breeze.linalg.operators.OpSub, DenseVector[Double]] = pureFromUpdate_Double(canSubInto_DV_S_Double)
  Vector.canSub_V_S_Double.register(canSub_DV_S_Double)


  class canSubInto_DV_V_Double private[linalg] () extends BinaryUpdateOp[DenseVector[Double], Vector[Double], breeze.linalg.operators.OpSub] {
    def apply(a: DenseVector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) - v
        }
        
    }
  }; implicit val canSubInto_DV_V_Double = new canSubInto_DV_V_Double ()

  Vector.canSubInto_V_V_Double.register(canSubInto_DV_V_Double)

  implicit val canSub_DV_V_Double: BinaryOp[DenseVector[Double], Vector[Double], breeze.linalg.operators.OpSub, DenseVector[Double]] = pureFromUpdate_Double(canSubInto_DV_V_Double)
  Vector.canSub_V_V_Double.register(canSub_DV_V_Double)


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
  }; implicit val canPowInto_DV_DV_Double = new canPowInto_DV_DV_Double ()

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
  }; implicit val canPowInto_DV_S_Double = new canPowInto_DV_S_Double ()

  Vector.canPowInto_V_S_Double.register(canPowInto_DV_S_Double)

  implicit val canPow_DV_S_Double: BinaryOp[DenseVector[Double], Double, breeze.linalg.operators.OpPow, DenseVector[Double]] = pureFromUpdate_Double(canPowInto_DV_S_Double)
  Vector.canPow_V_S_Double.register(canPow_DV_S_Double)


  class canPowInto_DV_V_Double private[linalg] () extends BinaryUpdateOp[DenseVector[Double], Vector[Double], breeze.linalg.operators.OpPow] {
    def apply(a: DenseVector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = scala.math.pow(a(i), v)
        }
        
    }
  }; implicit val canPowInto_DV_V_Double = new canPowInto_DV_V_Double ()

  Vector.canPowInto_V_V_Double.register(canPowInto_DV_V_Double)

  implicit val canPow_DV_V_Double: BinaryOp[DenseVector[Double], Vector[Double], breeze.linalg.operators.OpPow, DenseVector[Double]] = pureFromUpdate_Double(canPowInto_DV_V_Double)
  Vector.canPow_V_V_Double.register(canPow_DV_V_Double)


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
  }; implicit val canAddInto_DV_S_Double = new canAddInto_DV_S_Double ()

  Vector.canAddInto_V_S_Double.register(canAddInto_DV_S_Double)

  implicit val canAdd_DV_S_Double: BinaryOp[DenseVector[Double], Double, breeze.linalg.operators.OpAdd, DenseVector[Double]] = pureFromUpdate_Double(canAddInto_DV_S_Double)
  Vector.canAdd_V_S_Double.register(canAdd_DV_S_Double)


  class canAddInto_DV_V_Double private[linalg] () extends BinaryUpdateOp[DenseVector[Double], Vector[Double], breeze.linalg.operators.OpAdd] {
    def apply(a: DenseVector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) + v
        }
        
    }
  }; implicit val canAddInto_DV_V_Double = new canAddInto_DV_V_Double ()

  Vector.canAddInto_V_V_Double.register(canAddInto_DV_V_Double)

  implicit val canAdd_DV_V_Double: BinaryOp[DenseVector[Double], Vector[Double], breeze.linalg.operators.OpAdd, DenseVector[Double]] = pureFromUpdate_Double(canAddInto_DV_V_Double)
  Vector.canAdd_V_V_Double.register(canAdd_DV_V_Double)

}
/** This is an auto-generated trait providing operators for DenseVector. */
trait DenseVectorOps_Float { this: DenseVector.type =>

  def pureFromUpdate_Float[Other,Op<:OpType](op: BinaryUpdateOp[DenseVector[Float], Other, Op])(implicit copy: CanCopy[DenseVector[Float]]):BinaryOp[DenseVector[Float], Other, Op, DenseVector[Float]] = {
    new BinaryOp[DenseVector[Float], Other, Op, DenseVector[Float]] {
      override def apply(a : DenseVector[Float], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }
        

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
  }; implicit val canDivInto_DV_DV_Float = new canDivInto_DV_DV_Float ()

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
  }; implicit val canDivInto_DV_S_Float = new canDivInto_DV_S_Float ()

  Vector.canDivInto_V_S_Float.register(canDivInto_DV_S_Float)

  implicit val canDiv_DV_S_Float: BinaryOp[DenseVector[Float], Float, breeze.linalg.operators.OpDiv, DenseVector[Float]] = pureFromUpdate_Float(canDivInto_DV_S_Float)
  Vector.canDiv_V_S_Float.register(canDiv_DV_S_Float)


  class canDivInto_DV_V_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], Vector[Float], breeze.linalg.operators.OpDiv] {
    def apply(a: DenseVector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) / v
        }
        
    }
  }; implicit val canDivInto_DV_V_Float = new canDivInto_DV_V_Float ()

  Vector.canDivInto_V_V_Float.register(canDivInto_DV_V_Float)

  implicit val canDiv_DV_V_Float: BinaryOp[DenseVector[Float], Vector[Float], breeze.linalg.operators.OpDiv, DenseVector[Float]] = pureFromUpdate_Float(canDivInto_DV_V_Float)
  Vector.canDiv_V_V_Float.register(canDiv_DV_V_Float)


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
  }; implicit val canMulScalarInto_DV_DV_Float = new canMulScalarInto_DV_DV_Float ()

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
  }; implicit val canMulScalarInto_DV_S_Float = new canMulScalarInto_DV_S_Float ()

  Vector.canMulScalarInto_V_S_Float.register(canMulScalarInto_DV_S_Float)

  implicit val canMulScalar_DV_S_Float: BinaryOp[DenseVector[Float], Float, breeze.linalg.operators.OpMulScalar, DenseVector[Float]] = pureFromUpdate_Float(canMulScalarInto_DV_S_Float)
  Vector.canMulScalar_V_S_Float.register(canMulScalar_DV_S_Float)


  class canMulScalarInto_DV_V_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], Vector[Float], breeze.linalg.operators.OpMulScalar] {
    def apply(a: DenseVector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) * v
        }
        
    }
  }; implicit val canMulScalarInto_DV_V_Float = new canMulScalarInto_DV_V_Float ()

  Vector.canMulScalarInto_V_V_Float.register(canMulScalarInto_DV_V_Float)

  implicit val canMulScalar_DV_V_Float: BinaryOp[DenseVector[Float], Vector[Float], breeze.linalg.operators.OpMulScalar, DenseVector[Float]] = pureFromUpdate_Float(canMulScalarInto_DV_V_Float)
  Vector.canMulScalar_V_V_Float.register(canMulScalar_DV_V_Float)


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
  }; implicit val canSetInto_DV_DV_Float = new canSetInto_DV_DV_Float ()

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
  }; implicit val canSetInto_DV_S_Float = new canSetInto_DV_S_Float ()

  Vector.canSetInto_V_S_Float.register(canSetInto_DV_S_Float)

  implicit val canSet_DV_S_Float: BinaryOp[DenseVector[Float], Float, breeze.linalg.operators.OpSet, DenseVector[Float]] = pureFromUpdate_Float(canSetInto_DV_S_Float)
  Vector.canSet_V_S_Float.register(canSet_DV_S_Float)


  class canSetInto_DV_V_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], Vector[Float], breeze.linalg.operators.OpSet] {
    def apply(a: DenseVector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = v
        }
        
    }
  }; implicit val canSetInto_DV_V_Float = new canSetInto_DV_V_Float ()

  Vector.canSetInto_V_V_Float.register(canSetInto_DV_V_Float)

  implicit val canSet_DV_V_Float: BinaryOp[DenseVector[Float], Vector[Float], breeze.linalg.operators.OpSet, DenseVector[Float]] = pureFromUpdate_Float(canSetInto_DV_V_Float)
  Vector.canSet_V_V_Float.register(canSet_DV_V_Float)


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
  }; implicit val canModInto_DV_DV_Float = new canModInto_DV_DV_Float ()

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
  }; implicit val canModInto_DV_S_Float = new canModInto_DV_S_Float ()

  Vector.canModInto_V_S_Float.register(canModInto_DV_S_Float)

  implicit val canMod_DV_S_Float: BinaryOp[DenseVector[Float], Float, breeze.linalg.operators.OpMod, DenseVector[Float]] = pureFromUpdate_Float(canModInto_DV_S_Float)
  Vector.canMod_V_S_Float.register(canMod_DV_S_Float)


  class canModInto_DV_V_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], Vector[Float], breeze.linalg.operators.OpMod] {
    def apply(a: DenseVector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) % v
        }
        
    }
  }; implicit val canModInto_DV_V_Float = new canModInto_DV_V_Float ()

  Vector.canModInto_V_V_Float.register(canModInto_DV_V_Float)

  implicit val canMod_DV_V_Float: BinaryOp[DenseVector[Float], Vector[Float], breeze.linalg.operators.OpMod, DenseVector[Float]] = pureFromUpdate_Float(canModInto_DV_V_Float)
  Vector.canMod_V_V_Float.register(canMod_DV_V_Float)


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
  }; implicit val canSubInto_DV_DV_Float = new canSubInto_DV_DV_Float ()

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
  }; implicit val canSubInto_DV_S_Float = new canSubInto_DV_S_Float ()

  Vector.canSubInto_V_S_Float.register(canSubInto_DV_S_Float)

  implicit val canSub_DV_S_Float: BinaryOp[DenseVector[Float], Float, breeze.linalg.operators.OpSub, DenseVector[Float]] = pureFromUpdate_Float(canSubInto_DV_S_Float)
  Vector.canSub_V_S_Float.register(canSub_DV_S_Float)


  class canSubInto_DV_V_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], Vector[Float], breeze.linalg.operators.OpSub] {
    def apply(a: DenseVector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) - v
        }
        
    }
  }; implicit val canSubInto_DV_V_Float = new canSubInto_DV_V_Float ()

  Vector.canSubInto_V_V_Float.register(canSubInto_DV_V_Float)

  implicit val canSub_DV_V_Float: BinaryOp[DenseVector[Float], Vector[Float], breeze.linalg.operators.OpSub, DenseVector[Float]] = pureFromUpdate_Float(canSubInto_DV_V_Float)
  Vector.canSub_V_V_Float.register(canSub_DV_V_Float)


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
  }; implicit val canPowInto_DV_DV_Float = new canPowInto_DV_DV_Float ()

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
  }; implicit val canPowInto_DV_S_Float = new canPowInto_DV_S_Float ()

  Vector.canPowInto_V_S_Float.register(canPowInto_DV_S_Float)

  implicit val canPow_DV_S_Float: BinaryOp[DenseVector[Float], Float, breeze.linalg.operators.OpPow, DenseVector[Float]] = pureFromUpdate_Float(canPowInto_DV_S_Float)
  Vector.canPow_V_S_Float.register(canPow_DV_S_Float)


  class canPowInto_DV_V_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], Vector[Float], breeze.linalg.operators.OpPow] {
    def apply(a: DenseVector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = scala.math.pow(a(i), v).toFloat
        }
        
    }
  }; implicit val canPowInto_DV_V_Float = new canPowInto_DV_V_Float ()

  Vector.canPowInto_V_V_Float.register(canPowInto_DV_V_Float)

  implicit val canPow_DV_V_Float: BinaryOp[DenseVector[Float], Vector[Float], breeze.linalg.operators.OpPow, DenseVector[Float]] = pureFromUpdate_Float(canPowInto_DV_V_Float)
  Vector.canPow_V_V_Float.register(canPow_DV_V_Float)


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
  }; implicit val canAddInto_DV_DV_Float = new canAddInto_DV_DV_Float ()

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
  }; implicit val canAddInto_DV_S_Float = new canAddInto_DV_S_Float ()

  Vector.canAddInto_V_S_Float.register(canAddInto_DV_S_Float)

  implicit val canAdd_DV_S_Float: BinaryOp[DenseVector[Float], Float, breeze.linalg.operators.OpAdd, DenseVector[Float]] = pureFromUpdate_Float(canAddInto_DV_S_Float)
  Vector.canAdd_V_S_Float.register(canAdd_DV_S_Float)


  class canAddInto_DV_V_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], Vector[Float], breeze.linalg.operators.OpAdd] {
    def apply(a: DenseVector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) + v
        }
        
    }
  }; implicit val canAddInto_DV_V_Float = new canAddInto_DV_V_Float ()

  Vector.canAddInto_V_V_Float.register(canAddInto_DV_V_Float)

  implicit val canAdd_DV_V_Float: BinaryOp[DenseVector[Float], Vector[Float], breeze.linalg.operators.OpAdd, DenseVector[Float]] = pureFromUpdate_Float(canAddInto_DV_V_Float)
  Vector.canAdd_V_V_Float.register(canAdd_DV_V_Float)

}
/** This is an auto-generated trait providing operators for DenseVector. */
trait DenseVectorOps_Int { this: DenseVector.type =>

  def pureFromUpdate_Int[Other,Op<:OpType](op: BinaryUpdateOp[DenseVector[Int], Other, Op])(implicit copy: CanCopy[DenseVector[Int]]):BinaryOp[DenseVector[Int], Other, Op, DenseVector[Int]] = {
    new BinaryOp[DenseVector[Int], Other, Op, DenseVector[Int]] {
      override def apply(a : DenseVector[Int], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }
        

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
  }; implicit val canDivInto_DV_DV_Int = new canDivInto_DV_DV_Int ()

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
  }; implicit val canDivInto_DV_S_Int = new canDivInto_DV_S_Int ()

  Vector.canDivInto_V_S_Int.register(canDivInto_DV_S_Int)

  implicit val canDiv_DV_S_Int: BinaryOp[DenseVector[Int], Int, breeze.linalg.operators.OpDiv, DenseVector[Int]] = pureFromUpdate_Int(canDivInto_DV_S_Int)
  Vector.canDiv_V_S_Int.register(canDiv_DV_S_Int)


  class canDivInto_DV_V_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], Vector[Int], breeze.linalg.operators.OpDiv] {
    def apply(a: DenseVector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) / v
        }
        
    }
  }; implicit val canDivInto_DV_V_Int = new canDivInto_DV_V_Int ()

  Vector.canDivInto_V_V_Int.register(canDivInto_DV_V_Int)

  implicit val canDiv_DV_V_Int: BinaryOp[DenseVector[Int], Vector[Int], breeze.linalg.operators.OpDiv, DenseVector[Int]] = pureFromUpdate_Int(canDivInto_DV_V_Int)
  Vector.canDiv_V_V_Int.register(canDiv_DV_V_Int)


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
  }; implicit val canMulScalarInto_DV_DV_Int = new canMulScalarInto_DV_DV_Int ()

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
  }; implicit val canMulScalarInto_DV_S_Int = new canMulScalarInto_DV_S_Int ()

  Vector.canMulScalarInto_V_S_Int.register(canMulScalarInto_DV_S_Int)

  implicit val canMulScalar_DV_S_Int: BinaryOp[DenseVector[Int], Int, breeze.linalg.operators.OpMulScalar, DenseVector[Int]] = pureFromUpdate_Int(canMulScalarInto_DV_S_Int)
  Vector.canMulScalar_V_S_Int.register(canMulScalar_DV_S_Int)


  class canMulScalarInto_DV_V_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], Vector[Int], breeze.linalg.operators.OpMulScalar] {
    def apply(a: DenseVector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) * v
        }
        
    }
  }; implicit val canMulScalarInto_DV_V_Int = new canMulScalarInto_DV_V_Int ()

  Vector.canMulScalarInto_V_V_Int.register(canMulScalarInto_DV_V_Int)

  implicit val canMulScalar_DV_V_Int: BinaryOp[DenseVector[Int], Vector[Int], breeze.linalg.operators.OpMulScalar, DenseVector[Int]] = pureFromUpdate_Int(canMulScalarInto_DV_V_Int)
  Vector.canMulScalar_V_V_Int.register(canMulScalar_DV_V_Int)


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
  }; implicit val canSetInto_DV_DV_Int = new canSetInto_DV_DV_Int ()

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
  }; implicit val canSetInto_DV_S_Int = new canSetInto_DV_S_Int ()

  Vector.canSetInto_V_S_Int.register(canSetInto_DV_S_Int)

  implicit val canSet_DV_S_Int: BinaryOp[DenseVector[Int], Int, breeze.linalg.operators.OpSet, DenseVector[Int]] = pureFromUpdate_Int(canSetInto_DV_S_Int)
  Vector.canSet_V_S_Int.register(canSet_DV_S_Int)


  class canSetInto_DV_V_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], Vector[Int], breeze.linalg.operators.OpSet] {
    def apply(a: DenseVector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = v
        }
        
    }
  }; implicit val canSetInto_DV_V_Int = new canSetInto_DV_V_Int ()

  Vector.canSetInto_V_V_Int.register(canSetInto_DV_V_Int)

  implicit val canSet_DV_V_Int: BinaryOp[DenseVector[Int], Vector[Int], breeze.linalg.operators.OpSet, DenseVector[Int]] = pureFromUpdate_Int(canSetInto_DV_V_Int)
  Vector.canSet_V_V_Int.register(canSet_DV_V_Int)


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
  }; implicit val canModInto_DV_DV_Int = new canModInto_DV_DV_Int ()

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
  }; implicit val canModInto_DV_S_Int = new canModInto_DV_S_Int ()

  Vector.canModInto_V_S_Int.register(canModInto_DV_S_Int)

  implicit val canMod_DV_S_Int: BinaryOp[DenseVector[Int], Int, breeze.linalg.operators.OpMod, DenseVector[Int]] = pureFromUpdate_Int(canModInto_DV_S_Int)
  Vector.canMod_V_S_Int.register(canMod_DV_S_Int)


  class canModInto_DV_V_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], Vector[Int], breeze.linalg.operators.OpMod] {
    def apply(a: DenseVector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) % v
        }
        
    }
  }; implicit val canModInto_DV_V_Int = new canModInto_DV_V_Int ()

  Vector.canModInto_V_V_Int.register(canModInto_DV_V_Int)

  implicit val canMod_DV_V_Int: BinaryOp[DenseVector[Int], Vector[Int], breeze.linalg.operators.OpMod, DenseVector[Int]] = pureFromUpdate_Int(canModInto_DV_V_Int)
  Vector.canMod_V_V_Int.register(canMod_DV_V_Int)


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
  }; implicit val canSubInto_DV_DV_Int = new canSubInto_DV_DV_Int ()

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
  }; implicit val canSubInto_DV_S_Int = new canSubInto_DV_S_Int ()

  Vector.canSubInto_V_S_Int.register(canSubInto_DV_S_Int)

  implicit val canSub_DV_S_Int: BinaryOp[DenseVector[Int], Int, breeze.linalg.operators.OpSub, DenseVector[Int]] = pureFromUpdate_Int(canSubInto_DV_S_Int)
  Vector.canSub_V_S_Int.register(canSub_DV_S_Int)


  class canSubInto_DV_V_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], Vector[Int], breeze.linalg.operators.OpSub] {
    def apply(a: DenseVector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) - v
        }
        
    }
  }; implicit val canSubInto_DV_V_Int = new canSubInto_DV_V_Int ()

  Vector.canSubInto_V_V_Int.register(canSubInto_DV_V_Int)

  implicit val canSub_DV_V_Int: BinaryOp[DenseVector[Int], Vector[Int], breeze.linalg.operators.OpSub, DenseVector[Int]] = pureFromUpdate_Int(canSubInto_DV_V_Int)
  Vector.canSub_V_V_Int.register(canSub_DV_V_Int)


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
  }; implicit val canPowInto_DV_DV_Int = new canPowInto_DV_DV_Int ()

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
  }; implicit val canPowInto_DV_S_Int = new canPowInto_DV_S_Int ()

  Vector.canPowInto_V_S_Int.register(canPowInto_DV_S_Int)

  implicit val canPow_DV_S_Int: BinaryOp[DenseVector[Int], Int, breeze.linalg.operators.OpPow, DenseVector[Int]] = pureFromUpdate_Int(canPowInto_DV_S_Int)
  Vector.canPow_V_S_Int.register(canPow_DV_S_Int)


  class canPowInto_DV_V_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], Vector[Int], breeze.linalg.operators.OpPow] {
    def apply(a: DenseVector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = IntMath.ipow(a(i), v)
        }
        
    }
  }; implicit val canPowInto_DV_V_Int = new canPowInto_DV_V_Int ()

  Vector.canPowInto_V_V_Int.register(canPowInto_DV_V_Int)

  implicit val canPow_DV_V_Int: BinaryOp[DenseVector[Int], Vector[Int], breeze.linalg.operators.OpPow, DenseVector[Int]] = pureFromUpdate_Int(canPowInto_DV_V_Int)
  Vector.canPow_V_V_Int.register(canPow_DV_V_Int)


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
  }; implicit val canAddInto_DV_DV_Int = new canAddInto_DV_DV_Int ()

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
  }; implicit val canAddInto_DV_S_Int = new canAddInto_DV_S_Int ()

  Vector.canAddInto_V_S_Int.register(canAddInto_DV_S_Int)

  implicit val canAdd_DV_S_Int: BinaryOp[DenseVector[Int], Int, breeze.linalg.operators.OpAdd, DenseVector[Int]] = pureFromUpdate_Int(canAddInto_DV_S_Int)
  Vector.canAdd_V_S_Int.register(canAdd_DV_S_Int)


  class canAddInto_DV_V_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], Vector[Int], breeze.linalg.operators.OpAdd] {
    def apply(a: DenseVector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) + v
        }
        
    }
  }; implicit val canAddInto_DV_V_Int = new canAddInto_DV_V_Int ()

  Vector.canAddInto_V_V_Int.register(canAddInto_DV_V_Int)

  implicit val canAdd_DV_V_Int: BinaryOp[DenseVector[Int], Vector[Int], breeze.linalg.operators.OpAdd, DenseVector[Int]] = pureFromUpdate_Int(canAddInto_DV_V_Int)
  Vector.canAdd_V_V_Int.register(canAdd_DV_V_Int)

}
