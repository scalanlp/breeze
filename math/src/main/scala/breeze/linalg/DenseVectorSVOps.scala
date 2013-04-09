package breeze.linalg
import breeze.linalg.operators._
import breeze.linalg.support._
import breeze.math.Complex
import breeze.math.Complex._
import breeze.numerics._

/** This is an auto-generated trait providing operators for DenseVector and SparseVector*/
trait DenseVectorOps_SparseVector_Double { this: DenseVector.type =>

  class canAddInto_DV_SparseVector_Double private[linalg] () extends BinaryUpdateOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpAdd] {
    def apply(a: DenseVector[Double], b: SparseVector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        val aoff = a.offset
        val astride = a.stride

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize

        var i = 0
        while(i < bsize) {
          if(b.isActive(i)) {
                val j = aoff + bi(i) * astride
                adata(j) = adata(j) + bd(i)
          }
          i += 1
        }
        
    }
  }
  implicit val canAddInto_DV_SparseVector_Double = new canAddInto_DV_SparseVector_Double ()
    
  Vector.canAddInto_V_V_Double.register(canAddInto_DV_SparseVector_Double)

  implicit val canAdd_DV_SparseVector_Double: BinaryOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpAdd, DenseVector[Double]] = pureFromUpdate_Double(canAddInto_DV_SparseVector_Double)
  Vector.canAdd_V_V_Double.register(canAdd_DV_SparseVector_Double)


  class canModInto_DV_SparseVector_Double private[linalg] () extends BinaryUpdateOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpMod] {
    def apply(a: DenseVector[Double], b: SparseVector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        var j = a.offset
        val astride = a.stride

        var i = 0
        while(i < b.length) {
          adata(j) = adata(j) % b(i)
          i += 1
          j += astride
        }
        
    }
  }
  implicit val canModInto_DV_SparseVector_Double = new canModInto_DV_SparseVector_Double ()
    
  Vector.canModInto_V_V_Double.register(canModInto_DV_SparseVector_Double)

  implicit val canMod_DV_SparseVector_Double: BinaryOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpMod, DenseVector[Double]] = pureFromUpdate_Double(canModInto_DV_SparseVector_Double)
  Vector.canMod_V_V_Double.register(canMod_DV_SparseVector_Double)


  class canMulScalarInto_DV_SparseVector_Double private[linalg] () extends BinaryUpdateOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpMulScalar] {
    def apply(a: DenseVector[Double], b: SparseVector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        var j = a.offset
        val astride = a.stride

        var i = 0
        while(i < b.length) {
          adata(j) = adata(j) * b(i)
          i += 1
          j += astride
        }
        
    }
  }
  implicit val canMulScalarInto_DV_SparseVector_Double = new canMulScalarInto_DV_SparseVector_Double ()
    
  Vector.canMulScalarInto_V_V_Double.register(canMulScalarInto_DV_SparseVector_Double)

  implicit val canMulScalar_DV_SparseVector_Double: BinaryOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpMulScalar, DenseVector[Double]] = pureFromUpdate_Double(canMulScalarInto_DV_SparseVector_Double)
  Vector.canMulScalar_V_V_Double.register(canMulScalar_DV_SparseVector_Double)


  class canSetInto_DV_SparseVector_Double private[linalg] () extends BinaryUpdateOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpSet] {
    def apply(a: DenseVector[Double], b: SparseVector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        var j = a.offset
        val astride = a.stride

        var i = 0
        while(i < b.length) {
          adata(j) = b(i)
          i += 1
          j += astride
        }
        
    }
  }
  implicit val canSetInto_DV_SparseVector_Double = new canSetInto_DV_SparseVector_Double ()
    
  Vector.canSetInto_V_V_Double.register(canSetInto_DV_SparseVector_Double)

  implicit val canSet_DV_SparseVector_Double: BinaryOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpSet, DenseVector[Double]] = pureFromUpdate_Double(canSetInto_DV_SparseVector_Double)
  Vector.canSet_V_V_Double.register(canSet_DV_SparseVector_Double)


  class canSubInto_DV_SparseVector_Double private[linalg] () extends BinaryUpdateOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpSub] {
    def apply(a: DenseVector[Double], b: SparseVector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        val aoff = a.offset
        val astride = a.stride

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize

        var i = 0
        while(i < bsize) {
          if(b.isActive(i)) {
                val j = aoff + bi(i) * astride
                adata(j) = adata(j) - bd(i)
          }
          i += 1
        }
        
    }
  }
  implicit val canSubInto_DV_SparseVector_Double = new canSubInto_DV_SparseVector_Double ()
    
  Vector.canSubInto_V_V_Double.register(canSubInto_DV_SparseVector_Double)

  implicit val canSub_DV_SparseVector_Double: BinaryOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpSub, DenseVector[Double]] = pureFromUpdate_Double(canSubInto_DV_SparseVector_Double)
  Vector.canSub_V_V_Double.register(canSub_DV_SparseVector_Double)


  class canPowInto_DV_SparseVector_Double private[linalg] () extends BinaryUpdateOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpPow] {
    def apply(a: DenseVector[Double], b: SparseVector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        var j = a.offset
        val astride = a.stride

        var i = 0
        while(i < b.length) {
          adata(j) = scala.math.pow(adata(j), b(i))
          i += 1
          j += astride
        }
        
    }
  }
  implicit val canPowInto_DV_SparseVector_Double = new canPowInto_DV_SparseVector_Double ()
    
  Vector.canPowInto_V_V_Double.register(canPowInto_DV_SparseVector_Double)

  implicit val canPow_DV_SparseVector_Double: BinaryOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpPow, DenseVector[Double]] = pureFromUpdate_Double(canPowInto_DV_SparseVector_Double)
  Vector.canPow_V_V_Double.register(canPow_DV_SparseVector_Double)


  class canDivInto_DV_SparseVector_Double private[linalg] () extends BinaryUpdateOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpDiv] {
    def apply(a: DenseVector[Double], b: SparseVector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        var j = a.offset
        val astride = a.stride

        var i = 0
        while(i < b.length) {
          adata(j) = adata(j) / b(i)
          i += 1
          j += astride
        }
        
    }
  }
  implicit val canDivInto_DV_SparseVector_Double = new canDivInto_DV_SparseVector_Double ()
    
  Vector.canDivInto_V_V_Double.register(canDivInto_DV_SparseVector_Double)

  implicit val canDiv_DV_SparseVector_Double: BinaryOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpDiv, DenseVector[Double]] = pureFromUpdate_Double(canDivInto_DV_SparseVector_Double)
  Vector.canDiv_V_V_Double.register(canDiv_DV_SparseVector_Double)


   class canDotProductDV_SV_Double private[linalg] () extends BinaryOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpMulInner, Double] {
    def apply(a: DenseVector[Double], b: SparseVector[Double]) = {
      require(b.length == a.length, "Vectors must be the same length!")

      var result: Double = 0

      val bd = b.data
      val bi = b.index
      val bsize = b.iterableSize

      val adata = a.data
      val aoff = a.offset
      val stride = a.stride

      var i = 0
      if(b.allVisitableIndicesActive && stride == 1 && aoff == 0) {
         while(i < bsize) {
           result += adata(bi(i)) * bd(i)
           i += 1
         }
      } else {
         while(i < bsize) {
           if(b.isActive(i)) result += adata(aoff + bi(i) * stride) * bd(i)
           i += 1
         }
      }
      result
    }
  }; implicit val canDotProductDV_SV_Double = new canDotProductDV_SV_Double ()


   class canDotProductSV_DV_Double private[linalg] () extends BinaryOp[SparseVector[Double], DenseVector[Double], breeze.linalg.operators.OpMulInner, Double] {
    def apply(a: SparseVector[Double], b: DenseVector[Double]) = {
      require(b.length == a.length, "Vectors must be the same length!")
      canDotProductDV_SV_Double(b,a)
      
    }
  }; implicit val canDotProductSV_DV_Double = new canDotProductSV_DV_Double ()


  class canAxpy_DV_SV_Double private[linalg] () extends CanAxpy[Double, SparseVector[Double], DenseVector[Double]] {
    def apply(s: Double, b: SparseVector[Double], a: DenseVector[Double]) {
      if(s == 0) return;
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        val aoff = a.offset
        val astride = a.stride

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize

        var i = 0
        while(i < bsize) {
          if(b.isActive(i)) {
                val j = aoff + bi(i) * astride
                adata(j) = adata(j) + s * bd(i)
          }
          i += 1
        }
        
    }
  }
  implicit val canAxpy_DV_SV_Double = new canAxpy_DV_SV_Double ()
    
}

/** This is an auto-generated trait providing operators for DenseVector and SparseVector*/
trait DenseVectorOps_SparseVector_Float { this: DenseVector.type =>

  class canAddInto_DV_SparseVector_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpAdd] {
    def apply(a: DenseVector[Float], b: SparseVector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        val aoff = a.offset
        val astride = a.stride

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize

        var i = 0
        while(i < bsize) {
          if(b.isActive(i)) {
                val j = aoff + bi(i) * astride
                adata(j) = adata(j) + bd(i)
          }
          i += 1
        }
        
    }
  }
  implicit val canAddInto_DV_SparseVector_Float = new canAddInto_DV_SparseVector_Float ()
    
  Vector.canAddInto_V_V_Float.register(canAddInto_DV_SparseVector_Float)

  implicit val canAdd_DV_SparseVector_Float: BinaryOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpAdd, DenseVector[Float]] = pureFromUpdate_Float(canAddInto_DV_SparseVector_Float)
  Vector.canAdd_V_V_Float.register(canAdd_DV_SparseVector_Float)


  class canModInto_DV_SparseVector_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpMod] {
    def apply(a: DenseVector[Float], b: SparseVector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        var j = a.offset
        val astride = a.stride

        var i = 0
        while(i < b.length) {
          adata(j) = adata(j) % b(i)
          i += 1
          j += astride
        }
        
    }
  }
  implicit val canModInto_DV_SparseVector_Float = new canModInto_DV_SparseVector_Float ()
    
  Vector.canModInto_V_V_Float.register(canModInto_DV_SparseVector_Float)

  implicit val canMod_DV_SparseVector_Float: BinaryOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpMod, DenseVector[Float]] = pureFromUpdate_Float(canModInto_DV_SparseVector_Float)
  Vector.canMod_V_V_Float.register(canMod_DV_SparseVector_Float)


  class canMulScalarInto_DV_SparseVector_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpMulScalar] {
    def apply(a: DenseVector[Float], b: SparseVector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        var j = a.offset
        val astride = a.stride

        var i = 0
        while(i < b.length) {
          adata(j) = adata(j) * b(i)
          i += 1
          j += astride
        }
        
    }
  }
  implicit val canMulScalarInto_DV_SparseVector_Float = new canMulScalarInto_DV_SparseVector_Float ()
    
  Vector.canMulScalarInto_V_V_Float.register(canMulScalarInto_DV_SparseVector_Float)

  implicit val canMulScalar_DV_SparseVector_Float: BinaryOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpMulScalar, DenseVector[Float]] = pureFromUpdate_Float(canMulScalarInto_DV_SparseVector_Float)
  Vector.canMulScalar_V_V_Float.register(canMulScalar_DV_SparseVector_Float)


  class canSetInto_DV_SparseVector_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpSet] {
    def apply(a: DenseVector[Float], b: SparseVector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        var j = a.offset
        val astride = a.stride

        var i = 0
        while(i < b.length) {
          adata(j) = b(i)
          i += 1
          j += astride
        }
        
    }
  }
  implicit val canSetInto_DV_SparseVector_Float = new canSetInto_DV_SparseVector_Float ()
    
  Vector.canSetInto_V_V_Float.register(canSetInto_DV_SparseVector_Float)

  implicit val canSet_DV_SparseVector_Float: BinaryOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpSet, DenseVector[Float]] = pureFromUpdate_Float(canSetInto_DV_SparseVector_Float)
  Vector.canSet_V_V_Float.register(canSet_DV_SparseVector_Float)


  class canSubInto_DV_SparseVector_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpSub] {
    def apply(a: DenseVector[Float], b: SparseVector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        val aoff = a.offset
        val astride = a.stride

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize

        var i = 0
        while(i < bsize) {
          if(b.isActive(i)) {
                val j = aoff + bi(i) * astride
                adata(j) = adata(j) - bd(i)
          }
          i += 1
        }
        
    }
  }
  implicit val canSubInto_DV_SparseVector_Float = new canSubInto_DV_SparseVector_Float ()
    
  Vector.canSubInto_V_V_Float.register(canSubInto_DV_SparseVector_Float)

  implicit val canSub_DV_SparseVector_Float: BinaryOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpSub, DenseVector[Float]] = pureFromUpdate_Float(canSubInto_DV_SparseVector_Float)
  Vector.canSub_V_V_Float.register(canSub_DV_SparseVector_Float)


  class canPowInto_DV_SparseVector_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpPow] {
    def apply(a: DenseVector[Float], b: SparseVector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        var j = a.offset
        val astride = a.stride

        var i = 0
        while(i < b.length) {
          adata(j) = scala.math.pow(adata(j), b(i)).toFloat
          i += 1
          j += astride
        }
        
    }
  }
  implicit val canPowInto_DV_SparseVector_Float = new canPowInto_DV_SparseVector_Float ()
    
  Vector.canPowInto_V_V_Float.register(canPowInto_DV_SparseVector_Float)

  implicit val canPow_DV_SparseVector_Float: BinaryOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpPow, DenseVector[Float]] = pureFromUpdate_Float(canPowInto_DV_SparseVector_Float)
  Vector.canPow_V_V_Float.register(canPow_DV_SparseVector_Float)


  class canDivInto_DV_SparseVector_Float private[linalg] () extends BinaryUpdateOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpDiv] {
    def apply(a: DenseVector[Float], b: SparseVector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        var j = a.offset
        val astride = a.stride

        var i = 0
        while(i < b.length) {
          adata(j) = adata(j) / b(i)
          i += 1
          j += astride
        }
        
    }
  }
  implicit val canDivInto_DV_SparseVector_Float = new canDivInto_DV_SparseVector_Float ()
    
  Vector.canDivInto_V_V_Float.register(canDivInto_DV_SparseVector_Float)

  implicit val canDiv_DV_SparseVector_Float: BinaryOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpDiv, DenseVector[Float]] = pureFromUpdate_Float(canDivInto_DV_SparseVector_Float)
  Vector.canDiv_V_V_Float.register(canDiv_DV_SparseVector_Float)


   class canDotProductDV_SV_Float private[linalg] () extends BinaryOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpMulInner, Float] {
    def apply(a: DenseVector[Float], b: SparseVector[Float]) = {
      require(b.length == a.length, "Vectors must be the same length!")

      var result: Float = 0

      val bd = b.data
      val bi = b.index
      val bsize = b.iterableSize

      val adata = a.data
      val aoff = a.offset
      val stride = a.stride

      var i = 0
      if(b.allVisitableIndicesActive && stride == 1 && aoff == 0) {
         while(i < bsize) {
           result += adata(bi(i)) * bd(i)
           i += 1
         }
      } else {
         while(i < bsize) {
           if(b.isActive(i)) result += adata(aoff + bi(i) * stride) * bd(i)
           i += 1
         }
      }
      result
    }
  }; implicit val canDotProductDV_SV_Float = new canDotProductDV_SV_Float ()


   class canDotProductSV_DV_Float private[linalg] () extends BinaryOp[SparseVector[Float], DenseVector[Float], breeze.linalg.operators.OpMulInner, Float] {
    def apply(a: SparseVector[Float], b: DenseVector[Float]) = {
      require(b.length == a.length, "Vectors must be the same length!")
      canDotProductDV_SV_Float(b,a)
      
    }
  }; implicit val canDotProductSV_DV_Float = new canDotProductSV_DV_Float ()


  class canAxpy_DV_SV_Float private[linalg] () extends CanAxpy[Float, SparseVector[Float], DenseVector[Float]] {
    def apply(s: Float, b: SparseVector[Float], a: DenseVector[Float]) {
      if(s == 0) return;
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        val aoff = a.offset
        val astride = a.stride

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize

        var i = 0
        while(i < bsize) {
          if(b.isActive(i)) {
                val j = aoff + bi(i) * astride
                adata(j) = adata(j) + s * bd(i)
          }
          i += 1
        }
        
    }
  }
  implicit val canAxpy_DV_SV_Float = new canAxpy_DV_SV_Float ()
    
}

/** This is an auto-generated trait providing operators for DenseVector and SparseVector*/
trait DenseVectorOps_SparseVector_Int { this: DenseVector.type =>

  class canAddInto_DV_SparseVector_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpAdd] {
    def apply(a: DenseVector[Int], b: SparseVector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        val aoff = a.offset
        val astride = a.stride

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize

        var i = 0
        while(i < bsize) {
          if(b.isActive(i)) {
                val j = aoff + bi(i) * astride
                adata(j) = adata(j) + bd(i)
          }
          i += 1
        }
        
    }
  }
  implicit val canAddInto_DV_SparseVector_Int = new canAddInto_DV_SparseVector_Int ()
    
  Vector.canAddInto_V_V_Int.register(canAddInto_DV_SparseVector_Int)

  implicit val canAdd_DV_SparseVector_Int: BinaryOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpAdd, DenseVector[Int]] = pureFromUpdate_Int(canAddInto_DV_SparseVector_Int)
  Vector.canAdd_V_V_Int.register(canAdd_DV_SparseVector_Int)


  class canModInto_DV_SparseVector_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpMod] {
    def apply(a: DenseVector[Int], b: SparseVector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        var j = a.offset
        val astride = a.stride

        var i = 0
        while(i < b.length) {
          adata(j) = adata(j) % b(i)
          i += 1
          j += astride
        }
        
    }
  }
  implicit val canModInto_DV_SparseVector_Int = new canModInto_DV_SparseVector_Int ()
    
  Vector.canModInto_V_V_Int.register(canModInto_DV_SparseVector_Int)

  implicit val canMod_DV_SparseVector_Int: BinaryOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpMod, DenseVector[Int]] = pureFromUpdate_Int(canModInto_DV_SparseVector_Int)
  Vector.canMod_V_V_Int.register(canMod_DV_SparseVector_Int)


  class canMulScalarInto_DV_SparseVector_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpMulScalar] {
    def apply(a: DenseVector[Int], b: SparseVector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        var j = a.offset
        val astride = a.stride

        var i = 0
        while(i < b.length) {
          adata(j) = adata(j) * b(i)
          i += 1
          j += astride
        }
        
    }
  }
  implicit val canMulScalarInto_DV_SparseVector_Int = new canMulScalarInto_DV_SparseVector_Int ()
    
  Vector.canMulScalarInto_V_V_Int.register(canMulScalarInto_DV_SparseVector_Int)

  implicit val canMulScalar_DV_SparseVector_Int: BinaryOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpMulScalar, DenseVector[Int]] = pureFromUpdate_Int(canMulScalarInto_DV_SparseVector_Int)
  Vector.canMulScalar_V_V_Int.register(canMulScalar_DV_SparseVector_Int)


  class canSetInto_DV_SparseVector_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpSet] {
    def apply(a: DenseVector[Int], b: SparseVector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        var j = a.offset
        val astride = a.stride

        var i = 0
        while(i < b.length) {
          adata(j) = b(i)
          i += 1
          j += astride
        }
        
    }
  }
  implicit val canSetInto_DV_SparseVector_Int = new canSetInto_DV_SparseVector_Int ()
    
  Vector.canSetInto_V_V_Int.register(canSetInto_DV_SparseVector_Int)

  implicit val canSet_DV_SparseVector_Int: BinaryOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpSet, DenseVector[Int]] = pureFromUpdate_Int(canSetInto_DV_SparseVector_Int)
  Vector.canSet_V_V_Int.register(canSet_DV_SparseVector_Int)


  class canSubInto_DV_SparseVector_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpSub] {
    def apply(a: DenseVector[Int], b: SparseVector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        val aoff = a.offset
        val astride = a.stride

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize

        var i = 0
        while(i < bsize) {
          if(b.isActive(i)) {
                val j = aoff + bi(i) * astride
                adata(j) = adata(j) - bd(i)
          }
          i += 1
        }
        
    }
  }
  implicit val canSubInto_DV_SparseVector_Int = new canSubInto_DV_SparseVector_Int ()
    
  Vector.canSubInto_V_V_Int.register(canSubInto_DV_SparseVector_Int)

  implicit val canSub_DV_SparseVector_Int: BinaryOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpSub, DenseVector[Int]] = pureFromUpdate_Int(canSubInto_DV_SparseVector_Int)
  Vector.canSub_V_V_Int.register(canSub_DV_SparseVector_Int)


  class canPowInto_DV_SparseVector_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpPow] {
    def apply(a: DenseVector[Int], b: SparseVector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        var j = a.offset
        val astride = a.stride

        var i = 0
        while(i < b.length) {
          adata(j) = IntMath.ipow(adata(j), b(i))
          i += 1
          j += astride
        }
        
    }
  }
  implicit val canPowInto_DV_SparseVector_Int = new canPowInto_DV_SparseVector_Int ()
    
  Vector.canPowInto_V_V_Int.register(canPowInto_DV_SparseVector_Int)

  implicit val canPow_DV_SparseVector_Int: BinaryOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpPow, DenseVector[Int]] = pureFromUpdate_Int(canPowInto_DV_SparseVector_Int)
  Vector.canPow_V_V_Int.register(canPow_DV_SparseVector_Int)


  class canDivInto_DV_SparseVector_Int private[linalg] () extends BinaryUpdateOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpDiv] {
    def apply(a: DenseVector[Int], b: SparseVector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        var j = a.offset
        val astride = a.stride

        var i = 0
        while(i < b.length) {
          adata(j) = adata(j) / b(i)
          i += 1
          j += astride
        }
        
    }
  }
  implicit val canDivInto_DV_SparseVector_Int = new canDivInto_DV_SparseVector_Int ()
    
  Vector.canDivInto_V_V_Int.register(canDivInto_DV_SparseVector_Int)

  implicit val canDiv_DV_SparseVector_Int: BinaryOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpDiv, DenseVector[Int]] = pureFromUpdate_Int(canDivInto_DV_SparseVector_Int)
  Vector.canDiv_V_V_Int.register(canDiv_DV_SparseVector_Int)


   class canDotProductDV_SV_Int private[linalg] () extends BinaryOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpMulInner, Int] {
    def apply(a: DenseVector[Int], b: SparseVector[Int]) = {
      require(b.length == a.length, "Vectors must be the same length!")

      var result: Int = 0

      val bd = b.data
      val bi = b.index
      val bsize = b.iterableSize

      val adata = a.data
      val aoff = a.offset
      val stride = a.stride

      var i = 0
      if(b.allVisitableIndicesActive && stride == 1 && aoff == 0) {
         while(i < bsize) {
           result += adata(bi(i)) * bd(i)
           i += 1
         }
      } else {
         while(i < bsize) {
           if(b.isActive(i)) result += adata(aoff + bi(i) * stride) * bd(i)
           i += 1
         }
      }
      result
    }
  }; implicit val canDotProductDV_SV_Int = new canDotProductDV_SV_Int ()


   class canDotProductSV_DV_Int private[linalg] () extends BinaryOp[SparseVector[Int], DenseVector[Int], breeze.linalg.operators.OpMulInner, Int] {
    def apply(a: SparseVector[Int], b: DenseVector[Int]) = {
      require(b.length == a.length, "Vectors must be the same length!")
      canDotProductDV_SV_Int(b,a)
      
    }
  }; implicit val canDotProductSV_DV_Int = new canDotProductSV_DV_Int ()


  class canAxpy_DV_SV_Int private[linalg] () extends CanAxpy[Int, SparseVector[Int], DenseVector[Int]] {
    def apply(s: Int, b: SparseVector[Int], a: DenseVector[Int]) {
      if(s == 0) return;
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        val aoff = a.offset
        val astride = a.stride

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize

        var i = 0
        while(i < bsize) {
          if(b.isActive(i)) {
                val j = aoff + bi(i) * astride
                adata(j) = adata(j) + s * bd(i)
          }
          i += 1
        }
        
    }
  }
  implicit val canAxpy_DV_SV_Int = new canAxpy_DV_SV_Int ()
    
}

/** This is an auto-generated trait providing operators for DenseVector and SparseVector*/
trait DenseVectorOps_SparseVector_Complex { this: DenseVector.type =>

  class canAddInto_DV_SparseVector_Complex private[linalg] () extends BinaryUpdateOp[DenseVector[Complex], SparseVector[Complex], breeze.linalg.operators.OpAdd] {
    def apply(a: DenseVector[Complex], b: SparseVector[Complex]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        val aoff = a.offset
        val astride = a.stride

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize

        var i = 0
        while(i < bsize) {
          if(b.isActive(i)) {
                val j = aoff + bi(i) * astride
                adata(j) = adata(j) + bd(i)
          }
          i += 1
        }
        
    }
  }
  implicit val canAddInto_DV_SparseVector_Complex = new canAddInto_DV_SparseVector_Complex ()
    
  Vector.canAddInto_V_V_Complex.register(canAddInto_DV_SparseVector_Complex)

  implicit val canAdd_DV_SparseVector_Complex: BinaryOp[DenseVector[Complex], SparseVector[Complex], breeze.linalg.operators.OpAdd, DenseVector[Complex]] = pureFromUpdate_Complex(canAddInto_DV_SparseVector_Complex)
  Vector.canAdd_V_V_Complex.register(canAdd_DV_SparseVector_Complex)


  class canMulScalarInto_DV_SparseVector_Complex private[linalg] () extends BinaryUpdateOp[DenseVector[Complex], SparseVector[Complex], breeze.linalg.operators.OpMulScalar] {
    def apply(a: DenseVector[Complex], b: SparseVector[Complex]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        var j = a.offset
        val astride = a.stride

        var i = 0
        while(i < b.length) {
          adata(j) = adata(j) * b(i)
          i += 1
          j += astride
        }
        
    }
  }
  implicit val canMulScalarInto_DV_SparseVector_Complex = new canMulScalarInto_DV_SparseVector_Complex ()
    
  Vector.canMulScalarInto_V_V_Complex.register(canMulScalarInto_DV_SparseVector_Complex)

  implicit val canMulScalar_DV_SparseVector_Complex: BinaryOp[DenseVector[Complex], SparseVector[Complex], breeze.linalg.operators.OpMulScalar, DenseVector[Complex]] = pureFromUpdate_Complex(canMulScalarInto_DV_SparseVector_Complex)
  Vector.canMulScalar_V_V_Complex.register(canMulScalar_DV_SparseVector_Complex)


  class canSetInto_DV_SparseVector_Complex private[linalg] () extends BinaryUpdateOp[DenseVector[Complex], SparseVector[Complex], breeze.linalg.operators.OpSet] {
    def apply(a: DenseVector[Complex], b: SparseVector[Complex]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        var j = a.offset
        val astride = a.stride

        var i = 0
        while(i < b.length) {
          adata(j) = b(i)
          i += 1
          j += astride
        }
        
    }
  }
  implicit val canSetInto_DV_SparseVector_Complex = new canSetInto_DV_SparseVector_Complex ()
    
  Vector.canSetInto_V_V_Complex.register(canSetInto_DV_SparseVector_Complex)

  implicit val canSet_DV_SparseVector_Complex: BinaryOp[DenseVector[Complex], SparseVector[Complex], breeze.linalg.operators.OpSet, DenseVector[Complex]] = pureFromUpdate_Complex(canSetInto_DV_SparseVector_Complex)
  Vector.canSet_V_V_Complex.register(canSet_DV_SparseVector_Complex)


  class canSubInto_DV_SparseVector_Complex private[linalg] () extends BinaryUpdateOp[DenseVector[Complex], SparseVector[Complex], breeze.linalg.operators.OpSub] {
    def apply(a: DenseVector[Complex], b: SparseVector[Complex]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        val aoff = a.offset
        val astride = a.stride

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize

        var i = 0
        while(i < bsize) {
          if(b.isActive(i)) {
                val j = aoff + bi(i) * astride
                adata(j) = adata(j) - bd(i)
          }
          i += 1
        }
        
    }
  }
  implicit val canSubInto_DV_SparseVector_Complex = new canSubInto_DV_SparseVector_Complex ()
    
  Vector.canSubInto_V_V_Complex.register(canSubInto_DV_SparseVector_Complex)

  implicit val canSub_DV_SparseVector_Complex: BinaryOp[DenseVector[Complex], SparseVector[Complex], breeze.linalg.operators.OpSub, DenseVector[Complex]] = pureFromUpdate_Complex(canSubInto_DV_SparseVector_Complex)
  Vector.canSub_V_V_Complex.register(canSub_DV_SparseVector_Complex)


  class canPowInto_DV_SparseVector_Complex private[linalg] () extends BinaryUpdateOp[DenseVector[Complex], SparseVector[Complex], breeze.linalg.operators.OpPow] {
    def apply(a: DenseVector[Complex], b: SparseVector[Complex]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        var j = a.offset
        val astride = a.stride

        var i = 0
        while(i < b.length) {
          adata(j) = adata(j).pow(b(i))
          i += 1
          j += astride
        }
        
    }
  }
  implicit val canPowInto_DV_SparseVector_Complex = new canPowInto_DV_SparseVector_Complex ()
    
  Vector.canPowInto_V_V_Complex.register(canPowInto_DV_SparseVector_Complex)

  implicit val canPow_DV_SparseVector_Complex: BinaryOp[DenseVector[Complex], SparseVector[Complex], breeze.linalg.operators.OpPow, DenseVector[Complex]] = pureFromUpdate_Complex(canPowInto_DV_SparseVector_Complex)
  Vector.canPow_V_V_Complex.register(canPow_DV_SparseVector_Complex)


  class canDivInto_DV_SparseVector_Complex private[linalg] () extends BinaryUpdateOp[DenseVector[Complex], SparseVector[Complex], breeze.linalg.operators.OpDiv] {
    def apply(a: DenseVector[Complex], b: SparseVector[Complex]) {
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        var j = a.offset
        val astride = a.stride

        var i = 0
        while(i < b.length) {
          adata(j) = adata(j) / b(i)
          i += 1
          j += astride
        }
        
    }
  }
  implicit val canDivInto_DV_SparseVector_Complex = new canDivInto_DV_SparseVector_Complex ()
    
  Vector.canDivInto_V_V_Complex.register(canDivInto_DV_SparseVector_Complex)

  implicit val canDiv_DV_SparseVector_Complex: BinaryOp[DenseVector[Complex], SparseVector[Complex], breeze.linalg.operators.OpDiv, DenseVector[Complex]] = pureFromUpdate_Complex(canDivInto_DV_SparseVector_Complex)
  Vector.canDiv_V_V_Complex.register(canDiv_DV_SparseVector_Complex)


   class canDotProductDV_SV_Complex private[linalg] () extends BinaryOp[DenseVector[Complex], SparseVector[Complex], breeze.linalg.operators.OpMulInner, Complex] {
    def apply(a: DenseVector[Complex], b: SparseVector[Complex]) = {
      require(b.length == a.length, "Vectors must be the same length!")

      var result: Complex = Complex(0, 0)

      val bd = b.data
      val bi = b.index
      val bsize = b.iterableSize

      val adata = a.data
      val aoff = a.offset
      val stride = a.stride

      var i = 0
      if(b.allVisitableIndicesActive && stride == 1 && aoff == 0) {
         while(i < bsize) {
           result += adata(bi(i)) * bd(i)
           i += 1
         }
      } else {
         while(i < bsize) {
           if(b.isActive(i)) result += adata(aoff + bi(i) * stride) * bd(i)
           i += 1
         }
      }
      result
    }
  }; implicit val canDotProductDV_SV_Complex = new canDotProductDV_SV_Complex ()


   class canDotProductSV_DV_Complex private[linalg] () extends BinaryOp[SparseVector[Complex], DenseVector[Complex], breeze.linalg.operators.OpMulInner, Complex] {
    def apply(a: SparseVector[Complex], b: DenseVector[Complex]) = {
      require(b.length == a.length, "Vectors must be the same length!")
      canDotProductDV_SV_Complex(b,a)
      
    }
  }; implicit val canDotProductSV_DV_Complex = new canDotProductSV_DV_Complex ()


  class canAxpy_DV_SV_Complex private[linalg] () extends CanAxpy[Complex, SparseVector[Complex], DenseVector[Complex]] {
    def apply(s: Complex, b: SparseVector[Complex], a: DenseVector[Complex]) {
      if(s == 0) return;
      require(b.length == a.length, "Vectors must be the same length!")

        val adata = a.data
        val aoff = a.offset
        val astride = a.stride

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize

        var i = 0
        while(i < bsize) {
          if(b.isActive(i)) {
                val j = aoff + bi(i) * astride
                adata(j) = adata(j) + s * bd(i)
          }
          i += 1
        }
        
    }
  }
  implicit val canAxpy_DV_SV_Complex = new canAxpy_DV_SV_Complex ()
    
}
