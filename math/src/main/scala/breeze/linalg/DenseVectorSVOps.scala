package breeze.linalg
import breeze.linalg.operators._
import breeze.linalg.support._
import breeze.numerics._

/** This is an auto-generated trait providing operators for DenseVector and SparseVector*/
trait DenseVectorOps_SparseVector_Double { this: DenseVector.type =>

  implicit val canDivInto_DV_SparseVector_Double: BinaryUpdateOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpDiv] = {
    new BinaryUpdateOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpDiv] {
      def apply(a: DenseVector[Double], b: SparseVector[Double]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = a(i) / b(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canDivInto_V_V_Double.register(canDivInto_DV_SparseVector_Double)

  implicit val canDiv_DV_SparseVector_Double: BinaryOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpDiv, DenseVector[Double]] = pureFromUpdate_Double(canDivInto_DV_SparseVector_Double)
  Vector.canDiv_V_V_Double.register(canDiv_DV_SparseVector_Double)


  implicit val canSetInto_DV_SparseVector_Double: BinaryUpdateOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpSet] = {
    new BinaryUpdateOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpSet] {
      def apply(a: DenseVector[Double], b: SparseVector[Double]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = b(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canSetInto_V_V_Double.register(canSetInto_DV_SparseVector_Double)

  implicit val canSet_DV_SparseVector_Double: BinaryOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpSet, DenseVector[Double]] = pureFromUpdate_Double(canSetInto_DV_SparseVector_Double)
  Vector.canSet_V_V_Double.register(canSet_DV_SparseVector_Double)


  implicit val canSubInto_DV_SparseVector_Double: BinaryUpdateOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpSub] = {
    new BinaryUpdateOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpSub] {
      def apply(a: DenseVector[Double], b: SparseVector[Double]) {
        require(b.length == a.length, "Vectors must be the same length!")

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize
        var i = 0
        while(i < bsize) {
          if(b.isActive(i)) a(bi(i)) = a(bi(i)) - bd(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canSubInto_V_V_Double.register(canSubInto_DV_SparseVector_Double)

  implicit val canSub_DV_SparseVector_Double: BinaryOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpSub, DenseVector[Double]] = pureFromUpdate_Double(canSubInto_DV_SparseVector_Double)
  Vector.canSub_V_V_Double.register(canSub_DV_SparseVector_Double)


  implicit val canModInto_DV_SparseVector_Double: BinaryUpdateOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpMod] = {
    new BinaryUpdateOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpMod] {
      def apply(a: DenseVector[Double], b: SparseVector[Double]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = a(i) % b(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canModInto_V_V_Double.register(canModInto_DV_SparseVector_Double)

  implicit val canMod_DV_SparseVector_Double: BinaryOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpMod, DenseVector[Double]] = pureFromUpdate_Double(canModInto_DV_SparseVector_Double)
  Vector.canMod_V_V_Double.register(canMod_DV_SparseVector_Double)


  implicit val canPowInto_DV_SparseVector_Double: BinaryUpdateOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpPow] = {
    new BinaryUpdateOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpPow] {
      def apply(a: DenseVector[Double], b: SparseVector[Double]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = scala.math.pow(a(i), b(i))
          i += 1
        }
        
      }
    }
  }

  Vector.canPowInto_V_V_Double.register(canPowInto_DV_SparseVector_Double)

  implicit val canPow_DV_SparseVector_Double: BinaryOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpPow, DenseVector[Double]] = pureFromUpdate_Double(canPowInto_DV_SparseVector_Double)
  Vector.canPow_V_V_Double.register(canPow_DV_SparseVector_Double)


  implicit val canAddInto_DV_SparseVector_Double: BinaryUpdateOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpAdd] = {
    new BinaryUpdateOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpAdd] {
      def apply(a: DenseVector[Double], b: SparseVector[Double]) {
        require(b.length == a.length, "Vectors must be the same length!")

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize
        var i = 0
        while(i < bsize) {
          if(b.isActive(i)) a(bi(i)) = a(bi(i)) + bd(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canAddInto_V_V_Double.register(canAddInto_DV_SparseVector_Double)

  implicit val canAdd_DV_SparseVector_Double: BinaryOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpAdd, DenseVector[Double]] = pureFromUpdate_Double(canAddInto_DV_SparseVector_Double)
  Vector.canAdd_V_V_Double.register(canAdd_DV_SparseVector_Double)


  implicit val canMulScalarInto_DV_SparseVector_Double: BinaryUpdateOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpMulScalar] = {
    new BinaryUpdateOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpMulScalar] {
      def apply(a: DenseVector[Double], b: SparseVector[Double]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = a(i) * b(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canMulScalarInto_V_V_Double.register(canMulScalarInto_DV_SparseVector_Double)

  implicit val canMulScalar_DV_SparseVector_Double: BinaryOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpMulScalar, DenseVector[Double]] = pureFromUpdate_Double(canMulScalarInto_DV_SparseVector_Double)
  Vector.canMulScalar_V_V_Double.register(canMulScalar_DV_SparseVector_Double)


  implicit val canDotProductDV_SV_Double: BinaryOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpMulInner, Double] = {
    new BinaryOp[DenseVector[Double], SparseVector[Double], breeze.linalg.operators.OpMulInner, Double] {
      def apply(a: DenseVector[Double], b: SparseVector[Double]) = {
        require(b.length == a.length, "Vectors must be the same length!")

       var result: Double = 0

         val bd = b.data
         val bi = b.index
         val bsize = b.iterableSize
         var i = 0
         while(i < b.size) {
           if(b.isActive(i)) result += a(bi(i)) * bd(i)
           i += 1
         }
         result
      }
    }
  }

}

/** This is an auto-generated trait providing operators for DenseVector and SparseVector*/
trait DenseVectorOps_SparseVector_Float { this: DenseVector.type =>

  implicit val canDivInto_DV_SparseVector_Float: BinaryUpdateOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpDiv] = {
    new BinaryUpdateOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpDiv] {
      def apply(a: DenseVector[Float], b: SparseVector[Float]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = a(i) / b(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canDivInto_V_V_Float.register(canDivInto_DV_SparseVector_Float)

  implicit val canDiv_DV_SparseVector_Float: BinaryOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpDiv, DenseVector[Float]] = pureFromUpdate_Float(canDivInto_DV_SparseVector_Float)
  Vector.canDiv_V_V_Float.register(canDiv_DV_SparseVector_Float)


  implicit val canSetInto_DV_SparseVector_Float: BinaryUpdateOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpSet] = {
    new BinaryUpdateOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpSet] {
      def apply(a: DenseVector[Float], b: SparseVector[Float]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = b(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canSetInto_V_V_Float.register(canSetInto_DV_SparseVector_Float)

  implicit val canSet_DV_SparseVector_Float: BinaryOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpSet, DenseVector[Float]] = pureFromUpdate_Float(canSetInto_DV_SparseVector_Float)
  Vector.canSet_V_V_Float.register(canSet_DV_SparseVector_Float)


  implicit val canSubInto_DV_SparseVector_Float: BinaryUpdateOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpSub] = {
    new BinaryUpdateOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpSub] {
      def apply(a: DenseVector[Float], b: SparseVector[Float]) {
        require(b.length == a.length, "Vectors must be the same length!")

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize
        var i = 0
        while(i < bsize) {
          if(b.isActive(i)) a(bi(i)) = a(bi(i)) - bd(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canSubInto_V_V_Float.register(canSubInto_DV_SparseVector_Float)

  implicit val canSub_DV_SparseVector_Float: BinaryOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpSub, DenseVector[Float]] = pureFromUpdate_Float(canSubInto_DV_SparseVector_Float)
  Vector.canSub_V_V_Float.register(canSub_DV_SparseVector_Float)


  implicit val canModInto_DV_SparseVector_Float: BinaryUpdateOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpMod] = {
    new BinaryUpdateOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpMod] {
      def apply(a: DenseVector[Float], b: SparseVector[Float]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = a(i) % b(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canModInto_V_V_Float.register(canModInto_DV_SparseVector_Float)

  implicit val canMod_DV_SparseVector_Float: BinaryOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpMod, DenseVector[Float]] = pureFromUpdate_Float(canModInto_DV_SparseVector_Float)
  Vector.canMod_V_V_Float.register(canMod_DV_SparseVector_Float)


  implicit val canPowInto_DV_SparseVector_Float: BinaryUpdateOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpPow] = {
    new BinaryUpdateOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpPow] {
      def apply(a: DenseVector[Float], b: SparseVector[Float]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = scala.math.pow(a(i), b(i)).toFloat
          i += 1
        }
        
      }
    }
  }

  Vector.canPowInto_V_V_Float.register(canPowInto_DV_SparseVector_Float)

  implicit val canPow_DV_SparseVector_Float: BinaryOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpPow, DenseVector[Float]] = pureFromUpdate_Float(canPowInto_DV_SparseVector_Float)
  Vector.canPow_V_V_Float.register(canPow_DV_SparseVector_Float)


  implicit val canAddInto_DV_SparseVector_Float: BinaryUpdateOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpAdd] = {
    new BinaryUpdateOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpAdd] {
      def apply(a: DenseVector[Float], b: SparseVector[Float]) {
        require(b.length == a.length, "Vectors must be the same length!")

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize
        var i = 0
        while(i < bsize) {
          if(b.isActive(i)) a(bi(i)) = a(bi(i)) + bd(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canAddInto_V_V_Float.register(canAddInto_DV_SparseVector_Float)

  implicit val canAdd_DV_SparseVector_Float: BinaryOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpAdd, DenseVector[Float]] = pureFromUpdate_Float(canAddInto_DV_SparseVector_Float)
  Vector.canAdd_V_V_Float.register(canAdd_DV_SparseVector_Float)


  implicit val canMulScalarInto_DV_SparseVector_Float: BinaryUpdateOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpMulScalar] = {
    new BinaryUpdateOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpMulScalar] {
      def apply(a: DenseVector[Float], b: SparseVector[Float]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = a(i) * b(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canMulScalarInto_V_V_Float.register(canMulScalarInto_DV_SparseVector_Float)

  implicit val canMulScalar_DV_SparseVector_Float: BinaryOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpMulScalar, DenseVector[Float]] = pureFromUpdate_Float(canMulScalarInto_DV_SparseVector_Float)
  Vector.canMulScalar_V_V_Float.register(canMulScalar_DV_SparseVector_Float)


  implicit val canDotProductDV_SV_Float: BinaryOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpMulInner, Float] = {
    new BinaryOp[DenseVector[Float], SparseVector[Float], breeze.linalg.operators.OpMulInner, Float] {
      def apply(a: DenseVector[Float], b: SparseVector[Float]) = {
        require(b.length == a.length, "Vectors must be the same length!")

       var result: Float = 0

         val bd = b.data
         val bi = b.index
         val bsize = b.iterableSize
         var i = 0
         while(i < b.size) {
           if(b.isActive(i)) result += a(bi(i)) * bd(i)
           i += 1
         }
         result
      }
    }
  }

}

/** This is an auto-generated trait providing operators for DenseVector and SparseVector*/
trait DenseVectorOps_SparseVector_Int { this: DenseVector.type =>

  implicit val canDivInto_DV_SparseVector_Int: BinaryUpdateOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpDiv] = {
    new BinaryUpdateOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpDiv] {
      def apply(a: DenseVector[Int], b: SparseVector[Int]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = a(i) / b(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canDivInto_V_V_Int.register(canDivInto_DV_SparseVector_Int)

  implicit val canDiv_DV_SparseVector_Int: BinaryOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpDiv, DenseVector[Int]] = pureFromUpdate_Int(canDivInto_DV_SparseVector_Int)
  Vector.canDiv_V_V_Int.register(canDiv_DV_SparseVector_Int)


  implicit val canSetInto_DV_SparseVector_Int: BinaryUpdateOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpSet] = {
    new BinaryUpdateOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpSet] {
      def apply(a: DenseVector[Int], b: SparseVector[Int]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = b(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canSetInto_V_V_Int.register(canSetInto_DV_SparseVector_Int)

  implicit val canSet_DV_SparseVector_Int: BinaryOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpSet, DenseVector[Int]] = pureFromUpdate_Int(canSetInto_DV_SparseVector_Int)
  Vector.canSet_V_V_Int.register(canSet_DV_SparseVector_Int)


  implicit val canSubInto_DV_SparseVector_Int: BinaryUpdateOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpSub] = {
    new BinaryUpdateOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpSub] {
      def apply(a: DenseVector[Int], b: SparseVector[Int]) {
        require(b.length == a.length, "Vectors must be the same length!")

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize
        var i = 0
        while(i < bsize) {
          if(b.isActive(i)) a(bi(i)) = a(bi(i)) - bd(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canSubInto_V_V_Int.register(canSubInto_DV_SparseVector_Int)

  implicit val canSub_DV_SparseVector_Int: BinaryOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpSub, DenseVector[Int]] = pureFromUpdate_Int(canSubInto_DV_SparseVector_Int)
  Vector.canSub_V_V_Int.register(canSub_DV_SparseVector_Int)


  implicit val canModInto_DV_SparseVector_Int: BinaryUpdateOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpMod] = {
    new BinaryUpdateOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpMod] {
      def apply(a: DenseVector[Int], b: SparseVector[Int]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = a(i) % b(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canModInto_V_V_Int.register(canModInto_DV_SparseVector_Int)

  implicit val canMod_DV_SparseVector_Int: BinaryOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpMod, DenseVector[Int]] = pureFromUpdate_Int(canModInto_DV_SparseVector_Int)
  Vector.canMod_V_V_Int.register(canMod_DV_SparseVector_Int)


  implicit val canPowInto_DV_SparseVector_Int: BinaryUpdateOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpPow] = {
    new BinaryUpdateOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpPow] {
      def apply(a: DenseVector[Int], b: SparseVector[Int]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = IntMath.ipow(a(i), b(i))
          i += 1
        }
        
      }
    }
  }

  Vector.canPowInto_V_V_Int.register(canPowInto_DV_SparseVector_Int)

  implicit val canPow_DV_SparseVector_Int: BinaryOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpPow, DenseVector[Int]] = pureFromUpdate_Int(canPowInto_DV_SparseVector_Int)
  Vector.canPow_V_V_Int.register(canPow_DV_SparseVector_Int)


  implicit val canAddInto_DV_SparseVector_Int: BinaryUpdateOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpAdd] = {
    new BinaryUpdateOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpAdd] {
      def apply(a: DenseVector[Int], b: SparseVector[Int]) {
        require(b.length == a.length, "Vectors must be the same length!")

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize
        var i = 0
        while(i < bsize) {
          if(b.isActive(i)) a(bi(i)) = a(bi(i)) + bd(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canAddInto_V_V_Int.register(canAddInto_DV_SparseVector_Int)

  implicit val canAdd_DV_SparseVector_Int: BinaryOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpAdd, DenseVector[Int]] = pureFromUpdate_Int(canAddInto_DV_SparseVector_Int)
  Vector.canAdd_V_V_Int.register(canAdd_DV_SparseVector_Int)


  implicit val canMulScalarInto_DV_SparseVector_Int: BinaryUpdateOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpMulScalar] = {
    new BinaryUpdateOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpMulScalar] {
      def apply(a: DenseVector[Int], b: SparseVector[Int]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = a(i) * b(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canMulScalarInto_V_V_Int.register(canMulScalarInto_DV_SparseVector_Int)

  implicit val canMulScalar_DV_SparseVector_Int: BinaryOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpMulScalar, DenseVector[Int]] = pureFromUpdate_Int(canMulScalarInto_DV_SparseVector_Int)
  Vector.canMulScalar_V_V_Int.register(canMulScalar_DV_SparseVector_Int)


  implicit val canDotProductDV_SV_Int: BinaryOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpMulInner, Int] = {
    new BinaryOp[DenseVector[Int], SparseVector[Int], breeze.linalg.operators.OpMulInner, Int] {
      def apply(a: DenseVector[Int], b: SparseVector[Int]) = {
        require(b.length == a.length, "Vectors must be the same length!")

       var result: Int = 0

         val bd = b.data
         val bi = b.index
         val bsize = b.iterableSize
         var i = 0
         while(i < b.size) {
           if(b.isActive(i)) result += a(bi(i)) * bd(i)
           i += 1
         }
         result
      }
    }
  }

}
