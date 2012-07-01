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
        

  class canSetInto_V_V_Double extends BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: Vector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = v
        }
        
    }
  }; implicit val canSetInto_V_V_Double = new canSetInto_V_V_Double ()


  implicit val canSet_V_V_Double: BinaryRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpSet, Vector[Double]] = pureFromUpdate_Double(canSetInto_V_V_Double)


  class canSetInto_V_S_Double extends BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: Vector[Double], b: Double) {
      
        for( (i,v) <- a.iterator) {
          a(i) = b
        }
        
    }
  }; implicit val canSetInto_V_S_Double = new canSetInto_V_S_Double ()


  implicit val canSet_V_S_Double: BinaryRegistry[Vector[Double], Double, breeze.linalg.operators.OpSet, Vector[Double]] = pureFromUpdate_Double(canSetInto_V_S_Double)


  class canDivInto_V_V_Double extends BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: Vector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) / v
        }
        
    }
  }; implicit val canDivInto_V_V_Double = new canDivInto_V_V_Double ()


  implicit val canDiv_V_V_Double: BinaryRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpDiv, Vector[Double]] = pureFromUpdate_Double(canDivInto_V_V_Double)


  class canDivInto_V_S_Double extends BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: Vector[Double], b: Double) {
      
        for( (i,v) <- a.iterator) {
          a(i) = v / b
        }
        
    }
  }; implicit val canDivInto_V_S_Double = new canDivInto_V_S_Double ()


  implicit val canDiv_V_S_Double: BinaryRegistry[Vector[Double], Double, breeze.linalg.operators.OpDiv, Vector[Double]] = pureFromUpdate_Double(canDivInto_V_S_Double)


  class canPowInto_V_V_Double extends BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: Vector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = scala.math.pow(a(i), v)
        }
        
    }
  }; implicit val canPowInto_V_V_Double = new canPowInto_V_V_Double ()


  implicit val canPow_V_V_Double: BinaryRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpPow, Vector[Double]] = pureFromUpdate_Double(canPowInto_V_V_Double)


  class canPowInto_V_S_Double extends BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: Vector[Double], b: Double) {
      
        for( (i,v) <- a.iterator) {
          a(i) = scala.math.pow(v, b)
        }
        
    }
  }; implicit val canPowInto_V_S_Double = new canPowInto_V_S_Double ()


  implicit val canPow_V_S_Double: BinaryRegistry[Vector[Double], Double, breeze.linalg.operators.OpPow, Vector[Double]] = pureFromUpdate_Double(canPowInto_V_S_Double)


  class canModInto_V_V_Double extends BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpMod] {
    override def bindingMissing(a: Vector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) % v
        }
        
    }
  }; implicit val canModInto_V_V_Double = new canModInto_V_V_Double ()


  implicit val canMod_V_V_Double: BinaryRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpMod, Vector[Double]] = pureFromUpdate_Double(canModInto_V_V_Double)


  class canModInto_V_S_Double extends BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpMod] {
    override def bindingMissing(a: Vector[Double], b: Double) {
      
        for( (i,v) <- a.iterator) {
          a(i) = v % b
        }
        
    }
  }; implicit val canModInto_V_S_Double = new canModInto_V_S_Double ()


  implicit val canMod_V_S_Double: BinaryRegistry[Vector[Double], Double, breeze.linalg.operators.OpMod, Vector[Double]] = pureFromUpdate_Double(canModInto_V_S_Double)


  class canAddInto_V_V_Double extends BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: Vector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) + v
        }
        
    }
  }; implicit val canAddInto_V_V_Double = new canAddInto_V_V_Double ()


  implicit val canAdd_V_V_Double: BinaryRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpAdd, Vector[Double]] = pureFromUpdate_Double(canAddInto_V_V_Double)


  class canAddInto_V_S_Double extends BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: Vector[Double], b: Double) {
      
        for( (i,v) <- a.activeIterator) {
          a(i) = v + b
        }
        
    }
  }; implicit val canAddInto_V_S_Double = new canAddInto_V_S_Double ()


  implicit val canAdd_V_S_Double: BinaryRegistry[Vector[Double], Double, breeze.linalg.operators.OpAdd, Vector[Double]] = pureFromUpdate_Double(canAddInto_V_S_Double)


  class canMulScalarInto_V_V_Double extends BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: Vector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) * v
        }
        
    }
  }; implicit val canMulScalarInto_V_V_Double = new canMulScalarInto_V_V_Double ()


  implicit val canMulScalar_V_V_Double: BinaryRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpMulScalar, Vector[Double]] = pureFromUpdate_Double(canMulScalarInto_V_V_Double)


  class canMulScalarInto_V_S_Double extends BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: Vector[Double], b: Double) {
      
        for( (i,v) <- a.iterator) {
          a(i) = v * b
        }
        
    }
  }; implicit val canMulScalarInto_V_S_Double = new canMulScalarInto_V_S_Double ()


  implicit val canMulScalar_V_S_Double: BinaryRegistry[Vector[Double], Double, breeze.linalg.operators.OpMulScalar, Vector[Double]] = pureFromUpdate_Double(canMulScalarInto_V_S_Double)


  class canSubInto_V_V_Double extends BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: Vector[Double], b: Vector[Double]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) - v
        }
        
    }
  }; implicit val canSubInto_V_V_Double = new canSubInto_V_V_Double ()


  implicit val canSub_V_V_Double: BinaryRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpSub, Vector[Double]] = pureFromUpdate_Double(canSubInto_V_V_Double)


  class canSubInto_V_S_Double extends BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: Vector[Double], b: Double) {
      
        for( (i,v) <- a.activeIterator) {
          a(i) = v - b
        }
        
    }
  }; implicit val canSubInto_V_S_Double = new canSubInto_V_S_Double ()


  implicit val canSub_V_S_Double: BinaryRegistry[Vector[Double], Double, breeze.linalg.operators.OpSub, Vector[Double]] = pureFromUpdate_Double(canSubInto_V_S_Double)


  class canDotProductV_Double extends BinaryRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpMulInner, Double] {
    override def bindingMissing(a: Vector[Double], b: Vector[Double]) = {
      require(b.length == a.length, "Vectors must be the same length!")

       var result: Double = 0

         for( (i, v) <- b.activeIterator) {
           result += a(i) * v
         }
         result
    }
  }; implicit val canDotProductV_Double = new canDotProductV_Double()

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
        

  class canSetInto_V_V_Float extends BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: Vector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = v
        }
        
    }
  }; implicit val canSetInto_V_V_Float = new canSetInto_V_V_Float ()


  implicit val canSet_V_V_Float: BinaryRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpSet, Vector[Float]] = pureFromUpdate_Float(canSetInto_V_V_Float)


  class canSetInto_V_S_Float extends BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: Vector[Float], b: Float) {
      
        for( (i,v) <- a.iterator) {
          a(i) = b
        }
        
    }
  }; implicit val canSetInto_V_S_Float = new canSetInto_V_S_Float ()


  implicit val canSet_V_S_Float: BinaryRegistry[Vector[Float], Float, breeze.linalg.operators.OpSet, Vector[Float]] = pureFromUpdate_Float(canSetInto_V_S_Float)


  class canDivInto_V_V_Float extends BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: Vector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) / v
        }
        
    }
  }; implicit val canDivInto_V_V_Float = new canDivInto_V_V_Float ()


  implicit val canDiv_V_V_Float: BinaryRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpDiv, Vector[Float]] = pureFromUpdate_Float(canDivInto_V_V_Float)


  class canDivInto_V_S_Float extends BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: Vector[Float], b: Float) {
      
        for( (i,v) <- a.iterator) {
          a(i) = v / b
        }
        
    }
  }; implicit val canDivInto_V_S_Float = new canDivInto_V_S_Float ()


  implicit val canDiv_V_S_Float: BinaryRegistry[Vector[Float], Float, breeze.linalg.operators.OpDiv, Vector[Float]] = pureFromUpdate_Float(canDivInto_V_S_Float)


  class canPowInto_V_V_Float extends BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: Vector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = scala.math.pow(a(i), v).toFloat
        }
        
    }
  }; implicit val canPowInto_V_V_Float = new canPowInto_V_V_Float ()


  implicit val canPow_V_V_Float: BinaryRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpPow, Vector[Float]] = pureFromUpdate_Float(canPowInto_V_V_Float)


  class canPowInto_V_S_Float extends BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: Vector[Float], b: Float) {
      
        for( (i,v) <- a.iterator) {
          a(i) = scala.math.pow(v, b).toFloat
        }
        
    }
  }; implicit val canPowInto_V_S_Float = new canPowInto_V_S_Float ()


  implicit val canPow_V_S_Float: BinaryRegistry[Vector[Float], Float, breeze.linalg.operators.OpPow, Vector[Float]] = pureFromUpdate_Float(canPowInto_V_S_Float)


  class canModInto_V_V_Float extends BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpMod] {
    override def bindingMissing(a: Vector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) % v
        }
        
    }
  }; implicit val canModInto_V_V_Float = new canModInto_V_V_Float ()


  implicit val canMod_V_V_Float: BinaryRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpMod, Vector[Float]] = pureFromUpdate_Float(canModInto_V_V_Float)


  class canModInto_V_S_Float extends BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpMod] {
    override def bindingMissing(a: Vector[Float], b: Float) {
      
        for( (i,v) <- a.iterator) {
          a(i) = v % b
        }
        
    }
  }; implicit val canModInto_V_S_Float = new canModInto_V_S_Float ()


  implicit val canMod_V_S_Float: BinaryRegistry[Vector[Float], Float, breeze.linalg.operators.OpMod, Vector[Float]] = pureFromUpdate_Float(canModInto_V_S_Float)


  class canAddInto_V_V_Float extends BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: Vector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) + v
        }
        
    }
  }; implicit val canAddInto_V_V_Float = new canAddInto_V_V_Float ()


  implicit val canAdd_V_V_Float: BinaryRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpAdd, Vector[Float]] = pureFromUpdate_Float(canAddInto_V_V_Float)


  class canAddInto_V_S_Float extends BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: Vector[Float], b: Float) {
      
        for( (i,v) <- a.activeIterator) {
          a(i) = v + b
        }
        
    }
  }; implicit val canAddInto_V_S_Float = new canAddInto_V_S_Float ()


  implicit val canAdd_V_S_Float: BinaryRegistry[Vector[Float], Float, breeze.linalg.operators.OpAdd, Vector[Float]] = pureFromUpdate_Float(canAddInto_V_S_Float)


  class canMulScalarInto_V_V_Float extends BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: Vector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) * v
        }
        
    }
  }; implicit val canMulScalarInto_V_V_Float = new canMulScalarInto_V_V_Float ()


  implicit val canMulScalar_V_V_Float: BinaryRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpMulScalar, Vector[Float]] = pureFromUpdate_Float(canMulScalarInto_V_V_Float)


  class canMulScalarInto_V_S_Float extends BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: Vector[Float], b: Float) {
      
        for( (i,v) <- a.iterator) {
          a(i) = v * b
        }
        
    }
  }; implicit val canMulScalarInto_V_S_Float = new canMulScalarInto_V_S_Float ()


  implicit val canMulScalar_V_S_Float: BinaryRegistry[Vector[Float], Float, breeze.linalg.operators.OpMulScalar, Vector[Float]] = pureFromUpdate_Float(canMulScalarInto_V_S_Float)


  class canSubInto_V_V_Float extends BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: Vector[Float], b: Vector[Float]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) - v
        }
        
    }
  }; implicit val canSubInto_V_V_Float = new canSubInto_V_V_Float ()


  implicit val canSub_V_V_Float: BinaryRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpSub, Vector[Float]] = pureFromUpdate_Float(canSubInto_V_V_Float)


  class canSubInto_V_S_Float extends BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: Vector[Float], b: Float) {
      
        for( (i,v) <- a.activeIterator) {
          a(i) = v - b
        }
        
    }
  }; implicit val canSubInto_V_S_Float = new canSubInto_V_S_Float ()


  implicit val canSub_V_S_Float: BinaryRegistry[Vector[Float], Float, breeze.linalg.operators.OpSub, Vector[Float]] = pureFromUpdate_Float(canSubInto_V_S_Float)


  class canDotProductV_Float extends BinaryRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpMulInner, Float] {
    override def bindingMissing(a: Vector[Float], b: Vector[Float]) = {
      require(b.length == a.length, "Vectors must be the same length!")

       var result: Float = 0

         for( (i, v) <- b.activeIterator) {
           result += a(i) * v
         }
         result
    }
  }; implicit val canDotProductV_Float = new canDotProductV_Float()

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
        

  class canSetInto_V_V_Int extends BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: Vector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = v
        }
        
    }
  }; implicit val canSetInto_V_V_Int = new canSetInto_V_V_Int ()


  implicit val canSet_V_V_Int: BinaryRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpSet, Vector[Int]] = pureFromUpdate_Int(canSetInto_V_V_Int)


  class canSetInto_V_S_Int extends BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpSet] {
    override def bindingMissing(a: Vector[Int], b: Int) {
      
        for( (i,v) <- a.iterator) {
          a(i) = b
        }
        
    }
  }; implicit val canSetInto_V_S_Int = new canSetInto_V_S_Int ()


  implicit val canSet_V_S_Int: BinaryRegistry[Vector[Int], Int, breeze.linalg.operators.OpSet, Vector[Int]] = pureFromUpdate_Int(canSetInto_V_S_Int)


  class canDivInto_V_V_Int extends BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: Vector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) / v
        }
        
    }
  }; implicit val canDivInto_V_V_Int = new canDivInto_V_V_Int ()


  implicit val canDiv_V_V_Int: BinaryRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpDiv, Vector[Int]] = pureFromUpdate_Int(canDivInto_V_V_Int)


  class canDivInto_V_S_Int extends BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpDiv] {
    override def bindingMissing(a: Vector[Int], b: Int) {
      
        for( (i,v) <- a.iterator) {
          a(i) = v / b
        }
        
    }
  }; implicit val canDivInto_V_S_Int = new canDivInto_V_S_Int ()


  implicit val canDiv_V_S_Int: BinaryRegistry[Vector[Int], Int, breeze.linalg.operators.OpDiv, Vector[Int]] = pureFromUpdate_Int(canDivInto_V_S_Int)


  class canPowInto_V_V_Int extends BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: Vector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = IntMath.ipow(a(i), v)
        }
        
    }
  }; implicit val canPowInto_V_V_Int = new canPowInto_V_V_Int ()


  implicit val canPow_V_V_Int: BinaryRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpPow, Vector[Int]] = pureFromUpdate_Int(canPowInto_V_V_Int)


  class canPowInto_V_S_Int extends BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpPow] {
    override def bindingMissing(a: Vector[Int], b: Int) {
      
        for( (i,v) <- a.iterator) {
          a(i) = IntMath.ipow(v, b)
        }
        
    }
  }; implicit val canPowInto_V_S_Int = new canPowInto_V_S_Int ()


  implicit val canPow_V_S_Int: BinaryRegistry[Vector[Int], Int, breeze.linalg.operators.OpPow, Vector[Int]] = pureFromUpdate_Int(canPowInto_V_S_Int)


  class canModInto_V_V_Int extends BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpMod] {
    override def bindingMissing(a: Vector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) % v
        }
        
    }
  }; implicit val canModInto_V_V_Int = new canModInto_V_V_Int ()


  implicit val canMod_V_V_Int: BinaryRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpMod, Vector[Int]] = pureFromUpdate_Int(canModInto_V_V_Int)


  class canModInto_V_S_Int extends BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpMod] {
    override def bindingMissing(a: Vector[Int], b: Int) {
      
        for( (i,v) <- a.iterator) {
          a(i) = v % b
        }
        
    }
  }; implicit val canModInto_V_S_Int = new canModInto_V_S_Int ()


  implicit val canMod_V_S_Int: BinaryRegistry[Vector[Int], Int, breeze.linalg.operators.OpMod, Vector[Int]] = pureFromUpdate_Int(canModInto_V_S_Int)


  class canAddInto_V_V_Int extends BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: Vector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) + v
        }
        
    }
  }; implicit val canAddInto_V_V_Int = new canAddInto_V_V_Int ()


  implicit val canAdd_V_V_Int: BinaryRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpAdd, Vector[Int]] = pureFromUpdate_Int(canAddInto_V_V_Int)


  class canAddInto_V_S_Int extends BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpAdd] {
    override def bindingMissing(a: Vector[Int], b: Int) {
      
        for( (i,v) <- a.activeIterator) {
          a(i) = v + b
        }
        
    }
  }; implicit val canAddInto_V_S_Int = new canAddInto_V_S_Int ()


  implicit val canAdd_V_S_Int: BinaryRegistry[Vector[Int], Int, breeze.linalg.operators.OpAdd, Vector[Int]] = pureFromUpdate_Int(canAddInto_V_S_Int)


  class canMulScalarInto_V_V_Int extends BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: Vector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) * v
        }
        
    }
  }; implicit val canMulScalarInto_V_V_Int = new canMulScalarInto_V_V_Int ()


  implicit val canMulScalar_V_V_Int: BinaryRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpMulScalar, Vector[Int]] = pureFromUpdate_Int(canMulScalarInto_V_V_Int)


  class canMulScalarInto_V_S_Int extends BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpMulScalar] {
    override def bindingMissing(a: Vector[Int], b: Int) {
      
        for( (i,v) <- a.iterator) {
          a(i) = v * b
        }
        
    }
  }; implicit val canMulScalarInto_V_S_Int = new canMulScalarInto_V_S_Int ()


  implicit val canMulScalar_V_S_Int: BinaryRegistry[Vector[Int], Int, breeze.linalg.operators.OpMulScalar, Vector[Int]] = pureFromUpdate_Int(canMulScalarInto_V_S_Int)


  class canSubInto_V_V_Int extends BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: Vector[Int], b: Vector[Int]) {
      require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) - v
        }
        
    }
  }; implicit val canSubInto_V_V_Int = new canSubInto_V_V_Int ()


  implicit val canSub_V_V_Int: BinaryRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpSub, Vector[Int]] = pureFromUpdate_Int(canSubInto_V_V_Int)


  class canSubInto_V_S_Int extends BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpSub] {
    override def bindingMissing(a: Vector[Int], b: Int) {
      
        for( (i,v) <- a.activeIterator) {
          a(i) = v - b
        }
        
    }
  }; implicit val canSubInto_V_S_Int = new canSubInto_V_S_Int ()


  implicit val canSub_V_S_Int: BinaryRegistry[Vector[Int], Int, breeze.linalg.operators.OpSub, Vector[Int]] = pureFromUpdate_Int(canSubInto_V_S_Int)


  class canDotProductV_Int extends BinaryRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpMulInner, Int] {
    override def bindingMissing(a: Vector[Int], b: Vector[Int]) = {
      require(b.length == a.length, "Vectors must be the same length!")

       var result: Int = 0

         for( (i, v) <- b.activeIterator) {
           result += a(i) * v
         }
         result
    }
  }; implicit val canDotProductV_Int = new canDotProductV_Int()

}
