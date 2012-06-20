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
        

  implicit val canPowInto_V_V_Double: BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpPow] = {
    new BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpPow] {
      override def bindingMissing(a: Vector[Double], b: Vector[Double]) {
        require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = scala.math.pow(a(i), v)
        }
        
      }
    }
  }


  implicit val canPow_V_V_Double: BinaryOp[Vector[Double], Vector[Double], breeze.linalg.operators.OpPow, Vector[Double]] = pureFromUpdate_Double(canPowInto_V_V_Double)


  implicit val canPowInto_V_S_Double: BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpPow] = {
    new BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpPow] {
      override def bindingMissing(a: Vector[Double], b: Double) {
        
        for( (i,v) <- a.iterator) {
          a(i) = scala.math.pow(v, b)
        }
        
      }
    }
  }


  implicit val canPow_V_S_Double: BinaryOp[Vector[Double], Double, breeze.linalg.operators.OpPow, Vector[Double]] = pureFromUpdate_Double(canPowInto_V_S_Double)


  implicit val canSetInto_V_V_Double: BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpSet] = {
    new BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpSet] {
      override def bindingMissing(a: Vector[Double], b: Vector[Double]) {
        require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = v
        }
        
      }
    }
  }


  implicit val canSet_V_V_Double: BinaryOp[Vector[Double], Vector[Double], breeze.linalg.operators.OpSet, Vector[Double]] = pureFromUpdate_Double(canSetInto_V_V_Double)


  implicit val canSetInto_V_S_Double: BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpSet] = {
    new BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpSet] {
      override def bindingMissing(a: Vector[Double], b: Double) {
        
        for( (i,v) <- a.iterator) {
          a(i) = b
        }
        
      }
    }
  }


  implicit val canSet_V_S_Double: BinaryOp[Vector[Double], Double, breeze.linalg.operators.OpSet, Vector[Double]] = pureFromUpdate_Double(canSetInto_V_S_Double)


  implicit val canModInto_V_V_Double: BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpMod] = {
    new BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpMod] {
      override def bindingMissing(a: Vector[Double], b: Vector[Double]) {
        require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) % v
        }
        
      }
    }
  }


  implicit val canMod_V_V_Double: BinaryOp[Vector[Double], Vector[Double], breeze.linalg.operators.OpMod, Vector[Double]] = pureFromUpdate_Double(canModInto_V_V_Double)


  implicit val canModInto_V_S_Double: BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpMod] = {
    new BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpMod] {
      override def bindingMissing(a: Vector[Double], b: Double) {
        
        for( (i,v) <- a.iterator) {
          a(i) = v % b
        }
        
      }
    }
  }


  implicit val canMod_V_S_Double: BinaryOp[Vector[Double], Double, breeze.linalg.operators.OpMod, Vector[Double]] = pureFromUpdate_Double(canModInto_V_S_Double)


  implicit val canMulScalarInto_V_V_Double: BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpMulScalar] = {
    new BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpMulScalar] {
      override def bindingMissing(a: Vector[Double], b: Vector[Double]) {
        require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) * v
        }
        
      }
    }
  }


  implicit val canMulScalar_V_V_Double: BinaryOp[Vector[Double], Vector[Double], breeze.linalg.operators.OpMulScalar, Vector[Double]] = pureFromUpdate_Double(canMulScalarInto_V_V_Double)


  implicit val canMulScalarInto_V_S_Double: BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpMulScalar] = {
    new BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpMulScalar] {
      override def bindingMissing(a: Vector[Double], b: Double) {
        
        for( (i,v) <- a.iterator) {
          a(i) = v * b
        }
        
      }
    }
  }


  implicit val canMulScalar_V_S_Double: BinaryOp[Vector[Double], Double, breeze.linalg.operators.OpMulScalar, Vector[Double]] = pureFromUpdate_Double(canMulScalarInto_V_S_Double)


  implicit val canAddInto_V_V_Double: BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpAdd] = {
    new BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpAdd] {
      override def bindingMissing(a: Vector[Double], b: Vector[Double]) {
        require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) + v
        }
        
      }
    }
  }


  implicit val canAdd_V_V_Double: BinaryOp[Vector[Double], Vector[Double], breeze.linalg.operators.OpAdd, Vector[Double]] = pureFromUpdate_Double(canAddInto_V_V_Double)


  implicit val canAddInto_V_S_Double: BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpAdd] = {
    new BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpAdd] {
      override def bindingMissing(a: Vector[Double], b: Double) {
        
        for( (i,v) <- a.activeIterator) {
          a(i) = v + b
        }
        
      }
    }
  }


  implicit val canAdd_V_S_Double: BinaryOp[Vector[Double], Double, breeze.linalg.operators.OpAdd, Vector[Double]] = pureFromUpdate_Double(canAddInto_V_S_Double)


  implicit val canDivInto_V_V_Double: BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpDiv] = {
    new BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpDiv] {
      override def bindingMissing(a: Vector[Double], b: Vector[Double]) {
        require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) / v
        }
        
      }
    }
  }


  implicit val canDiv_V_V_Double: BinaryOp[Vector[Double], Vector[Double], breeze.linalg.operators.OpDiv, Vector[Double]] = pureFromUpdate_Double(canDivInto_V_V_Double)


  implicit val canDivInto_V_S_Double: BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpDiv] = {
    new BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpDiv] {
      override def bindingMissing(a: Vector[Double], b: Double) {
        
        for( (i,v) <- a.iterator) {
          a(i) = v / b
        }
        
      }
    }
  }


  implicit val canDiv_V_S_Double: BinaryOp[Vector[Double], Double, breeze.linalg.operators.OpDiv, Vector[Double]] = pureFromUpdate_Double(canDivInto_V_S_Double)


  implicit val canSubInto_V_V_Double: BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpSub] = {
    new BinaryUpdateRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpSub] {
      override def bindingMissing(a: Vector[Double], b: Vector[Double]) {
        require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) - v
        }
        
      }
    }
  }


  implicit val canSub_V_V_Double: BinaryOp[Vector[Double], Vector[Double], breeze.linalg.operators.OpSub, Vector[Double]] = pureFromUpdate_Double(canSubInto_V_V_Double)


  implicit val canSubInto_V_S_Double: BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpSub] = {
    new BinaryUpdateRegistry[Vector[Double], Double, breeze.linalg.operators.OpSub] {
      override def bindingMissing(a: Vector[Double], b: Double) {
        
        for( (i,v) <- a.activeIterator) {
          a(i) = v - b
        }
        
      }
    }
  }


  implicit val canSub_V_S_Double: BinaryOp[Vector[Double], Double, breeze.linalg.operators.OpSub, Vector[Double]] = pureFromUpdate_Double(canSubInto_V_S_Double)


  implicit val canDotProductV_Double: BinaryRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpMulInner, Double] = {
    new BinaryRegistry[Vector[Double], Vector[Double], breeze.linalg.operators.OpMulInner, Double] {
      override def bindingMissing(a: Vector[Double], b: Vector[Double]) = {
        require(b.length == a.length, "Vectors must be the same length!")

       var result: Double = 0

         for( (i, v) <- b.activeIterator) {
           result += a(i) * v
         }
         result
      }
    }
  }

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
        

  implicit val canPowInto_V_V_Float: BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpPow] = {
    new BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpPow] {
      override def bindingMissing(a: Vector[Float], b: Vector[Float]) {
        require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = scala.math.pow(a(i), v).toFloat
        }
        
      }
    }
  }


  implicit val canPow_V_V_Float: BinaryOp[Vector[Float], Vector[Float], breeze.linalg.operators.OpPow, Vector[Float]] = pureFromUpdate_Float(canPowInto_V_V_Float)


  implicit val canPowInto_V_S_Float: BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpPow] = {
    new BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpPow] {
      override def bindingMissing(a: Vector[Float], b: Float) {
        
        for( (i,v) <- a.iterator) {
          a(i) = scala.math.pow(v, b).toFloat
        }
        
      }
    }
  }


  implicit val canPow_V_S_Float: BinaryOp[Vector[Float], Float, breeze.linalg.operators.OpPow, Vector[Float]] = pureFromUpdate_Float(canPowInto_V_S_Float)


  implicit val canSetInto_V_V_Float: BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpSet] = {
    new BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpSet] {
      override def bindingMissing(a: Vector[Float], b: Vector[Float]) {
        require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = v
        }
        
      }
    }
  }


  implicit val canSet_V_V_Float: BinaryOp[Vector[Float], Vector[Float], breeze.linalg.operators.OpSet, Vector[Float]] = pureFromUpdate_Float(canSetInto_V_V_Float)


  implicit val canSetInto_V_S_Float: BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpSet] = {
    new BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpSet] {
      override def bindingMissing(a: Vector[Float], b: Float) {
        
        for( (i,v) <- a.iterator) {
          a(i) = b
        }
        
      }
    }
  }


  implicit val canSet_V_S_Float: BinaryOp[Vector[Float], Float, breeze.linalg.operators.OpSet, Vector[Float]] = pureFromUpdate_Float(canSetInto_V_S_Float)


  implicit val canModInto_V_V_Float: BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpMod] = {
    new BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpMod] {
      override def bindingMissing(a: Vector[Float], b: Vector[Float]) {
        require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) % v
        }
        
      }
    }
  }


  implicit val canMod_V_V_Float: BinaryOp[Vector[Float], Vector[Float], breeze.linalg.operators.OpMod, Vector[Float]] = pureFromUpdate_Float(canModInto_V_V_Float)


  implicit val canModInto_V_S_Float: BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpMod] = {
    new BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpMod] {
      override def bindingMissing(a: Vector[Float], b: Float) {
        
        for( (i,v) <- a.iterator) {
          a(i) = v % b
        }
        
      }
    }
  }


  implicit val canMod_V_S_Float: BinaryOp[Vector[Float], Float, breeze.linalg.operators.OpMod, Vector[Float]] = pureFromUpdate_Float(canModInto_V_S_Float)


  implicit val canMulScalarInto_V_V_Float: BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpMulScalar] = {
    new BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpMulScalar] {
      override def bindingMissing(a: Vector[Float], b: Vector[Float]) {
        require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) * v
        }
        
      }
    }
  }


  implicit val canMulScalar_V_V_Float: BinaryOp[Vector[Float], Vector[Float], breeze.linalg.operators.OpMulScalar, Vector[Float]] = pureFromUpdate_Float(canMulScalarInto_V_V_Float)


  implicit val canMulScalarInto_V_S_Float: BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpMulScalar] = {
    new BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpMulScalar] {
      override def bindingMissing(a: Vector[Float], b: Float) {
        
        for( (i,v) <- a.iterator) {
          a(i) = v * b
        }
        
      }
    }
  }


  implicit val canMulScalar_V_S_Float: BinaryOp[Vector[Float], Float, breeze.linalg.operators.OpMulScalar, Vector[Float]] = pureFromUpdate_Float(canMulScalarInto_V_S_Float)


  implicit val canAddInto_V_V_Float: BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpAdd] = {
    new BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpAdd] {
      override def bindingMissing(a: Vector[Float], b: Vector[Float]) {
        require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) + v
        }
        
      }
    }
  }


  implicit val canAdd_V_V_Float: BinaryOp[Vector[Float], Vector[Float], breeze.linalg.operators.OpAdd, Vector[Float]] = pureFromUpdate_Float(canAddInto_V_V_Float)


  implicit val canAddInto_V_S_Float: BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpAdd] = {
    new BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpAdd] {
      override def bindingMissing(a: Vector[Float], b: Float) {
        
        for( (i,v) <- a.activeIterator) {
          a(i) = v + b
        }
        
      }
    }
  }


  implicit val canAdd_V_S_Float: BinaryOp[Vector[Float], Float, breeze.linalg.operators.OpAdd, Vector[Float]] = pureFromUpdate_Float(canAddInto_V_S_Float)


  implicit val canDivInto_V_V_Float: BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpDiv] = {
    new BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpDiv] {
      override def bindingMissing(a: Vector[Float], b: Vector[Float]) {
        require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) / v
        }
        
      }
    }
  }


  implicit val canDiv_V_V_Float: BinaryOp[Vector[Float], Vector[Float], breeze.linalg.operators.OpDiv, Vector[Float]] = pureFromUpdate_Float(canDivInto_V_V_Float)


  implicit val canDivInto_V_S_Float: BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpDiv] = {
    new BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpDiv] {
      override def bindingMissing(a: Vector[Float], b: Float) {
        
        for( (i,v) <- a.iterator) {
          a(i) = v / b
        }
        
      }
    }
  }


  implicit val canDiv_V_S_Float: BinaryOp[Vector[Float], Float, breeze.linalg.operators.OpDiv, Vector[Float]] = pureFromUpdate_Float(canDivInto_V_S_Float)


  implicit val canSubInto_V_V_Float: BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpSub] = {
    new BinaryUpdateRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpSub] {
      override def bindingMissing(a: Vector[Float], b: Vector[Float]) {
        require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) - v
        }
        
      }
    }
  }


  implicit val canSub_V_V_Float: BinaryOp[Vector[Float], Vector[Float], breeze.linalg.operators.OpSub, Vector[Float]] = pureFromUpdate_Float(canSubInto_V_V_Float)


  implicit val canSubInto_V_S_Float: BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpSub] = {
    new BinaryUpdateRegistry[Vector[Float], Float, breeze.linalg.operators.OpSub] {
      override def bindingMissing(a: Vector[Float], b: Float) {
        
        for( (i,v) <- a.activeIterator) {
          a(i) = v - b
        }
        
      }
    }
  }


  implicit val canSub_V_S_Float: BinaryOp[Vector[Float], Float, breeze.linalg.operators.OpSub, Vector[Float]] = pureFromUpdate_Float(canSubInto_V_S_Float)


  implicit val canDotProductV_Float: BinaryRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpMulInner, Float] = {
    new BinaryRegistry[Vector[Float], Vector[Float], breeze.linalg.operators.OpMulInner, Float] {
      override def bindingMissing(a: Vector[Float], b: Vector[Float]) = {
        require(b.length == a.length, "Vectors must be the same length!")

       var result: Float = 0

         for( (i, v) <- b.activeIterator) {
           result += a(i) * v
         }
         result
      }
    }
  }

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
        

  implicit val canPowInto_V_V_Int: BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpPow] = {
    new BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpPow] {
      override def bindingMissing(a: Vector[Int], b: Vector[Int]) {
        require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = IntMath.ipow(a(i), v)
        }
        
      }
    }
  }


  implicit val canPow_V_V_Int: BinaryOp[Vector[Int], Vector[Int], breeze.linalg.operators.OpPow, Vector[Int]] = pureFromUpdate_Int(canPowInto_V_V_Int)


  implicit val canPowInto_V_S_Int: BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpPow] = {
    new BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpPow] {
      override def bindingMissing(a: Vector[Int], b: Int) {
        
        for( (i,v) <- a.iterator) {
          a(i) = IntMath.ipow(v, b)
        }
        
      }
    }
  }


  implicit val canPow_V_S_Int: BinaryOp[Vector[Int], Int, breeze.linalg.operators.OpPow, Vector[Int]] = pureFromUpdate_Int(canPowInto_V_S_Int)


  implicit val canSetInto_V_V_Int: BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpSet] = {
    new BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpSet] {
      override def bindingMissing(a: Vector[Int], b: Vector[Int]) {
        require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = v
        }
        
      }
    }
  }


  implicit val canSet_V_V_Int: BinaryOp[Vector[Int], Vector[Int], breeze.linalg.operators.OpSet, Vector[Int]] = pureFromUpdate_Int(canSetInto_V_V_Int)


  implicit val canSetInto_V_S_Int: BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpSet] = {
    new BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpSet] {
      override def bindingMissing(a: Vector[Int], b: Int) {
        
        for( (i,v) <- a.iterator) {
          a(i) = b
        }
        
      }
    }
  }


  implicit val canSet_V_S_Int: BinaryOp[Vector[Int], Int, breeze.linalg.operators.OpSet, Vector[Int]] = pureFromUpdate_Int(canSetInto_V_S_Int)


  implicit val canModInto_V_V_Int: BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpMod] = {
    new BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpMod] {
      override def bindingMissing(a: Vector[Int], b: Vector[Int]) {
        require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) % v
        }
        
      }
    }
  }


  implicit val canMod_V_V_Int: BinaryOp[Vector[Int], Vector[Int], breeze.linalg.operators.OpMod, Vector[Int]] = pureFromUpdate_Int(canModInto_V_V_Int)


  implicit val canModInto_V_S_Int: BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpMod] = {
    new BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpMod] {
      override def bindingMissing(a: Vector[Int], b: Int) {
        
        for( (i,v) <- a.iterator) {
          a(i) = v % b
        }
        
      }
    }
  }


  implicit val canMod_V_S_Int: BinaryOp[Vector[Int], Int, breeze.linalg.operators.OpMod, Vector[Int]] = pureFromUpdate_Int(canModInto_V_S_Int)


  implicit val canMulScalarInto_V_V_Int: BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpMulScalar] = {
    new BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpMulScalar] {
      override def bindingMissing(a: Vector[Int], b: Vector[Int]) {
        require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) * v
        }
        
      }
    }
  }


  implicit val canMulScalar_V_V_Int: BinaryOp[Vector[Int], Vector[Int], breeze.linalg.operators.OpMulScalar, Vector[Int]] = pureFromUpdate_Int(canMulScalarInto_V_V_Int)


  implicit val canMulScalarInto_V_S_Int: BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpMulScalar] = {
    new BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpMulScalar] {
      override def bindingMissing(a: Vector[Int], b: Int) {
        
        for( (i,v) <- a.iterator) {
          a(i) = v * b
        }
        
      }
    }
  }


  implicit val canMulScalar_V_S_Int: BinaryOp[Vector[Int], Int, breeze.linalg.operators.OpMulScalar, Vector[Int]] = pureFromUpdate_Int(canMulScalarInto_V_S_Int)


  implicit val canAddInto_V_V_Int: BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpAdd] = {
    new BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpAdd] {
      override def bindingMissing(a: Vector[Int], b: Vector[Int]) {
        require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) + v
        }
        
      }
    }
  }


  implicit val canAdd_V_V_Int: BinaryOp[Vector[Int], Vector[Int], breeze.linalg.operators.OpAdd, Vector[Int]] = pureFromUpdate_Int(canAddInto_V_V_Int)


  implicit val canAddInto_V_S_Int: BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpAdd] = {
    new BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpAdd] {
      override def bindingMissing(a: Vector[Int], b: Int) {
        
        for( (i,v) <- a.activeIterator) {
          a(i) = v + b
        }
        
      }
    }
  }


  implicit val canAdd_V_S_Int: BinaryOp[Vector[Int], Int, breeze.linalg.operators.OpAdd, Vector[Int]] = pureFromUpdate_Int(canAddInto_V_S_Int)


  implicit val canDivInto_V_V_Int: BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpDiv] = {
    new BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpDiv] {
      override def bindingMissing(a: Vector[Int], b: Vector[Int]) {
        require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.iterator) {
          a(i) = a(i) / v
        }
        
      }
    }
  }


  implicit val canDiv_V_V_Int: BinaryOp[Vector[Int], Vector[Int], breeze.linalg.operators.OpDiv, Vector[Int]] = pureFromUpdate_Int(canDivInto_V_V_Int)


  implicit val canDivInto_V_S_Int: BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpDiv] = {
    new BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpDiv] {
      override def bindingMissing(a: Vector[Int], b: Int) {
        
        for( (i,v) <- a.iterator) {
          a(i) = v / b
        }
        
      }
    }
  }


  implicit val canDiv_V_S_Int: BinaryOp[Vector[Int], Int, breeze.linalg.operators.OpDiv, Vector[Int]] = pureFromUpdate_Int(canDivInto_V_S_Int)


  implicit val canSubInto_V_V_Int: BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpSub] = {
    new BinaryUpdateRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpSub] {
      override def bindingMissing(a: Vector[Int], b: Vector[Int]) {
        require(b.length == a.length, "Vectors must be the same length!")

        for( (i,v) <- b.activeIterator) {
          a(i) = a(i) - v
        }
        
      }
    }
  }


  implicit val canSub_V_V_Int: BinaryOp[Vector[Int], Vector[Int], breeze.linalg.operators.OpSub, Vector[Int]] = pureFromUpdate_Int(canSubInto_V_V_Int)


  implicit val canSubInto_V_S_Int: BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpSub] = {
    new BinaryUpdateRegistry[Vector[Int], Int, breeze.linalg.operators.OpSub] {
      override def bindingMissing(a: Vector[Int], b: Int) {
        
        for( (i,v) <- a.activeIterator) {
          a(i) = v - b
        }
        
      }
    }
  }


  implicit val canSub_V_S_Int: BinaryOp[Vector[Int], Int, breeze.linalg.operators.OpSub, Vector[Int]] = pureFromUpdate_Int(canSubInto_V_S_Int)


  implicit val canDotProductV_Int: BinaryRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpMulInner, Int] = {
    new BinaryRegistry[Vector[Int], Vector[Int], breeze.linalg.operators.OpMulInner, Int] {
      override def bindingMissing(a: Vector[Int], b: Vector[Int]) = {
        require(b.length == a.length, "Vectors must be the same length!")

       var result: Int = 0

         for( (i, v) <- b.activeIterator) {
           result += a(i) * v
         }
         result
      }
    }
  }

}
