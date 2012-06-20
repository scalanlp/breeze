package breeze.linalg
import java.util._
import breeze.linalg.operators._
import breeze.linalg.support._
import breeze.numerics._

/** This is an auto-generated trait providing operators for SparseVector */
trait SparseVectorOps_Double { this: SparseVector.type =>

  def pureFromUpdate_Double[Other,Op<:OpType](op: BinaryUpdateOp[SparseVector[Double], Other, Op])(implicit copy: CanCopy[SparseVector[Double]]):BinaryOp[SparseVector[Double], Other, Op, SparseVector[Double]] = {
    new BinaryOp[SparseVector[Double], Other, Op, SparseVector[Double]] {
      override def apply(a : SparseVector[Double], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }
        

  implicit val canSubInto_VV_Double: BinaryUpdateOp[SparseVector[Double], SparseVector[Double], breeze.linalg.operators.OpSub] = {
    new BinaryUpdateOp[SparseVector[Double], SparseVector[Double], breeze.linalg.operators.OpSub] {
      def apply(a: SparseVector[Double], b: SparseVector[Double]) {
        require(b.length == a.length, "Vectors must be the same length!")

        // TODO: decide the appropriate value of 3 and 30 here.
        if(b.used > a.used * 3 && b.used > 30) {
          val c = copy(b)
          apply(c, a)
          c*= (-1).toDouble
          a.data = c.data
          a.index = c.index
          a.used = c.used
          return
        }

        var buf:Array[Double] = null
        var bufi:Array[Int] = null
        var nused = 0

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize
        var i = 0
        while(i < bsize) {
          if (a.contains(bi(i))) {
                // just add it in if it's there
                a(bi(i)) = a(bi(i)) - bd(i)
          } else { // not there
                if(buf eq null) {
                  buf = new Array[Double](b.used - i)
                  bufi = new Array[Int](b.used - i)
                } else if(buf.length == nused) {
                  buf = Arrays.copyOf(buf, nused + b.used - i)
                  bufi = Arrays.copyOf(bufi, nused + b.used - i)
                }

                // append to buffer to merged in later
                buf(nused) = buf(nused) - bd(i)
                bufi(nused) = bi(i)
                nused += 1
          }
          i += 1
        }

        // merge two disjoint sorted lists
        if(buf != null) {
          val result = new Array[Double](a.used + nused)
          val resultI = new Array[Int](a.used + nused)
          var ni = 0
          var ai = 0
          var out = 0

          while(ni < nused) {
                while(ai < a.used && a.index(ai) < bufi(ni) ) {
                  result(out) = a.data(ai)
                  resultI(out) = a.index(ai)
                  ai += 1
                  out += 1
                }
                result(out) = buf(ni)
                resultI(out) = bufi(ni)
                out += 1
                ni += 1
          }

          System.arraycopy(a.data, ai, result, out, result.length - out)
          System.arraycopy(a.index, ai, resultI, out, result.length - out)
          out = result.length

          a.data = result
          a.index = resultI
          a.used = out
        }
        
      }
    }
  }

  Vector.canSubInto_V_V_Double.register(canSubInto_VV_Double)

  implicit val canSub_VV_Double: BinaryOp[SparseVector[Double], SparseVector[Double], breeze.linalg.operators.OpSub, SparseVector[Double]] = pureFromUpdate_Double(canSubInto_VV_Double)
  Vector.canSub_V_V_Double.register(canSub_VV_Double)


  implicit val canSubInto_SV_S_Double: BinaryUpdateOp[SparseVector[Double], Double, breeze.linalg.operators.OpSub] = {
    new BinaryUpdateOp[SparseVector[Double], Double, breeze.linalg.operators.OpSub] {
      def apply(a: SparseVector[Double], b: Double) {
        

        var i = 0
        while(i < a.length) {
          a(i) = a(i) - b
          i += 1
        }
        
      }
    }
  }

  Vector.canSubInto_V_S_Double.register(canSubInto_SV_S_Double)

  implicit val canSub_SV_S_Double: BinaryOp[SparseVector[Double], Double, breeze.linalg.operators.OpSub, SparseVector[Double]] = pureFromUpdate_Double(canSubInto_SV_S_Double)
  Vector.canSub_V_S_Double.register(canSub_SV_S_Double)


  implicit val canSetInto_VV_Double: BinaryUpdateOp[SparseVector[Double], SparseVector[Double], breeze.linalg.operators.OpSet] = {
    new BinaryUpdateOp[SparseVector[Double], SparseVector[Double], breeze.linalg.operators.OpSet] {
      def apply(a: SparseVector[Double], b: SparseVector[Double]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = b(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canSetInto_V_V_Double.register(canSetInto_VV_Double)

  implicit val canSet_VV_Double: BinaryOp[SparseVector[Double], SparseVector[Double], breeze.linalg.operators.OpSet, SparseVector[Double]] = pureFromUpdate_Double(canSetInto_VV_Double)
  Vector.canSet_V_V_Double.register(canSet_VV_Double)


  implicit val canSetInto_SV_S_Double: BinaryUpdateOp[SparseVector[Double], Double, breeze.linalg.operators.OpSet] = {
    new BinaryUpdateOp[SparseVector[Double], Double, breeze.linalg.operators.OpSet] {
      def apply(a: SparseVector[Double], b: Double) {
        

        var i = 0
        while(i < a.length) {
          a(i) = b
          i += 1
        }
        
      }
    }
  }

  Vector.canSetInto_V_S_Double.register(canSetInto_SV_S_Double)

  implicit val canSet_SV_S_Double: BinaryOp[SparseVector[Double], Double, breeze.linalg.operators.OpSet, SparseVector[Double]] = pureFromUpdate_Double(canSetInto_SV_S_Double)
  Vector.canSet_V_S_Double.register(canSet_SV_S_Double)


  implicit val canAddInto_VV_Double: BinaryUpdateOp[SparseVector[Double], SparseVector[Double], breeze.linalg.operators.OpAdd] = {
    new BinaryUpdateOp[SparseVector[Double], SparseVector[Double], breeze.linalg.operators.OpAdd] {
      def apply(a: SparseVector[Double], b: SparseVector[Double]) {
        require(b.length == a.length, "Vectors must be the same length!")

        // TODO: decide the appropriate value of 3 and 30 here.
        if(b.used > a.used * 3 && b.used > 30) {
          val c = copy(b)
          apply(c, a)
          
          a.data = c.data
          a.index = c.index
          a.used = c.used
          return
        }

        var buf:Array[Double] = null
        var bufi:Array[Int] = null
        var nused = 0

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize
        var i = 0
        while(i < bsize) {
          if (a.contains(bi(i))) {
                // just add it in if it's there
                a(bi(i)) = a(bi(i)) + bd(i)
          } else { // not there
                if(buf eq null) {
                  buf = new Array[Double](b.used - i)
                  bufi = new Array[Int](b.used - i)
                } else if(buf.length == nused) {
                  buf = Arrays.copyOf(buf, nused + b.used - i)
                  bufi = Arrays.copyOf(bufi, nused + b.used - i)
                }

                // append to buffer to merged in later
                buf(nused) = buf(nused) + bd(i)
                bufi(nused) = bi(i)
                nused += 1
          }
          i += 1
        }

        // merge two disjoint sorted lists
        if(buf != null) {
          val result = new Array[Double](a.used + nused)
          val resultI = new Array[Int](a.used + nused)
          var ni = 0
          var ai = 0
          var out = 0

          while(ni < nused) {
                while(ai < a.used && a.index(ai) < bufi(ni) ) {
                  result(out) = a.data(ai)
                  resultI(out) = a.index(ai)
                  ai += 1
                  out += 1
                }
                result(out) = buf(ni)
                resultI(out) = bufi(ni)
                out += 1
                ni += 1
          }

          System.arraycopy(a.data, ai, result, out, result.length - out)
          System.arraycopy(a.index, ai, resultI, out, result.length - out)
          out = result.length

          a.data = result
          a.index = resultI
          a.used = out
        }
        
      }
    }
  }

  Vector.canAddInto_V_V_Double.register(canAddInto_VV_Double)

  implicit val canAdd_VV_Double: BinaryOp[SparseVector[Double], SparseVector[Double], breeze.linalg.operators.OpAdd, SparseVector[Double]] = pureFromUpdate_Double(canAddInto_VV_Double)
  Vector.canAdd_V_V_Double.register(canAdd_VV_Double)


  implicit val canAddInto_SV_S_Double: BinaryUpdateOp[SparseVector[Double], Double, breeze.linalg.operators.OpAdd] = {
    new BinaryUpdateOp[SparseVector[Double], Double, breeze.linalg.operators.OpAdd] {
      def apply(a: SparseVector[Double], b: Double) {
        

        var i = 0
        while(i < a.length) {
          a(i) = a(i) + b
          i += 1
        }
        
      }
    }
  }

  Vector.canAddInto_V_S_Double.register(canAddInto_SV_S_Double)

  implicit val canAdd_SV_S_Double: BinaryOp[SparseVector[Double], Double, breeze.linalg.operators.OpAdd, SparseVector[Double]] = pureFromUpdate_Double(canAddInto_SV_S_Double)
  Vector.canAdd_V_S_Double.register(canAdd_SV_S_Double)


  implicit val canPowInto_VV_Double: BinaryUpdateOp[SparseVector[Double], SparseVector[Double], breeze.linalg.operators.OpPow] = {
    new BinaryUpdateOp[SparseVector[Double], SparseVector[Double], breeze.linalg.operators.OpPow] {
      def apply(a: SparseVector[Double], b: SparseVector[Double]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = scala.math.pow(a(i), b(i))
          i += 1
        }
        
      }
    }
  }

  Vector.canPowInto_V_V_Double.register(canPowInto_VV_Double)

  implicit val canPow_VV_Double: BinaryOp[SparseVector[Double], SparseVector[Double], breeze.linalg.operators.OpPow, SparseVector[Double]] = pureFromUpdate_Double(canPowInto_VV_Double)
  Vector.canPow_V_V_Double.register(canPow_VV_Double)


  implicit val canPowInto_SV_S_Double: BinaryUpdateOp[SparseVector[Double], Double, breeze.linalg.operators.OpPow] = {
    new BinaryUpdateOp[SparseVector[Double], Double, breeze.linalg.operators.OpPow] {
      def apply(a: SparseVector[Double], b: Double) {
        

        var i = 0
        while(i < a.length) {
          a(i) = scala.math.pow(a(i), b)
          i += 1
        }
        
      }
    }
  }

  Vector.canPowInto_V_S_Double.register(canPowInto_SV_S_Double)

  implicit val canPow_SV_S_Double: BinaryOp[SparseVector[Double], Double, breeze.linalg.operators.OpPow, SparseVector[Double]] = pureFromUpdate_Double(canPowInto_SV_S_Double)
  Vector.canPow_V_S_Double.register(canPow_SV_S_Double)


  implicit val canModInto_VV_Double: BinaryUpdateOp[SparseVector[Double], SparseVector[Double], breeze.linalg.operators.OpMod] = {
    new BinaryUpdateOp[SparseVector[Double], SparseVector[Double], breeze.linalg.operators.OpMod] {
      def apply(a: SparseVector[Double], b: SparseVector[Double]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = a(i) % b(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canModInto_V_V_Double.register(canModInto_VV_Double)

  implicit val canMod_VV_Double: BinaryOp[SparseVector[Double], SparseVector[Double], breeze.linalg.operators.OpMod, SparseVector[Double]] = pureFromUpdate_Double(canModInto_VV_Double)
  Vector.canMod_V_V_Double.register(canMod_VV_Double)


  implicit val canModInto_SV_S_Double: BinaryUpdateOp[SparseVector[Double], Double, breeze.linalg.operators.OpMod] = {
    new BinaryUpdateOp[SparseVector[Double], Double, breeze.linalg.operators.OpMod] {
      def apply(a: SparseVector[Double], b: Double) {
        

        var i = 0
        while(i < a.length) {
          a(i) = a(i) % b
          i += 1
        }
        
      }
    }
  }

  Vector.canModInto_V_S_Double.register(canModInto_SV_S_Double)

  implicit val canMod_SV_S_Double: BinaryOp[SparseVector[Double], Double, breeze.linalg.operators.OpMod, SparseVector[Double]] = pureFromUpdate_Double(canModInto_SV_S_Double)
  Vector.canMod_V_S_Double.register(canMod_SV_S_Double)


  implicit val canDivInto_VV_Double: BinaryUpdateOp[SparseVector[Double], SparseVector[Double], breeze.linalg.operators.OpDiv] = {
    new BinaryUpdateOp[SparseVector[Double], SparseVector[Double], breeze.linalg.operators.OpDiv] {
      def apply(a: SparseVector[Double], b: SparseVector[Double]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = a(i) / b(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canDivInto_V_V_Double.register(canDivInto_VV_Double)

  implicit val canDiv_VV_Double: BinaryOp[SparseVector[Double], SparseVector[Double], breeze.linalg.operators.OpDiv, SparseVector[Double]] = pureFromUpdate_Double(canDivInto_VV_Double)
  Vector.canDiv_V_V_Double.register(canDiv_VV_Double)


  implicit val canDivInto_SV_S_Double: BinaryUpdateOp[SparseVector[Double], Double, breeze.linalg.operators.OpDiv] = {
    new BinaryUpdateOp[SparseVector[Double], Double, breeze.linalg.operators.OpDiv] {
      def apply(a: SparseVector[Double], b: Double) {
        
    var i = 0
    while(i < a.used) {
      a.data(i) = a.data(i) / b
      i += 1
    }
    
      }
    }
  }

  Vector.canDivInto_V_S_Double.register(canDivInto_SV_S_Double)

  implicit val canDiv_SV_S_Double: BinaryOp[SparseVector[Double], Double, breeze.linalg.operators.OpDiv, SparseVector[Double]] = pureFromUpdate_Double(canDivInto_SV_S_Double)
  Vector.canDiv_V_S_Double.register(canDiv_SV_S_Double)


  implicit val canMulScalarInto_VV_Double: BinaryUpdateOp[SparseVector[Double], SparseVector[Double], breeze.linalg.operators.OpMulScalar] = {
    new BinaryUpdateOp[SparseVector[Double], SparseVector[Double], breeze.linalg.operators.OpMulScalar] {
      def apply(a: SparseVector[Double], b: SparseVector[Double]) {
        require(b.length == a.length, "Vectors must be the same length!")

    val outD = new Array[Double](a.used min b.used)
    val outI = new Array[Int](a.used min b.used)
    var out = 0

    val looper = if(a.used < b.used) a else b
    val other = if(a.used < b.used) b else a

    var i = 0
    val bd = looper.data
    val bi = looper.index
    val bsize = looper.iterableSize
    while(i < bsize) {
      if(looper.isActive(i)) {
        val p = other(bi(i)) * bd(i)
        if (p != 0) {
          outD(i) = p
          outI(i) = bi(i)
        }
      }
      i += 1
    }

    a.data = outD; a.index = outI; a.used = out
    
      }
    }
  }



  implicit val canMulScalar_VV_Double: BinaryOp[SparseVector[Double], SparseVector[Double], breeze.linalg.operators.OpMulScalar, SparseVector[Double]] = {
    new BinaryOp[SparseVector[Double], SparseVector[Double], breeze.linalg.operators.OpMulScalar, SparseVector[Double]] {
      def apply(a: SparseVector[Double], b: SparseVector[Double]) = {
        require(b.length == a.length, "Vectors must be the same length!")

    val outD = new Array[Double](a.used min b.used)
    val outI = new Array[Int](a.used min b.used)
    var out = 0

    val looper = if(a.used < b.used) a else b
    val other = if(a.used < b.used) b else a

    var i = 0
    val bd = looper.data
    val bi = looper.index
    val bsize = looper.iterableSize
    while(i < bsize) {
      if(looper.isActive(i)) {
        val p = other(bi(i)) * bd(i)
        if (p != 0) {
          outD(i) = p
          outI(i) = bi(i)
        }
      }
      i += 1
    }

    new SparseVector(outI, outD, out, a.length)
    
      }
    }
  }



  implicit val canMulScalarInto_SV_S_Double: BinaryUpdateOp[SparseVector[Double], Double, breeze.linalg.operators.OpMulScalar] = {
    new BinaryUpdateOp[SparseVector[Double], Double, breeze.linalg.operators.OpMulScalar] {
      def apply(a: SparseVector[Double], b: Double) {
        
    var i = 0
    while(i < a.used) {
      a.data(i) = a.data(i) * b
      i += 1
    }
    
      }
    }
  }


  implicit val canMulScalar_SV_S_Double: BinaryOp[SparseVector[Double], Double, breeze.linalg.operators.OpMulScalar, SparseVector[Double]] = pureFromUpdate_Double(canMulScalarInto_SV_S_Double)


  implicit val canDotProductSV_Double: BinaryOp[SparseVector[Double], SparseVector[Double], breeze.linalg.operators.OpMulInner, Double] = {
    new BinaryOp[SparseVector[Double], SparseVector[Double], breeze.linalg.operators.OpMulInner, Double] {
      def apply(a: SparseVector[Double], b: SparseVector[Double]) = {
        require(b.length == a.length, "Vectors must be the same length!")

       var result: Double = 0

         val bd = b.data
         val bi = b.index
         val bsize = b.iterableSize
         var i = 0
         while(i < bsize) {
           if(b.isActive(i)) result += a(bi(i)) * bd(i)
           i += 1
         }
         result
      }
    }
  }

}

/** This is an auto-generated trait providing operators for SparseVector */
trait SparseVectorOps_Float { this: SparseVector.type =>

  def pureFromUpdate_Float[Other,Op<:OpType](op: BinaryUpdateOp[SparseVector[Float], Other, Op])(implicit copy: CanCopy[SparseVector[Float]]):BinaryOp[SparseVector[Float], Other, Op, SparseVector[Float]] = {
    new BinaryOp[SparseVector[Float], Other, Op, SparseVector[Float]] {
      override def apply(a : SparseVector[Float], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }
        

  implicit val canSubInto_VV_Float: BinaryUpdateOp[SparseVector[Float], SparseVector[Float], breeze.linalg.operators.OpSub] = {
    new BinaryUpdateOp[SparseVector[Float], SparseVector[Float], breeze.linalg.operators.OpSub] {
      def apply(a: SparseVector[Float], b: SparseVector[Float]) {
        require(b.length == a.length, "Vectors must be the same length!")

        // TODO: decide the appropriate value of 3 and 30 here.
        if(b.used > a.used * 3 && b.used > 30) {
          val c = copy(b)
          apply(c, a)
          c*= (-1).toFloat
          a.data = c.data
          a.index = c.index
          a.used = c.used
          return
        }

        var buf:Array[Float] = null
        var bufi:Array[Int] = null
        var nused = 0

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize
        var i = 0
        while(i < bsize) {
          if (a.contains(bi(i))) {
                // just add it in if it's there
                a(bi(i)) = a(bi(i)) - bd(i)
          } else { // not there
                if(buf eq null) {
                  buf = new Array[Float](b.used - i)
                  bufi = new Array[Int](b.used - i)
                } else if(buf.length == nused) {
                  buf = Arrays.copyOf(buf, nused + b.used - i)
                  bufi = Arrays.copyOf(bufi, nused + b.used - i)
                }

                // append to buffer to merged in later
                buf(nused) = buf(nused) - bd(i)
                bufi(nused) = bi(i)
                nused += 1
          }
          i += 1
        }

        // merge two disjoint sorted lists
        if(buf != null) {
          val result = new Array[Float](a.used + nused)
          val resultI = new Array[Int](a.used + nused)
          var ni = 0
          var ai = 0
          var out = 0

          while(ni < nused) {
                while(ai < a.used && a.index(ai) < bufi(ni) ) {
                  result(out) = a.data(ai)
                  resultI(out) = a.index(ai)
                  ai += 1
                  out += 1
                }
                result(out) = buf(ni)
                resultI(out) = bufi(ni)
                out += 1
                ni += 1
          }

          System.arraycopy(a.data, ai, result, out, result.length - out)
          System.arraycopy(a.index, ai, resultI, out, result.length - out)
          out = result.length

          a.data = result
          a.index = resultI
          a.used = out
        }
        
      }
    }
  }

  Vector.canSubInto_V_V_Float.register(canSubInto_VV_Float)

  implicit val canSub_VV_Float: BinaryOp[SparseVector[Float], SparseVector[Float], breeze.linalg.operators.OpSub, SparseVector[Float]] = pureFromUpdate_Float(canSubInto_VV_Float)
  Vector.canSub_V_V_Float.register(canSub_VV_Float)


  implicit val canSubInto_SV_S_Float: BinaryUpdateOp[SparseVector[Float], Float, breeze.linalg.operators.OpSub] = {
    new BinaryUpdateOp[SparseVector[Float], Float, breeze.linalg.operators.OpSub] {
      def apply(a: SparseVector[Float], b: Float) {
        

        var i = 0
        while(i < a.length) {
          a(i) = a(i) - b
          i += 1
        }
        
      }
    }
  }

  Vector.canSubInto_V_S_Float.register(canSubInto_SV_S_Float)

  implicit val canSub_SV_S_Float: BinaryOp[SparseVector[Float], Float, breeze.linalg.operators.OpSub, SparseVector[Float]] = pureFromUpdate_Float(canSubInto_SV_S_Float)
  Vector.canSub_V_S_Float.register(canSub_SV_S_Float)


  implicit val canSetInto_VV_Float: BinaryUpdateOp[SparseVector[Float], SparseVector[Float], breeze.linalg.operators.OpSet] = {
    new BinaryUpdateOp[SparseVector[Float], SparseVector[Float], breeze.linalg.operators.OpSet] {
      def apply(a: SparseVector[Float], b: SparseVector[Float]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = b(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canSetInto_V_V_Float.register(canSetInto_VV_Float)

  implicit val canSet_VV_Float: BinaryOp[SparseVector[Float], SparseVector[Float], breeze.linalg.operators.OpSet, SparseVector[Float]] = pureFromUpdate_Float(canSetInto_VV_Float)
  Vector.canSet_V_V_Float.register(canSet_VV_Float)


  implicit val canSetInto_SV_S_Float: BinaryUpdateOp[SparseVector[Float], Float, breeze.linalg.operators.OpSet] = {
    new BinaryUpdateOp[SparseVector[Float], Float, breeze.linalg.operators.OpSet] {
      def apply(a: SparseVector[Float], b: Float) {
        

        var i = 0
        while(i < a.length) {
          a(i) = b
          i += 1
        }
        
      }
    }
  }

  Vector.canSetInto_V_S_Float.register(canSetInto_SV_S_Float)

  implicit val canSet_SV_S_Float: BinaryOp[SparseVector[Float], Float, breeze.linalg.operators.OpSet, SparseVector[Float]] = pureFromUpdate_Float(canSetInto_SV_S_Float)
  Vector.canSet_V_S_Float.register(canSet_SV_S_Float)


  implicit val canAddInto_VV_Float: BinaryUpdateOp[SparseVector[Float], SparseVector[Float], breeze.linalg.operators.OpAdd] = {
    new BinaryUpdateOp[SparseVector[Float], SparseVector[Float], breeze.linalg.operators.OpAdd] {
      def apply(a: SparseVector[Float], b: SparseVector[Float]) {
        require(b.length == a.length, "Vectors must be the same length!")

        // TODO: decide the appropriate value of 3 and 30 here.
        if(b.used > a.used * 3 && b.used > 30) {
          val c = copy(b)
          apply(c, a)
          
          a.data = c.data
          a.index = c.index
          a.used = c.used
          return
        }

        var buf:Array[Float] = null
        var bufi:Array[Int] = null
        var nused = 0

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize
        var i = 0
        while(i < bsize) {
          if (a.contains(bi(i))) {
                // just add it in if it's there
                a(bi(i)) = a(bi(i)) + bd(i)
          } else { // not there
                if(buf eq null) {
                  buf = new Array[Float](b.used - i)
                  bufi = new Array[Int](b.used - i)
                } else if(buf.length == nused) {
                  buf = Arrays.copyOf(buf, nused + b.used - i)
                  bufi = Arrays.copyOf(bufi, nused + b.used - i)
                }

                // append to buffer to merged in later
                buf(nused) = buf(nused) + bd(i)
                bufi(nused) = bi(i)
                nused += 1
          }
          i += 1
        }

        // merge two disjoint sorted lists
        if(buf != null) {
          val result = new Array[Float](a.used + nused)
          val resultI = new Array[Int](a.used + nused)
          var ni = 0
          var ai = 0
          var out = 0

          while(ni < nused) {
                while(ai < a.used && a.index(ai) < bufi(ni) ) {
                  result(out) = a.data(ai)
                  resultI(out) = a.index(ai)
                  ai += 1
                  out += 1
                }
                result(out) = buf(ni)
                resultI(out) = bufi(ni)
                out += 1
                ni += 1
          }

          System.arraycopy(a.data, ai, result, out, result.length - out)
          System.arraycopy(a.index, ai, resultI, out, result.length - out)
          out = result.length

          a.data = result
          a.index = resultI
          a.used = out
        }
        
      }
    }
  }

  Vector.canAddInto_V_V_Float.register(canAddInto_VV_Float)

  implicit val canAdd_VV_Float: BinaryOp[SparseVector[Float], SparseVector[Float], breeze.linalg.operators.OpAdd, SparseVector[Float]] = pureFromUpdate_Float(canAddInto_VV_Float)
  Vector.canAdd_V_V_Float.register(canAdd_VV_Float)


  implicit val canAddInto_SV_S_Float: BinaryUpdateOp[SparseVector[Float], Float, breeze.linalg.operators.OpAdd] = {
    new BinaryUpdateOp[SparseVector[Float], Float, breeze.linalg.operators.OpAdd] {
      def apply(a: SparseVector[Float], b: Float) {
        

        var i = 0
        while(i < a.length) {
          a(i) = a(i) + b
          i += 1
        }
        
      }
    }
  }

  Vector.canAddInto_V_S_Float.register(canAddInto_SV_S_Float)

  implicit val canAdd_SV_S_Float: BinaryOp[SparseVector[Float], Float, breeze.linalg.operators.OpAdd, SparseVector[Float]] = pureFromUpdate_Float(canAddInto_SV_S_Float)
  Vector.canAdd_V_S_Float.register(canAdd_SV_S_Float)


  implicit val canPowInto_VV_Float: BinaryUpdateOp[SparseVector[Float], SparseVector[Float], breeze.linalg.operators.OpPow] = {
    new BinaryUpdateOp[SparseVector[Float], SparseVector[Float], breeze.linalg.operators.OpPow] {
      def apply(a: SparseVector[Float], b: SparseVector[Float]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = scala.math.pow(a(i), b(i)).toFloat
          i += 1
        }
        
      }
    }
  }

  Vector.canPowInto_V_V_Float.register(canPowInto_VV_Float)

  implicit val canPow_VV_Float: BinaryOp[SparseVector[Float], SparseVector[Float], breeze.linalg.operators.OpPow, SparseVector[Float]] = pureFromUpdate_Float(canPowInto_VV_Float)
  Vector.canPow_V_V_Float.register(canPow_VV_Float)


  implicit val canPowInto_SV_S_Float: BinaryUpdateOp[SparseVector[Float], Float, breeze.linalg.operators.OpPow] = {
    new BinaryUpdateOp[SparseVector[Float], Float, breeze.linalg.operators.OpPow] {
      def apply(a: SparseVector[Float], b: Float) {
        

        var i = 0
        while(i < a.length) {
          a(i) = scala.math.pow(a(i), b).toFloat
          i += 1
        }
        
      }
    }
  }

  Vector.canPowInto_V_S_Float.register(canPowInto_SV_S_Float)

  implicit val canPow_SV_S_Float: BinaryOp[SparseVector[Float], Float, breeze.linalg.operators.OpPow, SparseVector[Float]] = pureFromUpdate_Float(canPowInto_SV_S_Float)
  Vector.canPow_V_S_Float.register(canPow_SV_S_Float)


  implicit val canModInto_VV_Float: BinaryUpdateOp[SparseVector[Float], SparseVector[Float], breeze.linalg.operators.OpMod] = {
    new BinaryUpdateOp[SparseVector[Float], SparseVector[Float], breeze.linalg.operators.OpMod] {
      def apply(a: SparseVector[Float], b: SparseVector[Float]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = a(i) % b(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canModInto_V_V_Float.register(canModInto_VV_Float)

  implicit val canMod_VV_Float: BinaryOp[SparseVector[Float], SparseVector[Float], breeze.linalg.operators.OpMod, SparseVector[Float]] = pureFromUpdate_Float(canModInto_VV_Float)
  Vector.canMod_V_V_Float.register(canMod_VV_Float)


  implicit val canModInto_SV_S_Float: BinaryUpdateOp[SparseVector[Float], Float, breeze.linalg.operators.OpMod] = {
    new BinaryUpdateOp[SparseVector[Float], Float, breeze.linalg.operators.OpMod] {
      def apply(a: SparseVector[Float], b: Float) {
        

        var i = 0
        while(i < a.length) {
          a(i) = a(i) % b
          i += 1
        }
        
      }
    }
  }

  Vector.canModInto_V_S_Float.register(canModInto_SV_S_Float)

  implicit val canMod_SV_S_Float: BinaryOp[SparseVector[Float], Float, breeze.linalg.operators.OpMod, SparseVector[Float]] = pureFromUpdate_Float(canModInto_SV_S_Float)
  Vector.canMod_V_S_Float.register(canMod_SV_S_Float)


  implicit val canDivInto_VV_Float: BinaryUpdateOp[SparseVector[Float], SparseVector[Float], breeze.linalg.operators.OpDiv] = {
    new BinaryUpdateOp[SparseVector[Float], SparseVector[Float], breeze.linalg.operators.OpDiv] {
      def apply(a: SparseVector[Float], b: SparseVector[Float]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = a(i) / b(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canDivInto_V_V_Float.register(canDivInto_VV_Float)

  implicit val canDiv_VV_Float: BinaryOp[SparseVector[Float], SparseVector[Float], breeze.linalg.operators.OpDiv, SparseVector[Float]] = pureFromUpdate_Float(canDivInto_VV_Float)
  Vector.canDiv_V_V_Float.register(canDiv_VV_Float)


  implicit val canDivInto_SV_S_Float: BinaryUpdateOp[SparseVector[Float], Float, breeze.linalg.operators.OpDiv] = {
    new BinaryUpdateOp[SparseVector[Float], Float, breeze.linalg.operators.OpDiv] {
      def apply(a: SparseVector[Float], b: Float) {
        
    var i = 0
    while(i < a.used) {
      a.data(i) = a.data(i) / b
      i += 1
    }
    
      }
    }
  }

  Vector.canDivInto_V_S_Float.register(canDivInto_SV_S_Float)

  implicit val canDiv_SV_S_Float: BinaryOp[SparseVector[Float], Float, breeze.linalg.operators.OpDiv, SparseVector[Float]] = pureFromUpdate_Float(canDivInto_SV_S_Float)
  Vector.canDiv_V_S_Float.register(canDiv_SV_S_Float)


  implicit val canMulScalarInto_VV_Float: BinaryUpdateOp[SparseVector[Float], SparseVector[Float], breeze.linalg.operators.OpMulScalar] = {
    new BinaryUpdateOp[SparseVector[Float], SparseVector[Float], breeze.linalg.operators.OpMulScalar] {
      def apply(a: SparseVector[Float], b: SparseVector[Float]) {
        require(b.length == a.length, "Vectors must be the same length!")

    val outD = new Array[Float](a.used min b.used)
    val outI = new Array[Int](a.used min b.used)
    var out = 0

    val looper = if(a.used < b.used) a else b
    val other = if(a.used < b.used) b else a

    var i = 0
    val bd = looper.data
    val bi = looper.index
    val bsize = looper.iterableSize
    while(i < bsize) {
      if(looper.isActive(i)) {
        val p = other(bi(i)) * bd(i)
        if (p != 0) {
          outD(i) = p
          outI(i) = bi(i)
        }
      }
      i += 1
    }

    a.data = outD; a.index = outI; a.used = out
    
      }
    }
  }



  implicit val canMulScalar_VV_Float: BinaryOp[SparseVector[Float], SparseVector[Float], breeze.linalg.operators.OpMulScalar, SparseVector[Float]] = {
    new BinaryOp[SparseVector[Float], SparseVector[Float], breeze.linalg.operators.OpMulScalar, SparseVector[Float]] {
      def apply(a: SparseVector[Float], b: SparseVector[Float]) = {
        require(b.length == a.length, "Vectors must be the same length!")

    val outD = new Array[Float](a.used min b.used)
    val outI = new Array[Int](a.used min b.used)
    var out = 0

    val looper = if(a.used < b.used) a else b
    val other = if(a.used < b.used) b else a

    var i = 0
    val bd = looper.data
    val bi = looper.index
    val bsize = looper.iterableSize
    while(i < bsize) {
      if(looper.isActive(i)) {
        val p = other(bi(i)) * bd(i)
        if (p != 0) {
          outD(i) = p
          outI(i) = bi(i)
        }
      }
      i += 1
    }

    new SparseVector(outI, outD, out, a.length)
    
      }
    }
  }



  implicit val canMulScalarInto_SV_S_Float: BinaryUpdateOp[SparseVector[Float], Float, breeze.linalg.operators.OpMulScalar] = {
    new BinaryUpdateOp[SparseVector[Float], Float, breeze.linalg.operators.OpMulScalar] {
      def apply(a: SparseVector[Float], b: Float) {
        
    var i = 0
    while(i < a.used) {
      a.data(i) = a.data(i) * b
      i += 1
    }
    
      }
    }
  }


  implicit val canMulScalar_SV_S_Float: BinaryOp[SparseVector[Float], Float, breeze.linalg.operators.OpMulScalar, SparseVector[Float]] = pureFromUpdate_Float(canMulScalarInto_SV_S_Float)


  implicit val canDotProductSV_Float: BinaryOp[SparseVector[Float], SparseVector[Float], breeze.linalg.operators.OpMulInner, Float] = {
    new BinaryOp[SparseVector[Float], SparseVector[Float], breeze.linalg.operators.OpMulInner, Float] {
      def apply(a: SparseVector[Float], b: SparseVector[Float]) = {
        require(b.length == a.length, "Vectors must be the same length!")

       var result: Float = 0

         val bd = b.data
         val bi = b.index
         val bsize = b.iterableSize
         var i = 0
         while(i < bsize) {
           if(b.isActive(i)) result += a(bi(i)) * bd(i)
           i += 1
         }
         result
      }
    }
  }

}

/** This is an auto-generated trait providing operators for SparseVector */
trait SparseVectorOps_Int { this: SparseVector.type =>

  def pureFromUpdate_Int[Other,Op<:OpType](op: BinaryUpdateOp[SparseVector[Int], Other, Op])(implicit copy: CanCopy[SparseVector[Int]]):BinaryOp[SparseVector[Int], Other, Op, SparseVector[Int]] = {
    new BinaryOp[SparseVector[Int], Other, Op, SparseVector[Int]] {
      override def apply(a : SparseVector[Int], b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }
        

  implicit val canSubInto_VV_Int: BinaryUpdateOp[SparseVector[Int], SparseVector[Int], breeze.linalg.operators.OpSub] = {
    new BinaryUpdateOp[SparseVector[Int], SparseVector[Int], breeze.linalg.operators.OpSub] {
      def apply(a: SparseVector[Int], b: SparseVector[Int]) {
        require(b.length == a.length, "Vectors must be the same length!")

        // TODO: decide the appropriate value of 3 and 30 here.
        if(b.used > a.used * 3 && b.used > 30) {
          val c = copy(b)
          apply(c, a)
          c*= (-1).toInt
          a.data = c.data
          a.index = c.index
          a.used = c.used
          return
        }

        var buf:Array[Int] = null
        var bufi:Array[Int] = null
        var nused = 0

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize
        var i = 0
        while(i < bsize) {
          if (a.contains(bi(i))) {
                // just add it in if it's there
                a(bi(i)) = a(bi(i)) - bd(i)
          } else { // not there
                if(buf eq null) {
                  buf = new Array[Int](b.used - i)
                  bufi = new Array[Int](b.used - i)
                } else if(buf.length == nused) {
                  buf = Arrays.copyOf(buf, nused + b.used - i)
                  bufi = Arrays.copyOf(bufi, nused + b.used - i)
                }

                // append to buffer to merged in later
                buf(nused) = buf(nused) - bd(i)
                bufi(nused) = bi(i)
                nused += 1
          }
          i += 1
        }

        // merge two disjoint sorted lists
        if(buf != null) {
          val result = new Array[Int](a.used + nused)
          val resultI = new Array[Int](a.used + nused)
          var ni = 0
          var ai = 0
          var out = 0

          while(ni < nused) {
                while(ai < a.used && a.index(ai) < bufi(ni) ) {
                  result(out) = a.data(ai)
                  resultI(out) = a.index(ai)
                  ai += 1
                  out += 1
                }
                result(out) = buf(ni)
                resultI(out) = bufi(ni)
                out += 1
                ni += 1
          }

          System.arraycopy(a.data, ai, result, out, result.length - out)
          System.arraycopy(a.index, ai, resultI, out, result.length - out)
          out = result.length

          a.data = result
          a.index = resultI
          a.used = out
        }
        
      }
    }
  }

  Vector.canSubInto_V_V_Int.register(canSubInto_VV_Int)

  implicit val canSub_VV_Int: BinaryOp[SparseVector[Int], SparseVector[Int], breeze.linalg.operators.OpSub, SparseVector[Int]] = pureFromUpdate_Int(canSubInto_VV_Int)
  Vector.canSub_V_V_Int.register(canSub_VV_Int)


  implicit val canSubInto_SV_S_Int: BinaryUpdateOp[SparseVector[Int], Int, breeze.linalg.operators.OpSub] = {
    new BinaryUpdateOp[SparseVector[Int], Int, breeze.linalg.operators.OpSub] {
      def apply(a: SparseVector[Int], b: Int) {
        

        var i = 0
        while(i < a.length) {
          a(i) = a(i) - b
          i += 1
        }
        
      }
    }
  }

  Vector.canSubInto_V_S_Int.register(canSubInto_SV_S_Int)

  implicit val canSub_SV_S_Int: BinaryOp[SparseVector[Int], Int, breeze.linalg.operators.OpSub, SparseVector[Int]] = pureFromUpdate_Int(canSubInto_SV_S_Int)
  Vector.canSub_V_S_Int.register(canSub_SV_S_Int)


  implicit val canSetInto_VV_Int: BinaryUpdateOp[SparseVector[Int], SparseVector[Int], breeze.linalg.operators.OpSet] = {
    new BinaryUpdateOp[SparseVector[Int], SparseVector[Int], breeze.linalg.operators.OpSet] {
      def apply(a: SparseVector[Int], b: SparseVector[Int]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = b(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canSetInto_V_V_Int.register(canSetInto_VV_Int)

  implicit val canSet_VV_Int: BinaryOp[SparseVector[Int], SparseVector[Int], breeze.linalg.operators.OpSet, SparseVector[Int]] = pureFromUpdate_Int(canSetInto_VV_Int)
  Vector.canSet_V_V_Int.register(canSet_VV_Int)


  implicit val canSetInto_SV_S_Int: BinaryUpdateOp[SparseVector[Int], Int, breeze.linalg.operators.OpSet] = {
    new BinaryUpdateOp[SparseVector[Int], Int, breeze.linalg.operators.OpSet] {
      def apply(a: SparseVector[Int], b: Int) {
        

        var i = 0
        while(i < a.length) {
          a(i) = b
          i += 1
        }
        
      }
    }
  }

  Vector.canSetInto_V_S_Int.register(canSetInto_SV_S_Int)

  implicit val canSet_SV_S_Int: BinaryOp[SparseVector[Int], Int, breeze.linalg.operators.OpSet, SparseVector[Int]] = pureFromUpdate_Int(canSetInto_SV_S_Int)
  Vector.canSet_V_S_Int.register(canSet_SV_S_Int)


  implicit val canAddInto_VV_Int: BinaryUpdateOp[SparseVector[Int], SparseVector[Int], breeze.linalg.operators.OpAdd] = {
    new BinaryUpdateOp[SparseVector[Int], SparseVector[Int], breeze.linalg.operators.OpAdd] {
      def apply(a: SparseVector[Int], b: SparseVector[Int]) {
        require(b.length == a.length, "Vectors must be the same length!")

        // TODO: decide the appropriate value of 3 and 30 here.
        if(b.used > a.used * 3 && b.used > 30) {
          val c = copy(b)
          apply(c, a)
          
          a.data = c.data
          a.index = c.index
          a.used = c.used
          return
        }

        var buf:Array[Int] = null
        var bufi:Array[Int] = null
        var nused = 0

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize
        var i = 0
        while(i < bsize) {
          if (a.contains(bi(i))) {
                // just add it in if it's there
                a(bi(i)) = a(bi(i)) + bd(i)
          } else { // not there
                if(buf eq null) {
                  buf = new Array[Int](b.used - i)
                  bufi = new Array[Int](b.used - i)
                } else if(buf.length == nused) {
                  buf = Arrays.copyOf(buf, nused + b.used - i)
                  bufi = Arrays.copyOf(bufi, nused + b.used - i)
                }

                // append to buffer to merged in later
                buf(nused) = buf(nused) + bd(i)
                bufi(nused) = bi(i)
                nused += 1
          }
          i += 1
        }

        // merge two disjoint sorted lists
        if(buf != null) {
          val result = new Array[Int](a.used + nused)
          val resultI = new Array[Int](a.used + nused)
          var ni = 0
          var ai = 0
          var out = 0

          while(ni < nused) {
                while(ai < a.used && a.index(ai) < bufi(ni) ) {
                  result(out) = a.data(ai)
                  resultI(out) = a.index(ai)
                  ai += 1
                  out += 1
                }
                result(out) = buf(ni)
                resultI(out) = bufi(ni)
                out += 1
                ni += 1
          }

          System.arraycopy(a.data, ai, result, out, result.length - out)
          System.arraycopy(a.index, ai, resultI, out, result.length - out)
          out = result.length

          a.data = result
          a.index = resultI
          a.used = out
        }
        
      }
    }
  }

  Vector.canAddInto_V_V_Int.register(canAddInto_VV_Int)

  implicit val canAdd_VV_Int: BinaryOp[SparseVector[Int], SparseVector[Int], breeze.linalg.operators.OpAdd, SparseVector[Int]] = pureFromUpdate_Int(canAddInto_VV_Int)
  Vector.canAdd_V_V_Int.register(canAdd_VV_Int)


  implicit val canAddInto_SV_S_Int: BinaryUpdateOp[SparseVector[Int], Int, breeze.linalg.operators.OpAdd] = {
    new BinaryUpdateOp[SparseVector[Int], Int, breeze.linalg.operators.OpAdd] {
      def apply(a: SparseVector[Int], b: Int) {
        

        var i = 0
        while(i < a.length) {
          a(i) = a(i) + b
          i += 1
        }
        
      }
    }
  }

  Vector.canAddInto_V_S_Int.register(canAddInto_SV_S_Int)

  implicit val canAdd_SV_S_Int: BinaryOp[SparseVector[Int], Int, breeze.linalg.operators.OpAdd, SparseVector[Int]] = pureFromUpdate_Int(canAddInto_SV_S_Int)
  Vector.canAdd_V_S_Int.register(canAdd_SV_S_Int)


  implicit val canPowInto_VV_Int: BinaryUpdateOp[SparseVector[Int], SparseVector[Int], breeze.linalg.operators.OpPow] = {
    new BinaryUpdateOp[SparseVector[Int], SparseVector[Int], breeze.linalg.operators.OpPow] {
      def apply(a: SparseVector[Int], b: SparseVector[Int]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = IntMath.ipow(a(i), b(i))
          i += 1
        }
        
      }
    }
  }

  Vector.canPowInto_V_V_Int.register(canPowInto_VV_Int)

  implicit val canPow_VV_Int: BinaryOp[SparseVector[Int], SparseVector[Int], breeze.linalg.operators.OpPow, SparseVector[Int]] = pureFromUpdate_Int(canPowInto_VV_Int)
  Vector.canPow_V_V_Int.register(canPow_VV_Int)


  implicit val canPowInto_SV_S_Int: BinaryUpdateOp[SparseVector[Int], Int, breeze.linalg.operators.OpPow] = {
    new BinaryUpdateOp[SparseVector[Int], Int, breeze.linalg.operators.OpPow] {
      def apply(a: SparseVector[Int], b: Int) {
        

        var i = 0
        while(i < a.length) {
          a(i) = IntMath.ipow(a(i), b)
          i += 1
        }
        
      }
    }
  }

  Vector.canPowInto_V_S_Int.register(canPowInto_SV_S_Int)

  implicit val canPow_SV_S_Int: BinaryOp[SparseVector[Int], Int, breeze.linalg.operators.OpPow, SparseVector[Int]] = pureFromUpdate_Int(canPowInto_SV_S_Int)
  Vector.canPow_V_S_Int.register(canPow_SV_S_Int)


  implicit val canModInto_VV_Int: BinaryUpdateOp[SparseVector[Int], SparseVector[Int], breeze.linalg.operators.OpMod] = {
    new BinaryUpdateOp[SparseVector[Int], SparseVector[Int], breeze.linalg.operators.OpMod] {
      def apply(a: SparseVector[Int], b: SparseVector[Int]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = a(i) % b(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canModInto_V_V_Int.register(canModInto_VV_Int)

  implicit val canMod_VV_Int: BinaryOp[SparseVector[Int], SparseVector[Int], breeze.linalg.operators.OpMod, SparseVector[Int]] = pureFromUpdate_Int(canModInto_VV_Int)
  Vector.canMod_V_V_Int.register(canMod_VV_Int)


  implicit val canModInto_SV_S_Int: BinaryUpdateOp[SparseVector[Int], Int, breeze.linalg.operators.OpMod] = {
    new BinaryUpdateOp[SparseVector[Int], Int, breeze.linalg.operators.OpMod] {
      def apply(a: SparseVector[Int], b: Int) {
        

        var i = 0
        while(i < a.length) {
          a(i) = a(i) % b
          i += 1
        }
        
      }
    }
  }

  Vector.canModInto_V_S_Int.register(canModInto_SV_S_Int)

  implicit val canMod_SV_S_Int: BinaryOp[SparseVector[Int], Int, breeze.linalg.operators.OpMod, SparseVector[Int]] = pureFromUpdate_Int(canModInto_SV_S_Int)
  Vector.canMod_V_S_Int.register(canMod_SV_S_Int)


  implicit val canDivInto_VV_Int: BinaryUpdateOp[SparseVector[Int], SparseVector[Int], breeze.linalg.operators.OpDiv] = {
    new BinaryUpdateOp[SparseVector[Int], SparseVector[Int], breeze.linalg.operators.OpDiv] {
      def apply(a: SparseVector[Int], b: SparseVector[Int]) {
        require(b.length == a.length, "Vectors must be the same length!")

        var i = 0
        while(i < b.length) {
          a(i) = a(i) / b(i)
          i += 1
        }
        
      }
    }
  }

  Vector.canDivInto_V_V_Int.register(canDivInto_VV_Int)

  implicit val canDiv_VV_Int: BinaryOp[SparseVector[Int], SparseVector[Int], breeze.linalg.operators.OpDiv, SparseVector[Int]] = pureFromUpdate_Int(canDivInto_VV_Int)
  Vector.canDiv_V_V_Int.register(canDiv_VV_Int)


  implicit val canDivInto_SV_S_Int: BinaryUpdateOp[SparseVector[Int], Int, breeze.linalg.operators.OpDiv] = {
    new BinaryUpdateOp[SparseVector[Int], Int, breeze.linalg.operators.OpDiv] {
      def apply(a: SparseVector[Int], b: Int) {
        
    var i = 0
    while(i < a.used) {
      a.data(i) = a.data(i) / b
      i += 1
    }
    
      }
    }
  }

  Vector.canDivInto_V_S_Int.register(canDivInto_SV_S_Int)

  implicit val canDiv_SV_S_Int: BinaryOp[SparseVector[Int], Int, breeze.linalg.operators.OpDiv, SparseVector[Int]] = pureFromUpdate_Int(canDivInto_SV_S_Int)
  Vector.canDiv_V_S_Int.register(canDiv_SV_S_Int)


  implicit val canMulScalarInto_VV_Int: BinaryUpdateOp[SparseVector[Int], SparseVector[Int], breeze.linalg.operators.OpMulScalar] = {
    new BinaryUpdateOp[SparseVector[Int], SparseVector[Int], breeze.linalg.operators.OpMulScalar] {
      def apply(a: SparseVector[Int], b: SparseVector[Int]) {
        require(b.length == a.length, "Vectors must be the same length!")

    val outD = new Array[Int](a.used min b.used)
    val outI = new Array[Int](a.used min b.used)
    var out = 0

    val looper = if(a.used < b.used) a else b
    val other = if(a.used < b.used) b else a

    var i = 0
    val bd = looper.data
    val bi = looper.index
    val bsize = looper.iterableSize
    while(i < bsize) {
      if(looper.isActive(i)) {
        val p = other(bi(i)) * bd(i)
        if (p != 0) {
          outD(i) = p
          outI(i) = bi(i)
        }
      }
      i += 1
    }

    a.data = outD; a.index = outI; a.used = out
    
      }
    }
  }



  implicit val canMulScalar_VV_Int: BinaryOp[SparseVector[Int], SparseVector[Int], breeze.linalg.operators.OpMulScalar, SparseVector[Int]] = {
    new BinaryOp[SparseVector[Int], SparseVector[Int], breeze.linalg.operators.OpMulScalar, SparseVector[Int]] {
      def apply(a: SparseVector[Int], b: SparseVector[Int]) = {
        require(b.length == a.length, "Vectors must be the same length!")

    val outD = new Array[Int](a.used min b.used)
    val outI = new Array[Int](a.used min b.used)
    var out = 0

    val looper = if(a.used < b.used) a else b
    val other = if(a.used < b.used) b else a

    var i = 0
    val bd = looper.data
    val bi = looper.index
    val bsize = looper.iterableSize
    while(i < bsize) {
      if(looper.isActive(i)) {
        val p = other(bi(i)) * bd(i)
        if (p != 0) {
          outD(i) = p
          outI(i) = bi(i)
        }
      }
      i += 1
    }

    new SparseVector(outI, outD, out, a.length)
    
      }
    }
  }



  implicit val canMulScalarInto_SV_S_Int: BinaryUpdateOp[SparseVector[Int], Int, breeze.linalg.operators.OpMulScalar] = {
    new BinaryUpdateOp[SparseVector[Int], Int, breeze.linalg.operators.OpMulScalar] {
      def apply(a: SparseVector[Int], b: Int) {
        
    var i = 0
    while(i < a.used) {
      a.data(i) = a.data(i) * b
      i += 1
    }
    
      }
    }
  }


  implicit val canMulScalar_SV_S_Int: BinaryOp[SparseVector[Int], Int, breeze.linalg.operators.OpMulScalar, SparseVector[Int]] = pureFromUpdate_Int(canMulScalarInto_SV_S_Int)


  implicit val canDotProductSV_Int: BinaryOp[SparseVector[Int], SparseVector[Int], breeze.linalg.operators.OpMulInner, Int] = {
    new BinaryOp[SparseVector[Int], SparseVector[Int], breeze.linalg.operators.OpMulInner, Int] {
      def apply(a: SparseVector[Int], b: SparseVector[Int]) = {
        require(b.length == a.length, "Vectors must be the same length!")

       var result: Int = 0

         val bd = b.data
         val bi = b.index
         val bsize = b.iterableSize
         var i = 0
         while(i < bsize) {
           if(b.isActive(i)) result += a(bi(i)) * bd(i)
           i += 1
         }
         result
      }
    }
  }

}
