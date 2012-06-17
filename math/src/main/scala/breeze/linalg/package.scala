package breeze

import generic.{CanMapValues, UReduceable, URFunc}
import linalg.operators.{OpSub, BinaryOp, OpDiv}
import linalg.support.{CanNorm, CanCopy}

/**
 *
 * @author dlwh
 */
package object linalg {

  /**
   * returns a vector along the diagonal of v.
   * Requires a square matrix?
   * @param m the matrix
   * @tparam V
   */
  def diag[V](m: DenseMatrix[V]) = {
    require(m.rows == m.cols, "m must be square")
    new DenseVector(m.data, m.offset, m.majorStride + 1, m.rows)
  }

  /**
   * Generates a vector of linearly spaced values between a and b (inclusive).
   * The returned vector will have length elements, defaulting to 100.
   */
  def linspace(a : Double, b : Double, length : Int = 100) : DenseVector[Double] = {
    val increment = (b - a) / (length - 1)
    DenseVector.tabulate(length)(i => a + increment * i)
  }

  def copy[T](t: T)(implicit canCopy: CanCopy[T]): T = canCopy(t)

  def norm[T](t: T, v: Double = 2)(implicit canNorm: CanNorm[T]) = canNorm(t, v)


  /**
   * Normalizes the argument such that its norm is 1.0 (with respect to the argument n).
   * Returns value if value's norm is 0.
   */
  def normalize[T, U>:T](t: T, n: Double = 2)(implicit div: BinaryOp[T, Double, OpDiv, U], canNorm: CanNorm[T]): U = {
    val norm = canNorm(t, n)
    if(norm == 0) t
    else div(t,norm)
  }

  /**
   * logNormalizes the argument such that the softmax is 0.0.
   * Returns value if value's softmax is -infinity
   */
  def logNormalize[V,K](value: V)(implicit view: V => NumericOps[V],
                                  red: UReduceable[V, Double],
                                  op : BinaryOp[V,Double,OpSub,V]): V = {
    val max = softmax(value)
    if(max.isInfinite) value
    else value - max
  }

  /**
   * logs and then logNormalizes the argument such that the softmax is 0.0.
   * Returns value if value's softmax is -infinity
   */
  def logAndNormalize[V](value: V)(implicit view: V => NumericOps[V],
                                   red: UReduceable[V, Double],
                                   map: CanMapValues[V, Double, Double, V],
                                   op : BinaryOp[V,Double,OpSub,V]):V = {
    logNormalize(numerics.log(value))
  }


  val mean:URFunc[Double, Double] = new URFunc[Double, Double] {
    def apply(cc: TraversableOnce[Double]) =  {
      val (sum,n) = accumulateAndCount(cc)
      sum / n
    }

    def accumulateAndCount(it : TraversableOnce[Double]):(Double, Int) = it.foldLeft( (0.0,0) ) { (tup,d) =>
      (tup._1 + d, tup._2 + 1)
    }

    override def apply(arr: Array[Double], offset: Int, stride: Int, length: Int, isUsed: (Int) => Boolean) = {
      var i = 0
      var used = 0
      var sum = 0.0
      var off = offset
      while(i < length) {
        if(isUsed(i)) {
          sum += arr(off)
          used += 1
        }
        i += 1
        off += stride
      }
      sum / used
    }
  }


  val meanAndVariance:URFunc[Double, (Double,Double)] = new URFunc[Double, (Double,Double)] {
    def apply(it: TraversableOnce[Double]) = {
      val (mu,s,n) = it.foldLeft( (0.0,0.0,0)) { (acc,y) =>
        val (oldMu,oldVar,n) = acc
        val i = n+1
        val d = y - oldMu
        val mu = oldMu + 1.0/i * d
        val s = oldVar + (i-1) * d / i * d
        (mu,s,i)
      }
      (mu,s/(n-1))
    }

    override def apply(arr: Array[Double], offset: Int, stride: Int, length: Int, isUsed: (Int) => Boolean) = {
      var mu = 0.0
      var s = 0.0
      var n = 0
      var i = 0
      var off = offset
      while(i < length) {
        if(isUsed(i)) {
          val y = arr(off)
          n += 1
          val d = y - mu
          mu = mu + 1.0/n * d
          s = s + (n-1) * d / n * d
        }
        off += stride
        i += 1
      }
      (mu, s/(n-1))

    }
  }

  val variance:URFunc[Double, Double] = new URFunc[Double, Double] {
    def apply(cc: TraversableOnce[Double]) =  {
      meanAndVariance(cc)._2
    }


    override def apply(arr: Array[Double], offset: Int, stride: Int, length: Int, isUsed: (Int) => Boolean) = {
      meanAndVariance(arr, offset, stride, length, isUsed)._2
    }
  }

  val stddev:URFunc[Double, Double] = new URFunc[Double, Double] {
    def apply(cc: TraversableOnce[Double]) =  {
      scala.math.sqrt(variance(cc))
    }

    override def apply(arr: Array[Double], offset: Int, stride: Int, length: Int, isUsed: (Int) => Boolean) = {
      scala.math.sqrt(variance(arr, offset, stride, length, isUsed))
    }
  }

  val max:URFunc[Double, Double] = new URFunc[Double, Double] {
    def apply(cc: TraversableOnce[Double]) =  {
      cc.max
    }

    override def apply(arr: Array[Double], offset: Int, stride: Int,length: Int, isUsed: (Int) => Boolean) = {
      var max = Double.NegativeInfinity
      var i = 0
      var off = offset
      while(i < length) {
        if(isUsed(i)) {
          val m = arr(off)
          if(max < m) max = m
        }
        off += stride
        i += 1
      }
      max
    }
  }


  val min:URFunc[Double, Double] = new URFunc[Double, Double] {
    def apply(cc: TraversableOnce[Double]) =  {
      cc.min
    }

    override def apply(arr: Array[Double], offset: Int, stride: Int, length: Int, isUsed: (Int) => Boolean) = {
      var min = Double.NegativeInfinity
      var i = 0
      var off = offset
      while(i < length) {
        if(isUsed(i)) {
          val m = arr(off)
          if(min > m) min = m
        }
        off += stride
        i += 1
      }
      min
    }
  }

  val softmax:URFunc[Double, Double] = new URFunc[Double, Double] {
    def apply(cc: TraversableOnce[Double]) =  {
      val a = cc.toArray[Double]
      breeze.numerics.logSum(a, a.length)
      // apply(cc.toArray) breaks the compiler...
    }


    override def apply(a: Array[Double], length: Int) = {
      numerics.logSum(a, length)
    }

    override def apply(a: Array[Double], offset: Int, stride: Int, length: Int, isUsed: (Int) => Boolean) = {
      length match {
        case 0 => Double.NegativeInfinity
        case 1 => if(isUsed(0)) a(0) else Double.NegativeInfinity
        case 2 =>
          if (isUsed(0))
            if (isUsed(1))
              numerics.logSum(a(0),a(1))
            else a(0)
          else if(isUsed(1)) a(1)
          else Double.NegativeInfinity
        case _ =>
          val m = max(a, offset, stride, length, isUsed)
          if (m.isInfinite) m
          else {
            var i = 0
            var off = offset
            var accum = 0.0
            while(i < length) {
              if(isUsed(i))
                accum += scala.math.exp(a(off) - m)
              i += 1
              off += stride
            }
            if (i > 0)
              m + scala.math.log(accum)
            else Double.NegativeInfinity
          }
      }
    }
  }

}

