package breeze.linalg

import breeze.generic.UFunc
import org.netlib.util.intW
import com.github.fommil.netlib.LAPACK.{getInstance=>lapack}
import com.github.fommil.netlib.ARPACK
import org.netlib.util.intW
import org.netlib.util.doubleW
/**
  * Computes the SVD of a m by n matrix
  * Returns an m*m matrix U, a vector of singular values, and a n*n matrix V'
  */
object svd extends UFunc {
  implicit object Svd_DM_Impl extends Impl[DenseMatrix[Double], (DenseMatrix[Double], DenseVector[Double], DenseMatrix[Double])] {
    def apply(mat: DenseMatrix[Double]): (DenseMatrix[Double], DenseVector[Double], DenseMatrix[Double]) = {
      requireNonEmptyMatrix(mat)

      val m = mat.rows
      val n = mat.cols
      val S = DenseVector.zeros[Double](m min n)
      val U = DenseMatrix.zeros[Double](m,m)
      val Vt = DenseMatrix.zeros[Double](n,n)
      val iwork = new Array[Int](8 * (m min n) )
      val workSize = ( 3
        * scala.math.min(m, n)
        * scala.math.min(m, n)
        + scala.math.max(scala.math.max(m, n), 4 * scala.math.min(m, n)
          * scala.math.min(m, n) + 4 * scala.math.min(m, n))
      )
      val work = new Array[Double](workSize)
      val info = new intW(0)
      val cm = copy(mat)

      lapack.dgesdd(
        "A", m, n,
        cm.data, scala.math.max(1,m),
        S.data, U.data, scala.math.max(1,m),
        Vt.data, scala.math.max(1,n),
        work,work.length,iwork, info)

      if (info.`val` > 0)
        throw new NotConvergedException(NotConvergedException.Iterations)
      else if (info.`val` < 0)
        throw new IllegalArgumentException()

      (U,S,Vt)
    }
  }

/*
 returns
 u : shape=(M, k)
 Unitary matrix having left singular vectors as columns.

 s : Dense vector of singular values.

 vt : shape=(k, N)
 Unitary matrix having right singular vectors as rows.
 */

  implicit object Svd_SM_Impl extends Impl2[CSCMatrix[Double],Int, (DenseMatrix[Double], DenseVector[Double], DenseMatrix[Double])] {

     val arpack  = ARPACK.getInstance()

    def av( mat:CSCMatrix[Double], work:Array[Double], input_offset:Int, output_offset:Int) {
      val w = DenseVector(work)
      val x = w(input_offset until input_offset+mat.cols)
      val y = w(output_offset until output_offset+mat.cols)

      y := mat * x 
     
    }

    def apply(mt: CSCMatrix[Double],eigenvals:Int): (DenseMatrix[Double], DenseVector[Double], DenseMatrix[Double]) = {

      
      val tol = new doubleW(0.0001)
      val TOL = 0.0001

      
      requireNonEmptyMatrix(mt)

      val mat = mt * mt.t

      val n = mat.rows
      val m = mat.cols

      val nev = new intW(eigenvals)
      val ncv = scala.math.min(2*eigenvals,n)

      val bmat = "I"
      val which = "LM"
      
      var iparam = Array(1,0,300,0,0,0,1)

      var ido = new intW(0)
      var info = new intW(0)

      var resid:Array[Double] = new Array[Double](n)

      var v = new Array[Double](n*ncv)

      var workd = new Array[Double](3*n)

      var workl = new Array[Double](ncv*(ncv+8))

      var ipntr = new Array[Int](11)

      var i:Int = 0
      
      import scala.util.control.Breaks._
      breakable{
        while(true)
        {

          i = i+1
          arpack.dsaupd(ido,bmat,n,which,nev.`val`,tol,resid,ncv,v,n,iparam,ipntr,workd,workl,workl.length,info)

          if(ido.`val` == 99) break

          if (ido.`val` != -1 && ido.`val` != 1) throw new IllegalStateException("ido = " + ido.`val`)

          av(mat,workd, (ipntr(0) - 1), (ipntr(1) - 1))
        }
      }

      if (info.`val` != 0) throw new IllegalStateException("info = " + info.`val`)
      


      val d = new Array[Double](nev.`val`);
      val select = new Array[Boolean](ncv)
      val z = java.util.Arrays.copyOfRange(v, 0, nev.`val` * n)

      arpack.dseupd(true, "A", select, d, z, n, 0.0, bmat, n, which, nev, TOL, resid, ncv, v, n, iparam, ipntr, workd, workl, workl.length, info)


      val computed = iparam(4)

      val eigenVectors = new DenseVector(z)

    //  var mp:scala.collection.Map[Double,DenseVector[Double]] = Map[Double,DenseVector[Double]]()
      var mp = new Array[(Double,DenseVector[Double])](computed)

      var sa = Array(computed)

      for( i <- 0 until computed){
        val eigenVal = d(i)
        val eigenVec = eigenVectors(i*n until i*n + n)
        mp(i) = (scala.math.sqrt(eigenVal),eigenVec)
      }

      mp = mp.sortBy(-1*_._1)
      val sp = mp.map(_._1) //mp.map{case(k,v) => scala.math.sqrt(k)}

      val s = DenseVector(sp.toArray)
      val si = DenseVector(sp.map(u => 1/u).toArray)
      if(n <= m)
      {
        val va = mp.map{case(k,v) => v}
        val u = DenseMatrix(va.map(r => r.toArray).toSeq:_*)
        val v = (mt.t * u.t) * diag(si)
        
        (u,s,v.t)
      }

      else{
        val ua = mp.map{case(k,v) => v}
        val u = DenseMatrix(ua.map(r => r.toArray).toSeq:_*)
        
        val vt = (mt.t *u.t)*diag(si)
        val dg = diag(si)
        println(n,m)
        println(dg.rows,dg.cols)
        println(vt.rows,vt.cols)
        println("asdjkasjdjlkdsaljksdalkj")

        println(vt)
        
        (u,s,vt)
      }
    }

  }

}
