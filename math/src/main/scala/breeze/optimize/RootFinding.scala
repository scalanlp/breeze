package breeze.optimize

import scala.annotation.tailrec

/**
  * Root finding algorithms
  * @author abertout
  */

object RootFinding {
  lazy val eps: Double = math.ulp(1d)
  lazy val defaultMaxIter: Int = 1000

  /**
    * Generic method to compute a root approximation x of a function f such that f(x) = 0
    * Wrapper for Brent's method
    * @param fn function
    * @param x0 first root estimate
    * @param x1 optional second root estimate such that [x0,x1] brackets x
    * @return
    */
  def find(fn: Double => Double, x0: Double, x1: Option[Double] = None): Double = {
    //Generate a second estimation if needed
    val xx1 = x1 match {
      case Some(x) => x
      case None =>  //search in an ever-widening range around the initial point (taken from octave fzero)
        val search = Seq(-.01,.025, -.05, .1, -.25, .5, -1, 2.5, -5, 10, -50, 100, -500, 1000)
        val ff = search.view.map(s => x0 + x0 * s).find(b => fn(x0) * fn(b) <= 0)
        ff match {
          case Some(estimate) => estimate
          case None => throw new IllegalArgumentException("Search of second bracketing value failed")
        }
    }
    brent(fn, x0, xx1)
  }


  /**
    * Bisection bracketing method with linear convergence
    */
  def bisection(fn: Double => Double, a: Double, b: Double): Double = {
    val fa = fn(a)
    val fb = fn(b)
    require(fa.signum != fb.signum , "The root is not bracketed by the given interval")

    @tailrec
    def bis(a: Double, b: Double): Double = {
      val m = (a+b)/2
      val fm = fn(m)
      if(fm.abs < 2 * eps)
        return m
      if(fm.signum != fa.signum)
        bis(a,m)
      else bis(m,b)
    }
    bis(a,b)
  }

  /**
    * Newton-Raphson's open method with quadratic convergence (requires the derivative and a limited number of iterations to cope with divergence)
    */

  def newtonRaphson(fn: Double => Double, fd: Double => Double, x0: Double, maxIter: Int = defaultMaxIter ): Double = {
    @tailrec
    def nr(x: Double, iter: Int): Double = {
      if(fn(x).abs < 2 * eps || iter == maxIter)
        return x
      nr(x - fn(x)/fd(x),iter + 1)
    }
    nr(x0,0)
  }

  /**
    * Secant method (based on a linear approximation of the derivative between successive pair of points)
    */

  def secant(fn: Double => Double, x0: Double, x1: Double, maxIter: Int = defaultMaxIter ): Double = {
    @tailrec
    def se(x0: Double, x1: Double, iter: Int): Double = {
      if(fn(x1).abs < 2 * eps || iter == maxIter)
        return x1
      val fx1 = fn(x1)
      se(x1,x1 - (x1 - x0)/(fx1 - fn(x0))*fx1,iter + 1)
    }
    se(x0,x1, 0)
  }

  /**
    * Implementation of Brent root-finding algorithm Brent, R.,
    * Algorithms for Minimization Without Derivatives, Prentice-Hall, 1973.
    */
  def brent(fn: Double => Double, a: Double, b: Double): Double = {
    val (fa, fb) = (fn(a), fn(b))
    require(fa.signum != fb.signum, "The root is not bracketed by the given interval")


    @tailrec
    def brentAux(aa: Double, bb: Double, cc: Double, dd: Double, ee: Double,
                 ffa: Double, ffb: Double, ffc : Double): Double = {

      var (a,b,c,d,e) = (aa,bb,cc,dd,ee)
      var (fa,fb,fc) = (ffa,ffb,ffc)

      if (fb == 0) return b

      if (fc.signum == fb.signum){
        // if necessary rearrange points
        c = a; fc = fa; d = b - a; e = d
      }

      if (fc.abs < fb.abs){ // swap values to have |f(b)| <= |f(c)|
        a = b;b = c;c = a
        fa = fb;fb = fc; fc = fa
      }
      val m = .5 * (c - b)
      val tol = 2 * eps * math.max(b.abs,1)// b + t with t positive tolerance in the original article
      if(m.abs <= tol || fb == 0)
        return b
      // check if bisection is needed
      if(e.abs < tol || fa.abs <= fb.abs){
        e = m;d = m
      }else{ //...otherwise evaluate with
        val s = fb / fa
        val (p1, q1) =  if (a == c) {
          (2 * m * s, 1 - s) //secant method (linear interpolation)
        } else {
          val q0 = fa / fc
          val r = fb / fc
          //or inverse quadratic interpolation
          (s * (2*m*q0*(q0-r) - (b-a)*(r-1)), (q0-1)*(r-1)*(s-1))
        }
        val (q, p) = if (p1 > 0) (-q1, p1) else (q1, -p1)
        //s = e;e = d
        val (td,te) = if ((2*p < 3*m*q - (tol*q).abs) && (p < (.5*e*q).abs)) (p/q,d) else (m,m)
        d = td;e = te
      }
      a = b;fa = fb
      b += (if (d.abs > tol) d else if (m > 0) tol else -tol)
      fb = fn(b)
      brentAux(a,b,c,d,e,fa,fb,fc)
    }
    brentAux(a,b,a,b-a, b-a,fa,fb,fa)
  }


}

