package breeze.ode

import breeze.linalg._

/**
* Runge-Kutta ODE integration step routines.
*
* @author jaketimothy
*/
trait AdaptiveODEStepper {
  def apply(
    f: (Double, DenseVector[Double]) => DenseVector[Double],
    start: Double,
    end: Double,
    y0: DenseVector[Double],
    last: Option[DenseVector[Double]] = None
    ) : (DenseVector[Double], DenseVector[Double])
}

/**
* TODO: Implement First Same As Last speed up.
*/
trait AdaptiveRungeKuttaStepper extends AdaptiveODEStepper {
  def stages: Int

  def a: Array[Array[Double]]
  def b: Array[Double]
  def bStar: Array[Double]
  def c: Array[Double]

  final override def apply(
    f: (Double, DenseVector[Double]) => DenseVector[Double],
    start: Double,
    end: Double,
    y0: DenseVector[Double],
    last: Option[DenseVector[Double]] = None
    ) : (DenseVector[Double], DenseVector[Double]) = {

    val h = end-start

    val k = Array.fill[DenseVector[Double]](stages)(DenseVector.zeros(y0.length))

    k(0) = f(start, y0)
    for (i <- 0 until stages-1) {
      k(i) = f(start + h*c(i), y0 + h*sum(for (j <- 0 until i) yield a(i)(j)*k(j)))
    }

    val y = y0 + h*sum(for (i <- 0 until stages) yield b(i)*k(i))
    val error = y - (y0 + h*sum(for (i <- 0 until stages) yield bStar(i)*k(i)))
    (y, error)
  }
}

/**
* The Euler-Heun 1(2) method.
*/
object RKHEStepper extends AdaptiveRungeKuttaStepper {
  override val stages = 2

  override val a = Array(Array(1.0))

  override val b = Array(0.5, 0.5)
  override val bStar = Array(1.0, 0.0)
  override val c = Array(1.0)
}

/**
* The Bogacki-Shampine 2(3) method (First Same As Last).
*/
object RKBSStepper extends AdaptiveRungeKuttaStepper {
  override val stages = 4

  override val a = Array(
    Array(0.5),
    Array(0.0, 3.0/4.0),
    Array(2.0/9.0, 1.0/3.0, 4.0/9.0))

  override val b = Array(2.0/9.0, 1.0/3.0, 4.0/9.0, 0.0)
  override val bStar = Array(7.0/24.0, 0.25, 1.0/3.0, 1.0/8.0)
  override val c = Array(0.5, 0.75, 1.0)
}

/**
* The Runge-Kutta-Fehlberg 4(5) method.
*/
object RKFStepper extends AdaptiveRungeKuttaStepper {
  override val stages = 6

  override val a = Array(
    Array(0.25),
    Array(3.0, 3.0).map(_ / 32.0),
    Array(1932.0, -7200.0, 7296.0).map(_ / 2197.0),
    Array(439.0/216.0, -8.0, 3680.0/513.0, -845.0/4104.0),
    Array(-8.0/27.0, 2.0, -3544.0/2565.0, 1859.0/4104.0, -11.0/44.0))

  override val b = Array(16.0/135.0, 0.0, 6656.0/12825.0, 28561.0/56430.0, -9.0/50.0, 2.0/55.0)
  override val bStar = Array(25.0/216.0, 0.0, 1408.0/2565.0, 2197.0/4104.0, -0.2, 0.0)
  override val c = Array(0.25, 3.0/8.0, 12.0/13.0, 1.0, 0.5)
}

/**
* The Cash-Karp 4(5) method.
*/
object RKCKStepper extends AdaptiveRungeKuttaStepper {
  override val stages = 6

  override val a = Array(
    Array(0.2),
    Array(3.0, 9.0).map(_ / 40.0),
    Array(0.3, -0.9, 6.0/5.0),
    Array(-11.0/54.0, 5.0/2.0, -70.0/27.0, 35.0/27.0),
    Array(1631.0/55296.0, 175.0/512.0, 575.0/13824.0, 44275.0/110592.0, 253.0/4096.0))

  override val b = Array(37.0/378.0, 0.0, 250.0/621.0, 125.0/594.0, 0.0, 512.0/1771.0)
  override val bStar = Array(2825.0/27648.0, 0.0, 18575.0/48384.0, 13525.0/55296.0, 277.0/14336.0, 0.25)
  override val c = Array(0.2, 0.3, 0.6, 1.0, 7.0/8.0)
}

/**
* The Dormand-Prince 4(5) method (First Same As Last).
*/
object RKDPStepper extends AdaptiveRungeKuttaStepper {
  override val stages = 7

  override val a = Array(
    Array(0.2),
    Array(3.0, 9.0).map(_ / 40.0),
    Array(44.0/45.0, -56.0/15.0, 32.0/9.0),
    Array(19372.0/6561.0, -25360.0/2187.0, 64448.0/6561.0, -212.0/729.0),
    Array(9017.0/3168.0, -355.0/33.0, 46732.0/5247.0, 49.0/176.0, -5103.0/18656.0),
    Array(35.0/384.0, 0.0, 500.0/1113.0, 125.0/192.0, -2187.0/6784.0, 11.0/84.0))

  override val b = Array(35.0/384.0, 0.0, 500.0/1113.0, 125.0/192.0, -2187.0/6784.0, 11.0/84.0, 0.0)
  override val bStar = Array(5179.0/57600.0, 0.0, 7571.0/16695.0, 393.0/640.0, -92097.0/339200.0, 187.0/2100.0, 1.0/40.0)
  override val c = Array(0.2, 0.3, 0.8, 8.0/9.0, 1.0, 1.0)
}
