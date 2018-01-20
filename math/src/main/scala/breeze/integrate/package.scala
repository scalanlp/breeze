package breeze

import breeze.linalg._

package object integrate {

  def trapezoid(f: Double => Double, start: Double, end: Double, nodes: Int): Double = {
    if (nodes < 2)
      throw new Exception("When using trapezoid, you have to use at least two nodes.")

    val h = (end - start) / (nodes - 1)
    val s = sum(for (i <- 0 until nodes) yield f(start + i * h))
    h * (s - (f(start) + f(end)) / 2.0)
  }

  def simpson(f: Double => Double, start: Double, end: Double, nodes: Int): Double = {
    if (nodes < 2)
      throw new Exception("When using simpson, you have to use at least two nodes.")

    val h = (end - start) / (nodes - 1)
    val s = sum(for (i <- 0 until nodes - 1) yield f(start + (i + 0.5) * h))
    trapezoid(f, start, end, nodes) / 3.0 + s * 2 / 3.0 * h
  }

  /*
   * ODE functions return a sequence of states corresponding to each value in t.
   *
   * @param f a first order differential equation of the form dy = f(y, t)
   * @param y0 the initial values of the state y at initial time t(0)
   * @param t the times at which a calculation of state y is desired
   * @param relTol relative error tolerance values, must be same length as y0
   * @param absTol absolute error tolerance values, must be same length as y0
   */

  def ode45(
      f: (DenseVector[Double], Double) => DenseVector[Double],
      y0: DenseVector[Double],
      t: Array[Double],
      relTol: DenseVector[Double] = null,
      absTol: DenseVector[Double] = null
  ): Array[DenseVector[Double]] = {

    val integrator = new DormandPrince54Integrator(0.0, 1.0, relTol, absTol)
    integrator.integrate(f, y0, t)
  }
}
