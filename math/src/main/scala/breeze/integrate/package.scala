package breeze

import breeze.linalg._

package object integrate {
  
  def trapezoid(f: Double => Double, start: Double, end: Double, nodes: Int) : Double = {
    if (nodes < 2)
      throw new Exception("When using trapezoid, you have to use at least two nodes.")

    val h = (end-start) / (nodes-1)
    val s = sum(for (i <- 0 until nodes) yield f(start+i*h))
    h * (s - (f(start) + f(end))/2.0)
  }

  def simpson(f: Double => Double, start: Double, end: Double, nodes: Int) : Double = {
    if (nodes < 2)
      throw new Exception("When using simpson, you have to use at least two nodes.")

    val h = (end-start) / (nodes-1)
    val s = sum(for (i <- 0 until nodes-1) yield f(start+(i+0.5)*h))
    trapezoid(f, start, end, nodes) / 3.0 + s * 2/3.0 * h
  }

  def ode45(
    f: (DenseVector[Double], Double) => DenseVector[Double],
    y0: DenseVector[Double],
    t: Array[Double],
    relTol: Array[Double] = Array.empty,
    absTol: Array[Double] = Array.empty
    ) : Array[DenseVector[Double]] = {
    
    RungeKuttaOdeSolver(DormandPrinceTableau, f, y0, t, relTol, absTol)
  }

  def ode23(
    f: (DenseVector[Double], Double) => DenseVector[Double],
    y0: DenseVector[Double],
    t: Array[Double],
    relTol: Array[Double] = Array.empty,
    absTol: Array[Double] = Array.empty
    ) : Array[DenseVector[Double]] = {
    
    RungeKuttaOdeSolver(BogackiShampineTableau, f, y0, t, relTol, absTol)
  }
}
