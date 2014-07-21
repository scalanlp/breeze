package breeze

import breeze.linalg._

package object integrate {
  def trapezoid_integrate(f: Double => Double, start: Double, end: Double, nodes: Int) : Double = {
    if (nodes < 2)
      throw new Exception("When using trapezoid_integrate, you have to use at least two nodes.")

    val h = (end-start) / (nodes-1)
    val s = sum(for (i <- 0 until nodes) yield f(start+i*h))
    h * (s - (f(start) + f(end))/2.0)
  }

  def simpson_integrate(f: Double => Double, start: Double, end: Double, nodes: Int) : Double = {
    if (nodes < 2)
      throw new Exception("When using simpson_integrate, you have to use at least two nodes.")

    val h = (end-start) / (nodes-1)
    val s = sum(for (i <- 0 until nodes-1) yield f(start+(i+0.5)*h))
    trapezoid_integrate(f, start, end, nodes) / 3.0 + s * 2/3.0 * h
  }

  def foo() {
    println("BLB!")
  }
}