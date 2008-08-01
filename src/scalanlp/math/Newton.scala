package scalanlp.math;

object Newton {
  def optimize(df: Double=>Double, d2f : Double=>Double, x0 : Double, tol : Double) = { 
    var x = x0;
    var dfdx = df(x);
    while( Math.abs(dfdx) > tol) {
      x = x - dfdx/d2f(x);
      dfdx = df(x);
    }
  }
}
