package scalanlp.util

object Implicits extends DoubleImplicits {

}

trait DoubleImplicits {
  class RichDouble(x: Double) {
    def closeTo(y: Double, tol: Double=1E-5) = {
      (math.abs(x - y) / (math.abs(x) + math.abs(y) + 1e-10) < tol);
    }
    def isDangerous = x.isNaN || x.isInfinite
  }

  implicit def scEnrichDouble(x: Double) = new RichDouble(x);
}
