package breeze

/**
 *
 * @author dlwh
 */
package object math {
  val i = Complex.i

  class RichField(value : Double) {
    def + (c : Complex) : Complex = Complex(value,0) + c
    def - (c : Complex) : Complex = Complex(value,0) - c
    def * (c : Complex) : Complex = Complex(value,0) * c
    def / (c : Complex) : Complex = Complex(value,0) / c
  }

  implicit def richInt(value : Int) =
    new RichField(value)

  implicit def richLong(value : Long) =
    new RichField(value)

  implicit def richFloat(value : Float) =
    new RichField(value)

  implicit def richDouble(value : Double) =
    new RichField(value)
}

