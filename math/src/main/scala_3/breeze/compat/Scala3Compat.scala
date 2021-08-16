package breeze.compat

trait ConversionOrSubtype[T, +U] extends Conversion[T, U] {

}

sealed trait ConversionOrSubtypeLowPrio {
  given conversionOk[T, U](using ev: Conversion[T, U]): ConversionOrSubtype[T, U] with
    override def apply(v1: T): U = ev(v1)

}

object ConversionOrSubtype extends ConversionOrSubtypeLowPrio {
  given subtypeOk[T, U](using ev: T<:<U): ConversionOrSubtype[T, U] with
    override def apply(x: T) = ev(x)

}

object Scala3Compat {
  def Conversion[T, U](f: T=>U): Conversion[T, U] = (t: T) => f(t)
}
