package breeze.compat

object Scala3Compat {
  given [T, U](using ev: T<:<U): Conversion[T, U] with
    override def apply(x: T) = ev(x)

  def Conversion[T, U](f: T=>U): Conversion[T, U] = (t: T) => f(t)

}
