package breeze.compat

object Scala3Compat {
  given [T, U](using ev: T<:<U): Conversion[T, U] with
    override def apply(x: T) = ev(x)

}
