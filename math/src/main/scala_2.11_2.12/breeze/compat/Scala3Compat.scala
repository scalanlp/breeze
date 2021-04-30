package breeze.compat

object Scala3Compat {
  type Conversion[T, U] = T=>U

  def Conversion[T, U](f: T=>U): Conversion[T, U] = f

}
