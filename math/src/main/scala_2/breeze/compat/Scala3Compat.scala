package breeze.compat

object Scala3Compat {
  type Conversion[T, +U] = T => U

  def Conversion[T, U](f: T => U): Conversion[T, U] = f

  def given_Conversion_T_U[T, U](implicit ev: T<:<U): Conversion[T, U] = ev

}
