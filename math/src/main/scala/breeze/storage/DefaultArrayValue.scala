package breeze.storage

@SerialVersionUID(1l)
trait DefaultArrayValue[@specialized T] extends Serializable {
  def value : T
}

object DefaultArrayValue {
  def apply[T](v: T):DefaultArrayValue[T] = new DefaultArrayValue[T] {
    def value = v
  }

  implicit object IntDefaultArrayValue extends DefaultArrayValue[Int] {
    override def value = 0
  }

  implicit object ShortDefaultArrayValue extends DefaultArrayValue[Short] {
    override def value = 0.toShort
  }

  implicit object LongDefaultArrayValue extends DefaultArrayValue[Long] {
    override def value = 0l
  }

  implicit object ByteDefaultArrayValue extends DefaultArrayValue[Byte] {
    override def value = 0.toByte
  }

  implicit object CharDefaultArrayValue extends DefaultArrayValue[Char] {
    override def value = 0.toChar
  }

  implicit object FloatDefaultArrayValue extends DefaultArrayValue[Float] {
    override def value = 0.0f
  }

  implicit object DoubleDefaultArrayValue extends DefaultArrayValue[Double] {
    override def value = 0.0
  }

  implicit object BooleanDefaultArrayValue extends DefaultArrayValue[Boolean] {
    override def value = false
  }

  val refDefault = new DefaultArrayValue[AnyRef] {
    override def value : AnyRef = null
  }

  implicit def ObjectDefaultArrayValue[T<:AnyRef] =
    refDefault.asInstanceOf[DefaultArrayValue[T]]
}

