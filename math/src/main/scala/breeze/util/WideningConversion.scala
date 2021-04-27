package breeze.util

/** Sort of like [[Conversion]] but intended to be restricted to core numeric types */
trait WideningConversion[@specialized(Int, Float, Short, Byte) F, @specialized(Int, Long, Double) T] {
  def apply(f: F): T
}

object WideningConversion {
  implicit val int2Double: WideningConversion[Int, Double] = scala.Int.int2double _
  implicit val int2Long: WideningConversion[Int, Long] = scala.Int.int2long _
  implicit val float2Double: WideningConversion[Float, Double] = scala.Float.float2double _

  implicit val short2Int: WideningConversion[Short, Int] = scala.Short.short2int _
  implicit val short2Long: WideningConversion[Short, Long] = scala.Short.short2long _
  implicit val short2Float: WideningConversion[Short, Float] = scala.Short.short2float _
  implicit val short2Double: WideningConversion[Short, Double] = scala.Short.short2double _

  implicit val byte2Int: WideningConversion[Byte, Int] = scala.Byte.byte2int _
  implicit val byte2Short: WideningConversion[Byte, Short] = scala.Byte.byte2short _
  implicit val byte2Long: WideningConversion[Byte, Long] = scala.Byte.byte2long _
  implicit val byte2Float: WideningConversion[Byte, Float] = scala.Byte.byte2float _
  implicit val byte2Double: WideningConversion[Byte, Double] = scala.Byte.byte2double _
}
