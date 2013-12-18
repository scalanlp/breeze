package breeze.io

import java.io.{File,IOException}


/** See [[breeze.io.RandomAccessFile]], reads little endian.
  */
class RandomAccessFileLE(file: File, arg0: String = "r")  extends RandomAccessFile(file, arg0) {

  def this(filename: String, arg0: String) = this(new File(filename), arg0)

  override val converter: ByteConverter = breeze.io.ByteConverterLE

  ///// Int16 (Short) /////
  @throws(classOf[IOException])
  override def readInt16() ={
    val ba = readByte(2)
    converter.bytesToInt16(ba(0), ba(1))
  }

  ///// UInt16 (Unsigned Short) /////
  @throws(classOf[IOException])
  override def readUInt16(): Char = {
    val ba = readByte(2)
    converter.bytesToUInt16(ba(0), ba(1))
  }

  ///// Int32 (Int) /////
  @throws(classOf[IOException])
  override def readInt32():Int = {
    val ba = readByte(4)
    converter.bytesToInt32(ba(0), ba(1), ba(2), ba(3))
  }

  ///// Int64 (Long) /////
  @throws(classOf[IOException])
  override def readInt64(): Long = {
    val ba = readByte(8)
    converter.bytesToInt64(ba(0), ba(1), ba(2), ba(3), ba(4), ba(5), ba(6), ba(7))
  }

  ///// Floating Point /////
  @throws(classOf[IOException])
  override def readFloat():Float = {
    java.lang.Float.intBitsToFloat(readInt())
  }

  @throws(classOf[IOException])
  override def readDouble(): Double = {
	  java.lang.Double.longBitsToDouble(readLong())
  }

  @throws(classOf[IOException])
  override def writeDouble(v: Double) = {
    writeInt64(java.lang.Double.doubleToLongBits(v))
  }

  @throws(classOf[IOException])
  override def writeFloat(v: Float) =  {
    writeInt32(java.lang.Float.floatToIntBits(v))
  }

}

/** See [[breeze.io.ByteConverter]], reads little endian.
  */
object ByteConverterLE extends ByteConverter  {

  override def bytesToInt16(b0: Byte, b1: Byte)  = ByteConverterBE.bytesToInt16(b1, b0)
  override def bytesToUInt16(b0: Byte, b1: Byte) = ByteConverterBE.bytesToUInt16(b1, b0)
  override def bytesToInt32(b0: Byte, b1: Byte, b2: Byte, b3: Byte) = ByteConverterBE.bytesToInt32(b3, b2, b1, b0)
  override def bytesToUInt32(b0: Byte, b1: Byte, b2: Byte, b3: Byte) = ByteConverterBE.bytesToUInt32(b3, b2, b1, b0)
  override def bytesToInt64(b0: Byte, b1 : Byte, b2 : Byte, b3 : Byte, b4 : Byte, b5 : Byte, b6 : Byte, b7 : Byte)
    = ByteConverterBE.bytesToInt64(b7, b6, b5, b4, b3, b2, b1, b0)
  override def bytesToUInt64(b0: Byte, b1 : Byte, b2 : Byte, b3 : Byte, b4 : Byte, b5 : Byte, b6 : Byte, b7 : Byte)
  = ByteConverterBE.bytesToUInt64(b7, b6, b5, b4, b3, b2, b1, b0)
  override def bytesToUInt64Shifted(b0: Byte, b1 : Byte, b2 : Byte, b3 : Byte, b4 : Byte, b5 : Byte, b6 : Byte, b7 : Byte)
  = ByteConverterBE.bytesToUInt64Shifted(b7, b6, b5, b4, b3, b2, b1, b0)

  override def int16ToBytes(value: Short): Array[Byte]  = ByteConverterBE.int16ToBytes(value).reverse
  override def uInt16ToBytes(value: Char): Array[Byte]   = ByteConverterBE.uInt16ToBytes(value).reverse
  override def int32ToBytes(value: Int): Array[Byte]    = ByteConverterBE.int32ToBytes(value).reverse
  override def uInt32ToBytes(value: Long): Array[Byte]  = ByteConverterBE.uInt32ToBytes(value).reverse
  override def int64ToBytes(value: Long): Array[Byte]   = ByteConverterBE.int64ToBytes(value).reverse
  override def uInt64ToBytes(value: Long): Array[Byte]  = ByteConverterBE.uInt64ToBytes(value).reverse
  override def uInt64ShiftedToBytes(value: Long): Array[Byte] = ByteConverterBE.uInt64ShiftedToBytes(value).reverse

}
