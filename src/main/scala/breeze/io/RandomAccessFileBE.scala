package breeze.io

import java.io.{File, IOException}


/** See [[breeze.io.RandomAccessFile]], reads big endian.
  */
class RandomAccessFileBE(file: File, arg0: String = "r") extends breeze.io.RandomAccessFile(file, arg0) {

  def this(filename: String, arg0: String) = this(new File(filename), arg0)

  /** byte converter to encode (switched for RandomAccessFileLE)
    */
  override val converter: ByteConverter = breeze.io.ByteConverterBE


  ///// Int16 (Short) /////
  @throws(classOf[IOException])
  override def readInt16(): Short = rafObj.readShort()


  ///// UInt16 (Char) /////
  @throws(classOf[IOException])
  def readUInt16(): Char = rafObj.readChar()//readUnsignedShort()


  ///// Int32 (Int) /////

  @throws(classOf[IOException])
  def readInt32() = rafObj.readInt()


  ///// UInt32 (Long) /////


  ///// Int64 (Long) /////

  @throws(classOf[IOException])
  def readInt64() = rafObj.readLong()


  ///// UInt64 (Long) /////



  ///// UInt64Shifted (Long) /////



  ///// Floating Point /////

  override def readDouble() = rafObj.readDouble()
  override def readFloat() = rafObj.readFloat()

  @throws(classOf[IOException])
  def writeDouble(v: Double) = rafObj.writeDouble(v)
  @throws(classOf[IOException])
  def writeFloat(v: Float) = rafObj.writeFloat(v)


}


/** See [[breeze.io.ByteConverter]], reads big endian.
  */
object ByteConverterBE extends ByteConverter {

  ///// bytesToXXX /////
  def bytesToInt16(b0: Byte, b1: Byte): Short = {
    (b0 << 8 | b1 & 0xFF).toShort
  }

  def bytesToUInt16(b0: Byte, b1: Byte): Char = {
    ((b0.toInt & 0xFF) << 8 | (b1.toInt & 0xFF)).toChar
  }

  def bytesToInt32(b0: Byte, b1: Byte, b2: Byte, b3: Byte): Int = {
    b0.toInt << 24 | (b1 & 0xFF) << 16 | (b2 & 0xFF) << 8 | (b3 & 0xFF)
  }

  def bytesToUInt32(b0: Byte, b1: Byte, b2: Byte, b3: Byte): Long = {
    (b0.toLong & 0xFFL) << 24 | (b1.toLong & 0xFFL) << 16 | (b2.toLong & 0xFFL) << 8 | (b3.toLong & 0xFFL)
  }

  def bytesToUInt64(b0: Byte, b1: Byte, b2: Byte, b3: Byte, b4: Byte, b5: Byte, b6: Byte, b7: Byte): Long = {
    if ((b0/*.toInt*/ & 0x80) != 0x00) {
      throw new IOException("UInt64 too big to read given limitations of Long format.")
    } else {
      (b0.toLong & 0xFFL) << 56 | (b1.toLong & 0xFFL) << 48 | (b2.toLong & 0xFFL) << 40 | (b3.toLong & 0xFFL) << 32 |
        (b4.toLong & 0xFFL) << 24 | (b5.toLong & 0xFFL) << 16 | (b6.toLong & 0xFFL) << 8 | (b7.toLong & 0xFFL)
    }
  }

  def bytesToInt64(b0: Byte, b1: Byte, b2: Byte, b3: Byte, b4: Byte, b5: Byte, b6: Byte, b7: Byte): Long = {
    b0.toLong << 56 | (b1.toLong & 0xFFL) << 48 | (b2.toLong & 0xFFL) << 40 | (b3.toLong & 0xFFL) << 32 |
      (b4.toLong & 0xFFL) << 24 | (b5.toLong & 0xFFL) << 16 | (b6.toLong & 0xFFL) << 8 | (b7.toLong & 0xFFL)
  }

  def bytesToUInt64Shifted(b0: Byte, b1: Byte, b2: Byte, b3: Byte, b4: Byte, b5: Byte, b6: Byte, b7: Byte): Long = {
    (b0 ^ 0x80).toLong << 56 | (b1.toLong & 0xFFL) << 48 | (b2.toLong & 0xFFL) << 40 | (b3.toLong & 0xFFL) << 32 |
      (b4.toLong & 0xFFL) << 24 | (b5.toLong & 0xFFL) << 16 | (b6.toLong & 0xFFL) << 8 | (b7.toLong & 0xFFL)
  }
  ///// XXXToByte /////
  def int16ToBytes(value: Short): Array[Byte] = {
    val tempret = new Array[Byte](2)
    tempret(0) = (value >> 8).toByte
    tempret(1) = (value & 0xFF).toByte
    tempret
  }

  def uInt16ToBytes(value: Char): Array[Byte] = {
    require(value <= 65535 && value >= 0, "Value " + value + " is out of range of 2-byte unsigned array.")

    val tempret = new Array[Byte](2)
    tempret(0) = ((value >> 8) & 0xFF).toByte
    tempret(1) =  (value       & 0xFF).toByte
    tempret
  }

  def int32ToBytes(value: Int): Array[Byte] = {
    val tempret = new Array[Byte](4)
    tempret(0) =  (value >> 24).toByte
    tempret(1) = ((value >> 16) & 0xFF).toByte
    tempret(2) = ((value >> 8)  & 0xFF).toByte
    tempret(3) =  (value        & 0xFF).toByte
    tempret
  }

  def uInt32ToBytes(value: Long): Array[Byte] = {
    require(value <= 4294967295L && value >= 0L, "Value " + value + " is out of range of 4-byte unsigned array.")

    val tempret = new Array[Byte](4)
    tempret(0) = ((value >> 24) & 0xFF).toByte
    tempret(1) = ((value >> 16) & 0xFF).toByte
    tempret(2) = ((value >> 8)  & 0xFF).toByte
    tempret(3) =  (value        & 0xFF).toByte
    tempret
  }

  def int64ToBytes(value: Long): Array[Byte] = {
    val tempret = new Array[Byte](8)
    tempret(0) =  (value >> 56).toByte
    tempret(1) = ((value >> 48) & 0xFF).toByte
    tempret(2) = ((value >> 40) & 0xFF).toByte
    tempret(3) = ((value >> 32) & 0xFF).toByte
    tempret(4) = ((value >> 24) & 0xFF).toByte
    tempret(5) = ((value >> 16) & 0xFF).toByte
    tempret(6) = ((value >> 8)  & 0xFF).toByte
    tempret(7) =  (value        & 0xFF).toByte
    tempret
  }

  def uInt64ToBytes(value: Long): Array[Byte] = {
    require(value >= 0, "Value " + value + " is out of range of 4-byte unsigned array.")

    val tempret = new Array[Byte](8)
    tempret(0) = ((value >> 56) & 0xFF).toByte
    tempret(1) = ((value >> 48) & 0xFF).toByte
    tempret(2) = ((value >> 40) & 0xFF).toByte
    tempret(3) = ((value >> 32) & 0xFF).toByte
    tempret(4) = ((value >> 24) & 0xFF).toByte
    tempret(5) = ((value >> 16) & 0xFF).toByte
    tempret(6) = ((value >> 8)  & 0xFF).toByte
    tempret(7) =  (value        & 0xFF).toByte
    tempret
  }

  def uInt64ShiftedToBytes(value: Long): Array[Byte] = {

    val tempret = new Array[Byte](8)
    tempret(0) = (((value >> 56) & 0xFF) ^ 0x80).toByte
    tempret(1) = ((value >> 48) & 0xFF).toByte
    tempret(2) = ((value >> 40) & 0xFF).toByte
    tempret(3) = ((value >> 32) & 0xFF).toByte
    tempret(4) = ((value >> 24) & 0xFF).toByte
    tempret(5) = ((value >> 16) & 0xFF).toByte
    tempret(6) = ((value >> 8)  & 0xFF).toByte
    tempret(7) =  (value        & 0xFF).toByte
    tempret
  }

}