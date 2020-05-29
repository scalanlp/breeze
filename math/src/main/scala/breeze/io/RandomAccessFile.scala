package breeze.io

import java.io.{File, DataInput, DataOutput, Closeable, IOException}

import spire.math.ULong

/** Wrapper for [[java.io.RandomAccessFile]].
 *
 * The main differences to [[java.io.RandomAccessFile]] are
 * (1) naming (e.g. default naming is readInt64 instead of readLong)
 * (2) the readXXX(n: Int) functions, which will try to read n samples from the file. (Try to use these functions instead
 * of reading multiple times in a loop with readXXX(), as each individual read is costly
 * in terms of performance.
 *  *
 * Each function throws a [[java.io.IOException]] (including subclass [[java.io.EOFException]]). These can be caught in Scala
 * if necessary, to detect ends of files when reading, for example. Catching is obligatory in Java.
 *
 * <table border="2">
 * <tr><th>Type</th>                    <th>Java Type  </th>    <th>Scala Type  </th>   <th>Value Range  </th> </tr>
 * <tr><td>Int8: Signed 8-bit integer  </td>    <td>byte</td>         <td>Byte</td>         <td>[-128, 127]</td>        </tr>
 * <tr><td>UInt8: Unsigned 8-bit integer  </td>  <td>(int)</td>        <td>(Int)</td>        <td>[0, 255]</td> </tr>
 * <tr><td>Int16: Signed 16-bit integer  </td>   <td>short</td>        <td>Short</td>        <td>[-32768, 32767]</td>        </tr>
 * <tr><td>UInt16: Unsigned 16-bit integer  </td> <td>char</td>         <td>Char</td>        <td>[0, 65535]</td> </tr>
 * <tr><td>Int32: Signed 32-bit integer  </td>   <td>int</td>        <td>Int</td>        <td>[-2147483648, 2147483647]</td>        </tr>
 * <tr><td>UInt32: Unsigned 32-bit integer  </td> <td>(long)</td>         <td>(Long)</td>        <td>[0, 4294967295]</td> </tr>
 * <tr><td>Int64: Signed 64-bit integer  </td>   <td>long</td>        <td>Long</td>        <td>[-9223372036854775808, 9223372036854775807]</td>        </tr>
 * <tr><td>UInt64: Unsigned 64-bit integer*  </td> <td>(spire.math.Ulong)</td>         <td>(spire.math.ULong)</td>        <td>[0, <b>18446744073709551615</b>]</td> </tr>
 * <tr><td>UInt64Shifted: Unsigned 64-bit integer, shifted to signed range*  </td>   <td>(long)*</td>        <td>(Long)*</td>        <td>[0, 18446744073709551615*]</td>        </tr>
 * </table>
 *
 * *note: given that the JVM/Scala does not have a UInt64 type, nor a Int128 to promote to, UInt64s are dealt with in two
 * ways... (1) as a [[spire.math.ULong]] (which represents UInt64 wrapped as a regular Long where the negative
 * values represent their unsigned two's complement equivalent:
 * <table border="2">
 * <tr><th>Unsigned ULong value</th> <th>Internal Long (signed) wrapped by ULong</th></tr>
 * <tr><th>0</th> <th>0</th></tr>
 * <tr><th>2&#94;63-1</th> <th>2&#94;63-1</th></tr>
 * <tr><th>2&#94;63</th> <th>-2&#94;63</th></tr>
 * <tr><th>2&#94;64-1</th> <th>-1</th></tr>
 * </table>
 *
 * or (2) as a shifted Int64, where UInt64 is shifted down by 2&#94;63 in its range to cover
 * both positive and negative values of Int64 (this is compatible with + and -, for use as timestamps, for example,
 * but is of course not compatible with * and / operations)
 *
 * *implementation note: this class was not overriden from java.io.RandomAccessFile or implicitly "pimped," but instead
 * passes through to java.io.RandomAccessFile. This is mainly because the java.io.RandomAccessFile.readXXX functions are
 * declared final, and cannot be overridden.
 */
class RandomAccessFile(file: File, arg0: String = "r")(implicit converter: ByteConverter = ByteConverterBigEndian)
    extends DataInput
    with DataOutput
    with Closeable /*extends java.io.RandomAccessFile(file, arg0)*/ {

  def this(filename: String, arg0: String)(implicit converter: ByteConverter) =
    this(new File(filename), arg0)(converter)

  val rafObj = new java.io.RandomAccessFile(file, arg0)
  //protected var fileEnded = false

  ///// Int8 (Byte) /////

  //<editor-fold desc="Reading">

  /** Tries to read an Int8 (Byte) at the current getFilePointer().
   * Will throw an exception if it encounters an end of file.
   */
  @throws(classOf[IOException])
  final def readInt8(): Byte = rafObj.readByte()

  /** Tries to read n Int8s (Bytes) from the current getFilePointer().
   * Will throw an exception if it encounters an end of file.
   */
  @throws(classOf[IOException])
  final def readInt8(n: Int): Array[Byte] = {
    val tempret = new Array[Byte](n)
    rafObj.readFully(tempret)
    tempret
  }
  //(for(i <- 0 until calculateN(n) ) yield readByte() ).toArray
  //</editor-fold>

  //<editor-fold desc="Writing">

  /** Tries to write an Int8 (Byte) to the current getFilePointer().
   */
  @throws(classOf[IOException])
  final def writeInt8(v: Byte): Unit = {
    rafObj.write(v)
  }

  /** Tries to write n Int8s (Bytes) to the current getFilePointer().
   * Will throw an exception if it encounters an end of file.
   */
  @throws(classOf[IOException])
  final def writeInt8(v: Array[Byte]): Unit = {
    rafObj.write(v)
  }
  //</editor-fold>

  //<editor-fold desc="Aliases">

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.readInt8]]
   */
  @throws(classOf[IOException])
  final override def readByte(): Byte = readInt8()

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.readInt8]]
   */
  @throws(classOf[IOException])
  final def readByte(n: Int): Array[Byte] = readInt8(n)

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.writeInt8]]
   */
  @throws(classOf[IOException])
  final def write(v: Byte) = writeInt8(v)

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.writeInt8]]
   */
  @throws(classOf[IOException])
  final def write(v: Array[Byte]) = writeInt8(v)
  //</editor-fold>

  ///// UInt8 /////

  //<editor-fold desc="Reading">
  /** Tries to read a UInt8 as an Int16 at the current getFilePointer().
   * Will throw an exception if it encounters an end of file.
   */
  @throws(classOf[IOException])
  final def readUInt8() = rafObj.readUnsignedByte()

  /** Tries to read n UInt8s as Int from the current getFilePointer().
   * Will throw an exception if it encounters an end of file.
   */
  @throws(classOf[IOException])
  final def readUInt8(n: Int): Array[Short] = {
    val tr = new Array[Short](n)
    //the following is a hack to avoid the heavier Scala for loop
    var c = 0
    while (c < n) {
      tr(c) = readUnsignedByte().toShort
      c += 1
    }
    //for(c <- 0 until n) tr(c) = bytesToInt16(ba(c), ba(c + 1))
    tr
    //    (for(i <- 0 until n ) yield readUnsignedByte() ).toArray
  }
  //</editor-fold>

  //<editor-fold desc="Writing">

  /** Tries to write a UInt8 to the current getFilePointer().
   */
  @throws(classOf[IOException])
  final def writeUInt8(value: Short) = rafObj.write(Array[Byte](converter.uInt8ToByte(value)))

  /** Tries to write n UInt8s (Bytes) at the current getFilePointer().
   * Will throw an exception if it encounters an end of file.
   */
  @throws(classOf[IOException])
  final def writeUInt8(values: Array[Short]): Unit = {
    rafObj.write(values.map(converter.uInt8ToByte(_)))
  }
  //</editor-fold>

  //<editor-fold desc="Aliases">

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.readUInt8]]
   */
  @throws(classOf[IOException])
  final override def readUnsignedByte() = readUInt8()

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.readUInt8]]
   */
  @throws(classOf[IOException])
  final def readUnsignedByte(n: Int): Array[Short] = readUInt8(n)

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.writeUInt8]]
   */
  @throws(classOf[IOException])
  final def writeUnsignedByte(value: Short) = writeUInt8(value)

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.writeUInt8]]
   */
  @throws(classOf[IOException])
  final def writeUnsignedByte(values: Array[Short]) = writeUInt8(values)
  //</editor-fold>

  ///// Int16 (Short) /////

  //<editor-fold desc="Reading">

  /** Tries to read an Int16 at the current getFilePointer().
   * Will throw an exception if it encounters an end of file.
   */
  @throws(classOf[IOException])
  def readInt16() = {
    val ba = readByte(2)
    converter.bytesToInt16(ba(0), ba(1))
  }

  /** Tries to read n Int16s from the current getFilePointer().
   * Will throw an exception if it encounters an end of file.
   */
  @throws(classOf[IOException])
  final def readInt16(n: Int): Array[Short] = {
    val ba = new Array[Byte](n * 2)
    rafObj.readFully(ba) //reading is much faster if many bytes are read simultaneously
    val tr = new Array[Short](n)
    //the following is a hack to avoid the heavier Scala for loop
    var c = 0
    while (c < n) {
      tr(c) = converter.bytesToInt16(ba(c * 2), ba(c * 2 + 1))
      c += 1
    }
    //for(c <- 0 until n) tr(c) = bytesToInt16(ba(c), ba(c + 1))
    tr
  }
  //</editor-fold>

  //<editor-fold desc="Writing">

  /** Tries to write an Int16 (Short) to the current getFilePointer().
   */
  @throws(classOf[IOException])
  final def writeInt16(v: Short): Unit = {
    rafObj.write(converter.int16ToBytes(v))
  }

  /** Tries to write an array of Int16s (Shorts) to the current getFilePointer().
   */
  @throws(classOf[IOException])
  final def writeInt16(v: Array[Short]): Unit = {
    val writeArr = new Array[Byte](v.length * 2)
    var currIndex = 0
    for (cnt <- 0 until v.length) {
      val x = converter.int16ToBytes(v(cnt))
      writeArr(currIndex) = x(0); currIndex += 1
      writeArr(currIndex) = x(1); currIndex += 1
    }
    rafObj.write(writeArr)
//    rafObj.write( v.flatMap(converter.int16ToBytes(_)) )
  }
  //</editor-fold>

  //<editor-fold desc="Aliases">

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.readInt16]]
   */
  @throws(classOf[IOException])
  final override def readShort() = readInt16()

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.readInt16]]
   */
  @throws(classOf[IOException])
  final def readShort(n: Int): Array[Short] = readInt16(n)

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.readInt16]]
   */
  @throws(classOf[IOException])
  final def writeShort(v: Short) = writeInt16(v)

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.readInt16]]
   */
  @throws(classOf[IOException])
  override final def writeShort(v: Int) = writeInt16(v.toShort)

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.readInt16]]
   */
  @throws(classOf[IOException])
  final def writeShort(v: Array[Short]) = writeInt16(v)
  //</editor-fold>

  ///// UInt16 (Char) /////

  //<editor-fold desc="Reading">

  /** Tries to read a UInt16 (Char) at the current getFilePointer().
   * Will throw an exception if it encounters an end of file.
   */
  @throws(classOf[IOException])
  def readUInt16(): Char = {
    val ba = readByte(2)
    converter.bytesToUInt16(ba(0), ba(1))
  }

  /** Tries to read n UInt16s (Chars) from the current getFilePointer() as Int32.
   * Will throw an exception if it encounters an end of file.
   */
  @throws(classOf[IOException])
  final def readUInt16(n: Int): Array[Char] = {
    val ba = new Array[Byte](n * 2)
    rafObj.readFully(ba) //reading is much faster if many bytes are read simultaneously
    val tr = new Array[Char](n)
    //the following is a hack to avoid the heavier Scala for loop
    var c = 0
    while (c < n) {
      tr(c) = converter.bytesToUInt16(ba(c * 2), ba(c * 2 + 1))
      c += 1
    }
    //for(c <- 0 until n) tr(c) = bytesToInt16(ba(c), ba(c + 1))
    tr
  }
  //</editor-fold>

  //<editor-fold desc="Writing">

  /** Tries to write a UInt16 (Char) to the current getFilePointer().
   */
  @throws(classOf[IOException])
  final def writeUInt16(v: Char): Unit = {
    rafObj.write(converter.uInt16ToBytes(v))
  }

  /** Tries to write an array of UInt16s (Chars) to the current getFilePointer().
   */
  @throws(classOf[IOException])
  final def writeUInt16(v: Array[Char]): Unit = {
    rafObj.write(v.flatMap(converter.uInt16ToBytes(_)))
  }
  //</editor-fold>

  //<editor-fold desc="Aliases">

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.readUInt16]], but reads as Int.
   */
  @throws(classOf[IOException])
  final override def readUnsignedShort(): Int = readUInt16().toInt

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.readUInt16]], but reads as Int.
   */
  @throws(classOf[IOException])
  final def readUnsignedShort(n: Int): Array[Int] = readUInt16(n).map(_.toInt)

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.readUInt16]]
   */
  @throws(classOf[IOException])
  override final def readChar(): Char = readUInt16()

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.readUInt16]]
   */
  @throws(classOf[IOException])
  final def readChar(n: Int): Array[Char] = readUInt16(n)
//  {
//    val ba = new Array[Byte](n * 2)
//    rafObj.readFully(ba) //reading is much faster if many bytes are read simultaneously
//    val tr = new Array[Char](n)
//    //the following is a hack to avoid the heavier Scala for loop
//    var c = 0
//    while (c < n) {
//      tr(c) = converter.bytesToUInt16(ba(c * 2), ba(c * 2 + 1)).toChar
//      c += 1
//    }
//    //for(c <- 0 until n) tr(c) = bytesToInt16(ba(c), ba(c + 1))
//    tr
//  }

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.writeUInt16]], but reads as Int.
   */
  @throws(classOf[IOException])
  final def writeUnsignedShort(value: Int): Unit = writeUInt16(value.toChar)

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.writeUInt16]], but reads as Int.
   */
  @throws(classOf[IOException])
  final def writeUnsignedShort(value: Array[Int]): Unit = writeUInt16(value.map(_.toChar))

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.writeUInt16]]
   */
  @throws(classOf[IOException])
  final def writeChar(value: Char): Unit = writeUInt16(value)

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.writeUInt16]]
   */
  @throws(classOf[IOException])
  final override def writeChar(v: Int): Unit = writeUInt16(v.toChar)

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.writeUInt16]]
   */
  @throws(classOf[IOException])
  final def WriteChar(value: Array[Char]): Unit = writeUInt16(value)

  //
  //</editor-fold>

  ///// Int32 (Int) /////

  //<editor-fold desc="Reading">

  /** Tries to read an Int32 at the current getFilePointer().
   * Will throw an exception if it encounters an end of file.
   */
  @throws(classOf[IOException])
  final def readInt32(): Int = {
    val ba = readByte(4)
    converter.bytesToInt32(ba(0), ba(1), ba(2), ba(3))
  }

  /** Tries to read n Int32s from the current getFilePointer().
   * Will throw an exception if it encounters an end of file.
   */
  @throws(classOf[IOException])
  final def readInt32(n: Int): Array[Int] = {
    val ba = new Array[Byte](n * 4)
    rafObj.readFully(ba) //reading is much faster if many bytes are read simultaneously
    val tr = new Array[Int](n)
    //the following is a hack to avoid the heavier Scala for loop
    var c = 0
    while (c < n) {
      tr(c) = converter.bytesToInt32(ba(c * 4), ba(c * 4 + 1), ba(c * 4 + 2), ba(c * 4 + 3))
      c += 1
    }
    //for(c <- 0 until n) tr(c) = bytesToInt16(ba(c), ba(c + 1))
    tr
  }
  //</editor-fold>

  //<editor-fold desc="Writing">

  /** Tries to write an Int32 (Int) to the current getFilePointer().
   */
  @throws(classOf[IOException])
  final def writeInt32(v: Int): Unit = {
    rafObj.write(converter.int32ToBytes(v))
  }

  /** Tries to write an array of Int32s (Ints) to the current getFilePointer().
   */
  @throws(classOf[IOException])
  final def writeInt32(v: Array[Int]): Unit = {
    rafObj.write(v.flatMap(converter.int32ToBytes(_)))
  }

  //</editor-fold>

  //<editor-fold desc="Aliases">

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.readUInt32]]
   */
  @throws(classOf[IOException])
  final override def readInt() = readInt32()

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.readUInt32]]
   */
  @throws(classOf[IOException])
  final def readInt(n: Int): Array[Int] = readInt32(n)

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.writeInt32]]
   */
  @throws(classOf[IOException])
  final def writeInt(value: Int): Unit = writeInt32(value)

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.writeInt32]]
   */
  @throws(classOf[IOException])
  final def writeInt(value: Array[Int]): Unit = writeInt32(value)

  //</editor-fold>

  ///// UInt32 (Long) /////

  //<editor-fold desc="Reading">

  /** Tries to read a UInt32 as Long at the current getFilePointer().
   * Will throw an exception if it encounters an end of file.
   */
  @throws(classOf[IOException])
  final def readUInt32(): Long = {
    val ba = readByte(4)
    converter.bytesToUInt32(ba(0), ba(1), ba(2), ba(3))
  }

  /** Tries to read n UInt32s as Longs from the current getFilePointer().
   * Will throw an exception if it encounters an end of file.
   */
  @throws(classOf[IOException])
  final def readUInt32(n: Int): Array[Long] = {
    val ba = new Array[Byte](n * 4)
    rafObj.readFully(ba) //reading is much faster if many bytes are read simultaneously
    val tr = new Array[Long](n)
    //the following is a hack to avoid the heavier Scala for loop
    var c = 0
    while (c < n) {
      tr(c) = converter.bytesToUInt32(ba(c * 4), ba(c * 4 + 1), ba(c * 4 + 2), ba(c * 4 + 3))
      c += 1
    }
    //for(c <- 0 until n) tr(c) = bytesToInt16(ba(c), ba(c + 1))
    tr
  }
  //</editor-fold>

  //<editor-fold desc="Writing">

  /** Tries to write a UInt32 (represented by Int) to the current getFilePointer().
   */
  @throws(classOf[IOException])
  final def writeUInt32(v: Long): Unit = {
    rafObj.write(converter.uInt32ToBytes(v))
  }

  /** Tries to write an array of Int32s (Ints) to the current getFilePointer().
   */
  @throws(classOf[IOException])
  final def writeUInt32(v: Array[Long]): Unit = {
    rafObj.write(v.flatMap(converter.uInt32ToBytes(_)))
  }
  //</editor-fold>

  ///// Int64 (Long) /////

  //<editor-fold desc="Reading">

  /** Tries to read an Int64 at the current getFilePointer().
   * Will throw an exception if it encounters an end of file.
   */
  @throws(classOf[IOException])
  def readInt64(): Long = {
    val ba = readByte(8)
    converter.bytesToInt64(ba(0), ba(1), ba(2), ba(3), ba(4), ba(5), ba(6), ba(7))
  }

  /** Tries to read n Int64s from the current getFilePointer().
   * Will throw an exception if it encounters an end of file.
   */
  @throws(classOf[IOException])
  final def readInt64(n: Int): Array[Long] = {
    val ba = new Array[Byte](n * 8)
    rafObj.readFully(ba) //reading is much faster if many bytes are read simultaneously
    val tr = new Array[Long](n)
    //the following is a hack to avoid the heavier Scala for loop
    var c = 0
    while (c < n) {
      val c8 = c * 8
      tr(c) = converter.bytesToInt64(
        ba(c8),
        ba(c8 + 1),
        ba(c8 + 2),
        ba(c8 + 3),
        ba(c8 + 4),
        ba(c8 + 5),
        ba(c8 + 6),
        ba(c8 + 7)
      )
      c += 1
    }
    //for(c <- 0 until n) tr(c) = bytesToInt16(ba(c), ba(c + 1))
    tr
  }
  //    (for(i <- 0 until n ) yield readInt64).toArray
  //</editor-fold>

  //<editor-fold desc="Writing">

  /** Tries to write an Int64 (Long) to the current getFilePointer().
   */
  @throws(classOf[IOException])
  final def writeInt64(v: Long): Unit = {
    rafObj.write(converter.int64ToBytes(v))
  }

  /** Tries to write an array of Int64s (Longs) to the current getFilePointer().
   */
  @throws(classOf[IOException])
  final def writeInt64(v: Array[Long]): Unit = {
    rafObj.write(v.flatMap(converter.int64ToBytes(_)))
  }
  //</editor-fold>

  //<editor-fold desc="Aliases">

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.readInt64]]
   */
  @throws(classOf[IOException])
  final override def readLong() = readInt64()

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.readInt64]]
   */
  @throws(classOf[IOException])
  final def readLong(n: Int): Array[Long] = readInt64(n)

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.writeInt64]]
   */
  @throws(classOf[IOException])
  final def writeLong(value: Long): Unit = writeInt64(value)

  /** Alias, in java style, for [[breeze.io.RandomAccessFile.writeInt64]]
   */
  @throws(classOf[IOException])
  final def writeLong(value: Array[Long]): Unit = writeInt64(value)

  //</editor-fold>

  ///// UInt64 (ULong) /////

  //<editor-fold desc="Reading">

  /** Tries to read a UInt64 as [[spire.math.ULong]] at the current getFilePointer().
   * Will throw an exception for UInt64 values which are larger than the maximum Long.
   * Will throw an exception if it encounters an end of file.
   */
  @throws(classOf[IOException])
  final def readUInt64(): ULong = {
    val ba = readByte(8)
    converter.bytesToUInt64(ba(0), ba(1), ba(2), ba(3), ba(4), ba(5), ba(6), ba(7))
  }

  /** Tries to read n UInt64s from the current getFilePointer().
   * Will throw an exception for UInt64 values which are larger than the maximum Long.
   * Will throw an exception if it encounters an end of file.
   */
  @throws(classOf[IOException])
  final def readUInt64(n: Int): Array[ULong] = {
    val ba = new Array[Byte](n * 8)
    rafObj.readFully(ba) //reading is much faster if many bytes are read simultaneously
    val tr = new Array[ULong](n)
    //the following is a hack to avoid the heavier Scala for loop
    var c = 0
    while (c < n) {
      val c8 = c * 8
      tr(c) = converter.bytesToUInt64(
        ba(c8),
        ba(c8 + 1),
        ba(c8 + 2),
        ba(c8 + 3),
        ba(c8 + 4),
        ba(c8 + 5),
        ba(c8 + 6),
        ba(c8 + 7)
      )
      c += 1
    }
    //for(c <- 0 until n) tr(c) = bytesToInt16(ba(c), ba(c + 1))
    tr
  }

  //</editor-fold>

  //<editor-fold desc="Writing">

  /** Tries to write an UInt64 (described as Long) to the current getFilePointer().
   * Will throw error if value < 0.
   */
  @throws(classOf[IOException])
  final def writeUInt64(v: ULong): Unit = {
    rafObj.write(converter.uInt64ToBytes(v))
  }

  /** Tries to write an array of UInt64s (input as [[spire.math.ULong]]) to the current getFilePointer().
   * Will throw error if value < 0.
   */
  @throws(classOf[IOException])
  final def writeUInt64(v: Array[ULong]): Unit = {
    rafObj.write(v.flatMap(converter.uInt64ToBytes(_)))
  }

  //</editor-fold>

  ///// UInt64Shifted (Long) /////

  //<editor-fold desc="Reading">

  /** Tries to read a UInt64, shifted down in value to fit into Int64/Long at the current getFilePointer().
   * Will throw an exception if it encounters an end of file.
   */
  @throws(classOf[IOException])
  final def readUInt64Shifted(): Long = {
    val ba = readByte(8)
    converter.bytesToUInt64Shifted(ba(0), ba(1), ba(2), ba(3), ba(4), ba(5), ba(6), ba(7))
  }

  /** Tries to read n UInt64s, shifted down in value to fit into Int64/Longs from the current getFilePointer().
   * Will throw an exception if it encounters an end of file.
   */
  @throws(classOf[IOException])
  final def readUInt64Shifted(n: Int): Array[Long] = {
    val ba = new Array[Byte](n * 8)
    rafObj.readFully(ba) //reading is much faster if many bytes are read simultaneously
    val tr = new Array[Long](n)
    //the following is a hack to avoid the heavier Scala for loop
    var c = 0
    while (c < n) {
      val c8 = c * 8
      tr(c) = converter.bytesToUInt64Shifted(
        ba(c8),
        ba(c8 + 1),
        ba(c8 + 2),
        ba(c8 + 3),
        ba(c8 + 4),
        ba(c8 + 5),
        ba(c8 + 6),
        ba(c8 + 7)
      )
      c += 1
    }
    //for(c <- 0 until n) tr(c) = bytesToInt16(ba(c), ba(c + 1))
    tr
  }
  //</editor-fold>

  //<editor-fold desc="Writing">

  /** Tries to write an UInt64Shifted (shifted down to Long range) to the current getFilePointer().
   */
  @throws(classOf[IOException])
  final def writeUInt64Shifted(v: Long): Unit = {
    rafObj.write(converter.uInt64ShiftedToBytes(v))
  }

  /** Tries to write an array of UInt64Shifted (shifted down to Long range) to the current getFilePointer().
   */
  @throws(classOf[IOException])
  final def writeUInt64Shifted(v: Array[Long]): Unit = {
    rafObj.write(v.flatMap(converter.uInt64ShiftedToBytes(_)))
  }
  //</editor-fold>

  ///// Floating Point /////

  //<editor-fold desc="Reading">

  /** Tries to read a Double at the current getFilePointer().
   * Will throw an exception if it encounters an end of file.
   */
  @throws(classOf[IOException])
  override def readDouble(): Double = {
    java.lang.Double.longBitsToDouble(readLong())
  }

  /** Tries to read a Float at the current getFilePointer().
   * Will throw an exception if it encounters an end of file.
   */
  @throws(classOf[IOException])
  override def readFloat(): Float = {
    java.lang.Float.intBitsToFloat(readInt())
  }

  /** Tries to read n Doubles from the current getFilePointer().
   * Will throw an exception if it encounters an end of file.
   */
  @throws(classOf[IOException])
  final def readDouble(n: Int): Array[Double] = {
    val ba = new Array[Byte](n * 8)
    rafObj.readFully(ba) //reading is much faster if many bytes are read simultaneously
    val tr = new Array[Double](n)
    //the following is a hack to avoid the heavier Scala for loop
    var c = 0
    while (c < n) {
      val c8 = c * 8
      tr(c) = java.lang.Double.longBitsToDouble(
        converter
          .bytesToInt64(ba(c8), ba(c8 + 1), ba(c8 + 2), ba(c8 + 3), ba(c8 + 4), ba(c8 + 5), ba(c8 + 6), ba(c8 + 7))
      )
      c += 1
    }
    //for(c <- 0 until n) tr(c) = bytesToInt16(ba(c), ba(c + 1))
    tr
  }

  /** Tries to read n Floats from the current getFilePointer().
   * Will throw an exception if it encounters an end of file.
   */
  @throws(classOf[IOException])
  final def readFloat(n: Int): Array[Float] = {
    val ba = new Array[Byte](n * 4)
    rafObj.readFully(ba) //reading is much faster if many bytes are read simultaneously
    val tr = new Array[Float](n)
    //the following is a hack to avoid the heavier Scala for loop
    var c = 0
    while (c < n) {
      val c4 = c * 4
      tr(c) = java.lang.Float.intBitsToFloat(
        converter.bytesToInt32(ba(c4), ba(c4 + 1), ba(c4 + 2), ba(c4 + 3))
      )
      c += 1
    }
    //for(c <- 0 until n) tr(c) = bytesToInt16(ba(c), ba(c + 1))
    tr
  }
  //</editor-fold>

  //<editor-fold desc="Writing">

  @throws(classOf[IOException])
  def writeFloat(v: Float) = {
    writeInt32(java.lang.Float.floatToRawIntBits(v))
  }

  @throws(classOf[IOException])
  def writeDouble(v: Double) = {
    writeInt64(java.lang.Double.doubleToRawLongBits(v))
  }

  @throws(classOf[IOException])
  def writeDouble(v: Array[Double]): Unit = {
    val la = new Array[Long](v.length)
    //the following is a hack to avoid the heavier Scala for loop
    var c = 0
    while (c < v.length) {
      la(c) = java.lang.Double.doubleToRawLongBits(v(c))
      c += 1
    }
    writeLong(la)
  }

  @throws(classOf[IOException])
  def writeFloat(v: Array[Float]): Unit = {
    val ia = new Array[Int](v.length)
    //the following is a hack to avoid the heavier Scala for loop
    var c = 0
    while (c < v.length) {
      ia(c) = java.lang.Float.floatToRawIntBits(v(c))
      c += 1
    }
    writeInt(ia)
  }

  //</editor-fold>

  /////other RandomAccessFile overrides
  /** Pass on to [[java.io.RandomAccessFile]]
   */
  final override def readBoolean() = rafObj.readBoolean()

  /** Pass on to [[java.io.RandomAccessFile]]
   */
  override def readFully(b: Array[Byte]) = rafObj.readFully(b)

  /** Pass on to [[java.io.RandomAccessFile]]
   */
  override def readFully(b: Array[Byte], off: Int, len: Int) = rafObj.readFully(b, off, len)

  /** Pass on to [[java.io.RandomAccessFile]]
   */
  override def readLine() = rafObj.readLine()

  /** Pass on to [[java.io.RandomAccessFile]]
   */
  override def readUTF() = rafObj.readUTF()

  /** Pass on to [[java.io.RandomAccessFile]]
   */
  override final def writeChars(value: String): Unit = rafObj.writeChars(value)

  /** Pass on to [[java.io.RandomAccessFile]]
   */
  override def writeUTF(value: String) = rafObj.writeUTF(value)

  /** Pass on to [[java.io.RandomAccessFile]]
   */
  override def skipBytes(n: Int): Int = rafObj.skipBytes(n)

  /** like [[skipBytes]] but just jumps, does not return. For speed
   */
  def jumpBytes(n: Int): Unit = {
    //Speed optimization
    //rafObj.skipBytes(n)
    rafObj.seek(rafObj.getFilePointer + n)
  }

  /** Pass on to [[java.io.RandomAccessFile]]
   */
  def getFilePointer: Long = rafObj.getFilePointer

  /** Pass on to [[java.io.RandomAccessFile]]
   */
  def seek(pos: Long): Unit = rafObj.seek(pos)

  /** Pass on to [[java.io.RandomAccessFile]]
   */
  def length: Long = rafObj.length

  /** Pass on to [[java.io.RandomAccessFile]]
   */
  def setLength(newLength: Long): Unit = rafObj.setLength(newLength)

  /** Pass on to [[java.io.RandomAccessFile]]
   */
  def close: Unit = rafObj.close

  /** Pass on to [[java.io.RandomAccessFile]]
   */
  def getChannel = rafObj.getChannel

  /** Pass on to [[java.io.RandomAccessFile]]
   */
  def getFD = rafObj.getFD

  /** Pass on to [[java.io.RandomAccessFile]]
   */
  def write(b: Int): Unit = rafObj.write(b)

  /** Pass on to [[java.io.RandomAccessFile]]
   */
  def write(b: Array[Byte], off: Int, len: Int): Unit = rafObj.write(b, off, len)

  /** Pass on to [[java.io.RandomAccessFile]]
   */
  def writeBoolean(v: Boolean): Unit = rafObj.writeBoolean(v)

  /** Pass on to [[java.io.RandomAccessFile]]
   */
  def writeByte(v: Int): Unit = rafObj.writeByte(v)

  /** Pass on to [[java.io.RandomAccessFile]]
   */
  def writeBytes(s: String): Unit = rafObj.writeBytes(s)
}

/** Reads and writes data from byte values.
 */
abstract class ByteConverter {

  ///// bytesToXXX /////
  /**Takes 1 Byte and returns a UInt8 (as Short)*/
  def byteToUInt8(b0: Byte): Short = {
    if (b0 < 0) (b0 + 256).toShort else b0.toShort
  }

  /**Takes 2 Bytes and returns an Int16 (Short)*/
  def bytesToInt16(b0: Byte, b1: Byte): Short

  /**Takes 2 Bytes and returns a UInt16 (as Char)*/
  def bytesToUInt16(b0: Byte, b1: Byte): Char

  /**Takes 4 Bytes and returns a UInt32 (Int)*/
  def bytesToInt32(b0: Byte, b1: Byte, b2: Byte, b3: Byte): Int

  /**Takes 4 Bytes and returns a UInt32 (as Long)*/
  def bytesToUInt32(b0: Byte, b1: Byte, b2: Byte, b3: Byte): Long

  /**Takes 8 Bytes and returns a UInt64 (as ULong), throwing an error if it overflows Long, which is Int64*/
  final def bytesToUInt64(b0: Byte, b1: Byte, b2: Byte, b3: Byte, b4: Byte, b5: Byte, b6: Byte, b7: Byte): ULong = {
    ULong(bytesToInt64(b0, b1, b2, b3, b4, b5, b6, b7))
  }

  /**Takes 8 Bytes and returns a Int64 (Long)*/
  def bytesToInt64(b0: Byte, b1: Byte, b2: Byte, b3: Byte, b4: Byte, b5: Byte, b6: Byte, b7: Byte): Long

  /**Takes 8 Bytes and returns a UInt64 shifted down to the range of Int64 (Long). The shifted number range runs from
   * -2^63 to 2^63-1, so that UInt64 can be represented in the JVM long (Int64). Addition and subtraction
   * are valid with these long representations, multiplication and division, naturally, are not.*/
  def bytesToUInt64Shifted(b0: Byte, b1: Byte, b2: Byte, b3: Byte, b4: Byte, b5: Byte, b6: Byte, b7: Byte): Long

  ///// XXXToByte /////
  /**Takes an UInt8 (as Sort), and returns a bytes*/
  def uInt8ToByte(value: Short): Byte = {
    require(value <= 255 && value >= 0, "Value " + value + " is out of range of 1-byte unsigned integer.")
    if (value >= 128) (value - 256.toShort).toByte else value.toByte
  }

  /**Takes an Int16 (Short), and returns an array of 2 bytes*/
  def int16ToBytes(value: Short): Array[Byte]

  /**Takes a UInt16 (Char), and returns an array of 2 bytes*/
  def uInt16ToBytes(value: Char): Array[Byte]

  /**Takes an Int32 (Int), and returns an array of 4 bytes*/
  def int32ToBytes(value: Int): Array[Byte]

  /**Takes a UInt32 (as Long), and returns an array of 4 bytes*/
  def uInt32ToBytes(value: Long): Array[Byte]

  /**Takes an Int64 (Long), and returns an array of 8 bytes*/
  def int64ToBytes(value: Long): Array[Byte]

  /**Takes a UInt64 (as ULong), and returns an array of 8 bytes*/
  def uInt64ToBytes(value: ULong): Array[Byte]

  /**Takes an Int64 (Long), and returns an array of 8 bytes, shifted up to a UInt64.
   * See [[breeze.io.ByteConverter.bytesToUInt64Shifted()]]*/
  def uInt64ShiftedToBytes(value: Long): Array[Byte]

}

/** See [[breeze.io.ByteConverter]], reads big endian.
 */
object ByteConverterBigEndian extends ByteConverter {

  ///// bytesToXXX /////
  def bytesToInt16(b0: Byte, b1: Byte): Short = {
    (b0 << 8 | b1 & 0xff).toShort
  }

  def bytesToUInt16(b0: Byte, b1: Byte): Char = {
    ((b0.toInt & 0xff) << 8 | (b1.toInt & 0xff)).toChar
  }

  def bytesToInt32(b0: Byte, b1: Byte, b2: Byte, b3: Byte): Int = {
    b0.toInt << 24 | (b1 & 0xff) << 16 | (b2 & 0xff) << 8 | (b3 & 0xff)
  }

  def bytesToUInt32(b0: Byte, b1: Byte, b2: Byte, b3: Byte): Long = {
    (b0.toLong & 0xffL) << 24 | (b1.toLong & 0xffL) << 16 | (b2.toLong & 0xffL) << 8 | (b3.toLong & 0xffL)
  }

//  def bytesToUInt64(b0: Byte, b1: Byte, b2: Byte, b3: Byte, b4: Byte, b5: Byte, b6: Byte, b7: Byte): ULong = {
////    if ((b0/*.toInt*/ & 0x80) != 0x00) {
////      throw new IOException("UInt64 too big to read given limitations of Long format.")
////    } else {
//      (BigInt(b0) & 0xFFL) << 56 | (b1.toLong & 0xFFL) << 48 | (b2.toLong & 0xFFL) << 40 | (b3.toLong & 0xFFL) << 32 |
//        (b4.toLong & 0xFFL) << 24 | (b5.toLong & 0xFFL) << 16 | (b6.toLong & 0xFFL) << 8 | (b7.toLong & 0xFFL)
////    }
//  }

  def bytesToInt64(b0: Byte, b1: Byte, b2: Byte, b3: Byte, b4: Byte, b5: Byte, b6: Byte, b7: Byte): Long = {
    b0.toLong << 56 | (b1.toLong & 0xffL) << 48 | (b2.toLong & 0xffL) << 40 | (b3.toLong & 0xffL) << 32 |
      (b4.toLong & 0xffL) << 24 | (b5.toLong & 0xffL) << 16 | (b6.toLong & 0xffL) << 8 | (b7.toLong & 0xffL)
  }

  def bytesToUInt64Shifted(b0: Byte, b1: Byte, b2: Byte, b3: Byte, b4: Byte, b5: Byte, b6: Byte, b7: Byte): Long = {
    (b0 ^ 0x80).toLong << 56 | (b1.toLong & 0xffL) << 48 | (b2.toLong & 0xffL) << 40 | (b3.toLong & 0xffL) << 32 |
      (b4.toLong & 0xffL) << 24 | (b5.toLong & 0xffL) << 16 | (b6.toLong & 0xffL) << 8 | (b7.toLong & 0xffL)
  }
  ///// XXXToByte /////
  def int16ToBytes(value: Short): Array[Byte] = {
    val tempret = new Array[Byte](2)
    tempret(0) = (value >> 8).toByte
    tempret(1) = (value & 0xff).toByte
    tempret
  }

  def uInt16ToBytes(value: Char): Array[Byte] = {
    require(value <= 65535 && value >= 0, "Value " + value + " is out of range of 2-byte unsigned array.")

    val tempret = new Array[Byte](2)
    tempret(0) = ((value >> 8) & 0xff).toByte
    tempret(1) = (value & 0xff).toByte
    tempret
  }

  def int32ToBytes(value: Int): Array[Byte] = {
    val tempret = new Array[Byte](4)
    tempret(0) = (value >> 24).toByte
    tempret(1) = ((value >> 16) & 0xff).toByte
    tempret(2) = ((value >> 8) & 0xff).toByte
    tempret(3) = (value & 0xff).toByte
    tempret
  }

  def uInt32ToBytes(value: Long): Array[Byte] = {
    require(value <= 4294967295L && value >= 0L, "Value " + value + " is out of range of 4-byte unsigned array.")

    val tempret = new Array[Byte](4)
    tempret(0) = ((value >> 24) & 0xff).toByte
    tempret(1) = ((value >> 16) & 0xff).toByte
    tempret(2) = ((value >> 8) & 0xff).toByte
    tempret(3) = (value & 0xff).toByte
    tempret
  }

  def int64ToBytes(value: Long): Array[Byte] = {
    val tempret = new Array[Byte](8)
    tempret(0) = (value >> 56).toByte
    tempret(1) = ((value >> 48) & 0xff).toByte
    tempret(2) = ((value >> 40) & 0xff).toByte
    tempret(3) = ((value >> 32) & 0xff).toByte
    tempret(4) = ((value >> 24) & 0xff).toByte
    tempret(5) = ((value >> 16) & 0xff).toByte
    tempret(6) = ((value >> 8) & 0xff).toByte
    tempret(7) = (value & 0xff).toByte
    tempret
  }

  def uInt64ToBytes(value: ULong): Array[Byte] = {
//    require(value >= ZERO, s"Value $value is out of range of 8-byte unsigned array.")
//    require(value <= uInt64Max, s"Value $value is out of range of 8-byte unsigned array.")

    val tempret = new Array[Byte](8)
    val longValue = value.longValue

    tempret(0) = ((longValue >> 56) & 0xff).toByte
    tempret(1) = ((longValue >> 48) & 0xff).toByte
    tempret(2) = ((longValue >> 40) & 0xff).toByte
    tempret(3) = ((longValue >> 32) & 0xff).toByte
    tempret(4) = ((longValue >> 24) & 0xff).toByte
    tempret(5) = ((longValue >> 16) & 0xff).toByte
    tempret(6) = ((longValue >> 8) & 0xff).toByte
    tempret(7) = (longValue & 0xff).toByte
    tempret
  }

  def uInt64ShiftedToBytes(value: Long): Array[Byte] = {

    val tempret = new Array[Byte](8)
    tempret(0) = (((value >> 56) & 0xff) ^ 0x80).toByte
    tempret(1) = ((value >> 48) & 0xff).toByte
    tempret(2) = ((value >> 40) & 0xff).toByte
    tempret(3) = ((value >> 32) & 0xff).toByte
    tempret(4) = ((value >> 24) & 0xff).toByte
    tempret(5) = ((value >> 16) & 0xff).toByte
    tempret(6) = ((value >> 8) & 0xff).toByte
    tempret(7) = (value & 0xff).toByte
    tempret
  }

}

/** See [[breeze.io.ByteConverter]], reads little endian.
 */
object ByteConverterLittleEndian extends ByteConverter {

  override def bytesToInt16(b0: Byte, b1: Byte) = ByteConverterBigEndian.bytesToInt16(b1, b0)
  override def bytesToUInt16(b0: Byte, b1: Byte) = ByteConverterBigEndian.bytesToUInt16(b1, b0)
  override def bytesToInt32(b0: Byte, b1: Byte, b2: Byte, b3: Byte) =
    ByteConverterBigEndian.bytesToInt32(b3, b2, b1, b0)
  override def bytesToUInt32(b0: Byte, b1: Byte, b2: Byte, b3: Byte) =
    ByteConverterBigEndian.bytesToUInt32(b3, b2, b1, b0)
  override def bytesToInt64(b0: Byte, b1: Byte, b2: Byte, b3: Byte, b4: Byte, b5: Byte, b6: Byte, b7: Byte) =
    ByteConverterBigEndian.bytesToInt64(b7, b6, b5, b4, b3, b2, b1, b0)
//  override def bytesToUInt64(b0: Byte, b1 : Byte, b2 : Byte, b3 : Byte, b4 : Byte, b5 : Byte, b6 : Byte, b7 : Byte)
//  = ByteConverterBigEndian.bytesToUInt64(b7, b6, b5, b4, b3, b2, b1, b0)
  override def bytesToUInt64Shifted(b0: Byte, b1: Byte, b2: Byte, b3: Byte, b4: Byte, b5: Byte, b6: Byte, b7: Byte) =
    ByteConverterBigEndian.bytesToUInt64Shifted(b7, b6, b5, b4, b3, b2, b1, b0)

//reverse is pretty slow, and a time hog. Therefore, unfortunately, the following code reuse is not practical
//  override def int16ToBytes(value: Short): Array[Byte]  = ByteConverterBigEndian.int16ToBytes(value).reverse
//  override def uInt16ToBytes(value: Char): Array[Byte]   = ByteConverterBigEndian.uInt16ToBytes(value).reverse
//  override def int32ToBytes(value: Int): Array[Byte]    = ByteConverterBigEndian.int32ToBytes(value).reverse
//  override def uInt32ToBytes(value: Long): Array[Byte]  = ByteConverterBigEndian.uInt32ToBytes(value).reverse
//  override def int64ToBytes(value: Long): Array[Byte]   = ByteConverterBigEndian.int64ToBytes(value).reverse
//  override def uInt64ToBytes(value: Long): Array[Byte]  = ByteConverterBigEndian.uInt64ToBytes(value).reverse
//  override def uInt64ShiftedToBytes(value: Long): Array[Byte] = ByteConverterBigEndian.uInt64ShiftedToBytes(value).reverse

  ///// XXXToByte /////
  def int16ToBytes(value: Short): Array[Byte] = {
    val tempret = new Array[Byte](2)
    tempret(1) = (value >> 8).toByte
    tempret(0) = (value & 0xff).toByte
    tempret
  }

  def uInt16ToBytes(value: Char): Array[Byte] = {
    require(value <= 65535 && value >= 0, "Value " + value + " is out of range of 2-byte unsigned array.")

    val tempret = new Array[Byte](2)
    tempret(1) = ((value >> 8) & 0xff).toByte
    tempret(0) = (value & 0xff).toByte
    tempret
  }

  def int32ToBytes(value: Int): Array[Byte] = {
    val tempret = new Array[Byte](4)
    tempret(3) = (value >> 24).toByte
    tempret(2) = ((value >> 16) & 0xff).toByte
    tempret(1) = ((value >> 8) & 0xff).toByte
    tempret(0) = (value & 0xff).toByte
    tempret
  }

  def uInt32ToBytes(value: Long): Array[Byte] = {
    require(value <= 4294967295L && value >= 0L, "Value " + value + " is out of range of 4-byte unsigned array.")

    val tempret = new Array[Byte](4)
    tempret(3) = ((value >> 24) & 0xff).toByte
    tempret(2) = ((value >> 16) & 0xff).toByte
    tempret(1) = ((value >> 8) & 0xff).toByte
    tempret(0) = (value & 0xff).toByte
    tempret
  }

  def int64ToBytes(value: Long): Array[Byte] = {
    val tempret = new Array[Byte](8)
    tempret(7) = (value >> 56).toByte
    tempret(6) = ((value >> 48) & 0xff).toByte
    tempret(5) = ((value >> 40) & 0xff).toByte
    tempret(4) = ((value >> 32) & 0xff).toByte
    tempret(3) = ((value >> 24) & 0xff).toByte
    tempret(2) = ((value >> 16) & 0xff).toByte
    tempret(1) = ((value >> 8) & 0xff).toByte
    tempret(0) = (value & 0xff).toByte
    tempret
  }

  def uInt64ToBytes(value: ULong): Array[Byte] = {
//    require(value >= ZERO, s"Value $value is out of range of 8-byte unsigned array.")
//    require(value <= uInt64Max, s"Value $value is out of range of 8-byte unsigned array.")

    val tempret = new Array[Byte](8)
    val longValue = value.longValue
    tempret(7) = ((longValue >> 56) & 0xff).toByte
    tempret(6) = ((longValue >> 48) & 0xff).toByte
    tempret(5) = ((longValue >> 40) & 0xff).toByte
    tempret(4) = ((longValue >> 32) & 0xff).toByte
    tempret(3) = ((longValue >> 24) & 0xff).toByte
    tempret(2) = ((longValue >> 16) & 0xff).toByte
    tempret(1) = ((longValue >> 8) & 0xff).toByte
    tempret(0) = (longValue & 0xff).toByte
    tempret
  }

  def uInt64ShiftedToBytes(value: Long): Array[Byte] = {

    val tempret = new Array[Byte](8)
    tempret(7) = (((value >> 56) & 0xff) ^ 0x80).toByte
    tempret(6) = ((value >> 48) & 0xff).toByte
    tempret(5) = ((value >> 40) & 0xff).toByte
    tempret(4) = ((value >> 32) & 0xff).toByte
    tempret(3) = ((value >> 24) & 0xff).toByte
    tempret(2) = ((value >> 16) & 0xff).toByte
    tempret(1) = ((value >> 8) & 0xff).toByte
    tempret(0) = (value & 0xff).toByte
    tempret
  }

}
