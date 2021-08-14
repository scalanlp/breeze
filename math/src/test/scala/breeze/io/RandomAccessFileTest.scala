package breeze.io

import org.scalatest.funsuite.AnyFunSuite
import java.io.{File}
import spire.math.ULong

/**
 * Created with IntelliJ IDEA.
 * User: Kenta
 * Date: 11/4/13
 * Time: 6:22 PM
 * To change this template use File | Settings | File Templates.
 */
sealed trait RandomAccessFileTest extends AnyFunSuite {

  implicit def bc: ByteConverter
  def fileNameAppend: String

  lazy val fileHead = "tempRAF"

  type RAF = RandomAccessFile

  def getResource(fileNameHead: String) = File.createTempFile(fileNameHead, ".bin")

  ////Writing

  test("writeDouble") {
    val file = getResource(fileHead + "Double")
    val stream = new RAF(file, "rw")
    stream.writeDouble(0.0)
    stream.writeDouble(Array[Double](3.141592653589793, 2.718281828459045, 6.02214E23))
    stream.writeDouble(1.6726231000000002E-24)
    stream.close

    val stream2 = new RAF(file, "r")
    val result2 = stream2.readDouble(3)

    assert(result2(0) === 0.0)
    assert(result2(1) === 3.141592653589793)
    assert(result2(2) === 2.718281828459045)
    assert(stream2.readDouble === 6.02214E23)
    assert(stream2.readDouble === 1.6726231000000002E-24)
    stream2.close
  }

  test("writeFloat") {
    val file = getResource(fileHead + "Float")
    val stream = new RAF(file, "rw")
    stream.writeFloat(0.0F)
    stream.writeFloat(Array[Float](3.1415927F, 2.7182817F, 6.02214E23F))
    stream.writeFloat(1.6726232E-24F)
    stream.close

    val stream2 = new RAF(file, "r")
    val result2 = stream2.readFloat(3)
    assert(result2(0) === 0.0F)
    assert(result2(1) === 3.1415927F)
    assert(result2(2) === 2.7182817F)
    assert(stream2.readFloat === 6.02214E23F)
    assert(stream2.readFloat === 1.6726232E-24F)
    stream2.close
  }

  test("writeInt8/write") {
    val file = getResource(fileHead + "Float")
    val stream = new RAF(file, "rw")
    stream.writeInt8(0.toByte)
    stream.writeInt8(Array[Byte](1.toByte, (-1).toByte, (-128).toByte))
    stream.write(127.toByte)
    stream.close

    val stream2 = new RAF(file, "r")
    val result2 = stream2.readInt8(3)
    assert(result2(0) === 0.toByte)
    assert(result2(1) === 1.toByte)
    assert(result2(2) === (-1).toByte)
    assert(stream2.readInt8() === (-128).toByte)
    assert(stream2.readInt8() === 127.toByte)
    stream2.close
  }

  test("writeUInt8") {
    val file = getResource(fileHead + "Float")
    val stream = new RAF(file, "rw")
    stream.writeUInt8(0.toShort)
    stream.writeUInt8(Array[Short](1.toShort, 127.toShort, 128.toShort))
    stream.writeUInt8(255.toShort)
    stream.close

    val stream2 = new RAF(file, "r")
    val result2 = stream2.readUInt8(3)
    assert(result2(0) === 0.toShort)
    assert(result2(1) === 1.toShort)
    assert(result2(2) === 127.toShort)
    assert(stream2.readUInt8() === 128.toShort)
    assert(stream2.readUInt8() === 255.toShort)
    stream2.close
  }

  test("writeInt16/writeShort") {
    val file = getResource(fileHead + "Float")
    val stream = new RAF(file, "rw")
    stream.writeInt16(0.toShort)
    stream.writeInt16(Array[Short](1.toShort, (-1).toShort, (-32768).toShort))
    stream.writeShort(32767.toShort)
    stream.close

    val stream2 = new RAF(file, "r")
    val result2 = stream2.readInt16(3)

    assert(result2(0) === 0.toShort)
    assert(result2(1) === 1.toShort)
    assert(result2(2) === (-1).toShort)
    assert(stream2.readShort() === (-32768).toShort)
    assert(stream2.readInt16() === 32767.toShort)
    stream2.close
  }

  test("writeUInt16") {
    val file = getResource(fileHead + "UInt16")
    val stream = new RAF(file, "rw")
    stream.writeUInt16(0.toChar)
    stream.writeUInt16(Array[Char](1, 32767, 65535))
    stream.writeUInt16(65535.toChar)
    stream.writeUInt16(65535.toChar)
    stream.close

    val stream2 = new RAF(file, "r")
    val result2 = stream2.readUInt16(3)

    assert(result2(0) === 0)
    assert(result2(1) === 1)
    assert(result2(2) === 32767)
    assert(stream2.readUInt16() === 65535)
    assert(stream2.readUInt16() === 65535)
    assert(stream2.readUInt8(2).forall(_ == 0xFF))
    stream2.close
  }

  test("writeInt32/writeInt") {
    val file = getResource(fileHead + "Int32")
    val stream = new RAF(file, "rw")
    stream.writeInt32(0)
    stream.writeInt(Array[Int](1, -1, 2147483647))
    stream.writeInt32(-2147483648)

    //Tests for maximum and minimum value byte codes
    stream.writeInt(Array(2147483647, -2147483648))
    stream.close

    val stream2 = new RAF(file, "r")
    val result2 = stream2.readInt32(3)

    assert(result2(0) === 0)
    assert(result2(1) === 1)
    assert(result2(2) === -1)
    assert(stream2.readInt() === 2147483647)
    assert(stream2.readInt32() === -2147483648)

    //Tests for maximum and minimum value byte codes
    val tempMaxRead = stream2.readUInt8(4)
    assert(tempMaxRead.drop(1).dropRight(1).forall(_ == 0xFF))
    //Allows same test to be used for BigEndian or LittleEndian
    assert((tempMaxRead(0) == 0x7F && tempMaxRead(3) == 0xFF) || (tempMaxRead(0) == 0xFF && tempMaxRead(3) == 0x7F))
    val tempMinRead = stream2.readUInt8(4)
    assert(tempMinRead.drop(1).dropRight(1).forall(_ == 0x00))
    //Allows same test to be used for BigEndian or LittleEndian
    assert((tempMinRead(0) == 0x80 && tempMinRead(3) == 0x00) || (tempMinRead(0) == 0x00 && tempMinRead(3) == 0x80))

    stream2.close
  }

  test("writeUInt32") {
    val file = getResource(fileHead + "UInt32")
    val stream = new RAF(file, "rw")
    stream.writeUInt32(0L)
    stream.writeUInt32(Array[Long](1L, 32767L, 4294967295L))
    stream.writeUInt32(4294967295L)
    stream.writeUInt32(4294967295L)
    stream.close

    val stream2 = new RAF(file, "r")
    val result2 = stream2.readUInt32(3)
    assert(result2(0) === 0L)
    assert(result2(1) === 1L)
    assert(result2(2) === 32767L)
    assert(stream2.readUInt32() === 4294967295L)
    assert(stream2.readUInt32() === 4294967295L)
    assert(stream2.readUInt8(4).forall(_ == 0xFF))
    stream2.close
  }

  test("writeInt64/writeLong") {
    val file = getResource(fileHead + "Long")
    val stream = new RAF(file, "rw")
    stream.writeInt64(0L)
    stream.writeLong(Array[Long](1L, -1L, 9223372036854775807L))
    stream.writeInt64(-9223372036854775808L)

    //Tests for maximum and minimum value byte codes
    stream.writeLong(Array(9223372036854775807L, -9223372036854775808L))

    stream.close

    val stream2 = new RAF(file, "r")
    val result2 = stream2.readInt64(3)

    assert(result2(0) === 0L)
    assert(result2(1) === 1L)
    assert(result2(2) === -1L)
    assert(stream2.readLong() === 9223372036854775807L)
    assert(stream2.readInt64() === -9223372036854775808L)

    //Tests for maximum and minimum value byte codes
    val tempMaxRead = stream2.readUInt8(8)
    assert(tempMaxRead.drop(1).dropRight(1).forall(_ == 0xFF))
    //Allows same test to be used for BigEndian or LittleEndian
    assert((tempMaxRead(0) == 0x7F && tempMaxRead(7) == 0xFF) || (tempMaxRead(0) == 0xFF && tempMaxRead(7) == 0x7F))
    val tempMinRead = stream2.readUInt8(8)
    assert(tempMinRead.drop(1).dropRight(1).forall(_ == 0x00))
    //Allows same test to be used for BigEndian or LittleEndian
    assert((tempMinRead(0) == 0x80 && tempMinRead(7) == 0x00) || (tempMinRead(0) == 0x00 && tempMinRead(7) == 0x80))

    stream2.close
  }

  test("writeUInt64") {
    val file = getResource(fileHead + "UInt64")
    val stream = new RAF(file, "rw")
    val UInt64Max = ULong("18446744073709551615")

    stream.writeUInt64(ULong(0L))
    stream.writeUInt64(Array[ULong](ULong(1L), ULong(32767L), ULong(9223372036854775807L)))
    stream.writeUInt64(ULong(9223372036854775807L))
    stream.writeUInt64(Array(UInt64Max, UInt64Max))
    stream.close

    val stream2 = new RAF(file, "r")
    val result2 = stream2.readUInt64(3)

    //Strange failure here of === with implicit conversion ULong => Long
    //  needs explicit casting *.toLong just when running on Travis
    assert(result2(0).toLong === 0L)
    assert(result2(1).toLong === 1L)
    assert(result2(2).toLong === 32767L)
    assert(stream2.readUInt64().toLong === 9223372036854775807L)
    assert(stream2.readUInt64().toLong === 9223372036854775807L)
    //println( stream2.readUInt8(8).toList )
    assert(stream2.readUInt8(8).forall(_ == 0xFF))
    assert(stream2.readUInt64() == UInt64Max)
    stream2.close
  }

  test("writeUInt64Shifted") {
    val file = getResource(fileHead + "UInt64Shifted")
    val stream = new RAF(file, "rw")
    stream.writeUInt64Shifted(0L)
    stream.writeUInt64Shifted(Array[Long](1L, -32767L, -9223372036854775808L))
    stream.writeUInt64Shifted(9223372036854775807L)
    stream.close

    val stream2 = new RAF(file, "r")
    val result2 = stream2.readUInt64Shifted(3)

    assert(result2(0) === 0L)
    assert(result2(1) === 1L)
    assert(result2(2) === -32767L)
    assert(stream2.readUInt64Shifted() === -9223372036854775808L)
    assert(stream2.readUInt64Shifted() === 9223372036854775807L)
    stream2.close
  }

}

class RandomAccessFileTestBigEndian extends RandomAccessFileTest {
  implicit def bc: ByteConverter = breeze.io.ByteConverterBigEndian
  def fileNameAppend = "BE.bin"
}

class RandomAccessFileTestLittleEndian extends RandomAccessFileTest {
  implicit def bc: ByteConverter = breeze.io.ByteConverterLittleEndian
  def fileNameAppend = "LE.bin"
}
