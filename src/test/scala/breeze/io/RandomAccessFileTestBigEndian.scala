package breeze.io

import org.scalatest.FunSuite
import java.io.{File, IOException}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created with IntelliJ IDEA.
 * User: Kenta
 * Date: 11/4/13
 * Time: 6:22 PM
 * To change this template use File | Settings | File Templates.
 */
class RandomAccessFileTestBigEndian extends FunSuite {

  implicit val bc: ByteConverter = breeze.io.ByteConverterBigEndian
  val fileNameAppend = "BE.bin"
  lazy val fileHead = (new File(getClass.getResource("/Double"+fileNameAppend).getPath())).getParent

  type RAF = RandomAccessFile

  def getResource(fileNameHead: String) = getClass.getResource(fileNameHead).getFile

  //The following tests are common with RandomAccessFileLETest, edit that as well

  test("readDouble"){
    val stream = new RAF( getResource("/Double"+fileNameAppend), "r")
    val result = stream.readDouble(5)
    assert(result(0) == 0.0)
    assert(result(1) == 3.141592653589793)
    assert(result(2) == 2.718281828459045)
    assert(result(3) == 6.02214E23)
    assert(result(4) == 1.6726231000000002E-24)
    stream.close
  }

  test("readFloat"){
    val stream = new RAF( getResource("/Float"+fileNameAppend), "r")
    val result = stream.readFloat(5)
    assert(result(0) == 0.0)
    assert(result(1) == 3.1415927F)
    assert(result(2) == 2.7182817F)
    assert(result(3) == 6.02214E23F)
    assert(result(4) == 1.6726232E-24F)
    stream.close
  }

  test("readInt8/readByte"){
    val stream = new RAF( getResource("/Int8.bin"), "r")
    val res = stream.readInt8(5)
    stream.seek(0)
    val resB = stream.readByte(5)
    assert(res(0) == resB(0) && resB(0) == 0)
    assert(res(1) == resB(1) && resB(1) == 1)
    assert(res(2) == resB(2) && resB(2) == -1)
    assert(res(3) == resB(3) && resB(3) == -128)
    assert(res(4) == resB(4) && resB(4) == 127)
    stream.close
  }

  test("readUInt8/readUnsignedByte"){
    val stream = new RAF( getResource("/UInt8.bin"), "r")
    val res = stream.readUInt8(5)
    stream.seek(0)
    val resB = stream.readUnsignedByte(5)
    assert(res(0) == resB(0) && resB(0) == 0)
    assert(res(1) == resB(1) && resB(1) == 1)
    assert(res(2) == resB(2) && resB(2) == 1)
    assert(res(3) == resB(3) && resB(3) == 255)
    assert(res(4) == resB(4) && resB(4) == 255)
    stream.close
  }

  test("readInt16/readShort"){
    val stream = new RAF( getResource("/Int16"+fileNameAppend), "r")
    val res = stream.readInt16(5)
    stream.seek(0)
    val resB = stream.readShort(5)
    assert(res(0) == resB(0) && resB(0) == 0)
    assert(res(1) == resB(1) && resB(1) == 1)
    assert(res(2) == resB(2) && resB(2) == -1)
    assert(res(3) == resB(3) && resB(3) == -32768)
    assert(res(4) == resB(4) && resB(4) == 32767)
    stream.close
  }

  test("readUInt16/readUnsignedShort"){
    val stream = new RAF( getResource("/UInt16"+fileNameAppend), "r")
    val res = stream.readUInt16(5)
    stream.seek(0)
    val resB = stream.readUnsignedShort(5)
    assert(res(0) == resB(0) && resB(0) == 0)
    assert(res(1) == resB(1) && resB(1) == 1)
    assert(res(2) == resB(2) && resB(2) == 1)
    assert(res(3) == resB(3) && resB(3) == 65535)
    assert(res(4) == resB(4) && resB(4) == 65535)
    stream.close
  }

  test("readInt32/readInt"){
    val stream = new RAF( getResource("/Int32"+fileNameAppend), "r")
    val res = stream.readInt32(5)
    stream.seek(0)
    val resB = stream.readInt(5)
    assert(res(0) == resB(0) && resB(0) == 0)
    assert(res(1) == resB(1) && resB(1) == 1)
    assert(res(2) == resB(2) && resB(2) == -1)
    assert(res(3) == resB(3) && resB(3) == 2147483647)
    assert(res(4) == resB(4) && resB(4) == -2147483648)
    stream.close
  }

  test("readUInt32"){
    val stream = new RAF( getResource("/UInt32"+fileNameAppend), "r")
    val res = stream.readUInt32(5)
    assert(res(0) ==  0L)
    assert(res(1) ==  1L)
    assert(res(2) ==  1L)
    assert(res(3) ==  4294967295L)
    assert(res(4) ==  4294967295L)
    stream.close
  }

  test("readInt64/readLong"){
    val stream = new RAF( getResource("/Int64"+fileNameAppend), "r")
    val res = stream.readInt64(5)
    stream.seek(0)
    val resB = stream.readLong(5)
    assert(res(0) == resB(0) && resB(0) == 0)
    assert(res(1) == resB(1) && resB(1) == 1)
    assert(res(2) == resB(2) && resB(2) == -1)
    assert(res(3) == resB(3) && resB(3) == 9223372036854775807L)
    assert(res(4) == resB(4) && resB(4) == -9223372036854775808L)
    stream.close
  }

  test("readUInt64"){
    val stream = new RAF( getResource("/UInt64"+fileNameAppend), "r")
    val res = stream.readUInt64(4)
    assert(res(0) ==  0L)
    assert(res(1) ==  1L)
    assert(res(2) ==  1L)
    assert(res(3) ==  9223372036854775807L)
    try{
      stream.readUInt64
    }catch{
      case e:IOException => assert(true)
      case _: Throwable => assert(false)
    }
    stream.close
  }

  test("readUInt64Shifted"){
    val stream = new RAF( getResource("/UInt64"+fileNameAppend), "r")
    val res = stream.readUInt64Shifted(5)
    assert(res(0) ==  -9223372036854775808L)
    assert(res(1) ==  -9223372036854775807L)
    assert(res(2) ==  -9223372036854775807L)
    assert(res(3) ==  -1L)
    assert(res(4) ==  9223372036854775807L)
    stream.close
  }


  ////Writing

  test("writeDouble"){
    val stream = new RAF(fileHead + "/temp.bin", "rw")
    stream.writeDouble(0.0)
    stream.writeDouble( Array[Double](3.141592653589793, 2.718281828459045, 6.02214E23) )
    stream.writeDouble(1.6726231000000002E-24)
    stream.close

    val stream2 =  new RAF(fileHead + "/temp.bin", "r")
    val result2 = stream2.readDouble(3)
    assert(result2(0) == 0.0)
    assert(result2(1) == 3.141592653589793)
    assert(result2(2) == 2.718281828459045)
    assert(stream2.readDouble ==  6.02214E23)
    assert(stream2.readDouble == 1.6726231000000002E-24)
    stream2.close
  }

  test("writeFloat"){
    val stream = new RAF(fileHead + "/temp.bin", "rw")
    stream.writeFloat(0.0F)
    stream.writeFloat( Array[Float](3.1415927F, 2.7182817F, 6.02214E23F) )
    stream.writeFloat(1.6726232E-24F)
    stream.close

    val stream2 =  new RAF(fileHead + "/temp.bin", "r")
    val result2 = stream2.readFloat(3)
    assert(result2(0) == 0.0F)
    assert(result2(1) == 3.1415927F)
    assert(result2(2) == 2.7182817F)
    assert(stream2.readFloat ==  6.02214E23F)
    assert(stream2.readFloat == 1.6726232E-24F)
    stream2.close
  }

  test("writeInt8/write"){
    val stream = new RAF(fileHead + "/temp.bin", "rw")
    stream.writeInt8(0.toByte)
    stream.writeInt8( Array[Byte](1.toByte, (-1).toByte, (-128).toByte) )
    stream.write( 127.toByte )
    stream.close

    val stream2 =  new RAF(fileHead + "/temp.bin", "r")
    val result2 = stream2.readInt8(3)
    assert(result2(0) == 0.toByte)
    assert(result2(1) == 1.toByte)
    assert(result2(2) == (-1).toByte)
    assert(stream2.readInt8 ==  (-128).toByte)
    assert(stream2.readInt8 == 127.toByte)
    stream2.close
  }

  test("writeUInt8"){
    val stream = new RAF(fileHead + "/temp.bin", "rw")
    stream.writeUInt8(0.toShort)
    stream.writeUInt8( Array[Short](1.toShort, 127.toShort, 128.toShort) )
    stream.writeUInt8( 255.toShort )
    stream.close

    val stream2 =  new RAF(fileHead + "/temp.bin", "r")
    val result2 = stream2.readUInt8(3)
    assert(result2(0) == 0.toShort)
    assert(result2(1) == 1.toShort)
    assert(result2(2) == 127.toShort)
    assert(stream2.readUInt8 ==  128.toShort)
    assert(stream2.readUInt8 == 255.toShort)
    stream2.close
  }

  test("writeInt16/writeShort"){
    val stream = new RAF(fileHead + "/temp.bin", "rw")
    stream.writeInt16(0.toShort)
    stream.writeInt16( Array[Short](1.toShort, (-1).toShort, (-32768).toShort) )
    stream.writeShort( 32767.toShort )
    stream.close

    val stream2 =  new RAF(fileHead + "/temp.bin", "r")
    val result2 = stream2.readInt16(3)
    assert(result2(0) == 0.toShort)
    assert(result2(1) == 1.toShort)
    assert(result2(2) == (-1).toShort)
    assert(stream2.readShort ==  (-32768).toShort)
    assert(stream2.readInt16 == 32767.toShort)
    stream2.close
  }

  test("writeUInt16"){
    val stream = new RAF(fileHead + "/temp.bin", "rw")
    stream.writeUInt16( 0.toChar )
    stream.writeUInt16( Array[Char](1, 32767, 65535) )
    stream.writeUInt16( 65535.toChar )
    stream.close

    val stream2 =  new RAF(fileHead + "/temp.bin", "r")
    val result2 = stream2.readUInt16(3)
    assert(result2(0) == 0 )
    assert(result2(1) == 1 )
    assert(result2(2) == 32767 )
    assert(stream2.readUInt16 ==  65535)
    assert(stream2.readUInt16 == 65535)
    stream2.close
  }

  test("writeInt32/writeInt"){
    val stream = new RAF(fileHead + "/temp.bin", "rw")
    stream.writeInt32(0)
    stream.writeInt( Array[Int](1, -1, 2147483647) )
    stream.writeInt32( -2147483648 )
    stream.close

    val stream2 =  new RAF(fileHead + "/temp.bin", "r")
    val result2 = stream2.readInt32(3)
    assert(result2(0) == 0)
    assert(result2(1) == 1)
    assert(result2(2) == -1)
    assert(stream2.readInt == 2147483647 )
    assert(stream2.readInt32 == -2147483648 )
    stream2.close
  }

  test("writeUInt32"){
    val stream = new RAF(fileHead + "/temp.bin", "rw")
    stream.writeUInt32( 0L)
    stream.writeUInt32( Array[Long](1L, 32767L, 4294967295L) )
    stream.writeUInt32( 4294967295L )
    stream.close

    val stream2 =  new RAF(fileHead + "/temp.bin", "r")
    val result2 = stream2.readUInt32(3)
    assert(result2(0) == 0L )
    assert(result2(1) == 1L )
    assert(result2(2) == 32767L )
    assert(stream2.readUInt32 ==  4294967295L)
    assert(stream2.readUInt32 == 4294967295L)
    stream2.close
  }

  test("writeInt64/writeLong"){
    val stream = new RAF(fileHead + "/temp.bin", "rw")
    stream.writeInt64(0L)
    stream.writeLong( Array[Long](1L, -1L, 9223372036854775807L) )
    stream.writeInt64( -9223372036854775808L )
    stream.close

    val stream2 =  new RAF(fileHead + "/temp.bin", "r")
    val result2 = stream2.readInt64(3)
    assert(result2(0) == 0L)
    assert(result2(1) == 1L)
    assert(result2(2) == -1L)
    assert(stream2.readLong == 9223372036854775807L )
    assert(stream2.readInt64 == -9223372036854775808L )
    stream2.close
  }

  test("writeUInt64"){
    val stream = new RAF(fileHead + "/temp.bin", "rw")
    stream.writeUInt64( 0L)
    stream.writeUInt64( Array[Long](1L, 32767L, 9223372036854775807L) )
    stream.writeUInt64( 9223372036854775807L )
    stream.close

    val stream2 =  new RAF(fileHead + "/temp.bin", "r")
    val result2 = stream2.readUInt64(3)
    assert(result2(0) == 0L )
    assert(result2(1) == 1L )
    assert(result2(2) == 32767L )
    assert(stream2.readUInt64 ==  9223372036854775807L)
    assert(stream2.readUInt64 == 9223372036854775807L)
    stream2.close
  }

  test("writeUInt64Shifted"){
    val stream = new RAF(fileHead + "/temp.bin", "rw")
    stream.writeUInt64Shifted( 0L)
    stream.writeUInt64Shifted( Array[Long](1L, -32767L, -9223372036854775808L) )
    stream.writeUInt64Shifted( 9223372036854775807L )
    stream.close

    val stream2 =  new RAF(fileHead + "/temp.bin", "r")
    val result2 = stream2.readUInt64Shifted(3)
    assert(result2(0) == 0L )
    assert(result2(1) == 1L )
    assert(result2(2) == -32767L )
    assert(stream2.readUInt64Shifted ==  -9223372036854775808L)
    assert(stream2.readUInt64Shifted == 9223372036854775807L)
    stream2.close
  }

}
