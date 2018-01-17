package breeze.io

import org.scalatest.FunSuite
import breeze.io.ByteConverterBigEndian._
import spire.math.ULong

/**
 * Created by Kenta on 12/10/13.
 */
class ByteConverterBigEndian$Test extends FunSuite {

  test("UInt8") {
    var valueS: Short = 1
    var ba = uInt8ToByte(valueS)
    assert(ba == 0x1)
    assert(valueS == byteToUInt8(ba))

    valueS = 127
    ba = uInt8ToByte(valueS)
    assert(ba == 0x7F)
    assert(valueS == byteToUInt8(ba))

    valueS = 128
    ba = uInt8ToByte(valueS)
    assert(ba == -128)
    assert(valueS.toShort == byteToUInt8(ba))

    valueS = 255
    ba = uInt8ToByte(valueS)
    assert(ba == -1)
    assert(valueS == byteToUInt8(ba))

  }

  test("Int16") {
    var valueS: Short = -32768
    var ba = int16ToBytes(valueS)
    assert(ba.length == 2)
    assert(valueS == bytesToInt16(ba(0), ba(1)))

    valueS = 0
    ba = int16ToBytes(valueS)
    assert(valueS == bytesToInt16(ba(0), ba(1)))

    valueS = 32767
    ba = int16ToBytes(valueS)
    assert(valueS == bytesToInt16(ba(0), ba(1)))

//    valueS = -32769
//    ba = int16ToBytes( valueS )
//    assert(valueS == bytesToInt16(ba(0), ba(1)))

  }

  test("UInt16") {
    var valueC: Char = 0
    var ba = uInt16ToBytes(valueC)
    assert(ba.length == 2)
    assert(valueC == bytesToUInt16(ba(0), ba(1)))

    valueC = 30000
    ba = uInt16ToBytes(valueC)
    assert(valueC == bytesToUInt16(ba(0), ba(1)))

    valueC = 65535
    ba = uInt16ToBytes(valueC)
    assert(valueC == bytesToUInt16(ba(0), ba(1)))

//    valueC = 65536 //-1
//    ba = uInt16ToBytes( valueC )
//    assert(valueC == bytesToUInt16(ba(0), ba(1)))
  }

  test("Int32") {
    var valueI: Int = -2147483648
    var ba = int32ToBytes(valueI)
    assert(ba.length == 4)
    assert(valueI == bytesToInt32(ba(0), ba(1), ba(2), ba(3)))

    valueI = 0
    ba = int32ToBytes(valueI)
    assert(valueI == bytesToInt32(ba(0), ba(1), ba(2), ba(3)))

    valueI = 2147483647
    ba = int32ToBytes(valueI)
    assert(valueI == bytesToInt32(ba(0), ba(1), ba(2), ba(3)))

//    valueI = 2147483647+1
//    println(valueI)
//    ba = int32ToBytes( valueI )
//    assert(valueI == bytesToInt32(ba(0), ba(1), ba(2), ba(3)))

  }

  test("UInt32") {
    var valueL: Long = 0L
    var ba = uInt32ToBytes(valueL)
    assert(ba.length == 4)
    assert(valueL == bytesToUInt32(ba(0), ba(1), ba(2), ba(3)))

    valueL = 2000000000L
    ba = uInt32ToBytes(valueL)
    assert(valueL == bytesToUInt32(ba(0), ba(1), ba(2), ba(3)))

    valueL = 4294967295L
    ba = uInt32ToBytes(valueL)
    assert(valueL == bytesToUInt32(ba(0), ba(1), ba(2), ba(3)))

//    valueL = -1 //+= 1L
//    ba = uInt32ToBytes( valueL )
//    assert(valueL == bytesToUInt32(ba(0), ba(1), ba(2), ba(3)))

  }

  test("Int64") {
    var valueL: Long = 0L
    var ba = int64ToBytes(valueL)
    assert(ba.length == 8)
    assert(valueL == bytesToInt64(ba(0), ba(1), ba(2), ba(3), ba(4), ba(5), ba(6), ba(7)))

    valueL = -9223372036854775808L
    ba = int64ToBytes(valueL)
    assert(valueL == bytesToInt64(ba(0), ba(1), ba(2), ba(3), ba(4), ba(5), ba(6), ba(7)))

    valueL = 9223372036854775807L
    ba = int64ToBytes(valueL)
    assert(valueL == bytesToInt64(ba(0), ba(1), ba(2), ba(3), ba(4), ba(5), ba(6), ba(7)))
  }

  test("UInt64") {
    var valueL: ULong = ULong(0L)
    var ba = uInt64ToBytes(valueL)
    assert(ba.length == 8)
    assert(valueL == bytesToUInt64(ba(0), ba(1), ba(2), ba(3), ba(4), ba(5), ba(6), ba(7)))

    valueL = ULong(9223372036854775807L)
    ba = uInt64ToBytes(valueL)
    assert(valueL == bytesToUInt64(ba(0), ba(1), ba(2), ba(3), ba(4), ba(5), ba(6), ba(7)))

    valueL = ULong(9223372036854775807L) + ULong(1000)
    ba = uInt64ToBytes(valueL)
    assert(valueL == bytesToUInt64(ba(0), ba(1), ba(2), ba(3), ba(4), ba(5), ba(6), ba(7)))

    valueL = ULong(9223372036854775807L) + ULong(1)
    ba = uInt64ToBytes(valueL)
    assert((-9223372036854775807L - 1L == (bytesToInt64(ba(0), ba(1), ba(2), ba(3), ba(4), ba(5), ba(6), ba(7)))))

    valueL = ULong(4000000000000000000L)
    ba = uInt64ToBytes(valueL)
    assert(valueL == bytesToUInt64(ba(0), ba(1), ba(2), ba(3), ba(4), ba(5), ba(6), ba(7)))
  }

  test("UInt64Shifted") {
    var valueL: Long = 0L
    var ba = uInt64ShiftedToBytes(valueL)
    assert(ba.length == 8)
    assert(valueL == bytesToUInt64Shifted(ba(0), ba(1), ba(2), ba(3), ba(4), ba(5), ba(6), ba(7)))

    valueL = 9223372036854775807L
    ba = uInt64ShiftedToBytes(valueL)
    assert(valueL == bytesToUInt64Shifted(ba(0), ba(1), ba(2), ba(3), ba(4), ba(5), ba(6), ba(7)))

    valueL = 0xffffffffffffffffL
    ba = uInt64ShiftedToBytes(valueL)
    assert(valueL == bytesToUInt64Shifted(ba(0), ba(1), ba(2), ba(3), ba(4), ba(5), ba(6), ba(7)))
  }

}
