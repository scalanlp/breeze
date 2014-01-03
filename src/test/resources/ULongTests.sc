import nounous.io.RandomAccessFileBE

/**
 * Created by Kenta on 12/9/13.
 */

(0xFFL).toBinaryString
(1L ).toBinaryString
(-1L).toBinaryString
(8L).toBinaryString
(1L << 3)
val maxLong =          9223372036854775807L
maxLong.toBinaryString
//val minLong: Long = 0L−- maxLong // 9223372036854775807L
//minLong.toBinaryString
val fileHead = "V:/docs/bb/nounous/src/test/scala/nounous/io"
var stream: RandomAccessFileBE =
  new RandomAccessFileBE(fileHead + "/UInt64BE.bin", "r")
stream.readUInt64Shifted()
stream.readUInt64Shifted()
stream.readUInt64Shifted()
stream.readUInt64Shifted()
stream.readUInt64Shifted()
