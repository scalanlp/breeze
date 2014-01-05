package breeze.io

import java.io.IOException
import org.scalatest.FunSuite

/**
 * Created with IntelliJ IDEA.
 * User: Kenta
 * Date: 11/4/13
 * Time: 6:22 PM
 * To change this template use File | Settings | File Templates.
 */
class RandomAccessFileTestLittleEndian extends RandomAccessFileTestBigEndian {

  override implicit val bc: ByteConverter = breeze.io.ByteConverterLittleEndian
  override val fileNameAppend = "LE.bin"

}