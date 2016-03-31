package breeze.util

import java.io.File

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FileUtilTest extends FunSuite {

  val testPath = System.getProperty("java.io.tmpdir")

  test("Is equal to string path value") {
    assert(file"$testPath" === new File(testPath))
  }

  test("Complex interpolation is working") {
    val postfix = "test.csv"
    assert(file"$testPath/${1 + 1}/$postfix" === new File(s"$testPath/2/$postfix"))
  }
}
