package breeze.numerics

import org.scalatest.FunSuite
import breeze.numerics.constants._

/**
 * @author ktakagaki
 * @date 3/13/14.
 */
class constantsTest extends FunSuite {

  test("constants test") {
    assert(Database.unit("atomic mass constant energy equivalent") == "J")
    assert(Database.unit(""".*Planck.*""".r).size == 12)
  }

}
