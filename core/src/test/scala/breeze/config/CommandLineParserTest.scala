package breeze.config

import org.scalatest.FunSuite

/**
 * TODO
 *
 * @author dlwh
 **/
class CommandLineParserTest extends FunSuite {
  test("throws exceptions (==exits) if arguments unused") {
    val args = "--int 3 --double 4.0 --boolean --string testtestest --unused yepImUnused".split(" ")
    val ex: UnusedOptionsException[_] = intercept[UnusedOptionsException[_]]{
      CommandLineParser.readIn[DemoConfigurationClass](args, exitOnErrorInsteadOfThrow = false, printHelpOnError = false)
    }
    assert(ex.unused === Set("unused"))
  }

  test("doesn't throw exceptions (==exits) if arguments unused and we ask for no throwing") {
    val args = "--int 3 --double 4.0 --boolean --string testtestest --unused yepImUnused".split(" ")
    CommandLineParser.readIn[DemoConfigurationClass](args, enforceNoUnusedArguments = false, exitOnErrorInsteadOfThrow = false, printHelpOnError = false)
  }
}
