package breeze.gymnastics

import org.scalatest.funsuite.AnyFunSuite


class NotGivenTest extends AnyFunSuite {
  test("simple not given") {
    implicitly[NotGiven[String]]
  }

  test("subtype") {
    assertTypeError(
      """
        |class A
        |class B extends A
        |object Foo {
        |  implicitly[NotGiven[B <:< A]]
        |}
        |""".stripMargin
    )
  }
}
