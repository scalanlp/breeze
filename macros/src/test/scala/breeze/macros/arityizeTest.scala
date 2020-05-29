package breeze.macros

import org.scalatest.FunSuite

/**
 * TODO
 *
 * @author dlwh
 **/
class arityizeTest extends FunSuite {

  test("Compiles?") {
    @arityize(5)
    class Foo[@arityize.replicate T](@arityize.replicate x: T @arityize.relative(x)) {
      override def toString = List((x: @arityize.replicate)).mkString("(", ", ", ")")
    }

    val foo = new Foo2[Int, String](x1 = 3, x2 = "Foo")
    assert(foo.toString === "(3, Foo)")
  }

  test("Compiles defs?") {
    @arityize(5)
    def foo[@arityize.replicate T](@arityize.replicate x: T @arityize.relative(x)) = x: @arityize.relative(foo)

    assert(foo2(1, 2) === 2)

  }

  test("Snap example") {
    object Whatever {
      def invoke(args: Any*)(a2: Any*) = ""
    }

    @arityize(10)
    class CuKernel[@arityize.replicate T](fn: Any, blockDims: Array[Int]) {
      def apply(workSize1: Int = 1, workSize2: Int = 1, workSize3: Int = 1)(
          @arityize.replicate t: T @arityize.relative(t)
      ): Unit = {
        Whatever.invoke(Array(workSize1, workSize2, workSize3), blockDims, fn)((t: @arityize.replicate))
      }
    }

    new CuKernel2[Int, String](1, Array()).apply()(1, "2")
  }

  test("Snap example 2") {
    @arityize(2)
    class CuKernel[@arityize.replicate T](fn: Any, blockDims: Array[Int]) {
      def apply(workSize1: Int = 1)(@arityize.replicate t: T @arityize.relative(t)): Unit = {
        ???
      }
    }

    @arityize(2)
    def getKernel[@arityize.replicate T](
        name: String,
        blockDims: Array[Int] = Array(32, 1, 1)
    ): (CuKernel[T @arityize.replicate] @arityize.relative(getKernel)) = {
      new (CuKernel[T @arityize.replicate] @arityize.relative(getKernel))(name, blockDims)
    }
  }

  test("Breeze LiteralRow") {

    trait LiteralRow[K, V] {}

    @arityize(6)
    implicit def tuple[V]: LiteralRow[Tuple[V @arityize.repeat] @arityize.relative(tuple), V] =
      new LiteralRow[Tuple[V @arityize.repeat] @arityize.relative(tuple), V] {
        def foreach[X](tup: Tuple[V @arityize.repeat] @arityize.relative(tuple), fn: ((Int, V) => X)) = {
          for ((v, i) <- tup.productIterator.zipWithIndex) {
            fn(i, v.asInstanceOf[V])
          }
        }

        def length(tup: Tuple[V @arityize.repeat] @arityize.relative(tuple)) = __order__
      }
  }

  test("ktakagaki's tuple-y stuff") {
    @arityize(22)
    case class TupleToDenseVector(tuple: Tuple[Any @arityize.repeat] @arityize.relative(TupleToDenseVector))

    @arityize(22)
    implicit def tupleToDenseVector(tuple: Tuple[Any @arityize.repeat] @arityize.relative(tupleToDenseVector)) = {
      new (TupleToDenseVector @arityize.relative(tupleToDenseVector))(tuple)
    }
  }

}
