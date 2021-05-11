package breeze.stats.distributions

import org.apache.commons.math3.random.MersenneTwister
import org.scalatest.funsuite.AnyFunSuite

import org.scalatest.matchers.should.Matchers._

import java.io._

class ThreadLocalRandomGeneratorTest extends AnyFunSuite {
  test("ThreadLocalRandomGenerator should be serializable") {
    val generator = new ThreadLocalRandomGenerator(new MersenneTwister())
    serialize(generator)
  }

  test("ThreadLocalRandomGenerator should be serializable after usage") {
    val generator = new ThreadLocalRandomGenerator(new MersenneTwister())
    generator.nextInt()
    serialize(generator)
  }

  test("ThreadLocalRandomGenerator should be deserializable") {
    val generator = new ThreadLocalRandomGenerator(new MersenneTwister())
    val i1 = generator.nextInt()
    val bytes = serialize(generator)
    val deserialized = deserialize(bytes)
    val i2 = deserialized.nextInt()

    i1 should not be i2
  }


  private def serialize(generator: ThreadLocalRandomGenerator): Array[Byte] = {
    val outputStream = new ByteArrayOutputStream(512)
    val out = new ObjectOutputStream(outputStream)
    try {
      out.writeObject(generator)
      outputStream.toByteArray
    } catch {
      case _: IOException => fail("cannot serialize")
    } finally {
      if (out != null) out.close()
    }
  }

  private def deserialize(bytes: Array[Byte]): ThreadLocalRandomGenerator = {
    val in = new ObjectInputStream(new ByteArrayInputStream(bytes))
    try {
      in.readObject().asInstanceOf[ThreadLocalRandomGenerator]
    } catch {
      case _: IOException => fail("cannot deserialize")
    } finally {
      if (in != null) in.close()
    }
  }
}
