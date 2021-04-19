package breeze.stats.distributions

import com.sun.xml.internal.ws.encoding.soap.SerializationException
import org.apache.commons.math3.random.MersenneTwister
import org.scalatest.{FunSuite, Matchers}

import java.io._

class ThreadLocalRandomGeneratorTest extends FunSuite with Matchers {
  test("ThreadLocalRandomGeneratorTest should be serializable") {
    val generator = new ThreadLocalRandomGenerator(new MersenneTwister())
    serialize(generator)
  }

  test("ThreadLocalRandomGeneratorTest should be serializable after usage") {
    val generator = new ThreadLocalRandomGenerator(new MersenneTwister())
    generator.nextInt()
    serialize(generator)
  }

  test("ThreadLocalRandomGeneratorTest should be deserializable") {
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
      case ex: IOException => throw new SerializationException(ex)
    } finally {
      if (out != null) out.close()
    }
  }

  private def deserialize(bytes: Array[Byte]): ThreadLocalRandomGenerator = {
    val in = new ObjectInputStream(new ByteArrayInputStream(bytes))
    try {
      in.readObject().asInstanceOf[ThreadLocalRandomGenerator]
    } catch {
      case ex: IOException => throw new SerializationException(ex)
    } finally {
      if (in != null) in.close()
    }
  }
}
