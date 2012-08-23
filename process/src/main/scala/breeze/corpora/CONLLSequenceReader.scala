package breeze.corpora

import io.Source
import collection.mutable.ArrayBuffer
import breeze.data.{Observation, Example}
import java.io.{File, FileInputStream, InputStream}

/**
 * Reads tag sequences in the conll shared task format. See http://mlcomp.org/faq/domains "Sequence Tagging" for the spec.
 * @author dlwh
 */
object CONLLSequenceReader {
  def readTrain(f: InputStream, name: String = "sequence", splitToken:String =" "):Iterator[Example[IndexedSeq[String],IndexedSeq[IndexedSeq[String]]]] = {
    val source = Source.fromInputStream(f).getLines()
    new Iterator[Example[IndexedSeq[String],IndexedSeq[IndexedSeq[String]]]] {
      def hasNext = source.hasNext
      var index = 0
      def next():Example[IndexedSeq[String],IndexedSeq[IndexedSeq[String]]] = {
        val inputs = new ArrayBuffer[IndexedSeq[String]]()
        val outputs = new ArrayBuffer[String]
        import scala.util.control.Breaks._
        breakable {
          while(source.hasNext) {
            val line = source.next()
            if(line.trim().isEmpty) break

            val split = line.split(splitToken)
            inputs += split.take(split.length -1).toIndexedSeq
            outputs += split.last
          }
        }
        val id = name + "-" + index
        index += 1
        Example(outputs, inputs, id)
      }
    }
  }

  /**
   * This format reads a CONLL file with the last column (i.e. the label) missing. If you have the label,
   * use readTrain, even if you plan on testing with it. Silly, I know.
   * @param f
   * @param name
   * @return
   */
  def readTest(f: InputStream, name: String = "test-sequence", splitToken: String = " "):Iterator[Observation[IndexedSeq[IndexedSeq[String]]]] = {
    val source = Source.fromInputStream(f).getLines()
    new Iterator[Observation[IndexedSeq[IndexedSeq[String]]]] {
      def hasNext = source.hasNext
      var index = 0
      def next() = {
        val inputs = new ArrayBuffer[IndexedSeq[String]]()
        import scala.util.control.Breaks._
        breakable {
          while(source.hasNext) {
            val line = source.next()
            if(line.trim().isEmpty) break

            val split = line.split(splitToken)
            inputs += split
          }
        }
        val id = name + "-" + index
        index += 1
        Observation(id, inputs)
      }
    }
  }

  def main(args: Array[String]) {
    println(readTrain(new FileInputStream(new File(args(0)))).length)
    println(readTest(new FileInputStream(new File(args(1)))).length)


  }

}