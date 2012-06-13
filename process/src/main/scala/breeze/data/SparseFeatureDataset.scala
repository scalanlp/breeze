package breeze.data

import breeze.linalg.SparseVector
import io.Source
import collection.mutable.ArrayBuffer
import breeze.serialization.TextSerialization.Readable
import breeze.serialization.{SerializationFormat, TextSerialization}
import breeze.io.TextReader

/**
 * Dataset of the form <label> [featureIndex:featureValue*]
 * @author dlwh
 */
trait SparseFeatureDataset[Output] {
  def examples: Seq[Example[Output,SparseVector[Double]]]
}

object SparseFeatureDataset {
  def fromSource[Output:TextSerialization.Readable](s: Source, name: String=""):SparseFeatureDataset[Output] = {
    val impl = implicitly[TextSerialization.Readable[Output]]

    var maxIndex = 0

    implicit val dataReadable = new Readable[(Output,ArrayBuffer[Int],ArrayBuffer[Double])] {
      def read(source: TextSerialization.Input):(Output,ArrayBuffer[Int],ArrayBuffer[Double]) = {
        val output = impl.read(source)
        val line = source.readLine()

        val indices = new ArrayBuffer[Int]
        val values = new ArrayBuffer[Double]
        var offset = 0
        while(offset < line.length) {
          val nextColon = line.indexOf(':',offset)
          indices += line.substring(offset,nextColon).trim().toInt

          var nextSpace = line.indexOf(' ', nextColon)
          if(nextSpace < 0) nextSpace = line.length
          values += line.substring(nextColon+1,nextSpace).toDouble
          offset = nextSpace + 1
        }

        maxIndex = maxIndex max indices.max
        (output,indices,values)

      }
    }

    val data = {for(line <- s.getLines()) yield dataReadable.read(TextReader.fromString(line))}.toIndexedSeq

    val processed = for( ((output,indices,values),id) <- data.zipWithIndex) yield {
      val sparseVector = new SparseVector(indices.toArray,values.toArray,indices.length,maxIndex)
      Example(output, sparseVector, name + "-" + id)
    }

    new SparseFeatureDataset[Output] {
      def examples = processed
    }


  }
}