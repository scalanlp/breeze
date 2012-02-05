package scalanlp.sequences

import collection.immutable.BitSet
import scalanlp.util.{Encoder, Index}
import scalala.tensor.dense.DenseMatrix
import scalala.library.Library
import scalala.tensor.{Counter2, Tensor2, ::}

/**
 * 
 * @author dlwh
 */

case class HMM[L,W](states: Index[L],
               startSymbol: L,
               transitions: Tensor2[L,L,Double], emissions: Tensor2[L,W,Double]) { hmm =>

  val enc = Encoder.fromIndex(states)
  val encodedTransitions:DenseMatrix[Double] = enc.encode(transitions)
  val encodedEmissions = {
    enc.tabulateArray(emissions(_,::))
  }

  def asCRFModel = new CRFModel[L,Seq[W]] {
    val index = states

    def validSymbols(pos: Int, w: Seq[W]) = BitSet() ++ (0 until states.size)

    val startSymbol = hmm.startSymbol
    val start = states(startSymbol)

    def score(pos: Int, w: Seq[W], l: Int, ln: Int) = {
      encodedTransitions(l,ln) + encodedEmissions(ln)(w(pos))
    }
  }

}

