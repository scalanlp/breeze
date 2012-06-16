package breeze.sequences

import collection.immutable.BitSet
import breeze.util.{Encoder, Index}
import breeze.linalg.{DenseMatrix, Tensor}

/**
 * 
 * @author dlwh
 */

case class HMM[L,W](states: Index[L],
               startSymbol: L,
               transitions: Tensor[(L,L),Double], emissions: Tensor[(L,W),Double]) { hmm =>

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

