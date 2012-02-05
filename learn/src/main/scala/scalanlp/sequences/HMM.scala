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

object SimpleHMMTest {
  val states = Index(Seq('Start, 'Rainy, 'Sunny))

  val observations = Seq('walk, 'shop, 'clean)

  val transitions = Counter2(
    ('Rainy,'Rainy,0.7),
    ('Rainy,'Sunny,0.3),
    ('Rainy,'Start,0.0),

    ('Sunny,'Start,0.0),
    ('Sunny,'Rainy,0.4),
    ('Sunny,'Sunny,0.6),

    ('Start,'Sunny,0.6),
    ('Start,'Rainy,0.4),
    ('Start,'Start,0.0)
  ).values.map(math.log _)

  val emissions = Counter2(
    ('Rainy,'walk,0.1),
    ('Rainy,'shop,0.4),
    ('Rainy,'clean,0.4),
    ('Sunny,'walk,0.6),
    ('Sunny,'shop,0.3),
    ('Sunny,'clean,0.3),

    ('Start,'walk,0.0),
    ('Start,'shop,0.0),
    ('Start,'clean,0.0)
  ).values.map(math.log _)

  val hmm = new HMM[Symbol,Symbol](states,'Start,transitions,emissions)
  val crf = new CRF(hmm.asCRFModel)
  val cal = crf.calibrate(Seq('walk,'shop,'clean),3)
  println(hmm.enc.decode(cal.marginalAt(0)))

}