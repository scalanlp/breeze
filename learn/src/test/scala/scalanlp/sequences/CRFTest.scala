package scalanlp.sequences

import org.scalatest._;
import org.scalatest.junit._;
import org.scalatest.prop._;
import org.scalacheck._;
import org.junit.runner.RunWith
import scalanlp.util.Index._
import scalala.tensor.Counter2._
import scalanlp.util.Index
import scalala.tensor.Counter2


/**
 * 
 * @author dlwh
 */

@RunWith(classOf[JUnitRunner])
class CRFTest extends FunSuite {
  test("wikipedia basic hmm test") {
    val states = Index(Seq('Start, 'Rainy, 'Sunny))

    val transitions = Counter2(
      ('Rainy,'Rainy,0.7),
      ('Rainy,'Sunny,0.3),
      ('Rainy,'Start,0.0),

      ('Sunny,'Start,0.0),
      ('Sunny,'Rainy,0.3),
      ('Sunny,'Sunny,0.7),

      ('Start,'Sunny,0.5),
      ('Start,'Rainy,0.5),
      ('Start,'Start,0.0)
    ).values.map(math.log _)

    val emissions = Counter2(
      ('Rainy,'U,0.9),
      ('Rainy,'N,0.1),

      ('Sunny,'U,0.2),
      ('Sunny,'N,0.8),

      ('Start,'U,0.0),
      ('Start,'N,0.0)
    ).values.map(math.log _)

    val hmm = new HMM[Symbol,Symbol](states,'Start,transitions,emissions)
    val crf = new CRF(hmm.asCRFModel)
    val cal = crf.calibrate(Seq('U,'U,'N,'U,'U),5)
    val marginals = (0 until 5).map(cal.marginalAt(_)).map(hmm.asCRFModel.decode _)
    assert( (marginals(0)('Rainy) - math.log(0.8673)).abs < 1E-4)
    assert( (marginals(1)('Rainy) - math.log(0.8204)).abs < 1E-4)
    assert( (marginals(2)('Rainy) - math.log(0.3075)).abs < 1E-4)
    assert( (marginals(3)('Rainy) - math.log(0.8204)).abs < 1E-4)
    assert( (marginals(4)('Rainy) - math.log(0.8673)).abs < 1E-4)
  }


}