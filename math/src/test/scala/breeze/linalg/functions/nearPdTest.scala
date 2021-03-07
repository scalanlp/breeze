package breeze.linalg.functions

import breeze.linalg.DenseMatrix
import breeze.linalg.NearPD
import org.scalatest.FunSuite

class NearPdTest extends FunSuite {

  test("Higham(2002), p.334f - simple example") {
    val matA = DenseMatrix(Array(Array(1d, 1d, 0d),
                                Array(1d, 1d, 1d),
                                Array(0d, 1d, 1d)
                          ):_*
              )

    val expectedOutput = DenseMatrix(Array(Array(1.0, 0.7606899170501267, 0.15729807631287757),
        Array(0.7606899170501267, 1.0, 0.7606899170501272),
        Array(0.15729807631287773, 0.7606899170501272,1.0)
      ):_*
    )

    val testOut = NearPD(matA, corrBool = true, do2eigen = false)
    assert(breeze.linalg.sum(expectedOutput - testOut) < 0.000001)
  }

  test("A longer example, extended from Jens' original") {
    val matA = DenseMatrix(Array(Array(1d,     0.477, 0.644, 0.478, 0.651, 0.826),
                   Array(0.477, 1d,     0.516, 0.233, 0.682, 0.75),
                   Array(0.644, 0.516, 1d,     0.599, 0.581, 0.742),
                   Array(0.478, 0.233, 0.599, 1d,     0.741, 0.8),
                   Array(0.651, 0.682, 0.581, 0.741, 1d,     0.798),
                   Array(0.826, 0.75,  0.742, 0.8,   0.798, 1d)):_*
    )

    val expectedOutput = DenseMatrix(Array(Array(1.0056243002800214, 0.4848765090030602, 0.6429662739874669, 0.48707121693820393, 0.6459161928651831, 0.81377918203511530),
        Array(0.4848765090030602, 1.0110305904579697, 0.5145523264942492, 0.24570371579130584, 0.6748804224899866, 0.732885456104614),
        Array(0.6429662739874669, 0.5145523264942492, 1.0001899943801198, 0.5973327427958711, 0.5819343822330456, 0.7442461297261616),
        Array(0.48707121693820393, 0.2457037157913059, 0.5973327427958711, 1.0146306067261412, 0.7328005250370948, 0.7802895236306836),
        Array(0.6459161928651831, 0.6748804224899867, 0.5819343822330456, 0.7328005250370948, 1.0045952450012263, 0.8090463736460333),
        Array(0.8137791820351152, 0.732885456104614, 0.7442461297261616, 0.7802895236306836, 0.8090463736460333, 1.0265540552332308)
      ):_*
    )

    val testOut = NearPD(matA)
    assert(breeze.linalg.sum(expectedOutput - testOut) < 0.000001)
  }



}
