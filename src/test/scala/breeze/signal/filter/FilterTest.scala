package breeze.signal

import org.scalatest.FunSuite
import breeze.signal._
import breeze.linalg.{norm, DenseVector}

/**
 * @author ktakagaki
 * @date 2/11/14.
 */
class FilterTest  extends FunSuite {

  test("BP filtering tested against output from scipy.signal.firwin/ifilter (0.13.2-1)") {

    val testNormThreshold = 1.0E-6
    val spFirwin1 = DenseVector(  1.40718797E-02,  -7.94274374E-18,  -1.16636001E-01,
      -1.75719493E-01,   1.37640418E-01,   4.04633024E-01, 1.37640418E-01,  -1.75719493E-01,  -1.16636001E-01,
      -7.94274374E-18,   1.40718797E-02 )

    val testSignal = DenseVector(-1.27168894, -1.48442932, -0.5481519 , -0.53087595,  1.55426056,
    1.30248503,  0.06143029,  0.69251918,  0.68418296,  0.00401513,  -0.52352089,  0.07456588, -0.68864501,  1.24677189,  0.48492816,
    -1.10078968,  0.41359423,  0.13732803, -0.05686372, -0.17045011)
    val spTestSignalFiltered1 = DenseVector( -0.01789505, -0.02088871,  0.14061118,  0.38912801,  0.17161298,
      -0.54231618, -0.93826582, -0.69101276,  0.1064299 ,  0.91305795,
      0.67988086, -0.05227671,  0.01563969,  0.30337291, -0.00920251,
      -0.42358677, -0.48613482,  0.04960337,  0.6784219 ,  0.23758627  )


    val firwin1 = designFilterFirwin[Double](11, DenseVector(0.25, 0.5), zeroPass = false )
    assert( norm( spFirwin1 - firwin1.kernel) < testNormThreshold, "generated kernel is incorrect!" )

    val filtered1 = filterBP( testSignal, (0.25, 0.5), 11, overhang = OptOverhang.PreserveLength, padding = OptPadding.Zero )
    assert( norm( filtered1(0 to -6) - spTestSignalFiltered1(5 to -1)) < testNormThreshold, "filtered result is incorrect!" )


  }

}