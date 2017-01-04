package breeze.math

import breeze.linalg._
import breeze.numerics.pow
import org.scalacheck.{Gen, Prop, Arbitrary}

/**
 * breeze
 * 8/5/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 *
 */
trait OptimizationSpaceTest[M,V,S] extends TensorSpaceTestBase[V,Int,S] {
  override implicit val space: MutableOptimizationSpace[M,V,S]

  import space._

  implicit def genTripleM: Arbitrary[(M,M,M)]

  test("Addition is Associative - Matrix") {
    check(Prop.forAll{ (trip: (M, M, M)) =>
      val (a, b, c) = trip
      closeM((a + b) + c, a + (b + c), TOL)
    })

    check(Prop.forAll{ (trip: (M, M, M)) =>
      val (a, b, c) = trip
      val ab = a + b
      val bc = b + c
      ab += c
      bc += a
      closeM(ab, bc, TOL)
    })
  }

  test("Addition Commutes - Matrix") {
    check(Prop.forAll{ (trip: (M, M, M)) =>
      val (a, b, _) = trip
      closeM(a + b, b + a, TOL)
    })

    check(Prop.forAll{ (trip: (M, M, M)) =>
      val (a, b, _) = trip
      val ab = copyM(a)
      ab += b
      val ba = copyM(b)
      ba += a
      closeM(ab, ba, TOL)
    })
  }

  test("Zero is Zero - Matrix") {
    check(Prop.forAll{ (trip: (M, M, M)) =>
      val (a, b, c) = trip
      val z = zeroLikeM(a)
      closeM(a +:+ z, a, TOL)
    })
    check(Prop.forAll{ (trip: (M, M, M)) =>
      val (a, b, _) = trip
      val ab = copyM(a)
      val z = zeroLikeM(a)
      ab :+= z
      closeM(a, ab, TOL)
    })
  }

  test("a - a == 0 - Matrix") {
    check(Prop.forAll{ (trip: (M, M, M)) =>
      val (a, b, c) = trip
      val z = zeroLikeM(a)
      val ama: M = a - a
      closeM(ama, z, TOL)
    })

    check(Prop.forAll{ (trip: (M, M, M)) =>
      val (a, b, _) = trip
      val z = zeroLikeM(a)
      a -= a
      closeM(a, z, TOL)
    })

    check(Prop.forAll{ (trip: (M, M, M)) =>
      val (a, b, _) = trip
      val z = zeroLikeM(a)
      a :-= a
      closeM(a, z, TOL)
    })

    check(Prop.forAll{ (trip: (M, M, M)) =>
      val (a, b, _) = trip
      val z = zeroLikeM(a)
      val ab = a -:- b
      a -= b
      closeM(a, ab, TOL)
    })
  }

  test("Scalar mult distributes over vector addition - Matrix") {
    check(Prop.forAll{ (trip: (M, M, M), s: S) =>
      val (a, b, _) = trip
      closeM( (a + b) *:* s, (b *:* s) + (a *:* s), TOL)
    })

    //    check(Prop.forAll{ (trip: (M, M, M), s: S) =>
    //      val (a, b, _) = trip
    //      s == 0 || close( (a + b)/ s, (b / s +a / s), TOL)
    //    })

    check(Prop.forAll{ (trip: (M, M, M), s: S) =>
      val (a, b, _) = trip
      val ab = copyM(a)
      ab += b
      ab *= s
      val ba = copyM(a) *:* s
      ba += (b *:* s)
      closeM(ab, ba, TOL)
    })
  }

  test("daxpy is consistent - Matrix") {
    check(Prop.forAll{ (trip: (M, M, M), s: S) =>
      val (a, b, _) = trip
      val ac = copyM(a)
      val prod = a + (b *:* s)
      breeze.linalg.axpy(s, b, ac)
      closeM( prod, ac, TOL)
    })
  }


  test("Scalar mult distributes over field addition - Matrix") {
    check(Prop.forAll{ (trip: (M, M, M), s: S, t: S) =>
      val (a, _, _) = trip
      closeM( (a) *:* scalars.+(s,t), (a *:* s) + (a *:* t), TOLM)
    })

    check(Prop.forAll{ (trip: (M, M, M), s: S, t: S) =>
      val (a, _, _) = trip
      val ab = copyM(a)
      ab *= s
      ab += (a *:* t)
      val ba = copyM(a)
      ba *= scalars.+(s,t)
      closeM(ab, ba, TOLM)
    })
  }

  test("Compatibility of scalar multiplication with field multiplication - Matrix") {
    check(Prop.forAll{ (trip: (M, M, M), s: S, t: S) =>
      val (a, _, _) = trip
      closeM( (a) *:* scalars.*(s,t), a *:* s *:* t, TOL)
    })

    check(Prop.forAll{ (trip: (M, M, M), s: S, t: S) =>
      val (a, _, _) = trip
      val ab = copyM(a)
      ab *= s
      ab *= t
      val ba = copyM(a)
      ba *= scalars.*(s, t)
      closeM(ab, ba, TOL)
    })

    //     check(Prop.forAll{ (trip: (M, M, M), s: S, t: S) =>
    //       val (a, _, _) = trip
    //       s == scalars.zero || t == scalars.zero || {
    //       val ab = copy(a)
    //         ab /= s
    //         ab /= t
    //         val ba = copy(a)
    //         ba /= scalars.*(s, t)
    //         close(ab, ba, TOL)
    //       }
    //     })
  }

  // op set
  test("op set works - Matrix") {
    check(Prop.forAll{ (trip: (M, M, M)) =>
      val (a, b, _) = trip
      val ab = copyM(a)
      ab := b
      a + b == (a + ab)
    } )
  }


  test("1 is 1 - Matrix") {
    check(Prop.forAll{ (trip: (M, M, M)) =>
      val (a, b, c) = trip
      closeM(a *:* scalars.one, a, TOL)
    })

    check(Prop.forAll{ (trip: (M, M, M)) =>
      val (a, b, _) = trip
      val ab = copyM(a)
      ab *= scalars.one
      closeM(a, ab, TOL)
    })
  }

  // norm
  val TOLM = 1E-2
  test("norm positive homogeneity - Matrix") {
    check(Prop.forAll{ (trip: (M,M,M), s: S) =>
      val (a, b, c) = trip
      norm(a * s) - norm(s) * norm(a) <= TOL * norm(a * s)
    })
  }

  test("norm triangle inequality - Matrix") {
    check(Prop.forAll{ (trip: (M,M,M)) =>
      val (a, b, c) = trip
      ((1.0 - TOL) * norm(a + b) <= norm(b) + norm(a))
    })
  }

  test("norm(v) == 0 iff v == 0 - Matrix") {
    check(Prop.forAll{ (trip: (M,M,M)) =>
      val (a, b, c) = trip
      val z = zeroLikeM(a)
      norm(z) == 0.0 && ( closeM(z, a, TOL) || norm(a) != 0.0)
    })
  }

  // dot product distributes
  test("dot product distributes - Matrix") {
    check(Prop.forAll{ (trip: (M, M, M)) =>
      val (a, b, c) = trip
      val res = scalars.close(scalars.+(a dot b,a dot c),(a dot (b + c)), 1E-3 )
      if(!res)
        println(scalars.+(a dot b,a dot c) + " " + (a dot (b + c)))
      res
    })

    check(Prop.forAll{ (trip: (M, M, M), s: S) =>
      val (a, b, c) = trip
      scalars.close(scalars.*(a dot b,s),(a dot (b *:* s)) )
      scalars.close(scalars.*(s, a dot b),( (a *:* s) dot (b)) )
    })
  }

  // zip map values
  test("zip map of + is the same as + - Matrix") {
    check(Prop.forAll{ (trip: (M, M, M)) =>
      val (a, b, _) = trip
      zipMapValuesM.map(a,b,{scalars.+(_:S,_:S)}) == (a + b)
    })

  }

  test("Elementwise mult of vectors distributes over vector addition - Matrix") {
    check(Prop.forAll{ (trip: (M, M, M)) =>
      val (a, b, c) = trip
      val ab = copyM(a)
      ab += b
      ab :*= c
      val ba = copyM(a) *:* c
      ba :+= (b *:* c)
      closeM(ab, ba, TOL)
    })
  }

  test("Vector element-wise mult distributes over vector addition - Matrix") {
    check(Prop.forAll{ (trip: (M, M, M)) =>
      val (a, b, c) = trip
      closeM( (a + b) *:* c, (b *:* c) + (a *:* c), TOL)
    })


    //    check(Prop.forAll{ (trip: (M, M, M), s: S) =>
    //      val (a, b, _) = trip
    //      s == 0 || close( (a + b)/ s, (b / s +a / s), TOL)
    //    })

    check(Prop.forAll{ (trip: (M, M, M)) =>
      val (a, b, c) = trip
      val ab = copyM(a)
      ab += b
      ab :*= c
      val ba = copyM(a) *:* c
      ba += (b *:* c)
      closeM(ab, ba, TOL)
    })
  }
}

class DenseOptimizationSpaceTest_Double extends DenseVectorPropertyTestBase[Double] with OptimizationSpaceTest[DenseMatrix[Double], DenseVector[Double], Double] {
  override implicit val space: MutableOptimizationSpace[DenseMatrix[Double], DenseVector[Double], Double] =
    MutableOptimizationSpace.DenseDoubleOptimizationSpace.denseDoubleOptSpace


  val N = 30
  override implicit def genTripleM: Arbitrary[(DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double])] = {
    Arbitrary {
      for{x <- RandomInstanceSupport.genReasonableDouble.arbitrary
          y <- RandomInstanceSupport.genReasonableDouble.arbitrary
          z <- RandomInstanceSupport.genReasonableDouble.arbitrary
      } yield {
        (DenseMatrix.fill(N,N)(math.random * x),
          DenseMatrix.fill(N,N)(math.random * y),
          DenseMatrix.fill(N,N)(math.random * z))
      }
    }
  }

  def genScalar: Arbitrary[Double] = RandomInstanceSupport.genReasonableDouble
}

class SparseOptimizationSpaceTest_Double extends SparseVectorPropertyTestBase[Double] with OptimizationSpaceTest[CSCMatrix[Double],SparseVector[Double],Double] {
  override implicit val space: MutableOptimizationSpace[CSCMatrix[Double], SparseVector[Double], Double] =
    MutableOptimizationSpace.SparseDoubleOptimizationSpace.sparseDoubleOptSpace

  // TODO: generate arbitrarily dimensioned matrices
  val N = 30
  val M = 30


  override val TOLM = 1E-2

  def genScalar: Arbitrary[Double] = RandomInstanceSupport.genReasonableDouble

  val arbColIndex = Arbitrary(Gen.choose[Int](0,N-1))
  val arbRowIndex = Arbitrary(Gen.choose[Int](0,M-1))
  val genAS = Gen.chooseNum(0, pow(N, 2))
  implicit val arbEntry = Arbitrary.arbTuple3[Int,Int,Double](arbRowIndex, arbColIndex, genScalar)
  implicit val arbVals = Arbitrary(genAS flatMap( activeSize => Gen.listOfN[(Int,Int,Double)](activeSize,Arbitrary.arbitrary[(Int,Int,Double)])))
  def addToBuilder(bldr: CSCMatrix.Builder[Double],v: (Int,Int,Double)) = bldr.add(v._1,v._2,v._3)
  override implicit def genTripleM: Arbitrary[(CSCMatrix[Double], CSCMatrix[Double], CSCMatrix[Double])] = {
    Arbitrary {
      val xb = new CSCMatrix.Builder[Double](N, N)
      val yb = new CSCMatrix.Builder[Double](N, N)
      val zb = new CSCMatrix.Builder[Double](N, N)
      for {
        xvs <- Arbitrary.arbitrary[List[(Int, Int, Double)]]
        yvs <- Arbitrary.arbitrary[List[(Int, Int, Double)]]
        zvs <- Arbitrary.arbitrary[List[(Int, Int, Double)]]
      } yield ( {
        xvs.foreach(v => addToBuilder(xb, v))
        xb.result()
      }, {
        yvs.foreach(v => addToBuilder(yb, v))
        yb.result()
      }, {
        zvs.foreach(v => addToBuilder(zb, v))
        zb.result()
      })

    }
  }
}
