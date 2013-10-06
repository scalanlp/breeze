package breeze.math

import org.scalacheck.{Arbitrary, Prop}
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

/**
 * 
 * @author dlwh
 */
trait MutableVectorSpaceTestBase[V, S] extends FunSuite with Checkers {
  implicit val space: MutableVectorSpace[V,  S]
  import space._


  implicit def genTriple: Arbitrary[(V, V, V)]
  implicit def genScalar: Arbitrary[S]

  val TOL = 1E-6

  test("Addition is Associative") {
     check(Prop.forAll{ (trip: (V, V, V)) =>
       val (a, b, c) = trip
       close((a + b) + c, a + (b + c), TOL)
     })

     check(Prop.forAll{ (trip: (V, V, V)) =>
       val (a, b, c) = trip
       val ab = a + b
       val bc = b + c
       ab += c
       bc += a
       close(ab, bc, TOL)
     })
   }

   test("Addition Commutes") {
     check(Prop.forAll{ (trip: (V, V, V)) =>
       val (a, b, _) = trip
       close(a + b, b +a, TOL)
     })

     check(Prop.forAll{ (trip: (V, V, V)) =>
       val (a, b, _) = trip
       val ab = copy(a)
       ab += b
       val ba = copy(b)
       ba += a
       close(ab, ba, TOL)
     })
   }

   test("Zero is Zero") {
     check(Prop.forAll{ (trip: (V, V, V)) =>
       val (a, b, c) = trip
       val z = zeros(a)
       close(a :+ z, a, TOL)
     })

     check(Prop.forAll{ (trip: (V, V, V)) =>
       val (a, b, _) = trip
       val ab = copy(a)
       val z = zeros(a)
       ab :+= z
       close(a, ab, TOL)
     })
   }

   test("a + -a == 0") {
     check(Prop.forAll{ (trip: (V, V, V)) =>
       val (a, b, c) = trip
       val z = zeros(a)
       close(a + -a, z, TOL)
     })

     check(Prop.forAll{ (trip: (V, V, V)) =>
       val (a, b, _) = trip
       val z = zeros(a)
       a += -a
       close(a, z, TOL)
     })

     check(Prop.forAll{ (trip: (V, V, V)) =>
       val (a, b, _) = trip
       val z = zeros(a)
       a :-= a
       close(a, z, TOL)
     })

     check(Prop.forAll{ (trip: (V, V, V)) =>
       val (a, b, _) = trip
       val z = zeros(a)
       val ab = a :- b
       a -= b
       close(a, ab, TOL)
     })
   }

   test("Scalar mult distributes over vector addition") {
     check(Prop.forAll{ (trip: (V, V, V), s: S) =>
       val (a, b, _) = trip
       close( (a + b) * s, (b * s +a * s), TOL)
     })

 //    check(Prop.forAll{ (trip: (V, V, V), s: S) =>
 //      val (a, b, _) = trip
 //      s == 0 || close( (a + b)/ s, (b / s +a / s), TOL)
 //    })

     check(Prop.forAll{ (trip: (V, V, V), s: S) =>
       val (a, b, _) = trip
       val ab = copy(a)
       ab += b
       ab *= s
       val ba = copy(a) * s
       ba += (b * s)
       close(ab, ba, TOL)
     })



   }

  test("daxpy is consistent") {
    check(Prop.forAll{ (trip: (V, V, V), s: S) =>
      val (a, b, _) = trip
      val ac = copy(a)
      val prod = a + b * s
      breeze.linalg.axpy(s, b, ac)
      close( prod, ac, TOL)
    })

  }


   test("Scalar mult distributes over field addition") {
     check(Prop.forAll{ (trip: (V, V, V), s: S, t: S) =>
       val (a, _, _) = trip
       close( (a) * field.+(s,t), (a * s + a * t), 1E-4)
     })

     check(Prop.forAll{ (trip: (V, V, V), s: S, t: S) =>
       val (a, _, _) = trip
       val ab = copy(a)
       ab *= s
       ab += (a * t)
       val ba = copy(a)
       ba *= field.+(s,t)
       close(ab, ba, 1e-4)
     })
   }

   test("Compatibility of scalar multiplication with field multiplication") {
     check(Prop.forAll{ (trip: (V, V, V), s: S, t: S) =>
       val (a, _, _) = trip
       close( (a) * field.*(s,t), a * s * t, TOL)
     })

     check(Prop.forAll{ (trip: (V, V, V), s: S, t: S) =>
       val (a, _, _) = trip
       val ab = copy(a)
       ab *= s
       ab *= t
       val ba = copy(a)
       ba *= field.*(s, t)
       close(ab, ba, TOL)
     })

     check(Prop.forAll{ (trip: (V, V, V), s: S, t: S) =>
       val (a, _, _) = trip
       s == field.zero || t == field.zero || {
       val ab = copy(a)
         ab /= s
         ab /= t
         val ba = copy(a)
         ba /= field.*(s, t)
         close(ab, ba, TOL)
       }
     })
   }

  // op set
  test("op set works") {
    check(Prop.forAll{ (trip: (V, V, V)) =>
      val (a, b, _) = trip
      val ab = copy(a)
      ab := b
      a + b == (a + ab)
    })

  }


  test("1 is 1") {
    check(Prop.forAll{ (trip: (V, V, V)) =>
      val (a, b, c) = trip
      close(a * field.one, a, TOL)
    })

    check(Prop.forAll{ (trip: (V, V, V)) =>
      val (a, b, _) = trip
      val ab = copy(a)
      ab *= field.one
      close(a, ab, TOL)
    })
  }
}
