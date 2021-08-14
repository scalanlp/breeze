package breeze.math

import org.scalacheck.Prop.propBoolean
import org.scalacheck._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers

/**
 *
 * @author dlwh
 */
trait MutableModuleTestBase[V, S] extends AnyFunSuite with Checkers {
  implicit val space: MutableModule[V, S]
  import space._

  implicit def genSingle: Arbitrary[V]
  implicit def genTriple: Arbitrary[(V, V, V)]
  implicit def genScalar: Arbitrary[S]

  val TOL = 1E-3

  test("Addition is Associative") {
    check(Prop.forAll { (trip: (V, V, V)) =>
      val (a, b, c) = trip
      close((a + b) + c, a + (b + c), TOL)
    })

    check(Prop.forAll { (trip: (V, V, V)) =>
      val (a, b, c) = trip
      val ab = a + b
      val bc = b + c
      ab += c
      bc += a
      close(ab, bc, TOL)
    })
  }

  test("Addition Commutes") {
    check(Prop.forAll { (trip: (V, V, V)) =>
      val (a, b, _) = trip
      close(a + b, b + a, TOL)
    })

    check(Prop.forAll { (trip: (V, V, V)) =>
      val (a, b, _) = trip
      val ab = copy(a)
      ab += b
      val ba = copy(b)
      ba += a
      close(ab, ba, TOL)
    })
  }

  test("Zero is Zero") {
    check(Prop.forAll { (a: V) =>
      val z = zeroLike(a)
      close(a +:+ z, a, TOL)
    })

    check(Prop.forAll { (a: V) =>
      val ab = copy(a)
      val z = zeroLike(a)
      ab :+= z
      close(a, ab, TOL)
    })
  }

  test("a - a == 0") {
    check(Prop.forAll { (a: V) =>
      val z = zeroLike(a)
      close(a - a, z, TOL)
    })

    check(Prop.forAll { (a: V) =>
      val z = zeroLike(a)
      a -= a
      close(a, z, TOL)
    })

    check(Prop.forAll { (a: V) =>
      val z = zeroLike(a)
      a :-= a
      close(a, z, TOL)
    })

    check(Prop.forAll { (trip: (V, V, V)) =>
      val (a, b, _) = trip
      val z = zeroLike(a)
      val ab = a -:- b
      a -= b
      close(a, ab, TOL)
    })
  }

  test("Scalar mult distributes over vector addition") {
    check(Prop.forAll { (trip: (V, V, V), s: S) =>
      val (a, b, _) = trip
      close((a + b) *:* s, (b *:* s) + (a *:* s), TOL)
    })

    check(Prop.forAll { (trip: (V, V, V), s: S) =>
      val (a, b, _) = trip
      val ab = copy(a)
      ab += b
      ab *= s
      val ba = copy(a) *:* s
      ba += (b *:* s)
      close(ab, ba, TOL)
    })
  }

  test("axpy is consistent") {
    check(Prop.forAll { (trip: (V, V, V), s: S) =>
      val (a, b, _) = trip
      val ac = copy(a)
      val prod = a + (b *:* s)
      breeze.linalg.axpy(s, b, ac)
      close(prod, ac, TOL)
    })

  }

  test("Scalar mult distributes over field addition") {
    check(Prop.forAll { (a: V, s: S, t: S) =>
      val left = a *:* scalars.+(s, t)
      val right = (a *:* s) + (a *:* t)
      s"$left != $right" |: close(left, right, TOL)
    })

    check(Prop.forAll { (a: V, s: S, t: S) =>
      val ab = copy(a)
      ab *= s
      ab += a *:* t
      val ba = copy(a)
      ba *= scalars.+(s, t)
      close(ab, ba, TOL)
    })
  }

  test("Compatibility of scalar multiplication with field multiplication") {
    check(Prop.forAll { (a: V, s: S, t: S) =>
      close(a *:* scalars.*(s, t), a *:* s *:* t, TOL)
    })

    check(Prop.forAll { (a: V, s: S, t: S) =>
      val ab = copy(a)
      ab *= s
      ab *= t
      val ba = copy(a)
      ba *= scalars.*(s, t)
      close(ab, ba, TOL)
    })

//     check(Prop.forAll{ (trip: (V, V, V), s: S, t: S) =>
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

  test("op set works") {
    check(Prop.forAll { (trip: (V, V, V)) =>
      val (a, b, _) = trip
      val ab = copy(a)
      ab := b
      a + b == (a + ab)
    })
  }

  test("1 is 1") {
    check(Prop.forAll { (a: V) =>
      close(a *:* scalars.one, a, TOL)
    })

    check(Prop.forAll { (a: V) =>
      val ab = copy(a)
      ab *= scalars.one
      close(a, ab, TOL)
    })
  }
}
