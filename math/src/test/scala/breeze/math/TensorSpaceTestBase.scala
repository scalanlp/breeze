package breeze.math

/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.{Prop, Arbitrary}

/**
 *
 * @author dlwh
 */

trait TensorSpaceTestBase[V, I, S] extends FunSuite with Checkers {
  implicit val space: TensorSpace[V, I, S]

  implicit def genTriple: Arbitrary[(V, V, V)]
  implicit def genScalar: Arbitrary[S]

  val TOL = 1E-6

  import space._
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

    check(Prop.forAll{ (trip: (V, V, V)) =>
      val (a, b, c) = trip
      val ab = copy(a)
      ab += b
      ab :*= c
      val ba = copy(a) :* c
      ba :+= (b :* c)
      close(ab, ba, TOL)
    })

//    check(Prop.forAll{ (trip: (V, V, V), s: S) =>
//      val (a, b, _) = trip
//      s == 0 || {
//        val ab = copy(a)
//        ab += b
//        ab /= s
//        val ba = copy(a) / s
//        ba += (b / s)
//        close(ab, ba, TOL)
//      }
//    })
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

  // norm
  test("norm positive homogeneity") {
    check(Prop.forAll{ (trip: (V, V, V), s: S) =>
      val (a, b, c) = trip
      (norm(a * s) - field.norm(s) * norm(a)) <= TOL * norm(a * s)
    })
  }

  test("norm triangle inequality") {
    check(Prop.forAll{ (trip: (V, V, V)) =>
      val (a, b, c) = trip
      ((1.0 - TOL) * norm(a + b) <= norm(b) + norm(a))
    })
  }

  test("norm(v) == 0 iff v == 0") {
    check(Prop.forAll{ (trip: (V, V, V)) =>
      val (a, b, c) = trip
      val z = zeros(a)
      norm(z) == 0.0 && ( (z == a) || norm(a) != 0.0)
    })
  }

  // dot product distributes
  test("dot product distributes") {
    check(Prop.forAll{ (trip: (V, V, V)) =>
      val (a, b, c) = trip
      field.close(field.+(a dot b,a dot c),(a dot (b + c)), 2E-4 )
    })

    check(Prop.forAll{ (trip: (V, V, V), s: S) =>
      val (a, b, c) = trip
      field.close(field.*(a dot b,s),(a dot (b * s)) )
      field.close(field.*(s, a dot b),( (a * s) dot (b)) )
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

    check(Prop.forAll{ (trip: (V, V, V), s: S) =>
      val (a, b, _) = trip
      val ab = copy(a)
      ab := s
      a + s == (a + ab)
    })
  }
  // zip map values
  test("zip map of + is the same as +") {
    check(Prop.forAll{ (trip: (V, V, V)) =>
      val (a, b, _) = trip
      zipMapValues.map(a,b,{field.+(_:S,_:S)}) == (a + b)
    })

  }

  test("Vector element-wise mult distributes over vector addition") {
    check(Prop.forAll{ (trip: (V, V, V)) =>
      val (a, b, c) = trip
      close( (a + b) :* c, (b :* c) + (a :* c), TOL)
    })


//    check(Prop.forAll{ (trip: (V, V, V), s: S) =>
//      val (a, b, _) = trip
//      s == 0 || close( (a + b)/ s, (b / s +a / s), TOL)
//    })

    check(Prop.forAll{ (trip: (V, V, V)) =>
      val (a, b, c) = trip
      val ab = copy(a)
      ab += b
      ab :*= c
      val ba = copy(a) :* c
      ba += (b :* c)
      close(ab, ba, 1e-6)
    })
  }

}

trait DoubleValuedTensorSpaceTestBase[V, I] extends TensorSpaceTestBase[V, I, Double] {
    // normalization
  import space._
  test("normalization sets appropriate norm to 1") {
    check(Prop.forAll{ (trip: (V, V, V), n: Double) =>
      val (a, b, c) = trip
      val nn = n.abs % 100 + 1.0
      val normalized = breeze.linalg.normalize(a, nn)
      val v = breeze.linalg.norm(normalized, nn)
      (v - 1.0).abs <= 1E-5 || norm(normalized) == 0.0
    })


  }

}