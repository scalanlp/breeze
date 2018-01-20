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
import breeze.linalg.{norm, normalize}
import org.scalacheck.Prop

/**
 *
 * @author dlwh
 */
trait TensorSpaceTestBase[V, I, S] extends MutableModuleTestBase[V, S] {
  implicit val space: MutableEnumeratedCoordinateField[V, I, S]

  import space._

  // For computing the comparison point of relative tolerances. returns the max of the norms of the refs
  def tolRef(refs: V*): Double = refs.map(norm(_)).max

  // norm
  test("norm positive homogeneity") {
    check(Prop.forAll { (a: V, s: S) =>
      (norm(a * s) - norm(s) * norm(a)) <= TOL * norm(a * s)
    })
  }

  test("norm triangle inequality") {
    check(Prop.forAll { (trip: (V, V, V)) =>
      val (a, b, c) = trip
      ((1.0 - TOL) * norm(a + b) <= norm(b) + norm(a))
    })
  }

  test("norm(v) == 0 iff v == 0") {
    check(Prop.forAll { (a: V) =>
      val z = zeroLike(a)
      norm(z) == 0.0 && (close(z, a, TOL) || norm(a) != 0.0)
    })
  }

  test("dot product distributes over vector addition") {
    check(Prop.forAll { (trip: (V, V, V)) =>
      val (a, b, c) = trip
      val res = scalars.close(scalars.+(a.dot(b), a.dot(c)), a.dot(b + c), TOL * tolRef(a, b, c))
      if (!res)
        println(scalars.+(a.dot(b), a.dot(c)) + " " + (a.dot(b + c)))
      res
    })
  }

  test("dot product associates with scalar multiplication") {
    check(Prop.forAll { (trip: (V, V, V), s: S) =>
      val (a, b, _) = trip
      val tol = TOL * tolRef(a, b)
      scalars.close(scalars.*(a.dot(b), s), a.dot(b *:* s), tol) &&
      scalars.close(scalars.*(s, a.dot(b)), (a *:* s).dot(b), tol)
    })
  }

  // zip map values
  test("zip map of + is the same as +") {
    check(Prop.forAll { (trip: (V, V, V)) =>
      val (a, b, _) = trip
      zipMapValues.map(a, b, { scalars.+(_: S, _: S) }) == (a + b)
    })

  }

  test("Elementwise mult of vectors distributes over vector addition") {
    check(Prop.forAll { (trip: (V, V, V)) =>
      val (a, b, c) = trip
      val ab = copy(a)
      ab += b
      ab :*= c
      val ba = copy(a) *:* c
      ba :+= (b *:* c)
      close(ab, ba, TOL)
    })
  }

  test("Vector element-wise mult distributes over vector addition") {
    check(Prop.forAll { (trip: (V, V, V)) =>
      val (a, b, c) = trip
      close((a + b) *:* c, (b *:* c) + (a *:* c), TOL * tolRef(a, b, c))
    })

//    check(Prop.forAll{ (trip: (V, V, V), s: S) =>
//      val (a, b, _) = trip
//      s == 0 || close( (a + b)/ s, (b / s +a / s), TOL)
//    })

    check(Prop.forAll { (trip: (V, V, V)) =>
      val (a, b, c) = trip
      val ab = copy(a)
      ab += b
      ab :*= c
      val ba = copy(a) *:* c
      ba += (b *:* c)
      close(ab, ba, TOL * tolRef(a, b, c))
    })
  }
}

trait DoubleValuedTensorSpaceTestBase[V, I] extends TensorSpaceTestBase[V, I, Double] {
  import space._

  // normalization
  test("normalization sets appropriate norm to 1") {
    check(Prop.forAll { (a: V, n: Double) =>
      val nn = n.abs % 10 + 1.0
      val normalized = breeze.linalg.normalize(a, nn)
      val v = breeze.linalg.norm(normalized, nn)
      (v - 1.0).abs <= TOL || norm(normalized) == 0.0
    })
  }

  test("normalize") {
    check(Prop.forAll { (v: V) =>
      val aNorm = normalize(v)
      (norm(aNorm) - 1.0) <= TOL || norm(aNorm) == 0.0
    })
  }

}
