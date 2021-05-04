package breeze.gymnastics

import scala.language.experimental.macros

/**
 * trait emulating Scala 3's not given via the shapeless trick. It's a witness that you can't find an implicit of the type
 */
sealed trait NotGiven[+T]{}

object NotGiven {
  private val inst = new NotGiven[Nothing]{}

  implicit def neq[T] : NotGiven[T] = inst
  implicit def neqAmbig1[T](implicit t: T): NotGiven[T] = ???
  implicit def neqAmbig2[T](implicit t: T): NotGiven[T] = ???
}

