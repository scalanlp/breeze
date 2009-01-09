package scalanlp.data;

import scala.collection.Map;

/**
* Represents a single example from a collection of data. Intentionally
* overly general.
*
* @author dlwh
*/
trait MultilabeledExample[L,+T] extends Observation[T] with Multilabeled[L] { outer =>
  def id : String;
  // How do we marry real-valued features and categorial features? By being overly general.
  override def toString = {
    "Example { ids =" + id + ", labels = " + labels + ", features = " + features + "}"; 
  }

  // How do we marry real-valued features and categorial features? By being overly general.
  override def map[U](f: T=>U) = new MultilabeledExample[L,U] {
    def labels = outer.labels;
    def id = outer.id;
    def features = f(outer.features);
  }

  override def flatMap[U](f: T=>U) = map(f);

}
