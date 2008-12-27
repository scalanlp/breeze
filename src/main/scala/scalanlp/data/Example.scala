package scalanlp.data;

import scala.collection.Map;
import counters._;

/**
* Represents a single example from a collection of data. Intentionally overly general.
*
* @author dlwh
*/
trait Example[L,+T] extends MultilabeledExample[L,T] with Labeled[L] {outer=>
  def id : String;
  def label: L

  // How do we marry real-valued features and categorial features? By being overly general.
  override def map[U](f: T=>U) = new Example[L,U] {
    def label = outer.label;
    def id = outer.id;
    def features = f(outer.features);
  }

  override def flatMap[U](f: T=>U) = map(f);

  override def toString = {
    "Example { ids =" + id + ", label = " + label + ", features = " + features + "}"; 
  }
}
