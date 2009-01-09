package scalanlp.classify;

import counters._;
import data._;

trait Classifier[L,-T] extends (Observation[T]=>L) {
  def apply(o :Observation[T]) = classify(o);
  def classify(o :Observation[T]) = scores(o).argmax;
  def scores(o: Observation[T]): DoubleCounter[L];
}
