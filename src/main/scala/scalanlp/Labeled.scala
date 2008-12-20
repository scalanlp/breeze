package scalanlp.data;

trait Labeled[L] extends Multilabeled[L] {
  def label : L;
  def labels : Set[L] = Set(label);
}
