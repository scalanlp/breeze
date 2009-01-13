package scalanlp.data;

trait Labeled[+L] {
  def label : L;
}
