package scalanlp.data;

/**
* For any class that has one or more labels. Note that Label is a
* kind of Multilabeled.
* 
* @author dlwh
*/
trait Multilabeled[L] {
  def labels : Set[L];
}
