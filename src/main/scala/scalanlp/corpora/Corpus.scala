package scalanlp.corpora;

/**
* Represents a corpus of something.
*
* @author dlwh
*/
trait Corpus[T] {
  def name : String;
  def splits : Map[String,Collection[T]];
  def license:String = "Unknown";
  def author = "Unknown"
}
