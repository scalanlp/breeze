package scalanlp.stats.sampling

trait Process[T] extends Rand[T] {
  /** Force the "next" draw to be x, and return a new process. */
  def observe(x: T): Process[T];
  
  /** Draw a sample and the next step of the process along with it.*/
  def step(): (T,Process[T]) = {
    val x = get;
    (x,observe(x));
  }
}
