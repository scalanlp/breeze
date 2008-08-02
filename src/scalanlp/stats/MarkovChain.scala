package scalanlp.stats;
import Rand._;
import Math._;

/**
* Provides methods for doing MCMC.
*
* @author(dlwh)
*/
object MarkovChain {
  /**
  * Given an initial state and an arbitrary Markov transition, return a sampler 
  * for doing mcmc
  */
  def apply[T]( init : T)(resample : T=>Rand[T]) = new Rand[T] {
    var inner = resample(init);
    def get() = {
      val next = inner.get();
      inner = resample(next);
      next
    }
  }

  /**
  * Performs Metropolis sampling on a random variable. 
  * Note this is not Metropolis-Hastings
  *
  * @param init The initial parameter
  * @param logMeasure the distribution we want to sample from
  * @param proposal the <b>symmetric</b> proposal distribution generator
  *
  */
  def metropolis[T](init : T, logMeasure: T=>Double, proposal : T =>Rand[T]) = {
    MarkovChain(init) { t:T =>
      for(next <- proposal(t);
          newLL = logMeasure(next);
          oldLL = logMeasure(t);
          a = min(1,exp(newLL - oldLL));
          u <- uniform)
        yield if(u < a) next else t;
    }
  }

  /**
  * Performs Metropolis-Hastings sampling on a random variable. 
  *
  * @param init The initial parameter
  * @param logMeasure the distribution we want to sample from
  * @param proposal the proposal distribution generator
  *
  */
  def metropolisHastings[T](init : T, logMeasure: T=>Double, proposal : T =>Distribution[T]) = {
    MarkovChain(init) { t:T =>
      val prop = proposal(t);
      for(next <- prop;
          newLL = logMeasure(next);
          newP = prop.unnormalizedLogProbabilityOf(next);
          oldLL = logMeasure(t);
          oldP = prop.unnormalizedLogProbabilityOf(t);
          a = min(1,exp(newLL + newP - oldLL - oldP));
          u <- uniform)
        yield if(u < a) next else t;
    }
  }

   
  /**
   * Creates a slice sampler for a function. logMeasure should be an (unnormalized) log pdf.
   * @param logMeasure an unnormalized probability measure
   * @param init guess
   * @return a slice sampler
   */
  def slice(init : Double,logMeasure : Double=>Double) = new Rand[Double] {
    val WINDOW = init/100 + 0.1;
    val M = 1;
    var last = init;
    def get() = {
      // How bad are we willing to tolerate?
      val prop = log(uniform.get) + logMeasure(last);
      val u = uniform.get;
      // Find the boundaries
      var left = last - WINDOW * u;
      var right = last + WINDOW * u;
      while( prop < logMeasure(left)) {
        left = left - WINDOW;
      }
      while( prop < logMeasure(right)) {
        right = right + WINDOW;
      }
      var happy = false;
      var next = Double.NaN;
      val gen = for(x <- uniform) yield left + x * (right-left);
      while(!happy) {
        next = gen.get;
        if(prop < logMeasure(next)) {
          happy = true;
        } else if(next <= last) { //close the window
          left = next;
        } else {
          right = next;
        }
      }
      last = next;
      next;
    }
  }
}
