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
   * Provides Markov transition kernels for a few common MCMC techniques
   */
  object Kernels {
    /**
    * Note this is not Metropolis-Hastings
    *
    * @param logMeasure the distribution we want to sample from
    * @param proposal the <b>symmetric</b> proposal distribution generator
    *
    */
    def metropolis[T](proposal : T=> Rand[T])(logMeasure : T=>Double) = { t:T =>
      for(next <- proposal(t);
          newLL = logMeasure(next);
          oldLL = logMeasure(t);
          a = min(1,exp(newLL - oldLL));
          u <- uniform)
        yield if(u < a) next else t;
    }

    /**
    * @param logMeasure the distribution we want to sample from
    * @param proposal the proposal distribution generator
    *
    */
    def metropolisHastings[T](proposal: T =>Distribution[T])(logMeasure: T=>Double)= { t:T =>
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

    /**
    * Creates a slice sampler for a function. logMeasure should be an (unnormalized) log pdf.
    * @param logMeasure an unnormalized probability measure
    * @param init guess
    * @return a slice sampler
    */
    def slice(logMeasure : Double=>Double, valid : Double=>Boolean) = {
      val WINDOW = 2;
      val M = 10;
      (last:Double)=> {
        new Rand[Double] {
          def get() = {
            // How bad are we willing to tolerate?
            val prop = log(uniform.get) + logMeasure(last);
            val u = uniform.get;
            // Find the boundaries
            var left = last - WINDOW * u;
            if(!valid(left))
              left = last;
            var right = left + WINDOW;

            var j : Int =  (uniform.get() * M).asInstanceOf[Int];
            var k  = (M-1)-j;

            while( prop < logMeasure(left) && j > 0 && valid(left-WINDOW)) {
              left = left - WINDOW;
              j -= 1;
            } 

            if(!valid(right)) 
               right = last;
            else 
              while( prop < logMeasure(right) && k > 0 && valid(right+WINDOW)) {
                right = right + WINDOW;
                k-=1;
              }
            var happy = false;
            var next = Double.NaN;
            while(!happy) {
              next = left + uniform.get * (right - left);
              if(prop <= logMeasure(next)) {
                happy = true;
              } else if(next < last) { //close the window
                left = next;
              } else {
                right = next;
              }
            }
            next;
          }
        }
      }
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
  def metropolis[T](init : T, proposal : T =>Rand[T])(logMeasure: T=>Double) = {
    MarkovChain(init)(Kernels.metropolis(proposal)(logMeasure))
  }

  /**
  * Performs Metropolis-Hastings sampling on a random variable. 
  *
  * @param init The initial parameter
  * @param logMeasure the distribution we want to sample from
  * @param proposal the proposal distribution generator
  *
  */
  def metropolisHastings[T](init : T, proposal : T =>Distribution[T])(logMeasure: T=>Double) = {
    MarkovChain(init) { Kernels.metropolisHastings(proposal){logMeasure}};
  }

   
  /**
  * Creates a slice sampler for a function. logMeasure should be an (unnormalized) log pdf.
  * @param logMeasure an unnormalized probability measure
  * @param init guess
  * @return a slice sampler
  */
  def slice(init : Double,logMeasure : Double=>Double, valid : Double=>Boolean) = {
    MarkovChain(init)(Kernels.slice(logMeasure,valid));
  }
}
