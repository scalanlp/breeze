package scalanlp.stats;
import scalanlp.counters.Counters._;

/**
 * Particle Filter for Sequential Monte Carlo based on Chopin (2002). Given an inference procedure,
 * a Markov Kernel, an identify method, and a partition of the data, returns a list of weighted particles.
 * 
 * For simplicity, the Markov Kernel can only depend on the current particles and their weights.
 *
 * The identify method is requried to ensure the particles aren't variants of the same particle.
 * i.e. in a DirMult model that topics 1 and 2 are topics 2 and 1 in another particle.
 * The identify method should enforce a constraint like (p_1 > p_2 > ,,, p_K)
 * 
 * Still in progress!
 *
 * @author(dlwh)
 *
 */
class ParticleFilter[T](val particles : Seq[(T,Double)], transition : Seq[(T,Double)] => T => Rand[T],
  identify: T => T, numParticles : Int) extends Rand[T] {

  def this(initialParticle : Distribution[T], transition : Seq[(T,Double)] => T => Rand[T],
    identify : T=>T, numParticles : Int) =  {
    this(Array.fromFunction(x => initialParticle.get)(numParticles)
        .map(identify).map(p => (p,initialParticle.unnormalizedLogProbabilityOf(p)))
      ,transition,identify,numParticles);    
  }

  private val m = Multinomial(  aggregate(particles).normalized);

  def get() = m.get();
  def resample(f : T => Double) = {
    // init (particle,weight)
      val reweightedParts  = particles.map( p => (p._1,p._2 * f(p._1)))
      // resample and perturb
      val mult = Multinomial(aggregate(reweightedParts.elements).normalized);
      val parts = mult.flatMap(transition(reweightedParts)).map(identify);
      new ParticleFilter(parts.map( x => (x,f(x))).sample(numParticles), transition, identify, numParticles);
  }

}

object ParticleFilter {
  def defaultTransition[T](particles : Seq[(T,Double)]) = {
    
    
    
  }
}
