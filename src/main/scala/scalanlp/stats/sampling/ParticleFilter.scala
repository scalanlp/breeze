package scalanlp.stats.sampling;

/*
 Copyright 2009 David Hall, Daniel Ramage
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/

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

  def this(initialParticle : Measure[T] with Rand[T], transition : Seq[(T,Double)] => T => Rand[T],
    identify : T=>T, numParticles : Int) =  {
    this(Array.fromFunction(x => initialParticle.get)(numParticles)
        .map(identify).map(p => (p,initialParticle.logApply(p)))
      ,transition,identify,numParticles);    
  }

  private val m = Multinomial(  aggregate(particles));

  def draw() = m.get();
  def resample(f : T => Double) = {
    // init (particle,weight)
      val reweightedParts  = particles.map( p => (p._1,p._2 * f(p._1)))
      // resample and perturb
      val mult = Multinomial(aggregate(reweightedParts.elements));
      val parts = mult.flatMap(transition(reweightedParts)).map(identify);
      new ParticleFilter(parts.map( x => (x,f(x))).sample(numParticles), transition, identify, numParticles);
  }

}

object ParticleFilter {
  def defaultTransition[T](particles : Seq[(T,Double)]) = {
    
    
    
  }
}
