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

import scala.collection.mutable.ArrayBuffer;
import Rand._;
import math._;

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
  def apply[T]( init : T)(resample : T=>Rand[T]):Process[T] = new Process[T] {
    val inner = resample(init);
    def draw() = {
      val next = inner.draw();
      next
    }
    
    override def observe(x:T) = {
      MarkovChain(x)(resample)
    }
  }

  /**
  * Combinators for creating transition kernels from other kernels or things 
  * that are not quite transition kernels.
  * A kernel is a fn of type T=&lt;Rand[T]
  */
  object Combinators {
    /**
    * Extension methods for kernels
    */
    class RichKernel[T,U](k1: T=>Rand[U]) {
      /**
      * Sequence two transitions together. This is Kliesli arrow composition.
      */
      def >=>[V](k2: U=>Rand[V]) = { (t: T) =>
        k1(t).flatMap(k2);
      }
      /**
      * Sequence two transitions together, in reverse order
      * This is Kliesli arrow composition.
      */
      def <=<[V](k2: V=>Rand[T]) = { (t: V) =>
        k2(t).flatMap(k1);
      }

      /**
      * Promotes a kernel to map over sequence.
      * T=&lt;Rand[U] becomes Seq[T]=&lt;Rand[Seq[U]]
      */
      def promoteSeq: Seq[T]=>Rand[Seq[U]] = { (t: Seq[T]) => promote(t.map(k1)); }
      /**
      * Promotes a kernel to map over collection.
      * T=&lt;Rand[U] becomes Seq[T]=&lt;Rand[Collection[U]]
      */
      def promoteIterable: Iterable[T]=>Rand[Iterable[U]] = { (t: Iterable[T]) => promote(t.map(k1)); }
    }

    implicit def richKernel[T,U](k1: T=>Rand[U]) = new RichKernel(k1);

    /**
    * Extension methods for pseudo-kernels
    * A pseudo-kernel is a method of type (C,T)=&lt;Rand[U], with C being a context type
    */
    class RichPseudoKernel[C,T,U](k1: (C,T)=>Rand[U]) {
      /**
      * Sequence two transitions together. This is Kliesli arrow composition.
      */
      def >=>[V](k2: (C,U)=>Rand[V]) = { (c: C, t: T) =>
        k1(c,t).flatMap(u => k2(c,u) );
      }
      /**
      * Sequence two transitions together, in reverse order
      * This is Kliesli arrow composition.
      */
      def <=<[V](k2: (C,V)=>Rand[T]) = { (c: C, t: V) =>
        k2(c,t).flatMap(t => k1(c,t));
      }

      /**
      * Promotes a kernel to map over sequence.
      * T=&lt;Rand[U] becomes Seq[T]=&lt;Rand[Seq[U]]
      */
      def promoteSeq: (C,Seq[T])=>Rand[Seq[U]] = { (c:C, t: Seq[T]) =>
        promote(t.map(t => k1(c,t) )); 
      }
      /**
      * Promotes a kernel to map over collection.
      * T=&lt;Rand[U] becomes Seq[T]=&lt;Rand[Collection[U]]
      */
      def promoteIterable: (C,Iterable[T])=>Rand[Iterable[U]] = { (c: C, t: Iterable[T]) =>
        promote(t.map( t=>k1(c,t)));
      }
    }

    implicit def richPseudoKernel[C,T,U](k1: (C,T)=>Rand[U]) = new RichPseudoKernel(k1);

    /**
    * Tupleization of nearly-transition kernels to produce a transition kernel for tuples
    */
    def promoteTuple[A,B,C,D](k1: (A,B)=>Rand[C], k2: (C,B)=>Rand[D]) = {(t: (A,B)) =>
      for(c <- k1(t._1,t._2);
          d <- k2(c,t._2))
        yield (c,d);
    }

    /**
    * Tupleization of nearly-transition kernels to produce a kernel for tuples
    */
    def promoteTuple[T1,T2,T3,U1,U2,U3](k1: (T1,T2,T3)=>Rand[U1],
                                        k2: (U1,T2,T3)=>Rand[U2], 
                                        k3: (U1,U2,T3)=>Rand[U3]) = { (t: (T1,T2,T3)) =>
      for(c <- k1(t._1,t._2,t._3);
          d <- k2(c,t._2,t._3);
          e <- k3(c,d,t._3))
        yield (c,d,e);
    }


    /**
    * Tupleization of nearly-transition kernels to produce a kernel for tuples
    */
    def promoteTuple[T1,T2,T3,U1,U2,U3,T4,U4](k1: (T1,T2,T3,T4)=>Rand[U1],
                                        k2: (U1,T2,T3,T4)=>Rand[U2], 
                                        k3: (U1,U2,T3,T4)=>Rand[U3], 
                                        k4: (U1,U2,U3,T4)=>Rand[U4]) = { (t: (T1,T2,T3,T4)) =>
      for(c <- k1(t._1,t._2,t._3,t._4);
          d <- k2(c,t._2,t._3,t._4);
          e <- k3(c,d,t._3,t._4);
          f <- k4(c,d,e,t._4)
        ) yield (c,d,e,f);
    }

    /**
    * Creates a transition kernel over a sequence, given the ability to do one index at a time.
    * Useful for sequence models with a markov assumption.
    */
    def seqKernel[T](trans: (Seq[T],Int)=>Rand[T]) : Seq[T]=>Rand[Seq[T]] = { (seq: Seq[T]) =>
      fromBody {
        val arrbuf = new ArrayBuffer[T];
        arrbuf ++= seq;
        (0 until seq.length).foldLeft(arrbuf) { (buf, i) =>
          buf(i) = trans(buf,i).draw;
          buf
        }
      }
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
    def metropolis[T](proposal : T=> Rand[T])
                     (logMeasure : T=>Double)(implicit rand:RandBasis=Rand) = { t:T =>
      for(next <- proposal(t);
          newLL = logMeasure(next);
          oldLL = logMeasure(t);
          a = min(1,exp(newLL - oldLL));
          u <- rand.uniform) 
        yield if(u < a) next else t;
    }

    /**
    * @param logMeasure the distribution we want to sample from
    * @param proposal the proposal distribution generator
    *
    */
    def metropolisHastings[T](proposal: T =>(Measure[T] with Rand[T]))
                             (logMeasure: T=>Double)(implicit rand:RandBasis=Rand)= { t:T =>
      val prop = proposal(t);
      for(next <- prop;
        newLL = logMeasure(next);
        newP = prop.logApply(next);
        oldLL = logMeasure(t);
        oldP = prop.logApply(t);
        a = min(1,exp(newLL + newP - oldLL - oldP));
        u <- rand.uniform)
      yield if(u < a) next else t;
    }

    /**
    * Creates a slice sampler for a function. logMeasure should be an (unnormalized) log pdf.
    * @param logMeasure an unnormalized probability measure
    * @param init guess
    * @return a slice sampler
    */
    def slice(logMeasure : Double=>Double, valid : Double=>Boolean)(implicit rand: RandBasis = Rand) = {
      val WINDOW = 2;
      val M = 10;
      (last:Double)=> {
        new Rand[Double] {
          def draw() = {
            // How bad are we willing to tolerate?
            val prop = log(rand.uniform.draw) + logMeasure(last);
            val u = rand.uniform.draw;
            // Find the boundaries
            var left = last - WINDOW * u;
            if(!valid(left))
              left = last;
            var right = left + WINDOW;

            var j : Int =  (rand.uniform.draw() * M).asInstanceOf[Int];
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
              next = left + rand.uniform.draw * (right - left);
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
  def metropolisHastings[T](init : T, proposal : T =>(Measure[T] with Rand[T]))(logMeasure: T=>Double) = {
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
