package scalanlp.counters;
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

import scalala.Scalala._;
import scalala.tensor._;
import scalala.tensor.sparse._;
import scalala.tensor.operators._;
import Tensor1Types._;
import Tensor2Types._;
import scalala.tensor.operators.TensorShapes._;
import scalala.collection._;
import it.unimi.dsi.fastutil.ints.Int2DoubleOpenHashMap;

import scalanlp.util._;

/**
* Provides utilities for creating counters, once you specify a counter
* DoubleCounter[T] that you would like to create, and mkDoubleCounter(set) for
* creating new ones.
* <p/>
* See object Counters for an example implementation.
*
* @author dlwh
*/
trait DoubleCounterFactory {
  /**
  * Abstract type that implementers must define. It must be a 
  * subtype of the AbstractDoubleCounter[T] trait.
  */
  type DoubleCounter[T] <: AbstractDoubleCounter[T] with TensorSelfOp[T,DoubleCounter[T],Shape1Col];

  type PairedDoubleCounter[T,U] <: AbstractPairedDoubleCounter[T,U];

  protected type InternalDoubleCounter[T1,T2] <: DoubleCounter[T2] with PairStatsTracker[T1,T2];

  // Many many implicits to make all the math work.
  implicit def doubleCounterArith[T] = new Tensor1Arith[T,DoubleCounter[T],Tensor1[T],Shape1Col];
  implicit def doubleCounterArithR[T] = new Tensor1Arith[T,DoubleCounter[T],Tensor1[T],Shape1Row];
  implicit def pDoubleCounterArith[T1,T2] = 
    new TensorArith[(T1,T2),PairedDoubleCounter[T1,T2],Tensor2[T1,T2],Shape2];

  implicit def doubleCounterPBuilder[T,U]: TensorProductBuilder[DoubleCounter[T],DoubleCounter[U],PairedDoubleCounter[T,U],Shape1Col,Shape1Row,Shape2] =
    new TensorProductBuilder[DoubleCounter[T],DoubleCounter[U],PairedDoubleCounter[T,U],Shape1Col,Shape1Row,Shape2] {
      def create(t: DoubleCounter[T],t2: DoubleCounter[U]) = mkPairedDoubleCounter[T,U];
      def makeProduct(t: ColTensor1Op[DoubleCounter[T]], t2: RowTensor1Op[DoubleCounter[U]]) = {
        Tensor1OuterMultTensor1[T,U,DoubleCounter[T],DoubleCounter[U],PairedDoubleCounter[T,U]](t,t2);
      }
    }

   implicit def dcMultPdc[T,U]: TensorProductBuilder[DoubleCounter[T],PairedDoubleCounter[T,U],DoubleCounter[U],Shape1Row,Shape2,Shape1Row] = {
    new TensorProductBuilder[DoubleCounter[T],PairedDoubleCounter[T,U],DoubleCounter[U],Shape1Row,Shape2,Shape1Row] {
      def create(t: DoubleCounter[T],t2: PairedDoubleCounter[T,U]) = mkDoubleCounter[U];
      def makeProduct(t: RowTensor1Op[DoubleCounter[T]], t2: Tensor2Op[PairedDoubleCounter[T,U]]) = {
        RowTensor1MultTensor2[T,U,DoubleCounter[U],DoubleCounter[T],PairedDoubleCounter[T,U]](t,t2);
      }
    }
  }

  implicit def pCounterVPBuilder[T,U]: TensorProductBuilder[PairedDoubleCounter[T,U],DoubleCounter[U],DoubleCounter[T],Shape2,Shape1Col,Shape1Col] = {
    new TensorProductBuilder[PairedDoubleCounter[T,U],DoubleCounter[U],DoubleCounter[T],Shape2,Shape1Col,Shape1Col] {
      def create(t: PairedDoubleCounter[T,U], t2: DoubleCounter[U]):DoubleCounter[T] = mkDoubleCounter[T];
      def makeProduct(t: Tensor2Op[PairedDoubleCounter[T,U]], t2: ColTensor1Op[DoubleCounter[U]]) = {
        Tensor2MultColTensor1[T,U,DoubleCounter[U],PairedDoubleCounter[T,U],DoubleCounter[T]](t,t2);
      }
    }
  }

  implicit def pCounterPBuilder[T,U,V]: TensorProductBuilder[PairedDoubleCounter[T,U],PairedDoubleCounter[U,V],PairedDoubleCounter[T,V],Shape2,Shape2,Shape2] = {
    new TensorProductBuilder[PairedDoubleCounter[T,U],PairedDoubleCounter[U,V],PairedDoubleCounter[T,V],Shape2,Shape2,Shape2] {
      def create(t: PairedDoubleCounter[T,U], t2: PairedDoubleCounter[U,V]):PairedDoubleCounter[T,V] = mkPairedDoubleCounter[T,V];
      def makeProduct(t: Tensor2Op[PairedDoubleCounter[T,U]], t2: Tensor2Op[PairedDoubleCounter[U,V]]) = {
        Tensor2MultTensor2[T,U,V,PairedDoubleCounter[T,V],PairedDoubleCounter[T,U],PairedDoubleCounter[U,V]](t,t2);
      }
    }
  }

  /**
  * Factory method supplied by implementors
  */
  protected def mkDoubleCounter[T]: DoubleCounter[T];

  /**
  * Factory method supplied by implementors
  */
  protected def mkDoubleCounterFor[T1,T2](key: T1, pc: PairedDoubleCounter[T1,T2])
      : InternalDoubleCounter[T1,T2];

  /**
  * Factory method supplied by implementors
  */
  protected def mkPairedDoubleCounter[T,U]: PairedDoubleCounter[T,U];

  /**
  * Trait that implementors of DoubleCounterFactory should implement.
  */
  abstract trait AbstractDoubleCounter[T] extends BaseDoubleCounter[T]  { 
    override def copy = { 
      val c = mkDoubleCounter[T];
      c := this;
      c;
    }
    def like = mkDoubleCounter[T];
  }

  /**
  * Trait that implementors of DoubleCounterFactory should implement.
  */
  abstract trait AbstractPairedDoubleCounter[T1,T2] extends BasePairedDoubleCounter[T1,T2] 
      with TensorSelfOp[(T1,T2),PairedDoubleCounter[T1,T2],Shape2] { this: PairedDoubleCounter[T1,T2] =>

    type DoubleCounter = InternalDoubleCounter[T1,T2];
    protected def mkDoubleCounter(k1:T1): DoubleCounter = mkDoubleCounterFor(k1,this);

    override def copy = { 
      val c = mkPairedDoubleCounter[T1,T2];
      c := this;
      c;
    }
    def like = mkPairedDoubleCounter[T1,T2];
  }

  /**
  * Return a counter after adding in all the tuples.
  */ 
  def aggregate[T](xs: (T,Double)*):DoubleCounter[T] = aggregate(xs.iterator);

  /**
  * Return a counter after adding in all the tuples.
  */ 
  def aggregate[T](xs: Iterable[(T,Double)]):DoubleCounter[T] = aggregate(xs.iterator);

  /**
  * Return a counter after adding in all the tuples.
  */ 
  def aggregate[T](xs: Iterator[(T,Double)]) = {
    val c = mkDoubleCounter[T];
    for( (x,v) <- xs) {
      if(!c.contains(x)) c(x) = v;
      else c(x) += v;
    }
    c;
  }

}

/**
* Provides utilities for creating counters, once you specify a counter
* IntCounter[T] that you would like to create, and mkDoubleCounter(set) for
* creating new ones.
* <p/>
* See object Counters for an example implementation.
*
* @author dlwh
*/
trait IntCounterFactory {

  /**
  * Abstract type that implementers must define. It must be a 
  * subtype of the AbstractDoubleCounter[T] trait.
  */
  type IntCounter[T] <: AbstractIntCounter[T];

  type PairedIntCounter[K1,K2] <: AbstractPairedIntCounter[K1,K2];

  /**
  * Given a domain (in the form of a mergeable set) create a new IntCounter.
  */
  protected def mkIntCounter[T]: IntCounter[T];

  protected type InternalIntCounter[T1,T2] <: IntCounter[T2] with PairIntStatsTracker[T1,T2];

  /**
  * Factory method supplied by implementors
  */
  protected def mkIntCounterFor[T1,T2](key: T1, pc: PairedIntCounter[T1,T2])
      : InternalIntCounter[T1,T2];

  /**
  * Factory method supplied by implementors
  */
  protected def mkPairedIntCounter[T,U]: PairedIntCounter[T,U];


  /**
  * Trait that implementors of IntCounterFactory should implement.
  */
  abstract trait AbstractIntCounter[T] extends BaseIntCounter[T] {
    def copy() = { 
      val c = mkIntCounter[T];
      c += this;
      c;
    }
  }


  /**
  * Trait that implementors of IntCounterFactory should implement.
  */
  abstract trait AbstractPairedIntCounter[T1,T2] extends BasePairedIntCounter[T1,T2] { this: PairedIntCounter[T1,T2] =>

    type IntCounter = InternalIntCounter[T1,T2];
    protected def mkIntCounter(k1:T1): IntCounter = mkIntCounterFor(k1,this);

    def copy = { 
      val c = mkPairedIntCounter[T1,T2];
      c += this;
      c;
    }
    def like = mkPairedIntCounter[T1,T2];
  }


  /**
  * Return a new counter with counts equal to the number of 
  * times each x was seen.
  */ 
  def count[T](x: T, xs: T*):IntCounter[T] = {
    val c = count(xs.iterator);
    c.incrementCount(x,1);
    c;
  }

  /**
  * Return a new counter with counts equal to the number of 
  * times each x was seen.
  */ 
  def count[T](xs: Iterable[T]):IntCounter[T] = count(xs.iterator)

  /**
  * Return a new counter with counts equal to the number of 
  * times each x was seen.
  */ 
  def count[T](xs: Iterator[T]) = {
    val c = mkIntCounter[T];
    for(x <- xs) {
      c.incrementCount(x,1)
    }
    c
  }

}
