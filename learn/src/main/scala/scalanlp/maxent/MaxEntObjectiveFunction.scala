package scalanlp.maxent

/**
 * 
 * @author dlwh
 */
import scalala.Scalala._;
import scalala.tensor.Vector;
import scalala.tensor.adaptive.AdaptiveVector;
import scalala.tensor.sparse.SparseVector;

import scalala.tensor.counters.Counters
import scalala.tensor.counters.Counters.DoubleCounter
import scalala.tensor.counters.Counters.PairedDoubleCounter
import scalala.tensor.counters.LogCounters
import scalala.tensor.counters.LogCounters.LogPairedDoubleCounter
import scalala.tensor.dense.DenseVector
import scalanlp.concurrent.ParallelOps._;
import scalanlp.util.Index
import scalanlp.util.Encoder
import scalanlp.math.Numerics;
import scalanlp.collection.mutable.SparseArray
import scalanlp.util.Log
import scalanlp.util.Profiling
import scalanlp.optimize.{FirstOrderMinimizer, DiffFunction}

abstract class MaxEntObjectiveFunction extends DiffFunction[Int,DenseVector]  {
  type Context;
  type Decision;
  type Feature;

  val contextIndex: Index[Context];
  val decisionIndex: Index[Decision];
  val indexedDecisionsForContext:IndexedSeq[IndexedSeq[Int]];

  protected def features(d: Decision, c: Context):IndexedSeq[Feature];
  protected def initialValueForFeature(f: Feature):Double;
  // (Context -> Decision -> log p(decision|context)) => (log prob, expected count of (context,decision)
  protected def expectedCounts(logThetas: IndexedSeq[Vector]):(Double,IndexedSeq[Vector]);

  lazy val contextBroker = Encoder.fromIndex(contextIndex);
  protected lazy val decisionBroker = Encoder.fromIndex(decisionIndex);

  // feature grid is contextIndex -> decisionIndex -> Seq[feature index]
  lazy val (featureIndex: Index[Feature], featureGrid: Array[SparseArray[Array[Int]]]) = {
    val index = Index[Feature]();
    val grid = contextBroker.fillArray(decisionBroker.fillSparseArray(Array[Int]()));
    for(cI <- 0 until contextIndex.size;
        c = contextIndex.get(cI);
        dI <- indexedDecisionsForContext(cI)) {
      val d = decisionIndex.get(dI);
      val f = features(d,c);
      Profiling.time(c + " " + d + f.size) {
        if(!f.isEmpty) {
          grid(cI)(dI) = f.map(index.index).toArray.sorted;
        }
      }
    }
    (index,grid:Array[SparseArray[Array[Int]]]);
  }
  lazy val featureEncoder = Encoder.fromIndex(featureIndex);

  lazy val defaultInitWeights = Counters.aggregate(featureIndex.map{ f => (f,initialValueForFeature(f) + math.log(.02 * math.random + 0.99))});
  lazy val encodedInitialWeights = featureEncoder.encodeDense(defaultInitWeights);

  protected def decodeThetas(m: IndexedSeq[Vector]): LogPairedDoubleCounter[Context,Decision] = {
    val result = LogPairedDoubleCounter[Context,Decision];
    for( (vec,cI) <- m.iterator.zipWithIndex) {
      result(contextIndex.get(cI)) := decisionBroker.decode(vec);
    }
    result;
  }

  // Context -> Decision -> log p(decision|context)
  private def computeLogThetas(weights: DenseVector): IndexedSeq[Vector] = {
    val thetas = contextBroker.mkArray[Vector];
    for((dIs,cI) <- featureGrid.zipWithIndex) {
      thetas(cI) = decisionBroker.mkVector(Double.NegativeInfinity);
      for((dI,features) <- dIs) {
        val score = sumWeights(features,weights);
        thetas(cI)(dI) = score;
      }
    }
    logNormalizeRows(thetas);
  }

  private def logNormalizeRows(thetas: IndexedSeq[Vector])  = {
    for( vec <- thetas) {
      vec -= Numerics.logSum(vec);
    }
    thetas;
  }


  // basically just a dot product
  protected def sumWeights(indices: Array[Int], weights: DenseVector) = {
    var i = 0;
    var sum = 0.0;
    while(i < indices.length) {
      val f = indices(i);
      sum += weights(f);
      i += 1;
    }
    sum;
  }

  override def calculate(weights: DenseVector) = {
    val encodedThetas = computeLogThetas(weights);
    val (marginalLogProb,eCounts) = expectedCounts(encodedThetas);
    val encodedTotals = eCounts.map(Numerics.logSum _);
    val (expCompleteLogProb,grad) = computeGradient(weights, encodedThetas, eCounts, encodedTotals);
    (-marginalLogProb,grad);
  }


  override def valueAt(weights: DenseVector) = {
    val encodedThetas = computeLogThetas(weights);
    val (marginalLogProb,eCounts) = expectedCounts(encodedThetas);

    -marginalLogProb
  }

  protected def computeValue(featureWeights: Vector, logThetas: IndexedSeq[Vector], eCounts: IndexedSeq[Vector], eTotals: IndexedSeq[Double]) = {
    var logProb = 0.0;

    for( (vec,c) <- eCounts.zipWithIndex) {
      val cTheta = logThetas(c);
      val vec2 = vec match {
        case v: AdaptiveVector => v.innerVector;
        case v => v
      }
      vec2 match {
        case vec: SparseVector =>
          var i = 0;
          while(i < vec.used) {
            val d = vec.index(i);
            val e = vec.data(i);
            val lT = cTheta(d);
            logProb += e * lT;
            i += 1;
          }
        case _ =>
          for((d,e) <- vec.activeElements) {
            val lT = cTheta(d);
            logProb += e * lT;
          }
      }
    }
    -logProb
  }

  // computes expComplete log Likelihood and gradient
  protected def computeGradient(featureWeights: Vector, logThetas: IndexedSeq[Vector], eCounts: IndexedSeq[Vector], eTotals: IndexedSeq[Double]): (Double,DenseVector) = {
    // gradient is \sum_{d,c} e(d,c) * (f(d,c) - \sum_{d'} exp(logTheta(c,d')) f(d',c))
    // = \sum_{d,c} (e(d,c)  - e(*,c) exp(logTheta(d,c))) f(d,c)
    // = \sum_{d,c} margin(d,c) * f(d,c)
    //
    // e(*,c) = \sum_d e(d,c) == eCounts(c).total
    def featureGrad = featureEncoder.mkDenseVector(0.0);

    val (grad,prob) = eCounts.zipWithIndex.par(2000).fold( (featureGrad,0.0) ) { (gradObj,vecIndex) =>
      val (vec,c) = vecIndex;
      var (featureGrad,logProb) = gradObj;
      val cTheta = logThetas(c);
      val logTotal = math.log(eTotals(c));
      val vec2 = vec match {
        case v: AdaptiveVector => v.innerVector;
        case v => v
      }
      vec2 match {
        case vec: SparseVector =>
          var i = 0;
          while(i < vec.used) {
            val d = vec.index(i);
            val e = vec.data(i);
            val lT = cTheta(d);
            logProb += e * lT;

            val margin = e - math.exp(logTotal + lT);

            var j = 0;
            val grid = featureGrid(c)(d);
            while(j < grid.size) {
              val f = grid(j);
              featureGrad(f) += margin;
              j += 1;
            }
            i += 1;
          }
        case _ =>
          for((d,e) <- vec.activeElements) {
            val lT = cTheta(d);
            logProb += e * lT;

            val margin = e - math.exp(logTotal + lT);

            for( f <- featureGrid(c)(d))
              featureGrad(f) += margin;
          }
      }
      (featureGrad,logProb)

    } { (gradObj1,gradObj2) =>
      gradObj1._1 += gradObj2._1
      (gradObj1._1, gradObj1._2 + gradObj2._2)
    }

    val realProb = - prob
    val finalGrad = -grad

    (realProb,finalGrad value);
  }

  class mStepObjective(encodedCounts: IndexedSeq[Vector]) extends DiffFunction[Int,DenseVector]   {
    val encodedTotals = encodedCounts.map(Numerics.logSum _);
    override def calculate(weights: DenseVector) = {
      val logThetas = computeLogThetas(weights);
      computeGradient(weights,logThetas,encodedCounts,encodedTotals);
    }

    override def valueAt(weights: DenseVector) = {
      val logThetas = computeLogThetas(weights);
      computeValue(weights,logThetas,encodedCounts,encodedTotals);
    }

  }

  final case class State(encodedWeights: DenseVector, marginalLikelihood: Double) {
    private[MaxEntObjectiveFunction] lazy val encodedLogThetas =computeLogThetas(encodedWeights)
    lazy val logThetas = decodeThetas(encodedLogThetas);
    lazy val weights = featureEncoder.decode(encodedWeights);
  }

  def emIterations(initialWeights: DoubleCounter[Feature] = defaultInitWeights,
                   maxMStepIterations: Int=90,
                   optParams: FirstOrderMinimizer.OptParams): Iterator[State] = {
    val log = Log.globalLog;

    val weightsIterator = Iterator.iterate(State(featureEncoder.encodeDense(initialWeights),Double.NegativeInfinity)) { state =>
      val (marginalLogProb,eCounts) = expectedCounts(state.encodedLogThetas);
      val obj = new mStepObjective(eCounts);
      val optimizer = optParams.minimizer(obj);
      val newWeights = optimizer.minimize(obj, state.encodedWeights);
      val nrm = norm(state.encodedWeights - newWeights,2) / newWeights.size;
      State(newWeights,marginalLogProb);
    }

    weightsIterator drop 1 // initial iteration is crap
  }


}

trait EasyMaxEnt { maxent: MaxEntObjectiveFunction =>
  protected def decisionsForContext(c: Context): Iterator[Decision]
  protected def allContexts: Iterator[Context]

  val contextIndex: Index[Context] = Index(allContexts);
  val (decisionIndex,indexedDecisionsForContext) = {
    val decisionIndex = Index[Decision];
    val indexedDecisionsForContext = contextBroker.mkArray[IndexedSeq[Int]];
    for( (c,cI) <- contextIndex.pairs) {
      indexedDecisionsForContext(cI) = scala.util.Sorting.stableSort(decisionsForContext(c).map(decisionIndex.index _).toSeq);
    }
    (decisionIndex,indexedDecisionsForContext:IndexedSeq[IndexedSeq[Int]]);
  }

  /** Should compute marginal likelihood and expected counts for the data */
  protected def expectedCounts(logThetas: LogPairedDoubleCounter[Context,Decision]):(Double,PairedDoubleCounter[Context,Decision]);

  protected def expectedCounts(encodedThetas: IndexedSeq[Vector]):(Double,IndexedSeq[Vector]) = {
    val logThetas = decodeThetas(encodedThetas);
    val (marginalLogProb,eCounts) = expectedCounts(logThetas);
    (marginalLogProb,encodeCounts(eCounts));
  }

  private def encodeCounts(eCounts: PairedDoubleCounter[Context,Decision]): Array[Vector] = {
    val encCounts = contextBroker.mkArray[Vector];
    for( (c,ctr) <- eCounts.rows;
         cI = contextIndex(c);
         encCtr = decisionBroker.encode(ctr)
       ) {
      encCounts(cI) = encCtr;
    }

    encCounts
  }

  class mStepObjective(eCounts: PairedDoubleCounter[Context,Decision]) extends maxent.mStepObjective(encodeCounts(eCounts));

}
