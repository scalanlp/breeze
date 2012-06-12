package breeze.maxent

/**
 * 
 * @author dlwh
 */
import scalala.tensor.mutable.Vector;
import scalala.tensor.sparse.SparseVector;

import scalala.tensor.dense.DenseVector
import breeze.util.Index
import breeze.util.Encoder
import scalala.library.Library.softmax;
import breeze.util.Profiling
import breeze.optimize.{FirstOrderMinimizer, DiffFunction}
import scalala.collection.sparse.SparseArray
import scalala.tensor.{Counter, Counter2}
import scalala.tensor.::
import scalala.library.Numerics
import scalala.generic.math.CanSoftmax

abstract class MaxEntObjectiveFunction extends DiffFunction[DenseVector[Double]]  {
  type Context;
  type Decision;
  type Feature;

  val contextIndex: Index[Context];
  val decisionIndex: Index[Decision];
  val indexedDecisionsForContext:IndexedSeq[IndexedSeq[Int]];

  protected def features(d: Decision, c: Context):IndexedSeq[Feature];
  protected def initialValueForFeature(f: Feature):Double;
  // (Context -> Decision -> log p(decision|context)) => (log prob, expected count of (context,decision)
  protected def expectedCounts(logThetas: IndexedSeq[Vector[Double]]):(Double,IndexedSeq[Vector[Double]]);

  lazy val contextBroker = Encoder.fromIndex(contextIndex);
  protected lazy val decisionBroker = Encoder.fromIndex(decisionIndex);

  // feature grid is contextIndex -> decisionIndex -> Seq[feature index]
  lazy val (featureIndex: Index[Feature], featureGrid: Array[SparseArray[Array[Int]]]) = {
    val index = Index[Feature]();
    val grid = contextBroker.fillArray(decisionBroker.mkSparseArray[Array[Int]]);
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

  lazy val defaultInitWeights = Counter(featureIndex.map{ f => (f,initialValueForFeature(f) + math.log(.02 * math.random + 0.99))});
  lazy val encodedInitialWeights = featureEncoder.encodeDense(defaultInitWeights);

  protected def decodeThetas(m: IndexedSeq[Vector[Double]]): Counter2[Context,Decision,Double] = {
    val result = Counter2[Context,Decision,Double];
    for( (vec,cI) <- m.iterator.zipWithIndex) {
      result(contextIndex.get(cI),::) := decisionBroker.decode(vec);
    }
    result;
  }

  // Context -> Decision -> log p(decision|context)
  private def computeLogThetas(weights: DenseVector[Double]): IndexedSeq[Vector[Double]] = {
    val thetas = contextBroker.mkArray[Vector[Double]];
    for((dIs,cI) <- featureGrid.zipWithIndex) {
      thetas(cI) = decisionBroker.mkDenseVector(Double.NegativeInfinity);
      for((dI,features) <- dIs.activeIterator) {
        val score = sumWeights(features,weights);
        thetas(cI)(dI) = score;
      }
    }
    logNormalizeRows(thetas);
  }

  private def logNormalizeRows(thetas: IndexedSeq[Vector[Double]])  = {
    for( vec <- thetas) {
      vec -= softmax(vec);
    }
    thetas;
  }


  // basically just a dot product
  protected def sumWeights(indices: Array[Int], weights: DenseVector[Double]) = {
    var i = 0;
    var sum = 0.0;
    while(i < indices.length) {
      val f = indices(i);
      sum += weights(f);
      i += 1;
    }
    sum;
  }

  override def calculate(weights: DenseVector[Double]) = {
    val encodedThetas = computeLogThetas(weights);
    val (marginalLogProb,eCounts) = expectedCounts(encodedThetas);
    val sm = CanSoftmax.mkTensor1Softmax[Int,Vector[Double]];
    val encodedTotals = eCounts.map(v => softmax(v)(sm));
    val (expCompleteLogProb,grad) = computeGradient(weights, encodedThetas, eCounts, encodedTotals);
    (-marginalLogProb,grad);
  }


  override def valueAt(weights: DenseVector[Double]) = {
    val encodedThetas = computeLogThetas(weights);
    val (marginalLogProb,eCounts) = expectedCounts(encodedThetas);

    -marginalLogProb
  }

  // computes just the value
  protected def computeValue(featureWeights: Vector[Double], logThetas: IndexedSeq[Vector[Double]], eCounts: IndexedSeq[Vector[Double]], eTotals: IndexedSeq[Double]) = {
    var logProb = 0.0;

    for( (vec,c) <- eCounts.zipWithIndex) {
      val cTheta = logThetas(c);
      vec match {
        case vec: SparseVector[Double] =>
          var i = 0;
          while(i < vec.data.activeLength) {
            val d = vec.data.indexArray(i);
            val e = vec.data.valueArray(i);
            val lT = cTheta(d);
            logProb += e * lT;
            i += 1;
          }
        case _ =>
          for((d,e) <- vec.pairsIteratorNonZero) {
            val lT = cTheta(d);
            logProb += e * lT;
          }
      }
    }
    -logProb
  }

  // computes expected complete log Likelihood and gradient
  protected def computeGradient(featureWeights: Vector[Double], logThetas: IndexedSeq[Vector[Double]], eCounts: IndexedSeq[Vector[Double]], eTotals: IndexedSeq[Double]): (Double,DenseVector[Double]) = {
    // gradient is \sum_{d,c} e(d,c) * (f(d,c) - \sum_{d'} exp(logTheta(c,d')) f(d',c))
    // = \sum_{d,c} (e(d,c)  - e(*,c) exp(logTheta(d,c))) f(d,c)
    // = \sum_{d,c} margin(d,c) * f(d,c)
    //
    // e(*,c) = \sum_d e(d,c) == eCounts(c).total


    val (grad: DenseVector[Double],prob: Double) = eCounts.zipWithIndex.par.view.map { case (vec,c) =>
      val cTheta = logThetas(c);
      var logProb = 0.0;
      val logTotal = math.log(eTotals(c));
      val featureGrad = featureEncoder.mkDenseVector(0.0);
      vec match {
        case vec: SparseVector[Double] =>
          var i = 0;
          while(i < vec.data.activeLength) {
            val d = vec.data.indexArray(i);
            val e = vec.data.valueArray(i);
            val lT = cTheta(d);
            logProb += e * lT;

            val margin = e - math.exp(logTotal + lT);

            var j = 0;
            val grid = featureGrid(c)(d);
            if(grid != null)
              while(j < grid.size) {
                val f = grid(j);
                featureGrad(f) += margin;
                j += 1;
              }
            i += 1;
          }
        case _ =>
          for((d,e) <- vec.pairsIteratorNonZero) {
            val lT = cTheta(d);
            logProb += e * lT;

            val margin = e - math.exp(logTotal + lT);

            val grid = featureGrid(c)(d);
            if(grid != null)
            for( f <- grid)
              featureGrad(f) += margin;
          }
      }
      (featureGrad,logProb)

    }.fold((featureEncoder.mkDenseVector(0.0),0.0)) { (gradObj1,gradObj2) =>
      gradObj2._1 += gradObj1._1
      (gradObj2._1, gradObj1._2 + gradObj2._2)
    }

    val realProb = - prob
    val finalGrad = grad * -1;

    (realProb,finalGrad);
  }

  class mStepObjective(encodedCounts: IndexedSeq[Vector[Double]]) extends DiffFunction[DenseVector[Double]]   {
    val sm = CanSoftmax.mkTensor1Softmax[Int,Vector[Double]];
    val encodedTotals = encodedCounts.map(v => softmax(v)(sm));
    override def calculate(weights: DenseVector[Double]) = {
      val logThetas = computeLogThetas(weights);
      computeGradient(weights,logThetas,encodedCounts,encodedTotals);
    }

    override def valueAt(weights: DenseVector[Double]) = {
      val logThetas = computeLogThetas(weights);
      computeValue(weights,logThetas,encodedCounts,encodedTotals);
    }

  }

  final case class State(encodedWeights: DenseVector[Double], marginalLikelihood: Double) {
    private[MaxEntObjectiveFunction] lazy val encodedLogThetas =computeLogThetas(encodedWeights)
    lazy val logThetas = decodeThetas(encodedLogThetas);
    lazy val weights = featureEncoder.decode(encodedWeights);
  }

  /*
  def emIterations(initialWeights: Counter[Feature,Double] = defaultInitWeights,
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
  */


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
  protected def expectedCounts(logThetas: Counter2[Context,Decision,Double]):(Double,Counter2[Context,Decision,Double]);

  protected def expectedCounts(encodedThetas: IndexedSeq[Vector[Double]]):(Double,IndexedSeq[Vector[Double]]) = {
    val logThetas = decodeThetas(encodedThetas);
    val (marginalLogProb,eCounts) = expectedCounts(logThetas);
    (marginalLogProb,encodeCounts(eCounts));
  }

  private def encodeCounts(eCounts: Counter2[Context,Decision,Double]): Array[Vector[Double]] = {
    val encCounts = contextBroker.mkArray[Vector[Double]];
    for( c <- eCounts.domain._1) {
      val ctr = eCounts(c,::);
      val cI = contextIndex(c);
      val encCtr = decisionBroker.encode(ctr)
      encCounts(cI) = encCtr;
    }

    encCounts
  }

  class mStepObjective(eCounts: Counter2[Context,Decision,Double]) extends maxent.mStepObjective(encodeCounts(eCounts));

}
