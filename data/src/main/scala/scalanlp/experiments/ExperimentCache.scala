package scalanlp.experiments

/**
 * 
 * @author dlwh
 */

trait ExperimentCache {
  def logExperiment[Parameters, T](name: String, parameters: Parameters, result: T):Unit
  def close():Unit
}