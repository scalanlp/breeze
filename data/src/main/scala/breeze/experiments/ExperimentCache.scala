package breeze.experiments

/**
 * An experiment cache just saves information about some experiment to a file.
 * @author dlwh
 */
trait ExperimentCache {
  def logExperiment[Parameters, T](name: String, parameters: Parameters, result: T):Unit
  def close():Unit
}