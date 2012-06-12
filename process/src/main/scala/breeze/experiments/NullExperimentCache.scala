package breeze.experiments

/**
 * An ExperimetnCache that doesn't save any information.
 *
 * @author dlwh
 */
class NullExperimentCache extends ExperimentCache {
  def logExperiment[Parameters, T](name: String, parameters: Parameters, result: T) {}

  def close() {}
}