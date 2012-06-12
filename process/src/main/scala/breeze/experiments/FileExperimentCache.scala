package breeze.experiments

import java.io.{PrintStream, FileOutputStream, File}
import java.util.Date

/**
 * Writes out experiment information to a tab-delimted file.
 * @author dlwh
 */
class FileExperimentCache(path: File) extends ExperimentCache {
  def logExperiment[Parameters, T](name: String, parameters: Parameters, result: T)  {
    val in = new PrintStream(new FileOutputStream(path,true))
    in.println(name + "\t" + new Date() + "\t" + result + "\t" + parameters)
    in.close()
  }

  def close() {}
}