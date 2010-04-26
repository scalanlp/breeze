package scalanlp.concurrent

import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.TimeUnit
import scala.concurrent.FutureTaskRunner
import scala.concurrent.JavaConversions

/**
 * A class for creating FutureTaskRunners like in scala.concurrent. This one
 * is configurable in the number of processors it can use, which makes it a better citizen.
 * Code is lifted straight out of scala.concurrent.TaskRunners.scala
 *
 * @author dlwh
 */
object ThreadPoolRunner {
  private val numCores = Runtime.getRuntime().availableProcessors
  def apply(coreSize: Int=numCores, maxSize: Int = numCores) : FutureTaskRunner = {
    val keepAliveTime = 60000L
    val workQueue = new LinkedBlockingQueue[Runnable]
    val exec = new ThreadPoolExecutor(coreSize,
                                      maxSize,
                                      keepAliveTime,
                                      TimeUnit.MILLISECONDS,
                                      workQueue,
                                      new ThreadPoolExecutor.CallerRunsPolicy)
    JavaConversions.asTaskRunner(exec)
  }

}
