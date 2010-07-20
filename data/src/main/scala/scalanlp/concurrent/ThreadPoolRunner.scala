package scalanlp.concurrent
/*
 Copyright 2010 David Hall, Daniel Ramage

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
