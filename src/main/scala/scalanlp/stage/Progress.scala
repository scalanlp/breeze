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
package scalanlp.stage;

class Task(val name : String, val reporter : Reporter) {
  var active = true;
  
  /** Returns a new Progress object for the given subtask. */
  def subtask(name : String) : SubTask =
    new SubTask(this, name);
  
  /** Reports progress as a fraction of the total. */
  def progress(item : Int, total : Int) = {
    active = (item <= total); 
    reporter.report(this,item,total);
  }
  
  /** Returns true if the last call to progress had item != total. */
  def isActive =
    active;
  
  override def toString = name;
}

class SubTask(val parent : Task, val subname : String)
extends Task(parent.name+":"+subname,parent.reporter);

trait Reporter {
  def report(task : Task, item : Int, total : Int);
}

class PrintReporter(stream : java.io.PrintStream, minReport : Double) extends Reporter {
  val state = scala.collection.mutable.HashMap[Task,(Int,Int)]();
  
  override def report(task : Task, item : Int, total : Int) = synchronized {
    val (lastItem, lastTotal) = state.getOrElseUpdate(task, (0,0));
    if (lastTotal != total || (item - lastItem) >= minReport * total) {
      System.err.printf("%s : %0"+total.toString.length+"d / %d\n",
                        task.toString, int2Integer(item), int2Integer(total));
      state(task) = (item,total);
    }
  }
}

object ConsoleReporter extends PrintReporter(System.err, .05);
