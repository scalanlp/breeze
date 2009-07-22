package scalara.ra;

import scalara.pipes.Pipes;

//
// ResearchAssistant for Scala
//

case class FileResourceSet(roots : String*)(implicit val pipes : Pipes) {
  import java.io.File.separator;
  import pipes._;
  
  val _roots = roots.map(root =>
    if (root.endsWith(separator)) root.substring(0,root.length-separator.length) else root);
  
  /**
   * Returns the existing resource with the given name if it exists in the path,
   * otherwise returning the the implicit pipes' name.
   */
  def apply(name : String) : java.io.File = {
    val files = _roots.map(root => root + separator + name).filter(f => file(f).exists);
    if (files.length == 0) file(name) else file(files(0));
  }
}


case class Parameters(val stack : scala.collection.immutable.Stack[scala.collection.mutable.Map[String,Any]]) {
  def apply[V](name : String) : Option[V] = {
    def get(stack : scala.collection.immutable.Stack[scala.collection.mutable.Map[String,Any]]) : Option[Any] = {
      if (stack.isEmpty) {
        None;
      } else if (stack.top.isDefinedAt(name)) {
        Some(stack.top(name));
      } else {
        get(stack.pop);
      }
    }
    return get(stack).asInstanceOf[Option[V]];
  }
  
  def update[V](name : String, value : V) {
    stack.top(name) = value;
  }
}

case class RA(
  /** Returns the file system context of this branch. */
  val pipes : Pipes,
  
  /** Returns the parent of this context or null if it is the root context. */
  val parentRA : RA,
  
  /** Logs the given message to this context's logger. */
  val log : (String => Unit),
  
  /** Parameters in this context. */
  val parameters : Parameters,
  
  /** Context random number generator. */
  val random : java.util.Random
) {
  
  RA.init();
  
  /** Source of where this context was created. */
  val raSource = Thread.currentThread.getStackTrace.dropWhile(_.getClassName.startsWith(classOf[RA].getPackage.getName));

  def parameter[E](name : String)(value : =>E) : E = {
    try {
      val v = value;
      log("RA.parameter: "+name+" = "+v);
      parameters(name) = v;
      v;
    } catch {
      case ex : Throwable =>
        throw new ParameterException("Error getting value for "+name,ex);
    }
  }
  
  def task[E](name : String)(exec : =>E) : E = {
    log("RA: "+name+" started");
    val rv = exec;
    log("RA: "+name+" done");
    rv;
  }
  
  /** Returns a cached view of the given value, loading from a cell if possible. */
  def cache[V](cacheFile : java.io.File)(p : =>V)(implicit valType : scala.reflect.Manifest[V]) =
    cell(cacheFile)(p).get;
  
  def cell[V](cacheFile : java.io.File)(p : =>V)(implicit valType : scala.reflect.Manifest[V]) =
    new Cell(cacheFile,p)(valType,this);
  
  /** Gets a (named) child context in a new (named) subdirectory. */
  def subRA(name : String) = {
    val _pipes = Pipes(pipes);
    import _pipes._;
    
    if (file(name).exists && !file(name).isDirectory) {
      throw new IllegalArgumentException(file(name)+" already exists and is not a directory");
    }
    if (!file(name).exists) {
      file(name).mkdir();
    }
    _pipes.cd(file(name));
    
    RA(_pipes, this, log,
       new Parameters(parameters.stack + scala.collection.mutable.Map[String,Any]()),
       RA.nextRandom);
  }
  
}
  
class ParameterException(msg : String, cause : Throwable)
  extends RuntimeException(msg,cause);  

object RA {
  val bean = java.lang.management.ManagementFactory.getRuntimeMXBean();
  
  val seed = new java.util.Random().nextLong;
  
  val seeds = new scala.collection.mutable.HashMap[String,Int] with scala.collection.mutable.SynchronizedMap[String,Int] {
    override def default(x : String) = 0;
  }
  
  val pid = bean.getName+":"+bean.getStartTime;
  
  System.err.println("RA: "+pid);
  
  /** Global (default) RA context. */
  implicit val global =
    RA(Pipes.global,
       null,
       System.err.println,
       new Parameters(new scala.collection.immutable.Stack + new scala.collection.mutable.HashMap[String,Any]),
       RA.nextRandom);
  
  def nextRandom : java.util.Random = {
    val stack = Thread.currentThread.getStackTrace.dropWhile(_.getClassName.startsWith(classOf[RA].getPackage.getName));
    def mkRandom(name : String) = new java.util.Random(name.hashCode + seeds(name) + seed);
    if (stack.isEmpty) {
      mkRandom(getClass.getName);
    } else {
      mkRandom(stack(0).getClassName);
    }
  }
  
  /** Simple empty method to ensure the static initializer runs. */
  protected[ra] def init() { }
  
  protected case class Action(name : String, help : String, body : ()=>Unit);
  
  trait Main {
    implicit val ra = new RA(
      new Pipes(), null, System.err.println,
      new Parameters(new scala.collection.immutable.Stack + new scala.collection.mutable.HashMap[String,Any]),
      RA.nextRandom);
      
    private var _argv : Array[String] = null;
    def argv = _argv;
    
    def main(args : Array[String]) {
      _argv = args;
    }
  }
  
  trait ActionsMain extends Main {
  
    protected var actions = List[Action]();
    
    def action[A](name : String, help : String)(body : =>Unit) {
      actions ::= Action(name,help,body _);
    }
    
    action("help", "displays usage information") {
      for (action <- actions) {
        println(action.name);
        println("  "+action.help);
      }
    }
    
    override def main(args : Array[String]) {
      def doHelp() {
        actions.filter(_.name=="help").take(1)(0).body();
        System.exit(-1);
      }
      
      if (args.length == 0) {
        doHelp();
      }
      
      val action = actions.filter(_.name==args(0)).take(1);
      if (action.size == 0) {
        println("No action found for name "+args(0));
        println();
        doHelp();
      } else if (action.size > 1) {
        println("Multiple actions found with name "+args(0));
        println();
        doHelp();
      }
      
      super.main(args);
      action(0).body();
    }
  }
}
