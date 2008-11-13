package scalanlp.util

import java.io.File
import java.io.RandomAccessFile
import java.io.InputStream
import java.io.OutputStream
import java.lang.Process
import java.lang.ProcessBuilder

import scala.concurrent.ops._

/**
 * Helper methods for PipeProcess
 * 
 * @author dramage
 */
object PipeIO {
  /**
   * Read all bytes from the given input stream to the given output
   * stream, closing the input stream when finished reading.  Does
   * not close the output stream.
   */
  def drain(in : InputStream, out : OutputStream) {
    val buffer = new Array[byte](1024);

    var numRead = 0;
    do {
      numRead = in.read(buffer,0,buffer.length);
      if (numRead > 0) {
        // read some bytes
        out.write(buffer,0,numRead);
      } else if (numRead == 0) {
        // read no bytes, but not yet EOF
        Thread.sleep(100l);
      }
    } while (numRead >= 0)

    in.close();
  }
  
  /**
   * Reads all lines in the given input stream using Java's
   * BufferedReader.  The returned lines do not have a trailing
   * newline character.
   */
  def readLines(in : InputStream) : Iterator[String] = {
    val reader = new java.io.BufferedReader(new java.io.InputStreamReader(in));
    return new Iterator[String]() {
      var line = prepare();
      
      override def hasNext =
        line != null;
      
      override def next = {
        val rv = line;
        line = prepare();
        rv;
      }
      
      def prepare() = {
        val rv = reader.readLine();
        if (rv == null) {
          reader.close();
        }
        rv;
      }
    };
  }
}

/**
 * A richer Process object used for linking together in pipes.
 * 
 * @author dramage
 */
class PipeProcess(val process : Process)(implicit pipes : Pipes) {
  import PipeIO._

  /** where stdout and stderr go. */
  protected var out : OutputStream = pipes.stdout;
  protected var err : OutputStream = pipes.stderr;

  def waitFor : Int = process.waitFor();

  /** Close output pipes (on finish) if they are not stdout and stderr */
  private def closePipes() {
      if (out != pipes.stdout && out != pipes.stderr) {
        out.close();
      }
      if (err != pipes.stdout && err != pipes.stderr) {
        err.close();
      }
  }
  
  def |  (next : PipeProcess) : PipeProcess = {
    // stdout goes to the next process
    this.out = next.process.getOutputStream;

    spawn {
      val waitForStdin  = future { drain(process.getInputStream, out); }
      val waitForStderr = future { drain(process.getErrorStream, err); }

      waitForStdin();
      closePipes();
    }

    return next;
  }

  def |& (next : PipeProcess) : PipeProcess = {
    // stdout and stderr both go to the next process
    this.out = next.process.getOutputStream;
    this.err = next.process.getOutputStream;

    spawn {
      val waitForStdin  = future { drain(process.getInputStream, out); }
      val waitForStderr = future { drain(process.getErrorStream, err); }

      waitForStdin();
      waitForStderr();
      closePipes();
    }

    return next;
  }

  /** Redirects the given input stream as the source for the process */
  def <  (instream : InputStream) : PipeProcess = {
    spawn {
      val out = process.getOutputStream;
      drain(instream, process.getOutputStream);
      out.close();
    }

    return this;
  }

  /** Redirects output from the process to the given output stream */
  def |  (outstream : OutputStream) : PipeProcess = {
    this.out = outstream;

    spawn {
      val waitForStdin  = future { drain(process.getInputStream, out); }
      val waitForStderr = future { drain(process.getErrorStream, err); }

      waitForStdin();
      closePipes();
    }

    return this;
  }

  /** Redirects stdout and stderr from the process to the given output stream */
  def |& (outstream : OutputStream) : PipeProcess = {
    this.out = outstream;
    this.err = outstream;

    spawn {
      val waitForStdin  = future { drain(process.getInputStream, out); }
      val waitForStderr = future { drain(process.getErrorStream, err); }

      waitForStdin();
      waitForStderr();
      closePipes();
    }

    return this;
  }

  /** Pipes to a function that accepts an InputStream. */
  def |[T](func : (InputStream => T)) : T =
    func(process.getInputStream);
  
  /** Reads the lines from this file. */
  def getLines : Iterator[String] =
    readLines(process.getInputStream);
}

/**
 * An alternative richer InputStream that can be piped to an OutputStream,
 * Process, or function.
 * 
 * @author dramage
 */
class PipeInputStream(var stream : InputStream) {
  import PipeIO._;

  /**
   * Pipe to an OutputStream.  Returns when all bytes have been
   * written to out.  Does not close out.
   */
  def |(out : OutputStream) : Unit =
    drain(stream, out);

  /**
   * Pipe to Process, returning that Process instance.  Returns
   * immediately.  Spawns a background job to write all bytes
   * from the incoming stream to the process.
   */
  def |(process : PipeProcess) : PipeProcess =
    process < stream;

  /** Pipes to a function that accepts an InputStream. */
  def |[T](func : (InputStream => T)) : T =
    func(stream);
  
  /** Returns all lines in this Stream. */
  def getLines : Iterator[String] =
    readLines(stream);
}

/**
 * A pipeable iterator of Strings, to be written as lines to a stream.
 */
class PipeIterator(lines : Iterator[String])(implicit pipes : Pipes) {
  /**
   * Writes all lines to the given process.  Returns immediately.
   */
  def |(process : PipeProcess) : PipeProcess = {
    val pipeIn  = new java.io.PipedInputStream();
    val pipeOut = new java.io.PipedOutputStream(pipeIn);
    spawn { this | pipeOut; }
    process < pipeIn;
  }
  
  /**
   * Writes all lines to the given OutputStream, closing it when done
   * if it is not System.out or System.err.
   */
  def |(outstream : OutputStream) = {
    val ps = new java.io.PrintStream(outstream);
    for (line <- lines) {
      ps.println(line);
    }
    
    if (!(outstream == pipes.stdout || outstream == pipes.stderr)) {
      ps.close;
    }
  }
}

/**
 * Runtime exception thrown by the Pipes framework.
 * 
 * @author dramage
 */
class PipesException(message : String) extends RuntimeException(message);


/**
 * Utilities for executing shell scripts, etc.
 * 
 * To get started with a global pipes shell, use:
 * 
 * import scalanlp.util.Pipes.global._
 * 
 * @author dramage
 */
class Pipes {
  private var _cwd : File = new File(new File("").getAbsolutePath);
  private var _stdout : OutputStream = java.lang.System.out;
  private var _stderr : OutputStream = java.lang.System.err;
  private var _stdin  : InputStream  = java.lang.System.in;

  /** Returns the default stdout used in this context. */
  def stdout = _stdout;
  
  /** Returns the default stderr used in this context. */
  def stderr = _stderr;
  
  /** Returns the default stdin used in this context. */
  def stdin  = _stdin;
  
  /**
   * Runs the given command (via the system command shell if found)
   * in the current directory.
   */
  def sh(command : String) : java.lang.Process = {
    val os = System.getProperty("os.name");
    val pb = new ProcessBuilder().directory(_cwd);
    
    if (os == "Windows 95" || os == "Windows 98" || os == "Windows ME") {
      pb.command("command.exe", "/C", command);
    } else if (os.startsWith("Windows")) {
      pb.command("cmd.exe", "/C", command);
    } else {
      pb.command("/bin/sh", "-c", command);
    };
    
    return pb.start();
  }

  /**
   * Returns the current working directory.
   */
  def pwd : File = _cwd;

  /**
   * Changes to the given directory.
   */
  def cd(folder : File) = {
    if (!folder.exists) {
      error("Folder "+folder+" does not exist.");
    } else if (!folder.isDirectory) {
      error("Folder "+folder+" is not a directory");
    } else if (!folder.canRead) {
      error("Cannot access folder "+folder);
    }
    _cwd = folder;
  }
  
  /** Waits for the tiven process to finish. */
  def waitFor(process : PipeProcess) = process.waitFor;

  //
  //  implicit conversions
  //
  
  implicit def iPipeProcess(process : Process) =
    new PipeProcess(process)(this);
  
  implicit def iPipeInputStream(stream : InputStream) =
    new PipeInputStream(stream);
  
  implicit def iPipeInputStream(file : File) =
    new PipeInputStream(file);
  
  /**
   * Gets a FileInputStream for the given file.  If the filename
   * ends with .gz, automatically wraps the returned stream with
   * a java.util.zip.GZIPInputStream.
   */
  implicit def iInputStream(file : File) : InputStream = {
    val fis = new java.io.BufferedInputStream(new java.io.FileInputStream(file));
    if (file.getName.toLowerCase.endsWith(".gz")) {
      return new java.util.zip.GZIPInputStream(fis);
    } else {
      return fis;
    }
  }
  
  /**
   * Gets a FileOutputStream for the given file.  If the filename
   * ends with .gz, automatically wraps the returned stream with
   * a java.util.zip.GZIPOutputStream.
   */
  implicit def iOutputStream(file : File) : OutputStream = {
    val fos = new java.io.BufferedOutputStream(new java.io.FileOutputStream(file));
    if (file.getName.toLowerCase.endsWith(".gz")) {
      return new java.util.zip.GZIPOutputStream(fos);
    } else {
      return fos;
    }
  }
  
  /**
   * Returns a file with the given name, relative to the current
   * directory (if found and path does not start with 
   */
  implicit def File(path : String) : File = {
    if (!path.startsWith(java.io.File.pathSeparator)) {
      new File(_cwd,path);
    } else {
      new File(path);
    }
  }
  
  implicit def iPipeIterator[E](lines : Iterator[E]) =
    new PipeIterator(lines.map(_.toString))(this);
  
  implicit def iPipeIterator[E](lines : Iterable[E]) =
    new PipeIterator(lines.elements.map(_.toString))(this);
  
  private def error(message : String) : Unit = {
    throw new PipesException(message);
  }
}
  
/**
 * To get started with a global pipes shell, use:
 * 
 * import scalanlp.util.Pipes.global._
 * 
 * And take a look at the example code in the Pipes object's main method.
 */
object Pipes {
  /** A global instance for easy imports */
  val global = new Pipes();
  
  def apply() = {
    new Pipes();
  }
  
  def main(argv : Array[String]) {
    import global._;
    
    sh("sleep 1; echo '(sleep 1 async) prints 2nd'") | stdout;
    sh("echo '(no sleep async) prints 1st'") | stdout;
    waitFor(sh("sleep 2; echo '(sleep 2 sync) prints 3rd after pause'") | stdout);
    sh("echo '(stderr redirect) should show up on stdout' | cat >&2") |& stdout;
    sh("echo '(stderr redirect) should also show up on stdout' | cat >&2") |& sh("cat") | stdout;
    sh("echo '(pipe test line 1) should be printed'; echo '(pipe test line 2) should not be printed'") | sh("grep 1") | stdout;
    sh("echo '(translation test) should sound funny'") | sh("perl -pe 's/(a|e|i|o|u)+/oi/g';") | stdout;
    stdin | sh("egrep '[0-9]'") | stdout;
    
    (1 to 10).map(_.toString) | stdout;
    
    for (line <- sh("ls").getLines) {
      println(line.toUpperCase);
    }
  }
}
