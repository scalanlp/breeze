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
object PipeProcess {
  protected sealed abstract case class PipeSource();
  protected sealed case class PipeProcessPipeSource(process : PipeProcess) extends PipeSource();
  protected sealed case class InputStreamPipeSource(stream : InputStream) extends PipeSource();

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
}

/**
 * A richer Process object used for linking together in pipes.
 * 
 * @author dramage
 */
class PipeProcess(val process : Process) {
  import PipeProcess._

  /** which process feeds stding. */
  protected var source : PipeSource = InputStreamPipeSource(System.in);
  protected var stdout : OutputStream = System.out;
  protected var stderr : OutputStream = System.err;

  def waitFor : Int = process.waitFor();

  def |  (next : PipeProcess) : PipeProcess = {
    next.source = PipeProcessPipeSource(this);

    // stdout goes to the next process
    this.stdout = next.process.getOutputStream;

    spawn {
      val waitForStdin  = future { drain(process.getInputStream, stdout); }
      val waitForStderr = future { drain(process.getErrorStream, stderr); }

      waitForStdin();
      stdout.close();
    }

    return next;
  }

  def |& (next : PipeProcess) : PipeProcess = {
    next.source = PipeProcessPipeSource(this);

    // stdout and stderr both go to the next process
    this.stdout = next.process.getOutputStream;
    this.stderr = next.process.getOutputStream;

    spawn {
      val waitForStdin  = future { drain(process.getInputStream, stdout); }
      val waitForStderr = future { drain(process.getErrorStream, stderr); }

      waitForStdin();
      waitForStderr();
      stdout.close();
    }

    return next;
  }

  /**
   * Does this process, then the next in series.
   */
  def && (followup : PipeProcess) : PipeProcess = {
    throw new UnsupportedOperationException;
  }

  /** Redirects the given input stream as the source for the process */
  def <  (instream : InputStream) : PipeProcess = {
    this.source = InputStreamPipeSource(instream);

    spawn {
      val out = process.getOutputStream;
      drain(instream, process.getOutputStream);
      out.close();
    }

    return this;
  }

  /** Redirects output from the process to the given output stream */
  def >  (outstream : OutputStream) : PipeProcess = {
    this.stdout = outstream;

    spawn {
      val waitForStdin  = future { drain(process.getInputStream, stdout); }
      val waitForStderr = future { drain(process.getErrorStream, stderr); }

      waitForStdin();
    }

    return this;
  }

  /** Redirects stdout and stderr from the process to the given output stream */
  def >& (outstream : OutputStream) : PipeProcess = {
    this.stdout = outstream;
    this.stderr = outstream;

    spawn {
      val waitForStdin  = future { drain(process.getInputStream, stdout); }
      val waitForStderr = future { drain(process.getErrorStream, stderr); }

      waitForStdin();
      waitForStderr();
    }

    return this;
  }

  /** Pipes to a function that accepts an InputStream. */
  def |[T](func : (InputStream => T)) : T =
    func(process.getInputStream);
}

/**
 * An alternative richer InputStream that can be piped to an OutputStream,
 * Process, or function.
 * 
 * @author dramage
 */
class PipeInputStream(var stream : InputStream) {
  import PipeProcess.drain;

  /**
   * Pipe to an OutputStream.  Returns when all bytes have been
   * written to out.  Does not close out.
   */
  def |(out : OutputStream) : Unit = {
    drain(stream, out);
  }

  /**
   * Pipe to Process, returning that Process instance.  Returns
   * immediately.  Spawns a background job to write all bytes
   * from the incoming stream to the process.
   */
  def |(process : PipeProcess) : PipeProcess = {
    process < stream;
  }

  /** Pipes to a function that accepts an InputStream. */
  def |[T](func : (InputStream => T)) : T =
    func(stream);
}

/**
 * A pipeable iterator of Strings, to be written as lines to a stream.
 */
class PipeIterator(lines : Iterator[String]) {
  /**
   * Writes all lines to the given process.  Returns immediately.
   */
  def |(process : PipeProcess) : PipeProcess = {
    val pipeIn  = new java.io.PipedInputStream();
    val pipeOut = new java.io.PipedOutputStream(pipeIn);
    spawn { this > pipeOut; }
    process < pipeIn;
  }
  
  /**
   * Writes all lines to the given OutputStream, closing it when done
   * if it is not System.out or System.err.
   */
  def >(outstream : OutputStream) = {
    val ps = new java.io.PrintStream(outstream);
    for (line <- lines) {
      ps.println(line);
    }
    
    if (!(outstream == Pipes.stdout || outstream == Pipes.stderr)) {
      ps.close;
    }
  }
}

/**
 * Conext for pipes, holds current working directory
 * 
 * @author dramage
 */
class PipesContext {
  var cwd : File = new File(new File("").getAbsolutePath);
}

class PipesException(message : String) extends RuntimeException(message);

/**
 * Utilities for executing shell scripts, etc.
 * 
 * @author dramage
 */
object Pipes {
  implicit val _context = new PipesContext();
  
  def stdout = java.lang.System.out;
  def stderr = java.lang.System.err;
  def stdin  = java.lang.System.in;
  
  def sh(command : String)(implicit context : PipesContext) : java.lang.Process = {
    val os = System.getProperty("os.name");
    val pb = new ProcessBuilder().directory(context.cwd);
    
    if (os == "Windows 95" || os == "Windows 98" || os == "Windows ME") {
      pb.command("command.exe", "/C", command);
    } else if (os.startsWith("Windows")) {
      pb.command("cmd.exe", "/C", command);
    } else {
      pb.command("/bin/sh", "-c", command);
    };
    
    return pb.start();
  }

  private def error(message : String) : Unit = {
    throw new PipesException(message);
  }

  def pwd(implicit context : PipesContext) : File =
    context.cwd;

  def cd(folder : File)(implicit context : PipesContext) = {
    if (!folder.exists) {
      error("Folder "+folder+" does not exist.");
    } else if (!folder.isDirectory) {
      error("Folder "+folder+" is not a directory");
    } else if (!folder.canRead) {
      error("Cannot access folder "+folder);
    }
    context.cwd = folder;
  }

  def cd(folder : String)(implicit context : PipesContext) : Unit = {
    if (!folder.startsWith(java.io.File.pathSeparator)) {
      cd(new File(context.cwd,folder))(context);
    } else {
      cd(new File(folder))(context);
    }
  }
  
  def waitFor(process : PipeProcess) = process.waitFor;

  //
  //  implicit conversions
  //
  
  implicit def iPipeProcess(process : Process) =
    new PipeProcess(process);
  
  implicit def iPipeInputStream(stream : InputStream) =
    new PipeInputStream(stream);
  
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
  
  implicit def File(path : String) = new java.io.File(path);
  
  implicit def iPipeIterator(lines : Iterator[String]) =
    new PipeIterator(lines.map(_.toString));
  
  implicit def iPipeIterator(lines : Iterable[String]) = new
    PipeIterator(lines.elements.map(_.toString));
  
  def main(argv : Array[String]) {
    sh("sleep 1; echo '(sleep 1 async) prints 2nd'") > System.out;
    sh("echo '(no sleep async) prints 1st'") > System.out;
    waitFor(sh("sleep 2; echo '(sleep 2 sync) prints 3rd after pause'") > System.out);
    sh("echo '(stderr redirect) should show up on stdout' | cat >&2") >& System.out;
    sh("echo '(stderr redirect) should also show up on stdout' | cat >&2") |& sh("cat") > System.out;
    sh("echo '(pipe test line 1) should be printed'; echo '(pipe test line 2) should not be printed'") | sh("grep 1") > System.out;
    sh("echo '(translation test) should sound funny'") | sh("perl -pe 's/(a|e|i|o|u)+/oi/g';") > System.out;
    System.in | sh("egrep '[0-9]'") > System.out;
    (1 to 10).map(_.toString) > System.out;
  }
}
