/*
 * Distributed as part of ScalaRA, a scientific research tool.
 * 
 * Copyright (C) 2007 Daniel Ramage
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.

 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA 
 */
package scalanlp.pipes;


import java.io.File
import java.io.InputStream
import java.io.OutputStream
import java.lang.Process
import java.lang.ProcessBuilder

import scala.concurrent.ops._

/**
 * Utilities for executing shell scripts and reading from file in
 * a similar way to unix shell piping.  To get started with a global
 * pipes shell, use:
 * 
 * import Pipes.global._
 * 
 * and see examples in the main method.
 * 
 * @author dramage
 */
class Pipes {
  /** Throws an exception with the given errors message. */
  protected def error(message : String) : Unit = {
    throw new PipesException(message);
  }
  
  //
  // state variables
  //

  /** Current directory the java process launched in. */
  protected val _sysCwd : File = new File(new File("").getAbsolutePath);

  /** Current directory for our pipes instance. */
  protected var _cwd : File = _sysCwd;
  protected var _stdout : OutputStream = java.lang.System.out;
  protected var _stderr : OutputStream = java.lang.System.err;
  protected var _stdin  : InputStream  = java.lang.System.in;

  /**
   * Mutable map containing the current environmental variables
   * as seen by invoked processes.  Based initially on the
   * system environment.
   */
  protected val _env : scala.collection.mutable.Map[String,String] = {
    import scala.collection.JavaConversions._;
    scala.collection.mutable.Map() ++= java.lang.System.getenv;
  }
  
  //
  // context properties
  //
  
  /** Returns the default stdout used in this context. */
  def stdout = _stdout;
  
  /** Sets the default stdout used in this context. */
  def setStdout(stream : OutputStream) : Unit = _stdout = stream;
  
  /** Returns the default stderr used in this context. */
  def stderr = _stderr;
  
  /** Sets the default stderr used in this context. */
  def setStderr(stream : OutputStream) : Unit = _stderr = stream;
  
  /** Returns the default stdin used in this context. */
  def stdin  = _stdin;
  
  /** Sets the default stdin used in this context. */
  def setStdin(stream : InputStream) : Unit = _stdin = stream;
  
  //
  // path and directory access and update
  //
  
  /** Returns the current working directory. */
  def cwd : File = _cwd;

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
    if (folder.getAbsolutePath == _sysCwd.getAbsolutePath) {
      _cwd = _sysCwd;
    } else {
      _cwd = folder;
    }
  }

  /**
   * Returns a file referring to the given name.  The returned file
   * is relative to the current directory (cwd) if the path is not absolute.
   */
  implicit def file(path : String) : File = {
    val f = new File(path);
    if (f.isAbsolute) {
      f
    } else if (_cwd == _sysCwd) {
      new File(path);
    } else if (_cwd.getAbsolutePath.startsWith(_sysCwd.getAbsolutePath)) {
      new File(_cwd.getAbsolutePath.substring(_sysCwd.getAbsolutePath.length+1), path);
    } else {
      new File(cwd,path);
    }
  }

  /**
   * Returns a file relative to the given base, which itself is either
   * absolute or relative to the current working directory.
   */
  def file(base : String, path : String) : File =
    file(file(base), path);
  
  /**
   * Returns a file relative to the given base file.  This method is
   * not affected by the current working directory.
   */
  def file(base : java.io.File, path : String) : File =
    new File(base, path);
  
  //
  // environmental variables
  //

  /** An immutable map view of the current system environment. */
  def env : Map[String,String] =
    Map() ++ _env;

  /** Sets the given environmental variable key to the given value. */
  def env(key : String, value : String) =
    _env(key) = value;
  
  /** Returns the current value associated with the given environmental variable. */
  def env(key : String) =
    _env(key);
  
  //
  // process invocation
  //
  
  /**
   * Runs the given command (via the system command shell if found)
   * in the current directory.  Because the system command shell is
   * used to parse the arguments, all standard escaping and quoting
   * mechanims of the system are used to determine how to split
   * the command string into the appropriate arguments for invoking
   * the program.  Uses this instance's environment as the full process
   * execution environment.
   */
  def sh(command : String) : java.lang.Process = {
    val pb = new ProcessBuilder().directory(_cwd);
    
    val m = pb.environment();
    m.clear();
    for ((k,v) <- env) {
      m.put(k,v);
    }

    val os = System.getProperty("os.name");
    if (os == "Windows 95" || os == "Windows 98" || os == "Windows ME") {
      pb.command("command.exe", "/C", command);
    } else if (os.startsWith("Windows")) {
      pb.command("cmd.exe", "/C", command);
    } else {
      pb.command("/bin/sh", "-c", command);
    };
    
    return pb.start();
  }
  
  //
  //  implicit conversions
  //
  
  implicit def iPipeProcess(process : Process) =
    new PipeProcess(process)(this);
  
  implicit def iPipeInputStream(stream : InputStream) =
    new PipeInputStream(stream);
  
  implicit def iPipeInputStream(file : File) =
    new PipeInputStream(iInputStream(file));
  
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
  
  implicit def iPipeIterator(lines : Iterator[String]) =
    new PipeIterator(lines)(this);
  
  implicit def iPipeIterator(lines : Iterable[String]) =
    new PipeIterator(lines.iterator)(this);
}

/**
 * To get started with a global pipes shell, use:
 * 
 * import scalanlp.util.Pipes.global._
 * 
 * And take a look at the example code in the Pipes object's main method.
 */
object Pipes {
  private[pipes] type HasLines = {
    def getLines() : Iterator[String];
  }

  /** A global instance for easy imports */
  val global = Pipes();
  
  def apply() : Pipes = {
    new Pipes();
  }
  
  /** Copy constructor. */
  def apply(ref : Pipes) : Pipes = {
    val pipes = Pipes();
    pipes._cwd    = ref._cwd;
    pipes._stdout = ref._stdout;
    pipes._stderr = ref._stderr;
    pipes._stdin  = ref._stdin;
    
    pipes._env.clear;
    for ((k,v) <- ref._env) {
      pipes._env(k) = v;
    }
    
    pipes;
  }
}

object PipesExample {
  import Pipes.global._;
    
  def main(argv : Array[String]) {
    sh("echo '(no sleep) prints 1st'") | stdout;
    sh("sleep 1; echo '(sleep 1) prints 2nd'") | stdout;
    sh("echo '(stderr redirect) should show up on stdout' | cat >&2") |& stdout;
    sh("echo '(stderr redirect) should also show up on stdout' | cat >&2") |& sh("cat") | stdout;
    sh("echo '(pipe test line 1) should be printed'; echo '(pipe test line 2) should not be printed'") | sh("grep 1") | stdout;
    sh("echo '(translation test) should sound funny'") | sh("perl -pe 's/(a|e|i|o|u)+/oi/g';") | stdout;
    stdin | sh("egrep '[0-9]'") | stdout;
 
    sh("ls") | ((x : String) => x.toUpperCase) | stdout;
    
    (1 to 10).map(_.toString) | stderr;
    
    for (line <- sh("ls").getLines) {
      println(line.toUpperCase);
    }
  }
}

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
    val buffer = new Array[Byte](1024);

    try {
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
    } finally {
      in.close();
    }
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

  /** Piping to a process happens immediately via spawning. */
  def |  (process : Process) : PipeProcess = {
    spawn {
      this | process.getOutputStream;
    }
    return new PipeProcess(process);
  }
  
  /** Piping to a process happens immediately via spawning. */
  def |& (process : Process) : PipeProcess = {
    spawn {
      this |& process.getOutputStream;
    }
    return new PipeProcess(process);
  }

  /** Redirects the given input stream as the source for the process */
  def <  (instream : InputStream) : Process = {
    spawn {
      val out = process.getOutputStream;
      drain(instream, process.getOutputStream);
      out.close();
    }

    return process;
  }
  
  /**
   * Redirects output from the process to the given output stream.
   * Blocks until the process completes.
   */
  def |  (outstream : OutputStream) : Process = {
    this.out = outstream;

    val waitForStdin  = future { drain(process.getInputStream, out); }
    val waitForStderr = future { drain(process.getErrorStream, err); }

    waitForStdin();
    closePipes();
    
    process;
  }

  /**
   * Redirects stdout and stderr from the process to the given output stream.
   * Blocks until the process completes.
   */
  def |& (outstream : OutputStream) : Process = {
    this.out = outstream;
    this.err = outstream;

    val waitForStdin  = future { drain(process.getInputStream, out); }
    val waitForStderr = future { drain(process.getErrorStream, err); }
    
    waitForStdin();
    waitForStderr();
    closePipes();

    process;
  }

  /** Pipes to a function that accepts an InputStream. */
  def |[T](func : (InputStream => T)) : T =
    func(process.getInputStream);
  
  /** Pipes to a function that maps each line to.  */
  def |[T](func : (String => T)) : Iterator[T] =
    for (line <- getLines) yield func(line);
  
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
  def |(process : PipeProcess) : Process =
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
  def |(process : PipeProcess) : Process = {
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
