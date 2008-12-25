package scalanlp.corpora;

import scala.io._;
import scalanlp.util.JavaCollections._;
import java.net._;
import scala.collection.mutable.ArrayBuffer;

/**
* Represents a corpus of something.
*
* @author dlwh
*/
trait Corpus[+T] {
  def name : String;
  def splits : Map[String,Seq[T]];
  def license = "Unknown";
  def author = "Unknown";
}


/**
* Pretty useful for creating a corpus from a jar file where the
* resources are packaged in the jar file. Simply specify a mapFun
* to transform the data and a listing of which prefixes we care about.
*
* Those prefixes/directories are used as the entries to the maps.
*
* @author dlwh
*/
trait JarCorpus[+T] extends Corpus[T] {
  protected def directories: Array[String];
  protected def mapFun(path:String, s: scala.io.Source): T

  def splits = {
    val m = scala.collection.mutable.Map[String,ArrayBuffer[T]]();
    val jarURL = new URL("jar:"+classLocation).openConnection.asInstanceOf[JarURLConnection]; 
    val jarFile = jarURL.getJarFile();
    for(entry <- jarFile.entries;
        directory <- directories.find(entry.getName startsWith _ )) {
      val t = mapFun(entry.getName,Source.fromInputStream(jarFile.getInputStream(entry)));
      m.getOrElseUpdate(directory,new ArrayBuffer[T]) += t;
    }
    Map[String,Seq[T]]() ++ m;
  }

  protected lazy val classLocation = {
    this.getClass.getProtectionDomain.getCodeSource.getLocation;
  }
}
