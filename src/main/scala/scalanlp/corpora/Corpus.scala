package scalanlp.corpora;

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


import scala.io._;
import java.io.Reader;
import scalanlp.util.JavaCollections._;
import java.net._;
import java.io._;
import scala.collection.mutable.ArrayBuffer;

/**
* Represents a corpus of something.
*
* @author dlwh
*/
trait Corpus[+T] {
  def name : String;
  /** Splits could be "train" "test", or "small", "large", or whatever. */
  def splits : Map[String,Seq[T]];
  def license = "Unknown";
  def author = "Unknown";
}


/**
* Pretty useful for creating a corpus from a jar file where the
* resources are packaged in the jar file. Simply specify a mapFun
* to transform the data and and put a "data/categories" file in the 
* jar that has "splitName resourceName" pairs separated by a space,
* where the resourceName points to another resource with a list of
* files to be used in that split. It's easier than it sounds.
*
* @author dlwh
*/
trait JarCorpus[+T] extends Corpus[T] {
  protected def mapFun(category:String, path:String, s: Reader):T

  // Utility to read "k v" pairs from a resource.
  protected def stringsFromFile(cl: ClassLoader, s : String) = {
    val strm = cl.getResourceAsStream(s)
    val src = Source.fromInputStream(strm);
    val result = (for{ line <- src.getLines();
                      trimmed = line.trim} 
                      yield trimmed).toSeq;
    strm.close();
    result;
  }

  // place for top level categories files
  protected def categoryFile: String = "data/categories";

  /**
  * Provides a list of (categoryName,resourceName), where the
  * categoryName is the name of the split (like "train") and the
  * resourceName is a resource path that has a list of the files
  * to load.
  */
  protected def categories: Seq[(String,String)] = stringsFromFile(classLoader, categoryFile) map { x=>
    val res = x.split(" ");
    (res(0),res(1));
  };

  lazy val splits = {
    Map[String,Seq[T]]() ++ categories.map { case(cat,res) =>
      val paths = stringsFromFile(classLoader,res).view;
      val data = for{ path <- paths;
                      strm = classLoader.getResourceAsStream(path)
                    } yield {
                      val result = mapFun(cat,path,new BufferedReader(new InputStreamReader(strm)));
                      strm.close();
                      result;
                    }
      (cat,data)
    }
  }

  protected lazy val classLocation = {
    this.getClass.getProtectionDomain.getCodeSource.getLocation;
  }

  protected lazy val classLoader = {
    this.getClass.getClassLoader;
  }
}
