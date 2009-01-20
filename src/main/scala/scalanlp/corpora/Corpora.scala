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


import scalax.data.Implicits._;
import scalax.io.Implicits._;
import util.Implicits._;
import java.net._;
import java.io.File;
import scala.xml.XML;

/**
* Class to help load corpora. Each Corpus is represented by a class
* of type "Corpus" which knows how to load the data. 
*
* Corpora are located in one of two ways:
* <ul>
*  <li> Provide a class name for the corpus for a class already in the class path.</li>
*  <li>2) Located through one or more public repositories, usually your
*     MAVEN_HOME and the scalanlp maven repository.
*     
*     Files located in this way must be installed in:
*      /path/to/repository/org/scalanlp/corpora/(corpus name in lower case)/(version)/(corpus-name)-(version).jar
*
*     The jar file itself must have a file named scalanlp-corpus.xml which has the format:
*   <code>
*       <corpus>
*         <classname>absolute classname</classname>
*       </corpus>
*   </code>
*    This class is then created and returned.
*   </li>
* </ul>
*
* @author dlwh
*/
class Corpora(val repositories: Seq[URL]) {
  import Corpora._;
  def this(remoteURL: String) = this( Array(CorpusUtils.defaultCorpusRepo, new URL(remoteURL)));

  /**
  * Create a new instance of the corpus.
  */
  def load[T](clss: Class[U] forSome {type U<:Corpus[T]}) = {
    clss.newInstance;
  }

  /**
  * Either loads the Corpus with the given class name, or searches the repositories for the corpus with the (lowercase) name.
  */
  def load[T](name: String):Corpus[T] = { 
    val clss = cForName[T](name) orElse cForName[T]("scalanlp.corpora."+name);
    clss match {
      case None => locateCorpus(name);
      case Some(c) => load(c.asSubclass(classOf[Corpus[T]]))
    }
  }


  /**
  * Searches for a corpus in the available repositories.
  */
  def locateCorpus[T](name: String) : Corpus[T] = {
    val jarLocation = findJar(name);
    val cl = new URLClassLoader(Array(jarLocation));
    val className = {
      val strm = cl.getResourceAsStream("scalanlp-corpus.xml");
      val xml = XML.load(strm);
      println(xml);
      strm.close();
      (xml \ "classname").text.trim();
    }
    println(className + " " + jarLocation);
    load(cl.loadClass(className).asSubclass(classOf[Corpus[T]]));
  }

  private def findJar(name: String) = {
    val lowerName = name.toLowerCase;
    val urls = repositories.elements;

    var jar: URL = null;
    while(urls.hasNext && (jar eq null)) {
      val url = urls.next;
      try { // is it a directory?
        val f = new File(url.toURI);
        val location = f / "org/scalanlp/corpora/" / lowerName;
        println(location);
        if(location.exists && location.isDirectory) {
          val latestVersion = location.listFiles.filter(_.isDirectory).reduceLeft( (x,y) =>
            new File(lexicographicOrder(x.getName,y.getName))
          );
          println(latestVersion);
          jar = latestVersion.listFiles.filter(_.getName.endsWith(".jar"))(0).toURL
        }
      } catch { // not a file, so we'll hope it's a maven repo
        case _ => 
        val location = url.toString +"/org/scalanlp/corpora/"+lowerName;
        val metadata = new URL(location + "/maven-metadata.xml");
        try {
          val stream = metadata.openStream();
          try {
            val xml = XML.load(stream); 
            val latestVersion = (xml \ "version").text
            jar = new URL(location + "/" + latestVersion + "/"+lowerName + "-"+latestVersion + ".jar");
          } finally {
            stream.close();
          }
        } catch {
          case _ => ();
        } 
      }
    }
    if(jar == null) throw new RuntimeException("Couldn't find the corpus " + name);
    jar
  }


  private def lexicographicOrder(f1: String, f2: String) = {
    if(f1.split(".").zip(f2.split(".")).forall { case (s1,s2) =>
      try {
        s1.toInt > s2.toInt
      } catch {
        case _ => s1 > s2;
      }
    }) f1 else f2;
  }

  private def cForName[T](name: String) = try {
    Some(Class.forName(name).asInstanceOf[Class[Corpus[T]]])
  } catch {
    case _ => None;
  }

}

object Corpora extends Corpora("http://repo.scalanlp.org/repo/") {
}

object CorpusUtils {
  protected[corpora] def defaultCorpusRepo = {
    val repoFile = ( 
      System.getenv("SCALANLP_CORPORA") 
      ?: (System.getenv("MAVEN_HOME") 
        ?: System.getenv("HOME") + "/.m2/") + "repository/");
    new File(repoFile).toURL;
  }

}
