package breeze.config
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


import java.io.File

/**
 * ArgumentParsers are used by Configuration objects to process special arguments.
 * You can call ArgumentParser.addArgumentParser() to register a new one. By default,
 * we have one for all primitives, their java boxed variants, strings, and Files.
 *
 * @author dlwh
 */
trait ArgumentParser[T] { outer =>
  def parse(arg:String):T
  def map[U](f: T=>U): ArgumentParser[U] = new ArgumentParser[U] {
    def parse(arg: String) = f(outer.parse(arg));
  }
}

object ArgumentParser {
  implicit val intParser:ArgumentParser[Int] = new ArgumentParser[Int] {
    def parse(arg:String) = arg.toInt;
  }

  import java.{lang=>jl}

  implicit val jlIntegerParser: ArgumentParser[jl.Integer] = intParser.map(x => new jl.Integer(x));

  implicit val doubleParser:ArgumentParser[Double] = new ArgumentParser[Double] {
    def parse(arg:String) = arg.toDouble;
  }

  implicit val jlDoubleParser: ArgumentParser[jl.Double] = doubleParser.map(x => new jl.Double(x));

  implicit val booleanParser:ArgumentParser[Boolean] = new ArgumentParser[Boolean] {
    def parse(arg:String) = arg.toBoolean;
  }

  implicit val jlBooleanParser: ArgumentParser[jl.Boolean] = booleanParser.map(x => new jl.Boolean(x));

  implicit val stringParser:ArgumentParser[String] = new ArgumentParser[String] {
    def parse(arg:String) = arg.toString;
  }

  implicit val classParser: ArgumentParser[Class[_]] = new ArgumentParser[Class[_]] {
    override def parse(arg: String):Class[_] = Class.forName(arg.trim);
  }

  implicit val fileParser: ArgumentParser[File] = new ArgumentParser[File] {
    override def parse(arg: String) = new File(arg.trim);
  }

  implicit def seqParser[T:ArgumentParser]:ArgumentParser[Seq[T]] = new ArgumentParser[Seq[T]] {
    def parse(arg:String) = arg.split(",").map( implicitly[ArgumentParser[T]] parse _).toSeq;
  }

  private val argumentParsers = collection.mutable.HashMap[String,ArgumentParser[_]]()

  def addArgumentParser[T:ClassManifest](ap: ArgumentParser[T]) = {
    argumentParsers += (implicitly[ClassManifest[T]].toString -> ap);
  }

  addArgumentParser(intParser);
  addArgumentParser(doubleParser);
  addArgumentParser(booleanParser);
  addArgumentParser(stringParser);
  addArgumentParser(fileParser);

  protected[config] def getArgumentParser[T:ClassManifest]:Option[ArgumentParser[T]] = {
    if(implicitly[ClassManifest[T]].erasure == classOf[Class[_]]) Some(classParser.asInstanceOf[ArgumentParser[T]])
    else argumentParsers.get(implicitly[ClassManifest[T]].toString).asInstanceOf[Option[ArgumentParser[T]]];
  }
}