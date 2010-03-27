package scalanlp.config

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

  protected[config] def getArgumentParser[T:ClassManifest]:Option[ArgumentParser[T]] = {
    if(implicitly[ClassManifest[T]].erasure == classOf[Class[_]]) Some(classParser.asInstanceOf[ArgumentParser[T]])
    else argumentParsers.get(implicitly[ClassManifest[T]].toString).asInstanceOf[Option[ArgumentParser[T]]];
  }
}