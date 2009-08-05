package scalanlp.data.process

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._;
import scala.util.parsing.input._;

/**
* PTBTokenizer tokenizes sentences into treebank style sentences.
* Input must be a single sentence.
*/
object PTBTokenizer extends StdLexical with ImplicitConversions with Scanners {
  /**
  * Tokenize the input sentence using the PTBTokenizer.
  * Returns Left(List(tokens)) on success, and Right(error) on failure
  */
  def tokenize(input: String): Either[List[String],ParseResult[List[Token]]] = {
      phrase(words)(new CharSequenceReader(input)) match {
        case Success(result, _) => Left(result.map(_.chars))
        case x => Right(x);
      }
  }

  /**
  * @see tokenize
  */
  def apply(input: String) = tokenize(input);

  private def word:Parser[Token] = ( (letter) ~ rep((letter))^^{ case x ~ y=> StringLit( (x::y).mkString("")) } ) | number;

  private def number: Parser[Token] = {
    ( rep(accept('$')|'.'|'/'|'-') ~ digit ~ rep(number)) ^^ { case pref ~ d ~ tail => 
                                                                 SL(pref.mkString("") + d + tail.view.map(_.chars).mkString(""))
                                                             }
  }

  private def tokens:Parser[List[Token]] = (
      seg("cannot","an","not") 
    | seg("d'ye","'","ye") 
    | seg("gimme","im","me")
    | seg("gonna","on","na")
    | seg("gotta","ot","ta")
    | seg("Lemme","em","me")
    | seg("more'n","ore","'n")
    | seg("'tis","t","is")
    | seg("'Tis","T","is")
    | seg("Wanna","an","na")
    | seg("Whaddya","ha","dd","ya")
    | seg("Whatcha","ha","t","cha")
    | '.' ~> '.' ~> '.'              ^^ { _ => List(SL("..."))}
    | word ~ posessiveLike           ^^ {case w ~ xs => List(w, SL("'" + xs))}
    | word ~ '\''                    ^^ {case w ~ '\'' => List(w,SL("'"))}
    | number                         ^^ {case x => List(x) }
    | symbol                         ^^ {case x => List(SL(""+x))}
    | word ~ contraction             ^^ {case word ~ cont => List(word, cont) }
    | title <~ '.'                   ^^ {case word => List(SL(word + ".")) }
    | word <~ '.'                    ^^ {case word => List(word,SL(".")) }
    | word                           ^^ {case word => List(word) }
  )



  private def words:Parser[List[Token]] = (
    '"' ~ words ~ opt('"')               ^^ {case '"' ~ words ~ Some('"') =>
                                                 SL("``") :: (words ++  List(SL("''")))
                                               case '"' ~ words ~ None =>
                                                 SL("``") :: words
                                              }
    //| word ~ '.' ~ (accept('[')|']'|')'|'}'|'>'|'"'|'\'') ~ ws ~ words ^^ {case w ~ '.' ~ x ~ _ ~ words => w :: SL(".") :: SL(""+x) :: words }
    | repsep(tokens,ws)                    ^^ { _.foldLeft[List[Token]](Nil)(_++_) }
    | ws ^^ {x => Nil}
  )

  private def posessiveLike = '\'' ~> elem("something","smdSMD" contains _);

  private def title: Parser[String] = {
    acceptSeq("Dr") | acceptSeq("Mr") | acceptSeq("Prof") | acceptSeq("Mrs") | acceptSeq("Ms") 
  } ^^ { _.mkString("") }

  private def seg(s: String, m1: String, m2: String) = {
    val init :Parser[Char]= accept(s(0).toLowerCase) | s(0).toUpperCase;
    val rest = acceptSeq(s.drop(1));
    init <~ rest ^^ { case i  =>  List(SL(i + m1), SL(m2)) }
  }

  private def seg(s: String, m1: String, m2: String, m3:String) = {
    val init : Parser[Char] = accept(s(0).toLowerCase) | accept( s(0).toUpperCase);
    val rest = acceptSeq(s.drop(1));
    init <~ rest ^^ { case i => List(SL(i + m1), SL(m2), SL(m3)) }
  }


  // convenience
  private def ws = whitespace;
  private def SL(x:String) = StringLit(x);

  private def symbol = elem("sym",x => !x.isLetter && !x.isDigit && x > ' ') 

  private def contraction = { 
   'n' ~ ('\'' ~ 't') |
   '\'' ~ { 
	    'l' ~'l' |
	    'r' ~ 'e' |
	    'v' ~ 'e' |
	    'L' ~ 'L' |
	    'R' ~ 'E' |
	    'V' ~ 'E'
   }
  } ^^ { case x ~ (y ~ z) => StringLit("" + x + y)}

}
