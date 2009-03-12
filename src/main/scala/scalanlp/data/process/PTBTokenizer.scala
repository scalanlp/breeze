package scalanlp.data.process

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.lexical._;
import scala.util.parsing.input._;

/**
* PTBTokenizer tokenizes sentences into treebank style sentences.
* Input must be a single sentence.
*/
class PTBTokenizer extends StdLexical with ImplicitConversions with Scanners {
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
                                                                 SL(pref.mkString("") + d + tail.projection.map(_.chars).mkString(""))
                                                             }
  }


  private def words:Parser[List[Token]] = (
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
    | '"' ~ words ~ opt('"')               ^^ {case '"' ~ words ~ Some('"') =>
                                                 SL("``") :: (words ++  List(SL("''")))
                                               case '"' ~ words ~ None =>
                                                 SL("``") :: words
                                              }
    // ellipsis
    | '.' ~> '.' ~> '.' ~> words           ^^ {words => SL("...") :: words }
    | word ~ posessiveLike ~ (ws ~> words) ^^ {case w ~ x ~ words => w :: SL("'"+x) :: words}
    | word ~ '\'' ~ ws ~ words             ^^ {case w ~ '\'' ~ _ ~ words => w :: SL("'") :: words}
    | number ~ (opt(ws) ~> words)          ^^ {case x ~ words => x :: words}
    | symbol ~ (opt(ws) ~> words)          ^^ {case x ~ words => SL(""+x) :: words}
    | word ~ contraction ~ ws ~ words      ^^ {case word ~ cont ~ _ ~ rest => word :: cont :: rest }
    | word ~ '.' ~ opt(ws ~> words)        ^^ {case word ~ _ ~ None => 
                                                  word :: SL(".") :: Nil
                                               case word ~ _ ~ Some(Nil) =>
                                                   word :: SL(".") :: Nil
                                               case word ~ _ ~ Some(x) =>
                                                   SL(word.chars + ".") :: x
                                              }
    | word ~ '.' ~ (accept('[')|']'|')'|'}'|'>'|'"'|'\'') ~ ws ~ words ^^ {case w ~ '.' ~ x ~ _ ~ words => w :: SL(".") :: SL(""+x) :: words }
    | word ~ (ws ~> words)                 ^^ {case w ~ words =>  w :: words }
    | ws ^^ {x => Nil}
  )

  private def posessiveLike = '\'' ~> elem("something","smdSMD" contains _);

  private def seg(s: String, m1: String, m2: String) = {
    val init :Parser[Char]= accept(s(0).toLowerCase) | s(0).toUpperCase;
    val rest = acceptSeq(s.drop(1));
    init ~ (rest ~> ws ~> words) ^^ { case i ~ words =>  SL(i + m1) :: SL(m2) :: words }
  }

  private def seg(s: String, m1: String, m2: String, m3:String) = {
    val init : Parser[Char] = accept(s(0).toLowerCase) | accept( s(0).toUpperCase);
    val rest = acceptSeq(s.drop(1));
    init ~ (rest ~> words) ^^ { case i ~ words =>  SL(i + m1) :: SL(m2) :: SL(m3) :: words }
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
