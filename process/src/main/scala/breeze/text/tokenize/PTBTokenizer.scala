/*
 Copyright 2010 David Hall, Daniel Ramage

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
package breeze
package text
package tokenize

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.input._
import scala.collection.mutable.ArrayBuffer
import annotation.switch


/**
 * PTBTokenizer tokenizes sentences into treebank style sentences.
 * Input must be a single sentence.
 *
 * Should be replaced by a real lexer one day, meh.
 *
 * @author dlwh
 */
object PTBTokenizer extends Tokenizer {
  def apply(text: String): Iterable[String] = {
    val out = new ArrayBuffer[String]
    var begTok = 0
    var cur = 0
    var cLast = ' '
    var hasSingleQuote = false

    // adds tok [begTok,cur), advances tok to cur+=1
    def addToken() {
      if(begTok < cur) {
        val str = text.substring(begTok, cur)
        post(str, out, hasSingleQuote)
      }
      skipTok()
      hasSingleQuote = false
    }
    def skipTok() { begTok = cur + 1}

    def currentToken = text.substring(begTok,cur)

    while(cur < text.length) {
      val c = text.charAt(cur)
      val nextChar = if(cur + 1 == text.length) ' ' else text.charAt(cur + 1)
      val nextNextChar = if(cur + 2 >= text.length) ' ' else text.charAt(cur + 2)
      c match {
        case x if x.isSpaceChar =>
          // spaces end tokens
          addToken()
        case '-' if nextChar == '-' =>
          // -- is one character
          addToken()
          out += "--"
          cur += 1
          skipTok()
        case '"' =>
          // " may be a `` or a ''
          addToken()
          if(cLast.isSpaceChar) out += "``"
          else out += "''"
          skipTok()
        case '\'' if nextChar.isSpaceChar =>
          // ' followed by a ' ends a token and is its own token
          addToken()
          out += "'"
          skipTok()
        case '\'' if "sSmMdD".contains(nextChar) && nextNextChar.isSpaceChar =>
          // common contractions
          addToken()
          begTok = cur
        case '\'' =>
          // maybe we handle less common contractions later
          hasSingleQuote = true
        case '!' |  '?' =>
          // always separate '!' and '?'
          addToken()
          out += c.toString
          skipTok()
        case '.' if nextChar == '.' && nextNextChar == '.' =>
          // ellipsis
          addToken()
          out += "..."
          cur += 2
          skipTok()
        case '.' if cur == text.length - 1 || (!nextChar.isLetterOrDigit && cur == text.length - 2) =>
          // if the current token contains a '.' one back it's probably an acronym,
          // add new period as its own token
          if (cur > 1 && text(cur-2) == '.')
           cur += 1
          addToken()
          out += "."
          skipTok()
        case ':' if nextChar == '/' && nextNextChar == '/' =>
          // assume url:

          var endOfURL = text.indexWhere(ch => ch.isSpaceChar || ch == '"', cur)
          val possibleQuote = text.indexOf('"', cur)
          if (possibleQuote < endOfURL && possibleQuote >= 0)
            endOfURL = possibleQuote
          if(endOfURL == -1) {
            cur = text.length
          } else {
            cur = endOfURL
          }
          // add that url
          addToken()
        case c if ":@#&".contains(c) && nextChar.isSpaceChar =>
          // these are only boundaries if the next character is space
          addToken()
          post(c.toString, out, false)
          skipTok()
        case c if ",;$%][(){}<>".contains(c)  =>
          // most symbols are boundaries
          addToken()
          post(c.toString, out, false)
          skipTok()
        case _ =>
      }
      cLast = c
      cur += 1
    }
    addToken()

    out

  }

  private def post(tok: String, out: ArrayBuffer[String], hasSingleQuote: Boolean) = {
    var added = true
    (tok.charAt(0): @switch) match {
      case '(' =>
        out += "-LRB-"
      case ')' =>
        out += "-RRB-"
      case '[' =>
        out += "-LSB-"
      case ']' =>
        out += "-RSB-"
      case '{' =>
        out += "-LCB-"
      case '}' =>
        out += "-RCB-"
      case 'C' =>
        if(tok == "Cannot")  {
          out += "Can"
          out += "not"
        }  else {
          added = false
        }
      case 'c' =>
        if(tok == "cannot")  {
          out += "can"
          out += "not"
        } else {
          added = false
        }
      case 'D' =>
        if(tok == "D'ye")  {
          out += "D'"
          out += "ye"
        } else {
          added = false
        }
      case 'd' =>
        if(tok == "d'ye")  {
          out += "d'"
          out += "ye"
        } else {
          added = false
        }
      case 'G' =>
        if(tok == "Gimme")  {
          out += "Gim"
          out += "me"
        } else if (tok == "Gonna") {
          out += "Gon"
          out += "na"
        } else if (tok == "Gotta") {
          out += "Got"
          out += "ta"
        } else {
          added = false
        }
      case 'g' =>
        if(tok == "gimme")  {
          out += "gim"
          out += "me"
        } else if (tok == "gonna") {
          out += "gon"
          out += "na"
        } else if (tok == "gotta") {
          out += "got"
          out += "ta"
        } else {
          added = false
        }
      case 'L' =>
        if(tok == "Lemme")  {
          out += "Lem"
          out += "me"
        } else {
          added = false
        }
      case 'l' =>
        if(tok == "lemme")  {
          out += "lem"
          out += "me"
        } else {
          added = false
        }
      case 'M' =>
        if(tok == "More'n")  {
          out += "More"
          out += "'n"
        } else {
          added = false
        }
      case 'm' =>
        if(tok == "more'n")  {
          out += "more"
          out += "'n"
        } else {
          out += tok
        }
      case '\'' =>
        if(tok == "'Tis")  {
          out += "'T"
          out += "is"
        } else if (tok == "'Twas") {
          out += "'T"
          out += "was"
        } else if (tok == "'Twere") {
          out += "'T"
          out += "were"
        } else if(tok == "'tis")  {
          out += "'t"
          out += "is"
        } else if (tok == "'twas") {
          out += "'t"
          out += "was"
        } else if (tok == "'twere") {
          out += "'t"
          out += "were"
        } else {
          added = false
        }
      case 'W' =>
        if(tok == "Wanna")  {
          out += "Wan"
          out += "na"
        } else {
          added = false
        }
      case 'w' =>
        if(tok == "wanna")  {
          out += "wan"
          out += "na"
        } else {
          added = false
        }
      case _ =>
        added = false
    }
    // handle contractions
    if(!added && hasSingleQuote) {
      tok.last match {
        case 'l' if(tok.endsWith("'ll"))  =>
          out += tok.substring(0,tok.length-3)
          out += "'ll"
        case 'e' if(tok.endsWith("'re"))  =>
          out += tok.substring(0,tok.length-3)
          out += "'re"
        case 'e' if(tok.endsWith("'ve"))  =>
          out += tok.substring(0,tok.length-3)
          out += "'ve"
        case 't' if(tok.endsWith("n't"))  =>
          out += tok.substring(0,tok.length-3)
          out += "n't"
        case 'L' if(tok.endsWith("'LL"))  =>
          out += tok.substring(0,tok.length-3)
          out += "'LL"
        case 'E' if(tok.endsWith("'RE"))  =>
          out += tok.substring(0,tok.length-3)
          out += "'RE"
        case 'E' if(tok.endsWith("'VE"))  =>
          out += tok.substring(0,tok.length-3)
          out += "'VE"
        case 'T' if(tok.endsWith("N'T"))  =>
          out += tok.substring(0,tok.length-3)
          out += "N'T"
        case _ =>
          out += tok
      }
    } else if (!added) {
      out += tok
    }
  }
}
