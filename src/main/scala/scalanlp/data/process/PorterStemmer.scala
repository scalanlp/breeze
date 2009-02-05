package scalanlp.data.process;

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


/**
 * Converts words to their stemmed form.
 * @author dlwh
 */
object PorterStemmer extends (String=>String){
  def apply(w:String) = {
    if(w.length < 3) w.toLowerCase;
    else {
      val ret = {
        w.toLowerCase.replaceAll("([aeiou])y","$1Y").replaceAll("^y","Y");
      }
      step5(step4(step3(step2(step1(ret))))).toLowerCase;
    }
  }

  private def step1(w:String) = step1c(step1b(step1a(w)));

  // get rid of s's
  private def step1a(w:String) = {
    if(w.endsWith("sses") || w.endsWith("ies")) 
      w.substring(0,w.length-2);
    else if(w.endsWith("s") && w.charAt(w.length-2) != 's') 
      w.substring(0,w.length-1);
    else w
  }

  private def step1b(w:String) = {
    //println(w + " " + m(w));;
    def extra(w:String) = {
      if(w.endsWith("at") || w.endsWith("bl") || w.endsWith("iz") ) w + 'e';
      // double consonant:
      else if(doublec(w) && !("lsz".contains(w.last))) w.substring(0,w.length-1); 
      else if(m(w) == 1 && cvc(w)) w + "e";
      else w;
    }
    
    if(w.endsWith("eed")) {
      if(m(w.substring(0,w.length-3)) > 0)
        w.substring(0,w.length-1);
      else w;
    } else if(w.endsWith("ed")) {
      if(w.findIndexOf(isVowel) < (w.length - 2)) extra(w.substring(0,w.length-2));
      else w;
    } else if(w.endsWith("ing")) {
      if(w.findIndexOf(isVowel) < (w.length - 3)) extra(w.substring(0,w.length-3));
      else w;
    } else w;
  }

  def step1c(w: String) = {
    //println(w + " " + m(w));;
    if( (w.last == 'y' || w.last == 'Y') && w.findIndexOf(isVowel) < w.length -1) {
      w.substring(0,w.length-1) + 'i'
    } else w;
  }

  private def replaceSuffix(w:String, suffix: String, repl: String) = {
    if(w endsWith suffix) Some( (w.substring(0,w.length - suffix.length),repl));
    else None
  }

  private val mgt0 = { (w:(String,String)) => m(w._1) >0}
  private val mgt1 = { (w:(String,String)) => m(w._1) >1}

  private def step2(w:String) = { 
    //println(w + " " + m(w));;
    if(w.length < 3) w
    else {
      val opt = w(w.length-2) match {
        case 'a' => replaceSuffix(w,"ational","ate").orElse(replaceSuffix(w,"tional","tion"));
        case 'c' =>
          replaceSuffix(w,"enci","ence").orElse(replaceSuffix(w,"anci","ance"));
        case 'e' => replaceSuffix(w,"izer","ize");
        case 'g' => replaceSuffix(w,"logi","log");
        case 'l' => replaceSuffix(w,"bli","ble") orElse {
          replaceSuffix(w,"alli","al")
        } orElse {
          replaceSuffix(w,"entli","ent")
        } orElse {
          replaceSuffix(w,"eli","e")
        } orElse {
          replaceSuffix(w,"ousli","ous")
        }
        case 'o' => replaceSuffix(w,"ization","ize") orElse {
          replaceSuffix(w,"ator","ate")
        } orElse {
          replaceSuffix(w,"ation","ate")
        }
        case 's' => replaceSuffix(w,"alism","al") orElse {
          replaceSuffix(w,"iveness","ive")
        } orElse {
          replaceSuffix(w,"fulness","ful")
        } orElse {
          replaceSuffix(w,"ousness","ous")
        }
        case 't' =>
        replaceSuffix(w,"aliti","al") orElse {
          replaceSuffix(w,"iviti","ive")
        } orElse {
          replaceSuffix(w,"biliti","ble")
        }
        case _ => None;
      }
      opt.filter(mgt0).map{ case (a,b) => a+b}.getOrElse(w);
    }
  }

  private def step3(w:String) = {
    //println(w + " " + m(w));;
    if(w.length < 3) w else {
      val opt = w.last match {
        case 'e' =>
        replaceSuffix(w,"icate","ic") orElse {
          replaceSuffix(w,"alize","al")
        } orElse {
          replaceSuffix(w,"ative","")
        }
        case 'i' => replaceSuffix(w,"iciti","ic");
        case 'l' => replaceSuffix(w,"ical","ic").orElse(replaceSuffix(w,"ful",""));
        case 's' => replaceSuffix(w,"ness","");
        case _ => None;
      }
      opt.filter(mgt0).map{ case (a,b) => a+b}.getOrElse(w);
    }
  }

  private def step4(w:String) = {
    //println(w + " " + m(w));;
    if(w.length < 3)
      w 
    else {
      val opt = w(w.length-2) match {
        case 'a' => replaceSuffix(w,"al","");
        case 'c' => replaceSuffix(w,"ance","").orElse(replaceSuffix(w,"ence",""));
        case 'e' => replaceSuffix(w,"er","");
        case 'i' => replaceSuffix(w,"ic","");
        case 'l' => replaceSuffix(w,"able","").orElse(replaceSuffix(w,"ible",""));
        case 'n' => replaceSuffix(w,"ant","") orElse {
          replaceSuffix(w,"ement","")
        } orElse {
          //println("here");
          replaceSuffix(w,"ment","")
        } orElse {
          //println("hereX");
          replaceSuffix(w,"ent","")
        }
        case 'o' => replaceSuffix(w,"ion","").filter(a => a._1.endsWith("t") || a._1.endsWith("s")).
                      orElse(replaceSuffix(w,"ou",""));
        case 's' => replaceSuffix(w,"ism","");
        case 't' => replaceSuffix(w,"ate","").orElse(replaceSuffix(w,"iti",""));
        case 'u' => replaceSuffix(w,"ous","");
        case 'v' => replaceSuffix(w,"ive","");
        case 'z' => replaceSuffix(w,"ize","");
        case _ => None;
      }
      opt.filter(mgt1).map{ case (a,b) => a+b}.getOrElse(w);
    }
  }

  private def step5(w:String) = {
    //println(w + " " + m(w));;
    if(w.length < 3) w else 
    step5b(step5a(w));
  }


  private def step5a(w: String) = {
    if(w.length < 3) w else 
    if(w.last == 'e') {
      val n = m(w);
      if(n > 1) w.substring(0,w.length-1)
      else if(  n == 1 && !cvc(w.substring(0,w.length-1))) w.substring(0,w.length-1)
      else w;
    }
    else {w}
  }

  private def step5b(w:String) = {
    //println(w + " " + m(w));;
    if(w.last == 'l' && doublec(w) && m(w) > 1) w.substring(0,w.length-1)
    else w;
  }

  def m(w:String):Int = {
    val firstV = w.findIndexOf(isVowel);
    if(firstV == -1) 0;
    else {
      var m = 0;
      var x:Seq[Char] = w.substring(firstV);
      if(x.isEmpty) m
      else {
        while(!x.isEmpty) {
          x = x.dropWhile(isVowel);
          if(x.isEmpty) return m;
          m+=1;
          if(m>1) return m; // don't need anything bigger than this.
          x = x.dropWhile(isConsonant);
        }
        m;
      }
    }
  }

  private def cvc(w:String) = {
    w.length > 2 && isConsonant(w.last) && !("wxY" contains w.last) && isVowel(w(w.length-2)) && isConsonant(w.charAt(w.length-3))
  }

  private def doublec(w:String) = {
    (w.length > 2 && w.last == w.charAt(w.length -2) && isConsonant(w.last))
  }

  def isConsonant(letter:Char) = !isVowel(letter);
  def isVowel(letter:Char) = "aeiouy" contains letter;
}
