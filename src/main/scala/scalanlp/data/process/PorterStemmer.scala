package scalanlp.data.process;

object PorterStemmer extends (String=>String){
  def apply(w:String) = {
    if(w.length < 3) w.toLowerCase;
    else {
      val ret = {
        val w2 = w.toLowerCase;
        if(w2(0) == 'y') 'Y' + w2.substring(1) else w2;
      }
      (step1 andThen step2 andThen step3 andThen step4 andThen step5)(ret);
    }
  }

  private def step1(w:String) = step1c(step1b(step1a(w)));

  private def step1a(w:String) = {
    if(w endsWith "sses" || w endsWith "ies") 
      w.substring(0,w.length-2);
    else if(w endsWith 's' && w.charAt(w.length-1) != 's') 
      w.substring(0,w.length-1);
    else w
  }

  private def step1b(w:String) = {
    def extra(w:String) = {
      if(w endsWith "at" || w endsWith "bl" || w endsWith "iz" ) w + 'e';
      // double consonant:
      else if(w.last == w.charAt(w.length -2) 
          && isConsonant(w.last) 
          && ("lsz" indexOf w.last) == -1) w.substring(0,w.length-1); 
      else if(m(w) == 1 && isCVC(w)) w + "e";
      else w;
    }
    
    if(w endsWith "eed") {
      if(m(word.substring(0,w.length-3)) > 0)
        w.substring(0,w.length-1);
      else w;
    } else if(w endsWith "ed") {
      if(w.findIndexOf(isVowel) < (w.length - 1)) extra(w.substring(w,w.length-2));
      else w;
    } else if(w endsWith "ing") {
      if(w.findIndexOf(isVowel) < (w.length - 1)) extra(w.substring(w,w.length-3));
      else w;
    } else w;
  }

  private def step1c(w: String) = {
    if(w endsWith 'y' && w.findIndexOf(isVowel) < w.length -1) {
      w.substring(0,w.length-1) + 'i'
    } else w;
  }

  private def replaceSuffix(w:String, suffix: String, repl: String) = {
    if(w endsWith suffix) Right(w.substring(0,w.length - suffix.length) + repl);
    else Left(w);
  }

  @inline 
  private def prefixHasProperty(w:String, suffix: String, repl:String, prop: (String=>Boolean)) = {
    if(w endsWith suffix) {
      val prefix =  w.substring(0,w.length - suffix.length);
      if(prop(prefix)) Right(prefix+ repl) else Left(w)
    }
    else Left(w)
  }

  private val mgt0 = { (w:String) => m(w) >0}
  private val mgt1 = { (w:String) => m(w) >1}

  private def step2(w:String) = {
    w(w.length-2) match {
      case 'a' => 
        val e =
          for(l <- prefixHasProperty(w,"ational","ate",mgt0).left;
              l1 <- prefixHasProperty(w,"tional","tion",mgt0).left)
              yield l1;
        Either.merge(e);
      case 'c' =>
        val e =
          for(l <- prefixHasProperty(w,"enci","ence",mgt0).left;
              l1 <- prefixHasProperty(w,"anci","ance",mgt0).left)
              yield l1;
        Either.merge(e);
      case 'e' =>
        Either.merge(prefixHasProperty(w,"izer","ize",mgt0).left)
      case 'g' =>
        Either.merge(prefixHasProperty(w,"logi","log",mgt0).left)
      case 'l' =>
        val e =
          for(l <- prefixHasProperty(w,"bli","ble",mgt0).left;
              l1 <- prefixHasProperty(w,"alli","al",mgt0).left;
              l2 <- prefixHasProperty(w,"entli","ent",mgt0).left;
              l3 <- prefixHasProperty(w,"eli","e",mgt0).left;
              l4 <- prefixHasProperty(w,"ousli","ous",mgt0).left)
              yield l4;
        Either.merge(e);
      case 'o' =>
        val e =
          for(l <- prefixHasProperty(w,"ization","ize",mgt0).left;
              l1 <- prefixHasProperty(w,"ator","ate",mgt0).left;
              l2 <- prefixHasProperty(w,"ation","ate",mgt0).left)
              yield l2;
        Either.merge(e);
      case 's' =>
        val e =
          for(l <- prefixHasProperty(w,"alism","al",mgt0).left;
              l1 <- prefixHasProperty(w,"iveness","ive",mgt0).left;
              l2 <- prefixHasProperty(w,"fulness","ful",mgt0).left;
              l3 <- prefixHasProperty(w,"ousness","ous",mgt0).left)
              yield l3;
        Either.merge(e);
      case 't' =>
        val e =
          for(l <- prefixHasProperty(w,"aliti","al",mgt0).left;
              l1 <- prefixHasProperty(w,"iviti","ive",mgt0).left;
              l2 <- prefixHasProperty(w,"biliti","ble",mgt0).left)
              yield l2;
        Either.merge(e);
      case _ => w;
    }
  }

  private def step3(w:String) = {
    w.last match {
      case 'e' => 
        val e =
          for(l <- prefixHasProperty(w,"icate","ic",mgt0).left;
              l0 <- prefixHasProperty(w,"alize","al",mgt0).left;
              l1 <- prefixHasProperty(w,"ative","",mgt0).left)
              yield l1;
        Either.merge(e);
      case 'i' => 
        val e =
          for(l <- prefixHasProperty(w,"iciti","ic",mgt0).left;
              yield l;
        Either.merge(e);
      case 'l' => 
        val e =
          for(l <- prefixHasProperty(w,"ical","ic",mgt0).left;
              l0 <- prefixHasProperty(w,"ful","",mgt0).left)
              yield l0;
        Either.merge(e);
      case 's' => 
        val e =
          for(l <- prefixHasProperty(w,"ness","",mgt0).left)
              yield l;
        Either.merge(e);
      case _ => w;
  }

  private def step4(w:String) = {
    w(w.length-1) match {
      case 'a' => 
        val e =
          for(l <- prefixHasProperty(w,"al","",mgt1).left)
              yield l;
        Either.merge(e);
      case 'c' => 
        val e =
          for(l0 <- prefixHasProperty(w,"ance","",mgt1).left;
              l <- prefixHasProperty(w,"ence","",mgt1).left)
              yield l;
        Either.merge(e);
      case 'e' => 
        val e =
          for(l <- prefixHasProperty(w,"er","",mgt1).left)
              yield l;
        Either.merge(e);
      case 'i' => 
        val e =
          for(l <- prefixHasProperty(w,"ic","",mgt1).left)
              yield l;
        Either.merge(e);
      case 'l' => 
        val e =
          for(l <- prefixHasProperty(w,"able","",mgt1).left;
              l1 <- prefixHasProperty(w,"ible","",mgt1).left)
              yield l1;
        Either.merge(e);
      case 'n' =>
        val e =
          for(l2 <- prefixHasProperty(w,"ant","",mgt1).left;
              l1 <- prefixHasProperty(w,"ement","",mgt1).left;
              l3 <- prefixHasProperty(w,"ment","",mgt1).left;
              l <- prefixHasProperty(w,"ent","",mgt1).left)
              yield l;
        Either.merge(e);
      case 'o' =>
        val e =
          for(l2 <- prefixHasProperty(w,"ion","",mgt1).left;
              l <- prefixHasProperty(w,"ou","",mgt1).left)
              yield l;
        Either.merge(e);
      case 's' =>
        val e =
          for(l <- prefixHasProperty(w,"ism","",mgt1).left)
              yield l;
        Either.merge(e);
      case 't' =>
        val e =
          for(l2 <- prefixHasProperty(w,"ate","",mgt1).left;
              l <- prefixHasProperty(w,"iti","",mgt1).left)
              yield l;
        Either.merge(e);
      case 'u' =>
        val e =
          for(l <- prefixHasProperty(w,"ous","",mgt1).left)
              yield l;
        Either.merge(e);
      case 'v' =>
        val e =
          for(l <- prefixHasProperty(w,"ive","",mgt1).left)
              yield l;
        Either.merge(e);
      
      
      
      
    }
  }
}
