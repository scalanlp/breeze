package breeze.text.analyze

/**
 * Converts a string into another string with properties of that string
 * Useful for rare or 0 count words
 * @author dlwh
 */
@SerialVersionUID(1L)
object EnglishWordClassGenerator extends Analyzer with Serializable {
  def apply(x: String) = signatureFor(x)

  def signatureFor(word: String) = {
    val sb = new StringBuilder;
    val wlen = word.length();
    val numCaps = (word: Seq[Char]).count(_.isUpper);
    val hasDigit = word.exists(_.isDigit);
    val hasDash = word.contains('-');
    val hasLower = numCaps < wlen;
    val ch0 = word.charAt(0);
    val lowered = word.toLowerCase();
    if (Character.isUpperCase(ch0) || Character.isTitleCase(ch0)) {
      if (numCaps == 1) {
        sb.append("-INITC");
      } else {
        sb.append("-CAPS");
      }
    } else if (!Character.isLetter(ch0) && numCaps > 0) {
      sb.append("-CAPS");
    } else if (hasLower) {
      sb.append("-LC");
    }

    if (hasDigit) {
      sb.append("-NUM");
    }
    if (hasDash) {
      sb.append("-DASH");
    }
    if (lowered.endsWith("s") && wlen >= 3) {
      // here length 3, so you don't miss out on ones like 80s
      val ch2 = lowered.charAt(wlen - 2);
      // not -ess suffixes or greek/latin -us, -is
      if (ch2 != 's' && ch2 != 'i' && ch2 != 'u') {
        sb.append("-s");
      }
    } else if (word.length() >= 5 && !hasDash && !(hasDigit && numCaps > 0)) {
      if (lowered.endsWith("ed")) {
        sb.append("-ed");
      } else if (lowered.endsWith("ing")) {
        sb.append("-ing");
      } else if (lowered.endsWith("ion")) {
        sb.append("-ion");
      } else if (lowered.endsWith("er")) {
        sb.append("-er");
      } else if (lowered.endsWith("est")) {
        sb.append("-est");
      } else if (lowered.endsWith("ly")) {
        sb.append("-ly");
      } else if (lowered.endsWith("ity")) {
        sb.append("-ity");
      } else if (lowered.endsWith("y")) {
        sb.append("-y");
      } else if (lowered.endsWith("al")) {
        sb.append("-al");
      }
    }
    sb.toString;
  }
}