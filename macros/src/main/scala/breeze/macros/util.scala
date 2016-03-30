package breeze.macros

import java.io.File

object util {

  implicit class FileMacro(val sc: StringContext) extends AnyVal {

    def file(args: Any*): File = new File(sc.s(args: _*))
  }

}
