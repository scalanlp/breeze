package breeze.util

import org.slf4j.LoggerFactory

/**
 * Stupid Typesafe logging lib trait isn't serializable. This is just a better version.
 *
 * @author dlwh
 **/
trait SerializableLogging extends Serializable {
  @transient @volatile
  private var _the_logger: LazyLogger = null

  protected def logger: LazyLogger = {
    var logger = _the_logger
    if(logger eq null) {
      synchronized {
        logger = _the_logger
        if(logger eq null) {
          val ll = new LazyLogger(LoggerFactory.getLogger(this.getClass))
          _the_logger = ll
          logger = ll
        }
      }
    }
    logger
  }


}
