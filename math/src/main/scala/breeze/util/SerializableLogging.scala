package breeze.util

import org.slf4j.LoggerFactory
import com.typesafe.scalalogging.slf4j.Logger


/**
 * Stupid Typesafe logging lib trait isn't serializable. This is just a better version.
 *
 * @author dlwh
 **/
trait SerializableLogging extends Serializable {
  @transient @volatile
  private var _the_logger:Logger = null

  def logger: Logger = {
    var logger = _the_logger
    if(logger eq null) {
      synchronized {
        logger = _the_logger
        if(logger eq null) {
          val ll = Logger(LoggerFactory.getLogger(this.getClass))
          _the_logger = ll
          logger = ll
        }
      }
    }
    logger
  }


}
