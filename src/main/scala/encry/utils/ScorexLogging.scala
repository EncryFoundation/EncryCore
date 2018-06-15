package encry.utils

import com.typesafe.scalalogging.StrictLogging

trait ScorexLogging extends StrictLogging {
  @inline protected def log = logger
}
