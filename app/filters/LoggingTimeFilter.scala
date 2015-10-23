package filters

import play.api.Logger
import play.api.mvc.{Result, RequestHeader, Filter}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * Created by handy on 15/10/23.
 * Daumkakao china
 */
class LoggingTimeFilter extends Filter {

  def apply(nextFilter: RequestHeader => Future[Result]) (requestHeader: RequestHeader): Future[Result] = {

    val startTime = System.currentTimeMillis

    nextFilter(requestHeader).map { result =>

      val endTime = System.currentTimeMillis
      val requestTime = endTime - startTime

      Logger.info(s"${requestHeader.method} ${requestHeader.uri} " + s"took ${requestTime}ms from ${requestHeader.remoteAddress} and returned ${result.header.status}")

      result.withHeaders("Request-Time" -> requestTime.toString)
    }
  }
}
