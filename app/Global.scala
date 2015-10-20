import play.api._
import play.api.mvc.WithFilters


/**
 * Created by handy on 15/10/20.
 * Daumkakao china
 */
object Global extends WithFilters(AccessLoggingFilter) {

  override def onStart(app: Application): Unit = {
    Logger.info("start")
  }

}
