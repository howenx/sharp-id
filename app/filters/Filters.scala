package filters

import javax.inject.Inject

import play.api.http.HttpFilters

/**
 * Created by handy on 15/10/23.
 * Daumkakao china
 */
class Filters @Inject()(log: LoggingTimeFilter) extends HttpFilters {
  var filters = Seq(log)

}
