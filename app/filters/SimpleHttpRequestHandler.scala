package filters

/**
  * Created by howen on 16/5/25.
  */

import javax.inject.Inject

import models.{ChessPiece, Message}
import play.api.http._
import play.api.mvc.Results._
import play.api.mvc._
import play.api.routing.Router
import util.JsonUtil

class SimpleHttpRequestHandler @Inject()(router: Router) extends HttpRequestHandler {
  def handlerForRequest(request: RequestHeader) = {
    val result = collection.mutable.Map[String, Object]()
    result.put("message", Message(ChessPiece.BAD_USER_TOKEN.string, ChessPiece.BAD_USER_TOKEN.pointValue))
    if (request.headers == null || request.headers.get("User-Agent").isEmpty || request.headers.get("User-Agent").get.contains("Alibaba.Security")) {
      (request, Action(Ok(JsonUtil.toJson(result))))
    } else {
      result.put("message", Message(ChessPiece.FAILURE_REQUEST_HANDLER_NOT_FOUND.string, ChessPiece.FAILURE_REQUEST_HANDLER_NOT_FOUND.pointValue))
      router.routes.lift(request) match {
        case Some(handler) => (request, handler)
        case None => (request, Action(Ok(JsonUtil.toJson(result))))
      }
    }
  }
}
