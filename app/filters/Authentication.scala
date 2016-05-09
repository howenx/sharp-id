package filters

/**
  * 用户token验证
  * Created by howen on 16/3/16.
  */

import models.JsonConstModel._
import models.{ChessPiece, Message, UserJsResult}
import net.spy.memcached.MemcachedClient
import play.api.libs.json._
import play.api.mvc.Results._
import play.api.mvc._
import utils.JsonUtil

import scala.concurrent._

class Authentication(cache_client: MemcachedClient) {

  class AuthenticatedRequest[A](val userId: Long, val token: String, request: Request[A]) extends WrappedRequest[A](request)

  object Authenticated extends ActionBuilder[AuthenticatedRequest] {

    val result = collection.mutable.Map[String, Object]()
    result.put("message", Message(ChessPiece.BAD_USER_TOKEN.string, ChessPiece.BAD_USER_TOKEN.pointValue))

    override def invokeBlock[A](request: Request[A], block: (AuthenticatedRequest[A]) => Future[Result]): Future[Result] = {

      if (request.headers.get("User-Agent").contains("Alibaba.Security")) {
        Future.successful(Ok(JsonUtil.toJson(result)))
      } else {
        request.headers.get("id-token").map { token =>

          val id_token = cache_client.get(token)
          if (id_token != null) {
            val m: JsResult[UserJsResult] = Json.fromJson(Json.parse(id_token.toString))(userJsResultReads)
            if (m.asOpt.isDefined) {
              block(new AuthenticatedRequest(m.asOpt.get.id, token, request))
            } else {
              Future.successful(Ok(JsonUtil.toJson(result)))
            }
          } else {
            Future.successful(Ok(JsonUtil.toJson(result)))
          }
        } getOrElse {
          Future.successful(Ok(JsonUtil.toJson(result)))
        }
      }
    }
  }

}
