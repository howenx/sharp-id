package filters

/**
  * 用户token验证
  * Created by howen on 16/3/16.
  */

import models.JsonConstModel._
import models.{ChessPiece, Message, UserJsResult}
import net.spy.memcached.MemcachedClient
import play.api.Logger
import play.api.libs.json._
import play.api.mvc.Results._
import play.api.mvc._
import utils.JsonUtil

import scala.concurrent._
class Authentication(cache_client: MemcachedClient) {

  class AuthenticatedRequest[A](val username: String, request: Request[A]) extends WrappedRequest[A](request)

  object Authenticated extends ActionBuilder[AuthenticatedRequest]  {

    val result = collection.mutable.Map[String, Object]()
    result.put("message", Message(ChessPiece.BAD_USER_TOKEN.string, ChessPiece.BAD_USER_TOKEN.pointValue))

    override def invokeBlock[A](request: Request[A], block: (AuthenticatedRequest[A]) => Future[Result]): Future[Result] = {

      request.headers.get("id-token").map { token =>
        val id_token = cache_client.get(request.headers.get("id-token").get)
        val m: JsResult[UserJsResult] =Json.fromJson(Json.parse(id_token.toString))(userJsResultReads)
//        Logger.error(s"测试：$m")
//        val user_id = Json.parse(id_token.toString).\("id").asOpt[String]
        if (m.asOpt.isDefined) {
          block(new AuthenticatedRequest(m.asOpt.get.id, request))
        } else {
          Future.successful(Ok(JsonUtil.toJson(result)))
        }
      } getOrElse {
        Future.successful(Ok(JsonUtil.toJson(result)))
      }
    }
  }

}
