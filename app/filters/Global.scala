package filters

import models.{ChessPiece, Message}
import play.api._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc.Results._
import play.api.mvc._

import scala.concurrent.Future

object Global extends GlobalSettings {

  implicit lazy val messageReads: Reads[Message] = (
    (__ \ "message").read[String] and
      (__ \ "code").read[Int]
    ) (Message)

  implicit lazy val messageWrites: Writes[Message] = (
    (__ \ "message").write[String] and
      (__ \ "code").write[Int]
    ) (unlift(Message.unapply))

  override def onHandlerNotFound(request: RequestHeader) = {
    Logger.error("请求未找到: " + request.host + request.uri + " " + request.remoteAddress + " " + request.headers("User-Agent"))
    Future.successful(NotFound(Json.obj("message" -> Message(ChessPiece.FAILURE_REQUEST_HANDLER_NOT_FOUND.string, ChessPiece.FAILURE_REQUEST_HANDLER_NOT_FOUND.pointValue))))
  }

  override def onBadRequest(request: RequestHeader, error: String) = {
    Logger.error("有错误请求: " + request.host + request.uri + " " + request.remoteAddress + " " + request.headers("User-Agent"))
    Future.successful(NotFound(Json.obj("message" -> Message(ChessPiece.FAILURE_BAD_REQUEST.string, ChessPiece.FAILURE_BAD_REQUEST.pointValue))))
  }

  override def onError(request: RequestHeader, ex: Throwable) = {
    Logger.error("请求出错: " + request.host + request.uri + " " + request.remoteAddress + " " + request.headers("User-Agent"))
    ex.printStackTrace()
    Future.successful(NotFound(Json.obj("message" -> Message(ChessPiece.FAILURE_REQUEST_ERROR.string, ChessPiece.FAILURE_REQUEST_ERROR.pointValue))))
  }
}