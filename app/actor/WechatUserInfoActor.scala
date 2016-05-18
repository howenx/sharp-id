package actor

import java.io.ByteArrayInputStream
import javax.inject.{Inject, Named}

import akka.actor.{Actor, ActorRef}
import models.{IdThree, UserInfoModel, UserOpen}
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import play.api.libs.ws.WSClient
import play.api.{Configuration, Logger}
import util.SysParUtil

import scala.concurrent.ExecutionContext.Implicits.global

/**
  * 微信用户获取微信用户信息并更新用户信息
  * Created by howen on 16/3/30.
  */
case class WechatUser(
                       userId: Long,
                       accessToken: String,
                       openId: String,
                       unionId: String,
                       idType:String
                     )

class WechatUserInfoActor @Inject()(@Named("oss") oss: ActorRef, ws: WSClient, configuration: Configuration) extends Actor {


  override def receive = {
    case wechatUser: WechatUser =>
      if (wechatUser.accessToken!=null && wechatUser.idType.matches("WO|W")){
        ws.url(SysParUtil.WECHAT_USER_INFO + "access_token=" + wechatUser.accessToken + "&openid=" + wechatUser.accessToken + "&lang=zh_CN").get().map(wsResponse => {

          Logger.info("获取微信用户信息返回--->" + wsResponse.json.toString())

          val nickname: Option[String] = wsResponse.json.\("nickname").asOpt[String]
          val gender: Option[String] = wsResponse.json.\("sex").asOpt[String] match {
            case Some("1") => Some("M")
            case Some("2") => Some("F")
            case _ => None
          }

          val idArea: Option[String] = if (wsResponse.json.\("country").asOpt[String].isDefined && wsResponse.json.\("country").asOpt[String].get.equals("CN"))
            Option(wsResponse.json.\("country").asOpt[String].getOrElse("") + " " + wsResponse.json.\("province").asOpt[String].getOrElse("") + " " + wsResponse.json.\("city").asOpt[String].getOrElse(""))
          else None
          val headimgurl: Option[String] = wsResponse.json.\("headimgurl").asOpt[String]

          var userOpen: UserOpen = UserOpen(Some(wechatUser.userId), nickname, None, None, gender, None, None, None, None, None, None, None, None, None, None, None, None, Some("W"), Some(wechatUser.openId), idArea, None, None, None, None)

          if (headimgurl.isDefined) {
            val keyA = "users/photo" + "/" + DateTimeFormat.forPattern("yyyyMMdd").print(new DateTime) + "/" + System.currentTimeMillis + scala.util.Random.nextInt(6) + ".jpg"
            userOpen = UserOpen(Some(wechatUser.userId), nickname, None, None, gender, None, Some(keyA), None, None, None, None, None, None, None, None, None, None, Some("W"), Some(wechatUser.openId), idArea, None, None, None, None)
            getImg(headimgurl.get, keyA)
          }

          if (UserInfoModel.updateUser(userOpen) > 0) {
            Logger.info("Wechat user update personal info: " + userOpen.userId.get)
          } else {
            Logger.error("Wechat user update error")
          }
        })
      }
      val idThree: IdThree = IdThree(None, Some(wechatUser.openId), Some(wechatUser.idType), Some(wechatUser.userId), Some(wechatUser.unionId))
      if (UserInfoModel.id_three_insert(idThree) > 0) {
        Logger.info("Id three insert success : " + idThree.userId.get)
      } else {
        Logger.info("Id three insert error : " + idThree.userId.get)
      }

  }

  def getImg(headimgurl: String, keyA: String) = {
    ws.url(headimgurl).get().map(wsResponse => {
      val isa = new ByteArrayInputStream(wsResponse.bodyAsBytes)
      oss ! OSSIS(isa, keyA, wsResponse.bodyAsBytes.length)
    })
  }
}
