package actor

import java.io.ByteArrayInputStream
import javax.inject.{Inject, Named}

import akka.actor.{Actor, ActorRef}
import models.{IdThree, UserInfoModel, UserOpen}
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import play.api.libs.ws.WSClient
import play.api.{Configuration, Logger}
import util.SysParUtil._
import util.SnsSigCheck._
import util.{SnsSigCheck, SysParUtil}

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
                       idType: String
                     )

class WechatUserInfoActor @Inject()(@Named("oss") oss: ActorRef, ws: WSClient, configuration: Configuration) extends Actor {


  override def receive = {
    case wechatUser: WechatUser =>

      Logger.error("所有数据:--->" + wechatUser.toString)

      if (wechatUser.accessToken != null && wechatUser.idType.matches("WO|W")) {
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

          var userOpen: UserOpen = UserOpen(Option(wechatUser.userId), nickname, None, None, gender, None, None, None, None, None, None, None, None, None, None, None, None, Option("W"), Option(wechatUser.openId), idArea, None, None, None, None)

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
      } else if (wechatUser.accessToken != null && wechatUser.idType.matches("Q")) {
        qqGetInfo(wechatUser)
      }



      val idThree: IdThree = IdThree(None, Option(wechatUser.openId), Option(wechatUser.idType), Option(wechatUser.userId), Option(wechatUser.unionId))
      if (UserInfoModel.id_three_insert(idThree).isDefined) {
        Logger.info("Id three insert success : " + idThree.userId.get)
      } else {
        Logger.info("Id three insert error : " + idThree.userId.get)
      }

  }

  def qqGetInfo(wechatUser: WechatUser) = {
    // 指定OpenApi Cgi名字
    val scriptName: String = QQ_GET_INFO

    // 指定HTTP请求协议类型
    val protocol: String = "http"

    // 签名密钥
    val secret: String = QQ_SECRET + "&"

    val method: String = "GET"

    val serverName: String = QQ_SEVER

    // 填充URL请求参数
    val params: java.util.Map[String, String] = new java.util.HashMap[String, String]
    params.put("appid", QQ_APPID)
    params.put("openid", wechatUser.openId)
    params.put("openkey", wechatUser.accessToken)
    params.put("pf", "openmobile_android")
    params.put("format", "json")
    val sig = makeSig(method, scriptName, params, secret)
    params.put("sig", sig)
    val sb: StringBuilder = new StringBuilder()
    sb.append(protocol).append("://").append(serverName).append(scriptName).append("?")

    import collection.JavaConversions._
    for ((k: String, v: String) <- params) {
      sb.append(k).append("=").append(encodeUrl(v)).append("&")
    }

    val url: String = sb.toString().substring(0, sb.length - 1)

    ws.url(url).get().map(wsResponse => {
      Logger.error("获取QQ用户信息返回--->" + wsResponse.json.toString())
      val nickname: Option[String] = wsResponse.json.\("nickname").asOpt[String]
      val gender: Option[String] = wsResponse.json.\("gender").asOpt[String] match {
        case Some("男") => Some("M")
        case Some("女") => Some("F")
        case _ => None
      }

      val idArea: Option[String] = if (wsResponse.json.\("country").asOpt[String].isDefined && wsResponse.json.\("country").asOpt[String].get.equals("CN"))
        Option(wsResponse.json.\("country").asOpt[String].getOrElse("") + " " + wsResponse.json.\("province").asOpt[String].getOrElse("") + " " + wsResponse.json.\("city").asOpt[String].getOrElse(""))
      else None
      val headimgurl: Option[String] = wsResponse.json.\("figureurl").asOpt[String]

      var userOpen: UserOpen = UserOpen(Some(wechatUser.userId), nickname, None, None, gender, None, None, None, None, None, None, None, None, None, None, None, None, Some("Q"), Some(wechatUser.openId), idArea, None, None, None, None)

      if (headimgurl.isDefined) {
        val keyA = "users/photo" + "/" + DateTimeFormat.forPattern("yyyyMMdd").print(new DateTime) + "/" + System.currentTimeMillis + scala.util.Random.nextInt(6) + ".jpg"
        userOpen = UserOpen(Some(wechatUser.userId), nickname, None, None, gender, None, Some(keyA), None, None, None, None, None, None, None, None, None, None, Some("Q"), Some(wechatUser.openId), idArea, None, None, None, None)
        getImg(headimgurl.get, keyA)
      }

      if (UserInfoModel.updateUser(userOpen) > 0) {
        Logger.info("Wechat user update personal info: " + userOpen.userId.get)
      } else {
        Logger.error("Wechat user update error")
      }
    })
  }

  def getImg(headimgurl: String, keyA: String) = {
    ws.url(headimgurl).get().map(wsResponse => {
      val isa = new ByteArrayInputStream(wsResponse.bodyAsBytes)
      oss ! OSSIS(isa, keyA, wsResponse.bodyAsBytes.length)
    })
  }


}
