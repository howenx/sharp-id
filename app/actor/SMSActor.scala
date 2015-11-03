package actor

import javax.inject.Inject

import actor.SMSType.sms_type
import akka.actor.Actor
import play.api.{Logger, Configuration}
import play.api.libs.ws.{WSClient}
import scala.concurrent.ExecutionContext.Implicits.global

case class SMS(phone_num:String, code:String, sms_type: sms_type)

/**
 * Created by handy on 15/10/23.
 * Daumkakao china
 */
class SMSActor @Inject() (ws: WSClient, configuration: Configuration) extends Actor{

  val account = configuration.getString("send_name").getOrElse("")
  val password = configuration.getString("send_password").getOrElse("")

  override def receive = {
    case sms:SMS =>
      val mobile = sms.phone_num
      val code = sms.code
      var content = s"验证码为：$code，3分钟内有效。如非本人操作，请忽略该短信www.5dsy.cn【5游网】"
      sms.sms_type match {
        case SMSType.comm =>
          content = s"验证码为：$code，3分钟内有效。如非本人操作，请忽略该短信www.5dsy.cn【5游网】"
        case SMSType.passwd =>
          content = s"欢迎您注册5游网，您的初始密码为$code，建议您尽快登录www.5dsy.cn修改密码！【5游网】"
        case SMSType.resetPasswd =>
          content = s"重置密码为：$code，请尽快登录修改密码。如非本人操作，请忽略该短信www.5dsy.cn【5游网】"
      }
      val send_url = s"http://sms.chanzor.com:8001/sms.aspx?action=send&account=$account&password=$password&mobile=$mobile&content=$content&sendTime="
      Logger.debug(content)
      ws.url(send_url).get().map{ response =>
        Logger.debug(response.body)
      }


  }
}

object SMSType extends Enumeration {
  type sms_type = String
  val comm = "commCode"
  val passwd = "passwd"
  val resetPasswd = "resetPasswd"
}
