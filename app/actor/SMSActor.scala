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

  val account = configuration.getString("send_name").getOrElse("");
  val password = configuration.getString("send_password").getOrElse("")

  override def receive = {
    case sms:SMS =>
      val mobile = sms.phone_num
      sms.sms_type match {
        case SMSType.verify =>
          val code = sms.code
          val content = s"验证码为：$code，3分钟内有效。如非本人操作，请忽略该短信www.5dsy.cn【5游网】"
          val send_url = s"http://sms.chanzor.com:8001/sms.aspx?action=send&account=$account&password=$password&mobile=$mobile&content=$content&sendTime="
          Logger.debug(send_url)
          ws.url(send_url).get().map{ response =>
            Logger.debug(response.body)
          }
      }


  }
}

object SMSType extends Enumeration {
  type sms_type = Value
  val verify = Value
}
