package actor

import akka.actor.Actor
import play.api.Logger

case class SMS (phone_num:String, code:String)

/**
 * 发送短信Actor
 * Created by handy on 15/10/20.
 * Daumkakao china
 */
class SMSActor extends Actor{

  var send_url = ""

  def receive = {
    case sms:SMS =>
      Logger.debug(sms.toString)
  }

}
