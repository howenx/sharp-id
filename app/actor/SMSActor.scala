package actor

import java.net.URLEncoder
import javax.inject.Inject

import actor.SMSType.sms_type
import akka.actor.Actor
import net.spy.memcached.MemcachedClient
import play.api.libs.Codecs
import play.api.libs.ws.{WSClient, WSRequest}
import play.api.{Configuration, Logger}

import scala.concurrent.ExecutionContext.Implicits.global

case class SMS(phone_num: String, code: String, sms_type: sms_type)

/**
  * by howen
  *
  * @param ws            ws
  * @param configuration configuration
  * @param cache_client  cache_client
  */
class SMSActor @Inject()(ws: WSClient, configuration: Configuration, cache_client: MemcachedClient) extends Actor {

  val validTime = configuration.getString("sms.valid.time").get

  /**
    * 美联软通短信平台
    */

  val m5cEncode = configuration.getString("m5c.encode").get //编码

  val m5cUserName = configuration.getString("m5c.username").get //用户名

  val m5cPassword = configuration.getString("m5c.password").get //密码

  val m5cApiKey = configuration.getString("m5c.apikey").get //apikey秘钥

  val m5cSign = configuration.getString("m5c.sign").get //签名

  val m5cUrl = configuration.getString("m5c.url").get //请求地址

  val smsPurposeName = configuration.getString("sms.purpose.name").get //用于提示申请注册或申请重置密码时的提示

  override def receive = {
    case sms: SMS =>
      val mobile = sms.phone_num

      val code = sms.code

      val content: String = sms.sms_type match {
        case SMSType.register => s"感谢您注册$smsPurposeName"+","+s"验证码:$code($validTime 分钟内有效)。$m5cSign"
        case SMSType.reset => s"验证码:$code($validTime 分钟内有效),"+s"请按照页面提示完成密码重置,感谢使用$smsPurposeName!$m5cSign"
        case SMSType.comm => ""
      }
      val key = Codecs.md5((sms.phone_num + sms.sms_type).getBytes)
      var bl: Boolean = false
      if (cache_client.get(key) == null) {
        cache_client.set(key, 60, "1")
        bl = true
      }
      else {
        val number: Long = cache_client.get(key).toString.toLong
        if (number < 4) {
          cache_client.incr(key, Integer.valueOf(1))
          bl = true
        } else {
          Logger.info(s"同一类短信 一个手机号码 一分钟最多发送三次 :${sms.phone_num}")
          bl = false
        }
      }
      if (bl) {

        Logger.error(s"\n$content\n")

        val contentUrlEncode: String = URLEncoder.encode(content, m5cEncode)
        val password_md5:String =Codecs.md5(m5cPassword.getBytes()).toLowerCase()

        val params:String = s"?username=$m5cUserName&password_md5=$password_md5&mobile=$mobile&apikey=$m5cApiKey&content=$contentUrlEncode&encode=$m5cEncode"

        val wsRequest: WSRequest = ws.url(m5cUrl+params)
        wsRequest.get().map { response =>
          Logger.error(response.body)
        }
      }
  }
}

object SMSType extends Enumeration {
  type sms_type = String
  val comm = "comm"
  val register = "register"
  val reset = "reset"
}
