package actor

import java.nio.charset.StandardCharsets
import javax.inject.Inject

import actor.SMSType.sms_type
import akka.actor.Actor
import net.spy.memcached.MemcachedClient
import org.apache.commons.codec.binary.Base64
import play.api.libs.Codecs
import play.api.libs.json.Json
import play.api.libs.ws.{WSRequest, WSClient}
import play.api.{Configuration, Logger}

import scala.concurrent.ExecutionContext.Implicits.global

case class SMS(phone_num: String, code: String, sms_type: sms_type)

/**
  * by howen
  * @param ws ws
  * @param configuration configuration
  * @param cache_client cache_client
  */
class SMSActor @Inject()(ws: WSClient, configuration: Configuration, cache_client: MemcachedClient) extends Actor {

  val account = configuration.getString("send_name").getOrElse("")
  val password = configuration.getString("send_password").getOrElse("")

  val accountSid = configuration.getString("account.sid").get
  val authToken = configuration.getString("auth.token").get
  val url = configuration.getString("base.url").get+configuration.getString("soft.version").get
  val validTime = configuration.getString("sms.valid.time").get

  override def receive = {
    case sms: SMS =>
      val mobile = sms.phone_num
      val code = sms.code
      val content = s"【韩秘美】您的验证码为$code，请于$validTime 分钟内正确输入验证码。如非本人操作，请忽略该短信。"
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

        val format = new java.text.SimpleDateFormat("yyyyMMddHHmmss")
        val date = format.format(new java.util.Date())

        val send_url = url+s"/Accounts/$accountSid/Messages/templateSMS?sig="+sig(date)

        val wsRequest: WSRequest = ws.url(send_url)

        val data = Json.obj(
          "appId" -> configuration.getString("app.id"),
          "templateId" -> configuration.getString("template.id"),
          "to" -> mobile,
          "param" -> s"$code,$validTime"
        )
        val params = Json.obj(
          "templateSMS"->data
        )

        wsRequest.withHeaders("Accept" -> "application/json","Content-Type" -> "application/json;charset=utf-8","Authorization"->authorization(date)).post(params).map { response =>
          Logger.error(response.body)
        }
      }

  }

  def sig(date:String):String = {
    Codecs.md5((accountSid+authToken+date).getBytes()).toUpperCase
  }

  def authorization(date:String):String = {
    new String(Base64.encodeBase64((accountSid+":"+date).getBytes()), StandardCharsets.UTF_8)
  }
}

object SMSType extends Enumeration {
  type sms_type = String
  val comm = "commCode"
  val passwd = "passwd"
  val resetPasswd = "resetPasswd"
}
