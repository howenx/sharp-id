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


/**
  * by howen
  * @param ws ws
  * @param configuration configuration
  * @param cache_client cache_client
  */

class SmsContentActor @Inject()(ws: WSClient, configuration: Configuration, cache_client: MemcachedClient) extends Actor {

  val account = configuration.getString("send_name").getOrElse("")
  val password = configuration.getString("send_password").getOrElse("")

  val accountSid = configuration.getString("account.sid").get
  val authToken = configuration.getString("auth.token").get
  val url = configuration.getString("base.url").get+configuration.getString("soft.version").get
  val validTime = configuration.getString("sms.valid.time").get

  override def receive = {
    case sms: java.util.HashMap[String@unchecked,String@unchecked] =>
      val mobile:String = sms.get("phone_num")
      val sendContent:String = sms.get("send_content")
      val templateId:String = sms.get("template_id")
      val content:String = s"【韩秘美】$sendContent"
      val key = Codecs.md5((mobile+templateId).getBytes)
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
          Logger.info(s"同一类短信 一个手机号码 一分钟最多发送三次 :$mobile")
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
          "templateId" -> templateId,
          "to" -> mobile,
          "param" -> s"$sendContent"
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

