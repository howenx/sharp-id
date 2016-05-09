package util

import com.typesafe.config.ConfigFactory

/**
  * 查询参数表中的参数项
  * Created by howen on 16/3/18.
  */

object SysParUtil {

  val IMAGE_CODE_VALID_TIME: Integer = ConfigFactory.defaultApplication.getInt("image.code.valid.time")
  val ACCOUNT_SID: String = ConfigFactory.defaultApplication.getString("account.sid")
  val AUTH_TOKEN: String = ConfigFactory.defaultApplication.getString("auth.token")
  val SOFT_VERSION: String = ConfigFactory.defaultApplication.getString("soft.version")
  val APP_ID: String = ConfigFactory.defaultApplication.getString("app.id")
  val TEMPLATE_ID: String = ConfigFactory.defaultApplication.getString("template.id")
  val BASE_URL: String = ConfigFactory.defaultApplication.getString("base.url")
  val SMS_VALID_TIME: Integer = ConfigFactory.defaultApplication.getInt("sms.valid.time")

  val TOKEN_OVER_TIME: Integer = ConfigFactory.defaultApplication.getInt("token.over.time")

  val WECHAT_USER_INFO:String = ConfigFactory.defaultApplication.getString("weixin.user.info")

  val SMS_TIMES:Integer = ConfigFactory.defaultApplication.getInt("send.sms.times")
  var REDIS_URL: String = ConfigFactory.defaultApplication.getString("redis.host")
  var REDIS_PASSWORD: String = ConfigFactory.defaultApplication.getString("redis.password")
  var REDIS_PORT: Integer = ConfigFactory.defaultApplication.getInt("redis.port")
  var REDIS_CHANNEL: String = ConfigFactory.defaultApplication.getString("redis.channel")
}
