package utils

import play.Play

/**
  * 查询参数表中的参数项
  * Created by howen on 16/3/18.
  */

object SysParUtil {

  val IMAGE_CODE_VALID_TIME: Integer = Play.application.configuration.getInt("image.code.valid.time")
  val ACCOUNT_SID: String = Play.application.configuration.getString("account.sid")
  val AUTH_TOKEN: String = Play.application.configuration.getString("auth.token")
  val SOFT_VERSION: String = Play.application.configuration.getString("soft.version")
  val APP_ID: String = Play.application.configuration.getString("app.id")
  val TEMPLATE_ID: String = Play.application.configuration.getString("template.id")
  val BASE_URL: String = Play.application.configuration.getString("base.url")
  val SMS_VALID_TIME: Integer = Play.application.configuration.getInt("sms.valid.time")

  val TOKEN_OVER_TIME: Integer = Play.application.configuration.getInt("token.over.time")

}
