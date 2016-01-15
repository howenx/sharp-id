package controllers

import java.util.{GregorianCalendar, Calendar}
import javax.inject._

import actor.{SMS, SMSType}
import akka.actor.ActorRef
import models.{CouponsVo, User}
import net.spy.memcached.MemcachedClient
import play.api.data.Forms._
import play.api.data._
import play.api.libs.Codecs
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc.{Action, Controller}
import play.api.{Configuration, Logger}
import utils.SystemService

case class UserNameLoginInfo(name: String, password: String, code: String)

case class UserPhoneLoginInfo(phone: String, code: String)

case class ApiSendCodeForm(phone: String, msg: String)

case class ApiRegForm(phone: String, code: String, password: String)

case class ApiRegFormOptional(phone: Option[String], code: Option[String], password: Option[String], security: Option[String])

/**
  * Created by handy on 15/10/23.
  * Daumkakao china
  */
@Singleton
class Api @Inject()(cache_client: MemcachedClient, @Named("sms") sms: ActorRef, @Named("oss") oss: ActorRef, @Named("coupons") couponsActor: ActorRef, configuration: Configuration) extends Controller {

  /**
    * 使用昵称和密码登录
    * TODO 需要验证表单
    */
  val user_name_login_form = Form(mapping(
    "name" -> nonEmptyText,
    "password" -> nonEmptyText,
    "code" -> nonEmptyText
  )(UserNameLoginInfo.apply)(UserNameLoginInfo.unapply))


  val user_reg_form = Form(mapping(
    "phone" -> optional(text),
    "code" -> optional(text),
    "password" -> optional(text),
    "security" -> optional(text)
  )(ApiRegFormOptional.apply)(ApiRegFormOptional.unapply))

  /**
    * 使用电话号码和密码登录
    * TODO 需要检查电话号码
    */
  val user_phone_login_form = Form(mapping(
    "phone" -> nonEmptyText,
    "code" -> nonEmptyText
  )(UserPhoneLoginInfo.apply)(UserPhoneLoginInfo.unapply))

  /**
    * 发送验证码
    * TODO 需要检查电话号码、msg
    */
  val api_send_code_form = Form(mapping(
    "phone" -> nonEmptyText,
    "msg" -> nonEmptyText
  )(ApiSendCodeForm.apply)(ApiSendCodeForm.unapply))

  val api_reg_form = Form(mapping(
    "phone" -> nonEmptyText,
    "code" -> nonEmptyText,
    "password" -> nonEmptyText
  )(ApiRegForm.apply)(ApiRegForm.unapply))

  def send_code = Action { implicit request =>
    val data = api_send_code_form.bindFromRequest().get
    val phone: String = data.phone
    val msg = data.msg
    val code_times = cache_client.get("hmm-sms" + phone)

    if (!SystemService.checkPhoneNum(phone)) {
      Ok(Json.obj("message" -> Message(ChessPiece.INPUT_VERIFY_FAILED.string, ChessPiece.INPUT_VERIFY_FAILED.pointValue)))
    }
    else if (!Codecs.md5((phone + "hmm").getBytes()).equals(msg)) {
      Ok(Json.obj("message" -> Message(ChessPiece.SECURITY_ERROR.string, ChessPiece.SECURITY_ERROR.pointValue)))
    }
    else {
      Logger.error("发送次数->>>>> "+code_times)
      if (code_times != null && code_times.toString.toInt > 10) {
        Logger.info(s"此用户手机号：$phone api发送验证码当天超过10次")
        Ok(Json.obj("message" -> Message(ChessPiece.SEND_SMS_TOO_MANY.string, ChessPiece.SEND_SMS_TOO_MANY.pointValue)))
      } else {
        if (code_times == null) {
          val currentDate:Calendar = new GregorianCalendar()
          currentDate.set(Calendar.HOUR_OF_DAY, 23)
          currentDate.set(Calendar.MINUTE, 59)
          currentDate.set(Calendar.SECOND, 59)
          cache_client.set("hmm-sms" + phone, currentDate.getTimeInMillis.-(Calendar.getInstance().getTimeInMillis)./(1000).toInt, "1")
        } else {
          cache_client.incr("hmm-sms" + phone, Integer.valueOf(1))
        }
        val code: String = String.valueOf((100000 + Math.random * 900000).toInt)
        sms ! SMS(phone, code, SMSType.comm)
        cache_client.set("api" + phone, 180, code)
        Logger.info(s"用户手机号：$phone api发送验证码成功")
        Ok(Json.obj("message" -> Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue)))
      }
    }
  }

  def reg = Action { implicit request =>
    val data = api_reg_form.bindFromRequest().get
    val phone: String = data.phone.trim
    val code: String = data.code.trim
    val password: String = data.password.trim
    val cacheCode = cache_client.get("api" + phone)
    if (!SystemService.checkPhoneNum(phone)) {
      Ok(Json.obj("message" -> Message(ChessPiece.INPUT_VERIFY_FAILED.string, ChessPiece.INPUT_VERIFY_FAILED.pointValue)))
    }
    else if (cacheCode == null || !String.valueOf(cacheCode).equals(code)) {
      Ok(Json.obj("message" -> Message(ChessPiece.SMS_CODE_ERROR.string, ChessPiece.SMS_CODE_ERROR.pointValue)))
    }
    else if (!SystemService.checkPassword(password)) {
      Ok(Json.obj("message" -> Message(ChessPiece.PASSWORD_VERIFY_ERROR.string, ChessPiece.PASSWORD_VERIFY_ERROR.pointValue)))
    }
    else {
      User.find_by_phone(phone) match {
        //已经存在的用户自动登录
        case Some(userInfo) =>
          Logger.info(s"用户手机号：$phone 此用户已经注册")
          Ok(Json.obj("message" -> Message(ChessPiece.USER_EXISTS.string, ChessPiece.USER_EXISTS.pointValue)))
        case None =>
          User.insert(phone, password, request.remoteAddress) match {
            case Some(id) =>
              if (id > 0) {
                Logger.info(s"用户手机号：$phone 注册成功")
                Ok(Json.obj("message" -> Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue)))
              }
              else {
                Ok(Json.obj("message" -> Message(ChessPiece.ERROR.string, ChessPiece.ERROR.pointValue)))
              }
            case None =>
              Ok(Json.obj("message" -> Message(ChessPiece.ERROR.string, ChessPiece.ERROR.pointValue)))
          }
      }
    }
  }

  def reset_password = Action { implicit request =>
    val data = api_reg_form.bindFromRequest().get
    val phone: String = data.phone.trim
    val code: String = data.code.trim
    val password: String = data.password.trim
    val cacheCode = cache_client.get("api" + phone)
    if (!SystemService.checkPhoneNum(phone)) {
      Ok(Json.obj("message" -> Message(ChessPiece.INPUT_VERIFY_FAILED.string, ChessPiece.INPUT_VERIFY_FAILED.pointValue)))
    }
    else if (cacheCode == null || !String.valueOf(cacheCode).equals(code)) {
      Ok(Json.obj("message" -> Message(ChessPiece.SMS_CODE_ERROR.string, ChessPiece.SMS_CODE_ERROR.pointValue)))
    }
    else if (!SystemService.checkPassword(password)) {
      Ok(Json.obj("message" -> Message(ChessPiece.PASSWORD_VERIFY_ERROR.string, ChessPiece.PASSWORD_VERIFY_ERROR.pointValue)))
    }
    else {
      User.find_by_phone(phone) match {
        //已经存在的用户自动登录
        case Some(userInfo) =>
          if (User.reset_password(phone, password) > 0) {
            Logger.info(s"用户手机号：$phone 重置密码成功")
            Ok(Json.obj("message" -> Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue)))
          } else Ok(Json.obj("message" -> Message(ChessPiece.ERROR.string, ChessPiece.ERROR.pointValue)))
        case None =>
          Logger.info(s"用户手机号：$phone 此用户未注册")
          Ok(Json.obj("message" -> Message(ChessPiece.NOT_REGISTERED.string, ChessPiece.NOT_REGISTERED.pointValue)))
      }
    }
  }


  def verify_phone(choose: Int) = Action { implicit request =>

    val data = user_reg_form.bindFromRequest().get
    if (data.phone.isDefined) {
      val phone: String = data.phone.get.trim
      if (!SystemService.checkPhoneNum(phone)) {
        Ok(Json.obj("message" -> Message(ChessPiece.INPUT_VERIFY_FAILED.string, ChessPiece.INPUT_VERIFY_FAILED.pointValue)))
      } else {
        if (choose == 1) {
          User.find_by_phone(phone) match {
            //已经存在的用户自动登录
            case Some(userInfo) =>
              Logger.info(s"用户手机号：$phone 此用户已经注册")
              Ok(Json.obj("message" -> Message(ChessPiece.USER_EXISTS.string, ChessPiece.USER_EXISTS.pointValue)))
            case None =>
              Logger.info(s"用户手机号：$phone 此用户未注册")
              Ok(Json.obj("message" -> Message(ChessPiece.NOT_REGISTERED.string, ChessPiece.NOT_REGISTERED.pointValue)))
          }
        } else {
          if (data.code.isDefined) {
            if (cache_client.get(data.code.get.toUpperCase) != null && cache_client.get(data.code.get.toUpperCase).equals(data.code.get.toUpperCase)) {
              User.find_by_phone(phone) match {
                case Some(userInfo) =>
                  Logger.info(s"用户手机号：$phone 此用户已经注册")
                  Ok(Json.obj("message" -> Message(ChessPiece.USER_EXISTS.string, ChessPiece.USER_EXISTS.pointValue)))
                case None =>
                  Logger.info(s"用户手机号：$phone 此用户未注册")
                  Ok(Json.obj("message" -> Message(ChessPiece.NOT_REGISTERED.string, ChessPiece.NOT_REGISTERED.pointValue)))
              }
            } else {
              Ok(Json.obj("message" -> Message(ChessPiece.IMAGE_CODE_ERROR.string, ChessPiece.IMAGE_CODE_ERROR.pointValue)))
            }
          } else {
            Ok(Json.obj("message" -> Message(ChessPiece.BAD_PARAMETER.string, ChessPiece.BAD_PARAMETER.pointValue)))
          }
        }
      }
    }
    else {
      BadRequest(Json.obj("message" -> Message(ChessPiece.BAD_PARAMETER.string, ChessPiece.BAD_PARAMETER.pointValue)))
    }
  }

  implicit lazy val messageReads: Reads[Message] = (
    (__ \ "message").read[String] and
      (__ \ "code").read[Int]
    ) (Message)

  implicit lazy val messageWrites: Writes[Message] = (
    (__ \ "message").write[String] and
      (__ \ "code").write[Int]
    ) (unlift(Message.unapply))


  def login(id: Long, remoteAddress: String, name: String, password: String): String = {
    User.find_by_phone(name, password) match {
      case Some(user) =>
        User.login(user.id, remoteAddress)
        Logger.info(s"用户手机号码：$name 登陆成功")
        val token = Codecs.md5((System.currentTimeMillis + scala.util.Random.nextString(100)).getBytes())
        cache_client.set(token, 60 * 60 * 24 * 7, Json.stringify(Json.obj("id" -> JsString(String.valueOf(user.id)), "name" -> JsString(user.nickname), "photo" -> JsString(user.photo_url))))
        //用户一旦登录,就去更新用户将用户所有未使用的过期的优惠券置成状态"S",表示自动失效
        couponsActor ! CouponsVo(None, Some(user.id), None, None, None, None, Some("S"), None, None, None, None)
        Logger.info(s"用户手机号：$name 手机验证码形式登录成功")
        token
      case None => null
    }
  }


  def login_user_name() = Action { implicit request =>

    val data = user_name_login_form.bindFromRequest().get
    val name = data.name.trim
    val password = data.password.trim
    val code = data.code.trim
    val phone = "^1[3-8][0-9]{9}".r
    val verifyLockTimes = cache_client.get(name + "_check")
    val verifyLocked = cache_client.get(name + "_locked")

    name match {
      case phone() =>
        if (verifyLocked == null) {
          User.find_by_phone(name) match {
            //已经存在的用户登录
            case Some(userInfo) =>
              val token = login(userInfo.id, request.remoteAddress, name, password)

              if (verifyLockTimes == null) {
                if (token != null) {
                  Ok(Json.obj(
                    "message" -> Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue),
                    Systemcontents.API_RESULT_TOKEN -> JsString(token),
                    Systemcontents.API_RESULT_OVER_TIME -> JsNumber(60 * 60 * 24 * 7))
                  )

                } else {
                  cache_client.set(name + "_check", 60 * 60, 1)
                  Ok(Json.obj("message" -> Message(ChessPiece.USERNAME_OR_PASSWORD_ERROR.string, ChessPiece.USERNAME_OR_PASSWORD_ERROR.pointValue)))
                }

              } else if (verifyLockTimes.toString.toInt < 6) {
                Logger.info(s"输入错误次数：$verifyLockTimes")
                if (token != null) {
                  cache_client.delete(name + "_check")
                  Ok(Json.obj(
                    "message" -> Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue),
                    Systemcontents.API_RESULT_TOKEN -> JsString(token),
                    Systemcontents.API_RESULT_OVER_TIME -> JsNumber(60 * 60 * 24 * 7))
                  )
                } else {
                  cache_client.set(name + "_check", 60 * 60, verifyLockTimes.toString.toInt + 1)
                  Ok(Json.obj("message" -> Message(ChessPiece.USERNAME_OR_PASSWORD_ERROR.string, ChessPiece.USERNAME_OR_PASSWORD_ERROR.pointValue)))
                }
              } else if (verifyLockTimes.toString.toInt > 5 && verifyLockTimes.toString.toInt < 21) {
                Logger.info(s"输入错误次数：$verifyLockTimes")
                if (code.equals("-1")) {
                  Ok(Json.obj("message" -> Message(ChessPiece.PASSWORD_ERROR_TOO_MANY.string, ChessPiece.PASSWORD_ERROR_TOO_MANY.pointValue)))
                } else if (cache_client.get(code.toUpperCase) != null && cache_client.get(code.toUpperCase).equals(code.toUpperCase)) {

                  Logger.error("尼玛的验证码: " + cache_client.get(code.toUpperCase))

                  if (token != null) {
                    cache_client.delete(name + "_check")
                    Ok(Json.obj(
                      "message" -> Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue),
                      Systemcontents.API_RESULT_TOKEN -> JsString(token),
                      Systemcontents.API_RESULT_OVER_TIME -> JsNumber(60 * 60 * 24 * 7))
                    )
                  } else {
                    cache_client.set(name + "_check", 60 * 60, verifyLockTimes.toString.toInt + 1)
                    Ok(Json.obj("message" -> Message(ChessPiece.USERNAME_OR_PASSWORD_ERROR.string, ChessPiece.USERNAME_OR_PASSWORD_ERROR.pointValue)))
                  }
                } else {
                  cache_client.set(name + "_check", 60 * 60, verifyLockTimes.toString.toInt + 1)
                  Ok(Json.obj("message" -> Message(ChessPiece.IMAGE_CODE_ERROR.string, ChessPiece.IMAGE_CODE_ERROR.pointValue)))
                }
              } else {
                Logger.info(s"输入错误次数：$verifyLockTimes")
                cache_client.set(name + "_locked", 60 * 60, true)
                cache_client.delete(name + "_check")
                Ok(Json.obj("message" -> Message(ChessPiece.PASSWORD_ERROR_LOCKED.string, ChessPiece.PASSWORD_ERROR_LOCKED.pointValue)))
              }
            //未注册的用户
            case None =>
              Ok(Json.obj("message" -> Message(ChessPiece.NOT_REGISTERED.string, ChessPiece.NOT_REGISTERED.pointValue)))
          }
        } else {
          //          cache_client.delete(name + "_locked")
          //          cache_client.delete(name + "_check")
          Ok(Json.obj("message" -> Message(ChessPiece.PASSWORD_ERROR_LOCKED_NOTIFY.string, ChessPiece.PASSWORD_ERROR_LOCKED_NOTIFY.pointValue)))
        }
      case _ =>
        Ok(Json.obj("message" -> Message(ChessPiece.INPUT_VERIFY_FAILED.string, ChessPiece.INPUT_VERIFY_FAILED.pointValue)))
    }
  }

  case class RefreshForm(token: String)

  val refresh_form = Form(mapping(
    "token" -> nonEmptyText
  )(RefreshForm.apply)(RefreshForm.unapply))

  def refresh_token = Action { implicit request =>
    val data = refresh_form.bindFromRequest().get
    val token = data.token.trim
    if (cache_client.get(token) != null) {
      val newToken = Codecs.md5((System.currentTimeMillis + scala.util.Random.nextString(100)).getBytes())
      cache_client.set(newToken, 60 * 60 * 24 * 7, cache_client.get(token)) //把用户信息换乘新的key
      cache_client.delete(token) //删除旧的cache信息
      Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.API_RESULT_REFRESH_SUCCESS)
        , Systemcontents.API_RESULT_TOKEN -> JsString(newToken), Systemcontents.API_RESULT_OVER_TIME -> JsNumber(60 * 60 * 24 * 7)))
    } else {
      Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.API_RESULT_REFRESH_FAILED)))
    }
  }

  def login_phone_num = Action { implicit request =>
    val data = user_phone_login_form.bindFromRequest().get
    val phone: String = data.phone.trim
    val code: String = data.code.trim
    val cacheCode = cache_client.get("api" + phone)
    if (!SystemService.checkPhoneNum(phone)) {
      Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.PHONE_NUM_TYPE_ERROR)))
    }
    else if (cacheCode == null || !String.valueOf(cacheCode).equals(code)) {
      Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.PHONE_CODE_ERROR)))
    }
    else {
      User.find_by_phone(phone) match {
        //已经存在的用户自动登录
        case Some(userInfo) =>
          User.login(userInfo.id, request.remoteAddress)
          Logger.info(s"用户手机号：$phone 手机验证码形式登录成功")
          Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.LOGIN_SUCCESS)))
        //未注册的用户自动注册并登陆
        case None =>
          val password = String.valueOf((100000 + Math.random * 900000).toInt) //生产密码
          //注册并返回id
          User.insert(phone, password, request.remoteAddress) match {
            case Some(id) =>
              if (id > 0) {
                sms ! SMS(phone, password, SMSType.passwd) //actor发送短信
                User.login(id, request.remoteAddress)
                Logger.info(s"用户手机号：$phone 未注册自动注册登陆成功")
                Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.LOGIN_SUCCESS)))
              }
              else {
                Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.REG_FAILED)))
              }
            case None =>
              Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.REG_FAILED)))
          }
      }
    }
  }
}
