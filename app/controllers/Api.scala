package controllers

import java.io.{ByteArrayOutputStream, IOException, InputStream}
import java.util.{Calendar, GregorianCalendar}
import javax.inject._

import actor.{SMS, SMSType}
import akka.actor.ActorRef
import models.UserModel
import net.spy.memcached.MemcachedClient
import play.api.libs.Codecs
import play.api.libs.iteratee.Enumerator
import play.api.libs.json._
import play.api.mvc.{Action, Controller, ResponseHeader, Result}
import play.api.{Configuration, Logger}
import utils.{ImageCodeService, SystemService, Systemcontents}

/**
  * 所有用户登录注册相关
  * Created by howen on 16/3/16.
  */
@Singleton
class Api @Inject()(form: FormConstraint, cache_client: MemcachedClient, @Named("sms") sms: ActorRef, @Named("oss") oss: ActorRef, configuration: Configuration, @Named("coupons") couponsActor: ActorRef) extends Controller {

  import form._
  /**
    * 发送短信验证码
    *
    * @return
    */
  def send_code = Action { implicit request =>
    send_code_form.bindFromRequest().fold(
      formWithErrors => {
        Ok(Json.obj("message" -> Message(ChessPiece.BAD_PARAMETER.string, ChessPiece.BAD_PARAMETER.pointValue)))
      },
      data => {
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
          Logger.error("发送次数->>>>> " + (if (code_times == null) 0 else code_times))
          if (code_times != null && code_times.toString.toInt > 10) {
            Logger.info(s"此用户手机号：$phone api发送验证码当天超过10次")
            Ok(Json.obj("message" -> Message(ChessPiece.SEND_SMS_TOO_MANY.string, ChessPiece.SEND_SMS_TOO_MANY.pointValue)))
          } else {
            if (code_times == null) {
              val currentDate: Calendar = new GregorianCalendar()
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
    )
  }

  /**
    * 获取图形验证码
    *
    * @param id 随机数
    * @return
    */
  def getImageCodes(id: Integer) = Action {
    val code: String = ImageCodeService.getInstance.generateVerifyCode(4)
    val is: InputStream = ImageCodeService.getInstance.getImageIo(code)
    val out: ByteArrayOutputStream = new ByteArrayOutputStream
    var data: Array[Byte] = null
    var count: Int = -1
    try {
      val i: Int = is.available
      data = new Array[Byte](i)
      while ( {
        count = is.read(data, 0, i)
        count
      } != -1) out.write(data, 0, count)
      data = null
      cache_client.set(code.toUpperCase, 60 * 60 * 3, code)
      Result(
        header = ResponseHeader(200, Map(CONTENT_TYPE -> "text/plain")),
        body = Enumerator(out.toByteArray)
      )
    }
    catch {
      case ex: IOException => Ok("error")

    }
  }

  /**
    * 用户注册
    *
    * @return
    */
  def reg = Action { implicit request =>
    api_reg_form.bindFromRequest().fold(
      formWithErrors => {
        Ok(Json.obj("message" -> Message(ChessPiece.BAD_PARAMETER.string, ChessPiece.BAD_PARAMETER.pointValue)))
      },
      data => {
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
          UserModel.find_by_phone(phone) match {
            //已经存在的用户自动登录
            case Some(userInfo) =>
              Logger.info(s"用户手机号：$phone 此用户已经注册")
              Ok(Json.obj("message" -> Message(ChessPiece.USER_EXISTS.string, ChessPiece.USER_EXISTS.pointValue)))
            case None =>
              UserModel.insert(phone, password, request.remoteAddress) match {
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
    )
  }

  /**
    * 重置密码
    *
    * @return
    */
  def reset_password = Action { implicit request =>
    api_reg_form.bindFromRequest().fold(
      formWithErrors => {
        BadRequest(Json.obj("message" -> Message(ChessPiece.BAD_PARAMETER.string, ChessPiece.BAD_PARAMETER.pointValue)))
      },
      data => {
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
          UserModel.find_by_phone(phone) match {
            //已经存在的用户自动登录
            case Some(userInfo) =>
              if (UserModel.reset_password(phone, password) > 0) {
                Logger.info(s"用户手机号：$phone 重置密码成功")
                Ok(Json.obj("message" -> Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue)))
              } else Ok(Json.obj("message" -> Message(ChessPiece.ERROR.string, ChessPiece.ERROR.pointValue)))
            case None =>
              Logger.info(s"用户手机号：$phone 此用户未注册")
              Ok(Json.obj("message" -> Message(ChessPiece.NOT_REGISTERED.string, ChessPiece.NOT_REGISTERED.pointValue)))
          }
        }
      }
    )
  }

  /**
    * 用户注册和找回密码校验手机号
    *
    * @return
    */
  def verify_phone() = Action { implicit request =>
    verify_phone_form.bindFromRequest().fold(
      formWithErrors => {
        Ok(Json.obj("message" -> Message(ChessPiece.BAD_PARAMETER.string, ChessPiece.BAD_PARAMETER.pointValue)))
      },
      data => {
        val phone: String = data.phone.trim
        if (!SystemService.checkPhoneNum(phone)) {
          Ok(Json.obj("message" -> Message(ChessPiece.INPUT_VERIFY_FAILED.string, ChessPiece.INPUT_VERIFY_FAILED.pointValue)))
        } else {
          if (data.code.equals("-1")) {
            UserModel.find_by_phone(phone) match {
              //已经存在的用户自动登录
              case Some(userInfo) =>
                Logger.info(s"用户手机号：$phone 此用户已经注册")
                Ok(Json.obj("message" -> Message(ChessPiece.USER_EXISTS.string, ChessPiece.USER_EXISTS.pointValue)))
              case None =>
                Logger.info(s"用户手机号：$phone 此用户未注册")
                Ok(Json.obj("message" -> Message(ChessPiece.NOT_REGISTERED.string, ChessPiece.NOT_REGISTERED.pointValue)))
            }
          } else {
            if (cache_client.get(data.code.toUpperCase) != null && cache_client.get(data.code.toUpperCase).equals(data.code.toUpperCase)) {
              UserModel.find_by_phone(phone) match {
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
          }
        }
      }
    )
  }

  /**
    * 用户手机登录
    *
    * @return
    */
  def login_user_phone() = Action { implicit request =>

    Logger.error("错误信息--->\n" + user_phone_login_form.bindFromRequest().errors.mkString)
    user_phone_login_form.bindFromRequest().fold(
      formWithErrors => {
        Ok(Json.obj("message" -> Message(ChessPiece.BAD_PARAMETER.string, ChessPiece.BAD_PARAMETER.pointValue)))
      },
      data => {
        val phone = data.phone.trim
        val password = data.password.trim
        val code = data.code.trim
        val phone_regex = "^1[3-8][0-9]{9}".r
        val verifyLockTimes = cache_client.get(phone + "_check")
        val verifyLocked = cache_client.get(phone + "_locked")

        phone match {
          case phone_regex() =>
            if (verifyLocked == null) {
              UserModel.find_by_phone(phone) match {
                //已经存在的用户登录
                case Some(userInfo) =>
                  val token = login(userInfo.id, request.remoteAddress, phone, password)

                  if (verifyLockTimes == null) {
                    if (token != null) {
                      Ok(Json.obj(
                        "message" -> Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue),
                        Systemcontents.API_RESULT_USER_ID -> JsNumber(userInfo.id),
                        Systemcontents.API_RESULT_TOKEN -> JsString(token),
                        Systemcontents.API_RESULT_OVER_TIME -> JsNumber(60 * 60 * 24 * 7))
                      )

                    } else {
                      cache_client.set(phone + "_check", 60 * 60, 1)
                      Ok(Json.obj("message" -> Message(ChessPiece.USERNAME_OR_PASSWORD_ERROR.string, ChessPiece.USERNAME_OR_PASSWORD_ERROR.pointValue)))
                    }

                  } else if (verifyLockTimes.toString.toInt < 6) {
                    Logger.info(s"输入错误次数：$verifyLockTimes")
                    if (token != null) {
                      cache_client.delete(phone + "_check")
                      Ok(Json.obj(
                        "message" -> Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue),
                        Systemcontents.API_RESULT_USER_ID -> JsNumber(userInfo.id),
                        Systemcontents.API_RESULT_TOKEN -> JsString(token),
                        Systemcontents.API_RESULT_OVER_TIME -> JsNumber(60 * 60 * 24 * 7))
                      )
                    } else {
                      cache_client.set(phone + "_check", 60 * 60, verifyLockTimes.toString.toInt + 1)
                      Ok(Json.obj("message" -> Message(ChessPiece.USERNAME_OR_PASSWORD_ERROR.string, ChessPiece.USERNAME_OR_PASSWORD_ERROR.pointValue)))
                    }
                  } else if (verifyLockTimes.toString.toInt > 5 && verifyLockTimes.toString.toInt < 21) {
                    Logger.info(s"输入错误次数：$verifyLockTimes")
                    if (code.equals("-1")) {
                      Ok(Json.obj("message" -> Message(ChessPiece.PASSWORD_ERROR_TOO_MANY.string, ChessPiece.PASSWORD_ERROR_TOO_MANY.pointValue)))
                    } else if (cache_client.get(code.toUpperCase) != null && cache_client.get(code.toUpperCase).equals(code.toUpperCase)) {
                      if (token != null) {
                        cache_client.delete(phone + "_check")
                        Ok(Json.obj(
                          "message" -> Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue),
                          Systemcontents.API_RESULT_USER_ID -> JsNumber(userInfo.id),
                          Systemcontents.API_RESULT_TOKEN -> JsString(token),
                          Systemcontents.API_RESULT_OVER_TIME -> JsNumber(60 * 60 * 24 * 7))
                        )
                      } else {
                        cache_client.set(phone + "_check", 60 * 60, verifyLockTimes.toString.toInt + 1)
                        Ok(Json.obj("message" -> Message(ChessPiece.USERNAME_OR_PASSWORD_ERROR.string, ChessPiece.USERNAME_OR_PASSWORD_ERROR.pointValue)))
                      }
                    } else {
                      cache_client.set(phone + "_check", 60 * 60, verifyLockTimes.toString.toInt + 1)
                      Ok(Json.obj("message" -> Message(ChessPiece.IMAGE_CODE_ERROR.string, ChessPiece.IMAGE_CODE_ERROR.pointValue)))
                    }
                  } else {
                    Logger.info(s"输入错误次数：$verifyLockTimes")
                    cache_client.set(phone + "_locked", 60 * 60, true)
                    cache_client.delete(phone + "_check")
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
    )
  }

  /**
    * 刷新token
    *
    * @return
    */
  def refresh_token = Action { implicit request =>
    refresh_form.bindFromRequest().fold(
      formWithErrors => {
        Ok(Json.obj("message" -> Message(ChessPiece.BAD_PARAMETER.string, ChessPiece.BAD_PARAMETER.pointValue)))
      },
      data => {
        val token = data.token.trim
        if (cache_client.get(token) != null) {
          val newToken = Codecs.md5((System.currentTimeMillis + scala.util.Random.nextString(100)).getBytes())
          cache_client.set(newToken, 60 * 60 * 24 * 7, cache_client.get(token)) //把用户信息换乘新的key
          cache_client.set(Json.parse(cache_client.get(token).toString).\("id").asOpt[String].getOrElse(""), 60 * 60 * 24 * 7, newToken) //把用户信息换乘新的key
          cache_client.delete(token) //删除旧的cache信息
          Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.API_RESULT_REFRESH_SUCCESS)
            , Systemcontents.API_RESULT_USER_ID -> JsNumber(Json.parse(cache_client.get(token).toString).\("id").as[Long]), Systemcontents.API_RESULT_TOKEN -> JsString(newToken), Systemcontents.API_RESULT_OVER_TIME -> JsNumber(60 * 60 * 24 * 7)))
        } else {
          Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.API_RESULT_REFRESH_FAILED)))
        }
      }
    )
  }
}
