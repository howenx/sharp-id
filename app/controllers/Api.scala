package controllers

import java.io.{ByteArrayOutputStream, IOException, InputStream}
import java.util.{Calendar, GregorianCalendar}
import javax.inject._

import actor.{SMS, SMSType}
import akka.actor.ActorRef
import filters.Authentication
import models.FormConstModel._
import models._
import net.spy.memcached.MemcachedClient
import play.api.Logger
import play.api.libs.Codecs
import play.api.libs.iteratee.Enumerator
import play.api.libs.json._
import play.api.mvc._
import utils.ImageCodeUtil
import utils.SysParUtil._

/**
  * 所有用户登录注册相关
  * Created by howen on 16/3/16.
  */
@Singleton
class Api @Inject()(cache_client: MemcachedClient, @Named("sms") sms: ActorRef, @Named("oss") oss: ActorRef, @Named("coupons") couponsActor: ActorRef) extends Controller {

  val auth = new Authentication(cache_client)

  import auth.Authenticated

  /**
    * 登录操作
    *
    * @param id            id
    * @param remoteAddress remoteAddress
    * @param name          name
    * @param password      password
    * @return
    */
  def login(id: Long, remoteAddress: String, name: String, password: String): String = {
    UserModel.find_by_phone(name, password) match {
      case Some(user) =>
        UserModel.login(user.id, remoteAddress)
        Logger.info(s"用户手机号码：$name 登陆成功")
        val token = Codecs.md5((System.currentTimeMillis + scala.util.Random.nextString(100)).getBytes())
        //设置

        cache_client.set(token, TOKEN_OVER_TIME, Json.stringify(Json.obj("id" -> JsNumber(user.id), "name" -> JsString(user.nickname), "photo" -> JsString(user.photo_url))))
        cache_client.set(user.id.toString, TOKEN_OVER_TIME, token)
        //用户一旦登录,就去更新用户将用户所有未使用的过期的优惠券置成状态"S",表示自动失效
        couponsActor ! CouponsVo(None, Some(user.id), None, None, None, None, Some("S"), None, None, None, None)
        token
      case None => null
    }
  }

  /**
    * 发送短信验证码
    *
    * @return
    */
  def send_code = Action { implicit request =>
    send_code_form.bindFromRequest().fold(
      formWithErrors => {
        Logger.error("表单校验错误信息--->" + send_code_form.bindFromRequest().errors.mkString)
        Ok(Json.obj("message" -> Message(ChessPiece.BAD_PARAMETER.string, ChessPiece.BAD_PARAMETER.pointValue)))
      },
      data => {
        val phone: String = data.phone
        val msg = data.msg
        val code_times = cache_client.get("hmm-sms" + phone)

        if (!Codecs.md5((phone + "hmm").getBytes()).equals(msg)) {
          Ok(Json.obj("message" -> Message(ChessPiece.SECURITY_ERROR.string, ChessPiece.SECURITY_ERROR.pointValue)))
        }
        else {
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
            cache_client.set("api" + phone, SMS_VALID_TIME * 60, code)
            Logger.info(s"用户手机号：$phone api发送验证码成功,发送次数" + (if (code_times == null) 0 else code_times) + "次")
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
    val code: String = ImageCodeUtil.getInstance.generateVerifyCode(4)
    val is: InputStream = ImageCodeUtil.getInstance.getImageIo(code)
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
      cache_client.set(code.toUpperCase, IMAGE_CODE_VALID_TIME, code)
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
        Logger.error("表单校验错误信息--->" + api_reg_form.bindFromRequest().errors.mkString)
        Ok(Json.obj("message" -> Message(ChessPiece.BAD_PARAMETER.string, ChessPiece.BAD_PARAMETER.pointValue)))
      },
      data => {
        val phone: String = data.phone.trim
        val code: String = data.code.trim
        val password: String = data.password.trim
        val cacheCode = cache_client.get("api" + phone)
        if (cacheCode == null || !String.valueOf(cacheCode).equals(code)) {
          Ok(Json.obj("message" -> Message(ChessPiece.SMS_CODE_ERROR.string, ChessPiece.SMS_CODE_ERROR.pointValue)))
        } else {
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
        Logger.error("表单校验错误信息--->" + api_reg_form.bindFromRequest().errors.mkString)
        BadRequest(Json.obj("message" -> Message(ChessPiece.BAD_PARAMETER.string, ChessPiece.BAD_PARAMETER.pointValue)))
      },
      data => {
        val phone: String = data.phone.trim
        val code: String = data.code.trim
        val password: String = data.password.trim
        val cacheCode = cache_client.get("api" + phone)
        if (cacheCode == null || !String.valueOf(cacheCode).equals(code)) {
          Ok(Json.obj("message" -> Message(ChessPiece.SMS_CODE_ERROR.string, ChessPiece.SMS_CODE_ERROR.pointValue)))
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
        Logger.error("表单校验错误信息--->" + verify_phone_form.bindFromRequest().errors.mkString)
        Ok(Json.obj("message" -> Message(ChessPiece.BAD_PARAMETER.string, ChessPiece.BAD_PARAMETER.pointValue)))
      },
      data => {
        val phone: String = data.phone.trim
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
    )
  }

  /**
    * 用户手机登录
    *
    * @return
    */
  def login_user_phone() = Action { implicit request =>

    user_phone_login_form.bindFromRequest().fold(
      formWithErrors => {
        Logger.error("表单校验错误信息--->" + user_phone_login_form.bindFromRequest().errors.mkString)
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
                        "result" -> UserResultVo(userInfo.id, token, TOKEN_OVER_TIME))
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
                        "result" -> UserResultVo(userInfo.id, token, TOKEN_OVER_TIME))
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
                          "result" -> UserResultVo(userInfo.id, token, TOKEN_OVER_TIME))
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
  def refresh_token = Authenticated { implicit request =>
    val token = request.token
    val newToken = Codecs.md5((System.currentTimeMillis + scala.util.Random.nextString(100)).getBytes())
    cache_client.set(newToken, TOKEN_OVER_TIME, cache_client.get(token))
    cache_client.set(request.userId.toString, TOKEN_OVER_TIME, newToken)
    cache_client.delete(token) //删除旧的cache信息
    Ok(Json.obj(
      "message" -> Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue),
      "result" -> UserResultVo(request.userId, newToken, TOKEN_OVER_TIME))
    )
  }
}
