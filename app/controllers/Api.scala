package controllers

import java.io.{ByteArrayOutputStream, IOException, InputStream}
import java.util.{Calendar, GregorianCalendar}
import javax.inject._

import actor.{SMS, SMSType, WechatUser}
import akka.actor.ActorRef
import filters.Authentication
import models.FormConstModel._
import models._
import net.spy.memcached.MemcachedClient
import play.api.Logger
import play.api.libs.Codecs
import play.api.libs.iteratee.Enumerator
import util.ImageCodeUtil
import util.SysParUtil._
import play.api.libs.json._
import play.api.mvc._

/**
  * 所有用户登录注册相关
  * Created by howen on 16/3/16.
  */
@Singleton
class Api @Inject()(cache_client: MemcachedClient, @Named("sms") sms: ActorRef, @Named("oss") oss: ActorRef, @Named("coupons") couponsActor: ActorRef, @Named("wechatUserInfoActor") wechatUserInfoActor: ActorRef) extends Controller {

  val auth = new Authentication(cache_client)

  import auth.Authenticated

  val idTypeMatch = "W|WO|A|Q|S|B"

  /**
    * 登录操作
    *
    * @param user          UserOpen
    * @param remoteAddress remoteAddress
    * @return
    */
  def login(user: UserOpen, remoteAddress: String): String = {
    UserInfoModel.queryUser(user) match {
      //已经存在的用户登录
      case Some(userInfo) =>
        UserInfoModel.login(userInfo.userId.get, remoteAddress)
        Logger.info(s"用户手机号码：${userInfo.phoneNum.get} 登陆成功")
        val token = Codecs.md5((System.currentTimeMillis + scala.util.Random.nextString(100)).getBytes())
        //设置
        cache_client.set(token, TOKEN_OVER_TIME, Json.stringify(Json.obj("id" -> JsNumber(userInfo.userId.get), "name" -> JsString(userInfo.nickname.get), "photo" -> JsString(userInfo.photoUrl.get))))
        cache_client.set(userInfo.userId.get.toString, TOKEN_OVER_TIME, token)
        //用户一旦登录,就去更新用户将用户所有未使用的过期的优惠券置成状态"S",表示自动失效
        couponsActor ! CouponsVo(None, Some(userInfo.userId.get), None, None, None, None, Some("S"), None, None, None, None)
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
          if (code_times != null && code_times.toString.toInt > SMS_TIMES) {
            Logger.info(s"此用户手机号：$phone api发送验证码当天超过$SMS_TIMES 次")
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
            sms ! SMS(phone, code, data.smsType.getOrElse(SMSType.comm))
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
    * 用户注册,包含绑定三方登录账号注册
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
          var openUser: UserOpen = UserOpen(None, None, None, Some(phone), None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)

          UserInfoModel.queryUser(openUser) match {
            //已经存在的用户自动登录
            case Some(userInfo) =>
              Logger.info(s"用户手机号：$phone 此用户已经注册")
              Ok(Json.obj("message" -> Message(ChessPiece.USER_EXISTS.string, ChessPiece.USER_EXISTS.pointValue)))
            case None =>
              if (UserInfoModel.insert(phone, password, request.remoteAddress).isDefined) {
                Logger.info(s"用户手机号：$phone 注册成功")

                openUser = UserOpen(None, None, Some(password), Some(phone), None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)
                UserInfoModel.queryUser(openUser) match {
                  case Some(userOpen) =>
                    val token = login(openUser, request.remoteAddress)
                    if (token != null) {
                      if (data.idType.isDefined && data.idType.get.matches(idTypeMatch) && data.openId.isDefined) {
                        wechatUserInfoActor ! WechatUser(userOpen.userId.get, data.accessToken.orNull, data.openId.get, data.unionId.orNull,data.idType.get)
                      }
                      Ok(Json.obj(
                        "message" -> Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue),
                        "result" -> UserResultVo(userOpen.userId.get, token, TOKEN_OVER_TIME))
                      )
                    } else {
                      cache_client.set(openUser.phoneNum.orNull + "_check", 60 * 60, 1)
                      Ok(Json.obj("message" -> Message(ChessPiece.USERNAME_OR_PASSWORD_ERROR.string, ChessPiece.USERNAME_OR_PASSWORD_ERROR.pointValue)))
                    }
                  case None =>
                    Ok(Json.obj("message" -> Message(ChessPiece.NOT_REGISTERED.string, ChessPiece.NOT_REGISTERED.pointValue)))
                }
              }
              else {
                Ok(Json.obj("message" -> Message(ChessPiece.ERROR.string, ChessPiece.ERROR.pointValue)))
              }
          }
        }
      }
    )
  }

  /**
    * 重置密码并返回token,包含微信重置密码绑定登录
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
          var openUser: UserOpen = UserOpen(None, None, None, Some(phone), None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)
          UserInfoModel.queryUser(openUser) match {
            case Some(userOpen) =>
              //已经存在的用户自动登录
              if (UserInfoModel.reset_password(phone, password) > 0) {
                Logger.info(s"用户手机号：$phone 重置密码成功")

                openUser = UserOpen(None, None, Some(password), Some(phone), None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)
                val token = login(openUser, request.remoteAddress)
                if (token != null) {
                  if (data.idType.isDefined && data.idType.get.matches(idTypeMatch) && data.openId.isDefined) {
                    wechatUserInfoActor ! WechatUser(userOpen.userId.get, data.accessToken.orNull, data.openId.get, data.unionId.orNull,data.idType.get)
                  }
                  Ok(Json.obj(
                    "message" -> Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue),
                    "result" -> UserResultVo(userOpen.userId.get, token, TOKEN_OVER_TIME))
                  )
                } else {
                  cache_client.set(openUser.phoneNum.orNull + "_check", 60 * 60, 1)
                  Ok(Json.obj("message" -> Message(ChessPiece.USERNAME_OR_PASSWORD_ERROR.string, ChessPiece.USERNAME_OR_PASSWORD_ERROR.pointValue)))
                }
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
          val openUser: UserOpen = UserOpen(None, None, None, Some(phone), None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)
          UserInfoModel.queryUser(openUser) match {
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
            val openUser: UserOpen = UserOpen(None, None, None, Some(phone), None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)
            UserInfoModel.queryUser(openUser) match {
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
    * 用户手机登录以及三方登录绑定手机号
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
              var openUser: UserOpen = UserOpen(None, None, None, Some(phone), None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)
              UserInfoModel.queryUser(openUser) match {
                //已经存在的用户登录
                case Some(userInfo) =>
                  openUser = UserOpen(None, None, Some(password), Some(phone), None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)
                  val token = login(openUser, request.remoteAddress)

                  if (verifyLockTimes == null) {
                    if (token != null) {
                      if (data.idType.isDefined && data.idType.get.matches(idTypeMatch) && data.openId.isDefined) {
                        wechatUserInfoActor ! WechatUser(userInfo.userId.get, data.accessToken.orNull, data.openId.get, data.unionId.orNull,data.idType.get)
                      }
                      Ok(Json.obj(
                        "message" -> Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue),
                        "result" -> UserResultVo(userInfo.userId.get, token, TOKEN_OVER_TIME))
                      )
                    } else {
                      cache_client.set(phone + "_check", 60 * 60, 1)
                      Ok(Json.obj("message" -> Message(ChessPiece.USERNAME_OR_PASSWORD_ERROR.string, ChessPiece.USERNAME_OR_PASSWORD_ERROR.pointValue)))
                    }

                  } else if (verifyLockTimes.toString.toInt < 6) {
                    Logger.info(s"输入错误次数：$verifyLockTimes")
                    if (token != null) {
                      cache_client.delete(phone + "_check")
                      if (data.idType.isDefined && data.idType.get.matches(idTypeMatch) && data.openId.isDefined) {
                        wechatUserInfoActor ! WechatUser(userInfo.userId.get, data.accessToken.orNull, data.openId.get, data.unionId.orNull,data.idType.get)
                      }
                      Ok(Json.obj(
                        "message" -> Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue),
                        "result" -> UserResultVo(userInfo.userId.get, token, TOKEN_OVER_TIME))
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
                        if (data.idType.isDefined && data.idType.get.matches(idTypeMatch) && data.openId.isDefined) {
                          wechatUserInfoActor ! WechatUser(userInfo.userId.get, data.accessToken.orNull, data.openId.get, data.unionId.orNull,data.idType.get)
                        }
                        Ok(Json.obj(
                          "message" -> Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue),
                          "result" -> UserResultVo(userInfo.userId.get, token, TOKEN_OVER_TIME))
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


  /**
    * 查询此微信用户是否注册过
    *
    * @param openId 微信平台用户唯一识别ID
    * @return
    */
  def verify_open_user(openId: String, idType: String, unionId: String) = Action { implicit request =>
    if (openId != "" && idType != null && idType.matches(idTypeMatch)) {
      val idThree: IdThree = IdThree(None, Option(openId), Option(idType), None, Option(unionId))

      Logger.error("前台数据:"+idThree)

      UserInfoModel.id_three_query(idThree) match {
        case Some(userOpen) =>
          val openUser: UserOpen = UserOpen(userOpen.userId, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)
          Logger.info("Wechat user sign up: " + userOpen.userId)
          val token = login(openUser, request.remoteAddress)
          if (token != null) {
            Ok(Json.obj(
              "message" -> Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue),
              "result" -> UserResultVo(userOpen.userId.get, token, TOKEN_OVER_TIME))
            )
          } else {
            Ok(Json.obj("message" -> Message(ChessPiece.USERNAME_OR_PASSWORD_ERROR.string, ChessPiece.USERNAME_OR_PASSWORD_ERROR.pointValue)))
          }
        case None =>
          if (unionId != null) {
            var idThree: IdThree = IdThree(None, None, None, None, Option(unionId))
            UserInfoModel.id_three_query(idThree) match {
              case Some(userOpen) =>
                val openUser: UserOpen = UserOpen(userOpen.userId, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)
                Logger.info("Wechat user sign up: " + userOpen.userId)
                val token = login(openUser, request.remoteAddress)
                if (token != null) {
                  idThree = IdThree(None, Option(openId), Option(idType), userOpen.userId, Option(unionId))
                  UserInfoModel.id_three_insert(idThree)
                  Ok(Json.obj(
                    "message" -> Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue),
                    "result" -> UserResultVo(userOpen.userId.get, token, TOKEN_OVER_TIME))
                  )
                } else {
                  Ok(Json.obj("message" -> Message(ChessPiece.USERNAME_OR_PASSWORD_ERROR.string, ChessPiece.USERNAME_OR_PASSWORD_ERROR.pointValue)))
                }
              case None =>
                Ok(Json.obj("message" -> Message(ChessPiece.NOT_REGISTERED.string, ChessPiece.NOT_REGISTERED.pointValue)))
            }
          }
          Ok(Json.obj("message" -> Message(ChessPiece.NOT_REGISTERED.string, ChessPiece.NOT_REGISTERED.pointValue)))
      }
    } else Ok(Json.obj("message" -> Message(ChessPiece.BAD_PARAMETER.string, ChessPiece.BAD_PARAMETER.pointValue)))
  }

}
