package controllers

import javax.inject._

import actor.{SMS, SMSType}
import akka.actor.ActorRef
import models.{CouponsVo, User}
import net.spy.memcached.MemcachedClient
import play.api.data.Forms._
import play.api.data._
import play.api.libs.Codecs
import play.api.libs.json._
import play.api.mvc.{Action, Controller}
import play.api.{Configuration, Logger}
import utils.SystemService

case class UserNameLoginInfo(name: String, password: String)

case class UserPhoneLoginInfo(phone: String, code: String)

case class ApiSendCodeForm(phone: String, msg: String)

case class ApiRegForm(phone: String, code: String, password: String)

/**
  * Created by handy on 15/10/23.
  * Daumkakao china
  */
@Singleton
class Api @Inject()(cache_client: MemcachedClient, @Named("sms") sms: ActorRef, @Named("oss") oss: ActorRef,@Named("coupons") couponsActor: ActorRef, configuration: Configuration) extends Controller {

  /**
    * 使用昵称和密码登录
    * TODO 需要验证表单
    */
  val user_name_login_form = Form(mapping(
    "name" -> nonEmptyText,
    "password" -> nonEmptyText
  )(UserNameLoginInfo.apply)(UserNameLoginInfo.unapply))

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
    if (!SystemService.checkPhoneNum(phone)) {
      Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.PHONE_NUM_TYPE_ERROR)))
    }
    else if (!Codecs.md5((phone + "hmm").getBytes()).equals(msg)) {
      Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.API_RESULT_MESSAGE -> JsString("验证失败")))
    }
    else {
      val code: String = String.valueOf((100000 + Math.random * 900000).toInt)
      sms ! SMS(phone, code, SMSType.comm)
      cache_client.set("api" + phone, 180, code)
      Logger.info(s"用户手机号：$phone api发送验证码成功")
      Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.SEND_SUCCESS)))
    }

  }

  def reg = Action { implicit request =>
    val data = api_reg_form.bindFromRequest().get
    val phone: String = data.phone.trim
    val code: String = data.code.trim
    val password: String = data.password.trim
    val cacheCode = cache_client.get("api" + phone)
    if (!SystemService.checkPhoneNum(phone)) {
      Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.PHONE_NUM_TYPE_ERROR)))
    }
    else if (cacheCode == null || !String.valueOf(cacheCode).equals(code)) {
      Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.PHONE_CODE_ERROR)))
    }
    else if (password.length > 20) {
      Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.API_RESULT_MESSAGE -> JsString("密码长度不能超过20位")))
    }
    else {
      User.insert(phone, password, request.remoteAddress) match {
        case Some(id) =>
          if (id > 0) {
            Logger.info(s"用户手机号：$phone 注册成功")
            Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.REG_SUCCESS)))
          }
          else {
            Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.REG_FAILED)))
          }
        case None =>
          Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.REG_FAILED)))
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
      Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.PHONE_NUM_TYPE_ERROR)))
    }
    else if (cacheCode == null || !String.valueOf(cacheCode).equals(code)) {
      Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.PHONE_CODE_ERROR)))
    }
    else if (password.length > 20) {
      Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.API_RESULT_MESSAGE -> JsString("密码长度不能超过20位")))
    }
    else if (User.reset_password(phone, password) > 0) {
      Logger.info(s"用户手机号：$phone 重置密码成功")
      Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.CHANGE_SUCCESS)))
    } else {
      Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.CHANGE_FAILED)))
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


  def login_user_name = Action { implicit request =>

    val data = user_name_login_form.bindFromRequest().get
    val name = data.name.trim
    val password = data.password.trim
    val phone = "^1[3-8][0-9]{9}".r
    val email = "\\b[a-zA-Z0-9.!#$%&\'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*\\b".r
    name match {
      case phone() =>
        User.find_by_phone(name, password) match {
          case Some(user) =>
            User.login(user.id, request.remoteAddress)
            Logger.info(s"用户手机号码：$name 登陆成功")
            val token = Codecs.md5((System.currentTimeMillis + scala.util.Random.nextString(100)).getBytes())
            cache_client.set(token, 60 * 60 * 24 * 7, Json.stringify(Json.obj("id" -> JsString(String.valueOf(user.id)), "name" -> JsString(user.nickname), "photo" -> JsString(user.photo_url))))
            //用户一旦登录,就去更新用户将用户所有未使用的过期的优惠券置成状态"S",表示自动失效
            couponsActor ! CouponsVo(None,Some(user.id),None,None,None,None,Some("S"),None,None,None,None)
            Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.LOGIN_SUCCESS)
              , Systemcontents.API_RESULT_TOKEN -> JsString(token), Systemcontents.API_RESULT_OVER_TIME -> JsNumber(60 * 60 * 24 * 7)))
          case None =>
            Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.USER_PASSWORD_ERROR)))
        }
      case email() =>
        User.find_by_email(name, password) match {
          case Some(user) =>
            User.login(user.id, request.remoteAddress)
            Logger.info(s"用户邮件：$name 登陆成功")
            val token = Codecs.md5((System.currentTimeMillis + scala.util.Random.nextString(100)).getBytes())
            cache_client.set(token, 60 * 60 * 24 * 7, Json.stringify(Json.obj("id" -> JsString(String.valueOf(user.id)), "name" -> JsString(user.nickname), "photo" -> JsString(user.photo_url))))
            Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.LOGIN_SUCCESS)
              , Systemcontents.API_RESULT_TOKEN -> JsString(token), Systemcontents.API_RESULT_OVER_TIME -> JsNumber(60 * 60 * 24 * 7)))
          case None =>
            Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.USER_PASSWORD_ERROR)))
        }
      case _ =>
        User.find_by_nickname(name, password) match {
          case Some(user) =>
            User.login(user.id, request.remoteAddress)
            Logger.info(s"用户昵称：$name 登陆成功")
            val token = Codecs.md5((System.currentTimeMillis + scala.util.Random.nextString(100)).getBytes())
            cache_client.set(token, 60 * 60 * 24 * 7, Json.stringify(Json.obj("id" -> JsNumber(user.id), "name" -> JsString(user.nickname), "photo" -> JsString(user.photo_url))))
            Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.LOGIN_SUCCESS)
              , Systemcontents.API_RESULT_TOKEN -> JsString(token), Systemcontents.API_RESULT_OVER_TIME -> JsNumber(60 * 60 * 24 * 7)))
          case None =>
            Ok(Json.obj(Systemcontents.API_RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.API_RESULT_MESSAGE -> JsString(Systemcontents.USER_PASSWORD_ERROR)))
        }
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
}
