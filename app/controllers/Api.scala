package controllers

import javax.inject._
import actor.{UserLoginWithoutCache, SMSType, SMS}
import akka.actor.ActorRef
import net.spy.memcached.MemcachedClient
import play.api.data._
import play.api.data.Forms._
import play.api.libs.Codecs
import play.api.libs.json.{JsString, JsBoolean, Json}
import play.api.mvc.{Action, Controller}
import play.api.{Configuration, Logger}
import models.User
import utils.SystemService

case class UserNameLoginInfo(name: String, passwd: String)

case class UserPhoneLoginInfo(phone:String,code:String)

case class ApiSendCodeForm(phone:String,msg:String)
/**
 * Created by handy on 15/10/23.
 * Daumkakao china
 */
@Singleton
class Api @Inject() (cache_client: MemcachedClient, @Named("sms") sms:ActorRef, @Named("userLogin") userLogin:ActorRef,configuration: Configuration) extends Controller {

  /**
   *使用昵称和密码登录
   * TODO 需要验证表单
   */
  val user_name_login_form = Form(mapping(
  "name" ->nonEmptyText,
  "passwd" -> nonEmptyText
  )(UserNameLoginInfo.apply)(UserNameLoginInfo.unapply))

  /**
   * 使用电话号码和密码登录
   * TODO 需要检查电话号码
   */
  val user_phone_login_form = Form(mapping(
    "phone" ->nonEmptyText,
    "code" -> nonEmptyText
  )(UserPhoneLoginInfo.apply)(UserPhoneLoginInfo.unapply))

  /**
   * 发送验证码杰克
   * TODO 需要检查电话号码、msg
   */
  val api_send_code_form  = Form(mapping(
    "phone" ->nonEmptyText,
    "msg" -> nonEmptyText
  )(ApiSendCodeForm.apply)(ApiSendCodeForm.unapply))

  def send_code = Action { implicit request =>
    val data = api_send_code_form.bindFromRequest().get
    val phone:String = data.phone
    val msg = data.msg
    if(!SystemService.checkPhoneNum(phone)){
      Ok(Json.obj("if" -> JsBoolean(false), "back" -> JsString("电话号码不符合规则")))
    }
    else if(!Codecs.md5((phone+"5dsy").getBytes()).equals(msg)){
      Ok(Json.obj("if" -> JsBoolean(false), "back" -> JsString("验证失败")))
    }
    else {
      val code:String = String.valueOf((100000 + Math.random * 900000).toInt)
      sms!SMS(phone,code,SMSType.comm)
      cache_client.set("api"+phone,180,code)
      Ok(Json.obj("if" -> JsBoolean(true), "back" -> JsString("发送成功")))
    }

  }

  def login_phone_num = Action { implicit request =>
    val data = user_phone_login_form.bindFromRequest().get
    val phone : String = data.phone
    val code : String = data.code
    val cacheCode = cache_client.get("api"+phone)
    if(!SystemService.checkPhoneNum(phone)){
      Ok(Json.obj("if" -> JsBoolean(false), "back" -> JsString("电话号码不符合规则")))
    }
    else if(cacheCode ==null|| !String.valueOf(cacheCode).equals(code)){
      Ok(Json.obj("if" -> JsBoolean(false), "back" -> JsString("验证码错误")))
    }
    else {
      User.find_by_phone(phone) match {
        //已经存在的用户自动登录
        case Some(userInfo) =>
          userLogin!UserLoginWithoutCache(userInfo.id,request.remoteAddress)
          Ok(Json.obj("if" -> JsBoolean(true), "back" -> JsString("登录成功")))

        //未注册的用户自动注册并登陆
        case None =>
          val password = String.valueOf((100000 + Math.random * 900000).toInt)//生产密码
          //注册并返回id
          User.insert(phone,password,request.remoteAddress) match {
            case Some(id)=>
              if(id >0 ){
                sms ! SMS(phone,password,SMSType.passwd)//actor发送短信
                userLogin!UserLoginWithoutCache(id,request.remoteAddress)
                Ok(Json.obj("if" -> JsBoolean(true), "back" -> JsString("登录成功")))
              }
              else{
                Ok(Json.obj("if" -> JsBoolean(false), "back" -> JsString("注册失败!")))
              }
            case None =>
              Ok(Json.obj("if" -> JsBoolean(false), "back" -> JsString("注册失败")))
          }
      }
    }
  }


  def login_user_name = Action { implicit request =>
    val data = user_name_login_form.bindFromRequest().get
    val name = data.name
    val passwd = data.passwd
    val phone = ("^1[3-8][0-9]{9}").r
    val email = ("\\b[a-zA-Z0-9.!#$%&\'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*\\b").r
    name match{
      case phone =>
        User.find_by_phone(phone,passwd) match{
          case Some(user)=>
            userLogin!UserLoginWithoutCache(user.id,request.remoteAddress)
            Ok(Json.obj("if" -> JsBoolean(true), "back" -> JsString("登录成功")))
          case None =>
            Ok(Json.obj("if" -> JsBoolean(false), "back" -> JsString("用户密码错误")))
        }
      case email =>
        User.find_by_email(name,passwd)match{
          case Some(user)=>
            userLogin!UserLoginWithoutCache(user.id,request.remoteAddress)
            Ok(Json.obj("if" -> JsBoolean(true), "back" -> JsString("登录成功")))
          case None =>
            Ok(Json.obj("if" -> JsBoolean(false), "back" -> JsString("用户密码错误")))
        }
      case _ =>
        User.find_by_nickname(name,passwd)match{
          case Some(user)=>
            userLogin!UserLoginWithoutCache(user.id,request.remoteAddress)
            Ok(Json.obj("if" -> JsBoolean(true), "back" -> JsString("登录成功")))
          case None =>
            Ok(Json.obj("if" -> JsBoolean(false), "back" -> JsString("用户密码错误")))
        }
    }
  }

}
