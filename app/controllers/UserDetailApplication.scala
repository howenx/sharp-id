package controllers

import javax.inject.{Named, Inject}
import actor.{SMSType, SMS}
import akka.actor.ActorRef
import models.{User}
import modules.OSSClientProvider
import net.spy.memcached.MemcachedClient
import org.apache.commons.lang3.StringUtils
import play.api.Logger
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.Codecs
import play.api.libs.json.{JsString, JsBoolean,Json}
import play.api.mvc.{Action, Controller}
import utils.SystemService

/**
 * Created by china_005 on 15/10/28.
 */

case class DetailSendCodeForm(phoneNum:String,userId:Long)

case class ChangePhoneForm(phoneNum:String,newCode:String,oldCode :String,userId:Long)

case class ChangePasswordForm(newPassword:String,oldPassword:String,userId:Long,againPassword:String)

class UserDetailApplication @Inject() (oss_client : OSSClientProvider, cache_client: MemcachedClient, @Named("sms") sms:ActorRef) extends  Controller{

  val detailSendCodeForm :Form[DetailSendCodeForm] = Form(
    mapping(
      "phoneNum" -> text,
      "userId" -> longNumber
    )(DetailSendCodeForm.apply)(DetailSendCodeForm.unapply)
  )
  val changePhoneForm :Form[ChangePhoneForm] = Form(
    mapping(
      "phoneNum" -> text,
      "newCode" -> text,
      "oldCode" ->text,
      "userId" ->longNumber
    )(ChangePhoneForm.apply)(ChangePhoneForm.unapply)
  )

  val changePasswordForm :Form[ChangePasswordForm] = Form(
    mapping(
      "newPassword" -> text,
      "oldPassword" -> text,
      "userId" ->longNumber,
      "againPassword" ->text
    )(ChangePasswordForm.apply)(ChangePasswordForm.unapply)
  )

  def toUserDetail = Action{ implicit request=>
    try{
      val cookie = request.cookies("web_token")
      cookie match {
        case null =>
          Redirect("/toLogin?url=toUserDetail")
        case _ =>
          val cacheInfo = cache_client.get(cookie.value.toString)
          cacheInfo match {
            case null =>
              Redirect("/toLogin?url=toUserDetail")
            case _ =>
              User.find_by_id_more(play.libs.Json.parse(String.valueOf(cacheInfo)).get("user_id").intValue()) match{
                case Some(user) =>
                  Ok(views.html.userDetail.render(user))
                case None =>
                  Redirect("/toLogin?url=toUserDetail")
              }
          }
      }
    }catch {
      case ex:RuntimeException =>
        Redirect("/toLogin?url=toUserDetail")
    }

  }

  //发送短信验证码
  def sendCode  = Action { implicit request =>
    val data = detailSendCodeForm.bindFromRequest.get
    val userId:Long = data.userId
    val phoneNum:String = data.phoneNum
    if(!SystemService.checkPhoneNum(phoneNum)){//检查手机号码格式
      Ok(Json.obj("if" -> JsBoolean(false), "back" -> JsString("电话号码不符合规则")))
    }else{
        try{
          val cookie = request.cookies("web_token")
          cookie match {
            case null =>
              Redirect("/toLogin?url=toUserDetail")
            case _ =>
              val cacheInfo = cache_client.get(cookie.value.toString)
              if(cacheInfo==null || userId!=(play.libs.Json.parse(String.valueOf(cacheInfo))).get("user_id").intValue())
                Ok(Json.obj("if" -> JsBoolean(false), "back" -> JsString("用户信息读取错误！")))
          }
        }catch {
          case ex:RuntimeException =>
            Redirect("/toLogin?url=toUserDetail")
        }
      User.find_by_phone(phoneNum) match {
        case Some(user) =>
          Ok(Json.obj("if" -> JsBoolean(false), "back" -> JsString("电话号码已经注册")))
        case None =>
          User.find_by_id_more(userId) match{
            case Some(user)=>
              val code = String.valueOf((100000 + Math.random * 900000).toInt)//生成code
              sms ! SMS(phoneNum,code,SMSType.comm)//actor发送短信
              cache_client.set(Codecs.md5(("detailNew"+phoneNum).getBytes()),180,code)//code存入缓存
              Logger.debug(s"更换新手机:$phoneNum 发送验证码:$code")

              val oldCode = String.valueOf((100000 + Math.random * 900000).toInt)//生成code
              sms ! SMS(user.phone_num,oldCode,SMSType.comm)//actor发送短信
              cache_client.set(Codecs.md5(("detailOld"+user.phone_num.trim).getBytes()),180,oldCode)//code存入缓存
              Logger.debug(s"更换旧手机:${user.phone_num.trim} 发送验证码:$oldCode")

              Ok(Json.obj("if" -> JsBoolean(true), "back" -> JsString("发送成功")))
            case None =>
              Ok(Json.obj("if" -> JsBoolean(false), "back" -> JsString("用户信息读取错误")))
          }
      }
    }
  }

  def changePhone = Action { implicit request =>
    val data = changePhoneForm.bindFromRequest.get
    val phoneNum = data.phoneNum
    val newCode = data.newCode
    val oldCOde = data.oldCode
    val userId = data.userId
    if (StringUtils.isEmpty(phoneNum) || StringUtils.isEmpty(newCode) || StringUtils.isEmpty(oldCOde) || userId == null) {
      Ok(Json.obj("if" -> JsBoolean(false), "back" -> JsString("验证码不能为空")))
    }
    else {
      try{
        val cookie = request.cookies("web_token")
        cookie match {
          case null =>
            Redirect("/toLogin?url=toUserDetail")
          case _ =>
            val cacheInfo = cache_client.get(cookie.value.toString)
            if(cacheInfo==null || userId!=(play.libs.Json.parse(String.valueOf(cacheInfo))).get("user_id").intValue())
              Ok(Json.obj("if" -> JsBoolean(false), "back" -> JsString("用户信息读取错误！")))
        }
      }catch {
        case ex:RuntimeException =>
          Redirect("/toLogin?url=toUserDetail")
      }
      User.find_by_id_more(userId) match {
        case Some(user) =>
          Logger.info(cache_client.get(Codecs.md5(("detailNew"+phoneNum).getBytes())).toString+"======"+cache_client.get(Codecs.md5(("detailOld"+user.phone_num.trim).getBytes())).toString)
          if (cache_client.get(Codecs.md5(("detailNew"+phoneNum).getBytes()))==null||cache_client.get(Codecs.md5(("detailOld"+user.phone_num.trim).getBytes()))==null
          || !cache_client.get(Codecs.md5(("detailNew"+phoneNum).getBytes())).equals(newCode)|| !cache_client.get(Codecs.md5(("detailOld"+user.phone_num.trim).getBytes())).equals(oldCOde)) {
            Ok(Json.obj("if" -> JsBoolean(false), "back" -> JsString("验证码错误")))
          }else {
            if (User.change_phone(phoneNum, userId) > 0) {
              Ok(Json.obj("if" -> JsBoolean(true), "back" -> JsString("修改成功")))
            } else {
              Ok(Json.obj("if" -> JsBoolean(false), "back" -> JsString("修改失败")))
            }
          }
        case None =>
          Ok(Json.obj("if" -> JsBoolean(false), "back" -> JsString("用户信息读取错误")))
      }
    }
  }

  def changePassword = Action { implicit request =>
    val data = changePasswordForm.bindFromRequest.get
    val id = data.userId
    val newPassword = data.newPassword
    val oldPassword = data.oldPassword
    val againPassword = data.againPassword
    if (StringUtils.isEmpty(newPassword) || StringUtils.isEmpty(oldPassword) || StringUtils.isEmpty(againPassword) || id == null) {
      Ok(Json.obj("if" -> JsBoolean(false), "back" -> JsString("密码不能为空")))
    }
    else if (!newPassword.equals(againPassword)){
      Ok(Json.obj("if" -> JsBoolean(false), "back" -> JsString("两次密码不一致")))
    }
    else if (oldPassword.equals(againPassword)){
      Ok(Json.obj("if" -> JsBoolean(false), "back" -> JsString("新旧密码不能一致")))
    }
    else{
      try{
        val cookie = request.cookies("web_token")
        cookie match {
          case null =>
            Redirect("/toLogin?url=toUserDetail")
          case _ =>
            val cacheInfo = cache_client.get(cookie.value.toString)
            if(cacheInfo==null || id!=(play.libs.Json.parse(String.valueOf(cacheInfo))).get("user_id").intValue())
              Ok(Json.obj("if" -> JsBoolean(false), "back" -> JsString("用户信息读取错误！")))
        }
      }catch {
        case ex:RuntimeException =>
          Redirect("/toLogin?url=toUserDetail")
      }
      if(User.change_password(newPassword,id,oldPassword)>0){
        Ok(Json.obj("if" -> JsBoolean(true), "back" -> JsString("修改成功")))
      }else{
        Ok(Json.obj("if" -> JsBoolean(false), "back" -> JsString("修改失败")))
      }
    }
  }

}
