package controllers

import javax.inject.{Named, Inject}
import actor.{SMSType, SMS}
import akka.actor.ActorRef
import models.{User}
import modules.OSSClientProvider
import net.spy.memcached.MemcachedClient
import org.apache.commons.lang3.StringUtils
import play.api.{Configuration, Logger}
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.Codecs
import play.api.libs.json.{JsString, JsBoolean,Json}
import play.api.mvc.{Action, Controller}
import play.libs.mailer.{MailerPlugin, Email}
import utils.{SystemService}

/**
 * Created by china_005 on 15/10/28.
 */

case class DetailSendCodeForm(phoneNum:String,userId:Long)

case class ChangePhoneForm(phoneNum:String,newCode:String,oldCode :String,userId:Long)

case class ChangePasswordForm(newPassword:String,oldPassword:String,userId:Long,againPassword:String)

case class SendEmailForm(id:Long,email:String)

class UserDetailApplication @Inject() (oss_client : OSSClientProvider, cache_client: MemcachedClient, @Named("sms") sms:ActorRef,configuration: Configuration) extends  Controller{

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

  val sendEmailForm :Form[SendEmailForm] = Form(
    mapping(
      "id" ->longNumber,
      "email" ->nonEmptyText
    )(SendEmailForm.apply)(SendEmailForm.unapply)
  )

  def toUserDetail = Action{ implicit request=>
    try{
      val cookie = request.cookies(Systemcontents.WEB_TOKEN)
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
      Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.PHONE_NUM_TYPE_ERROR)))
    }else{
      //判定修改的是本人的信息
        try{
          val cookie = request.cookies(Systemcontents.WEB_TOKEN)
          cookie match {
            case null =>
              Redirect("/toLogin?url=toUserDetail")
            case _ =>
              val cacheInfo = cache_client.get(cookie.value.toString)
              if(cacheInfo==null || userId!=(play.libs.Json.parse(String.valueOf(cacheInfo))).get("user_id").intValue())
                Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.USER_INFO_ERROR)))
          }
        }catch {
          case ex:RuntimeException =>
            Redirect("/toLogin?url=toUserDetail")
        }
      User.find_by_phone(phoneNum) match {
        case Some(user) =>
          Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.PHONE_NUM_EXTISTS)))
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

              Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.SEND_SUCCESS)))
            case None =>
              Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.USER_INFO_ERROR)))
          }
      }
    }
  }

  def changePhone = Action { implicit request =>
    val data = changePhoneForm.bindFromRequest.get
    val phoneNum = data.phoneNum
    val newCode = data.newCode
    val oldCOde = data.oldCode
    val userId:Long = data.userId
    if (StringUtils.isEmpty(phoneNum) || StringUtils.isEmpty(newCode) || StringUtils.isEmpty(oldCOde)) {
      Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.PHONE_CODE_ERROR)))
    }
    else {
      //判定修改的是本人的信息
      try{
        val cookie = request.cookies(Systemcontents.WEB_TOKEN)
        cookie match {
          case null =>
            Redirect("/toLogin?url=toUserDetail")
          case _ =>
            val cacheInfo = cache_client.get(cookie.value.toString)
            if(cacheInfo==null || userId!=(play.libs.Json.parse(String.valueOf(cacheInfo))).get("user_id").intValue())
              Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.USER_INFO_ERROR)))
        }
      }catch {
        case ex:RuntimeException =>
          Redirect("/toLogin?url=toUserDetail")
      }
      User.find_by_id_more(userId) match {
        case Some(user) =>
          val cacheNew = cache_client.get(Codecs.md5(("detailNew"+phoneNum).getBytes()))
          val cacheOld = cache_client.get(Codecs.md5(("detailOld"+user.phone_num.trim).getBytes()))
          if (cacheNew!=null && cacheNew.equals(newCode)) {
            if(cacheOld!=null && cacheOld.equals(oldCOde)){
              if (User.change_phone(phoneNum, userId) > 0) {
                Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.CHANGE_SUCCESS)))
              } else {
                Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.CHANGE_FAILED)))
              }
            }
            else {
              User.find_by_phone(user.phone_num.trim,oldCOde) match{
                case Some(user) =>
                  if (User.change_phone(phoneNum, userId) > 0) {
                    Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.CHANGE_SUCCESS)))
                  } else {
                    Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.CHANGE_FAILED)))
                  }
                case None =>
                  Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.PHONE_CODE_ERROR)))
              }
            }
          }
          else {
            Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.PHONE_CODE_ERROR)))
          }
        case None =>
          Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.USER_INFO_ERROR)))
      }
    }
  }

  def changePassword = Action { implicit request =>
    val data = changePasswordForm.bindFromRequest.get
    val id :Long = data.userId
    val newPassword = data.newPassword
    val oldPassword = data.oldPassword
    val againPassword = data.againPassword
    if (StringUtils.isEmpty(newPassword) || StringUtils.isEmpty(oldPassword) || StringUtils.isEmpty(againPassword)) {
      Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString("密码不能为空")))
    }
    else if (!newPassword.equals(againPassword)){
      Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString("两次密码不一致")))
    }
    else if (oldPassword.equals(againPassword)){
      Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString("新旧密码不能一致")))
    }
    else{
      try{
        val cookie = request.cookies(Systemcontents.WEB_TOKEN)
        cookie match {
          case null =>
            Redirect("/toLogin?url=toUserDetail")
          case _ =>
            val cacheInfo = cache_client.get(cookie.value.toString)
            if(cacheInfo==null || id!=(play.libs.Json.parse(String.valueOf(cacheInfo))).get("user_id").intValue())
              Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.USER_INFO_ERROR)))
        }
      }catch {
        case ex:RuntimeException =>
          Redirect("/toLogin?url=toUserDetail")
      }
      if(User.change_password(newPassword,id,oldPassword)>0){
        Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.CHANGE_SUCCESS)))
      }else{
        Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.CHANGE_FAILED)))
      }
    }
  }

  def sendEmail = Action{ implicit request =>
    val data = sendEmailForm.bindFromRequest().get
    Logger.info(data.toString)
    val id:Long = data.id
    val email:String = data.email.trim
    if(StringUtils.isEmpty(email) || !SystemService.checkEmail(email)){
      Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.EMAIL_TYPE_ERROR)))
    }
    else {
      try {
        val cookie = request.cookies(Systemcontents.WEB_TOKEN)
        cookie match {
          case null =>
            Redirect("/toLogin?url=toUserDetail")
          case _ =>
            val cacheInfo = cache_client.get(cookie.value.toString)
            if (cacheInfo == null || id != (play.libs.Json.parse(String.valueOf(cacheInfo))).get("user_id").intValue())
              Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.USER_INFO_ERROR)))
        }
      } catch {
        case ex: RuntimeException =>
          Redirect("/toLogin?url=toUserDetail")
      }
      User.find_by_id_more(id) match{
        case Some(user) =>
          if(user.active.equals("Y")){Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.EMAIL_ACTIVE_OREADY)))}
          else{
            Logger.info("准备发送邮件")
            val emailSender = new Email
            emailSender.setSubject("HMM账户激活")
            emailSender.setFrom(configuration.getString("emailFrom").getOrElse(""))
            emailSender.addTo(email.trim)
            val securityStr: String = Codecs.md5((id+email.trim).getBytes)
            emailSender.setBodyHtml(SystemService.getEmailHtml(configuration.getString("activeUrl").getOrElse("")+securityStr))
            try {
              val result: String = MailerPlugin.send(emailSender)
              if (StringUtils.isEmpty(result)) {
                Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.SEND_EMAIL_FAILED)))
              }
              else {
                cache_client.set(securityStr, 30 * 60, id)
                Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.SEND_EMAIL_SUCCESS)))
              }
            }
            catch {
              case e: Exception => {
                Logger.info(e.toString)
                Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.SEND_EMAIL_FAILED)))
              }
            }
          }
        case None =>
          Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.USER_INFO_ERROR)))
      }
    }
  }

  def active(str:String) = Action{ implicit request =>
    if(StringUtils.isEmpty(str)){
      Ok(views.html.active.render(Systemcontents.EMAIL_ACTIVE_FAILED))
    }else if(cache_client.get(str)==null){
      Ok(views.html.active.render(Systemcontents.EMAIL_ACTIVE_FAILED))
    }else {
      val id = cache_client.get(str).toString.toLong
      User.find_by_id_more(id) match {
        case Some(user) =>
          if(Codecs.md5((user.id+user.email).getBytes).equals(str)){
            if(User.active(id)>0){
              Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.EMAIL_ACTIVE_SUCCESS)))
            }else{
              Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.EMAIL_ACTIVE_FAILED)))
            }
          }else{
            Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.EMAIL_ACTIVE_FAILED)))
          }
        case None =>
          Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.EMAIL_ACTIVE_FAILED)))
      }
    }
  }

}
