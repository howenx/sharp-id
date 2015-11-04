package controllers

import javax.inject.{Named, Inject}

import actor.{SMSType, SMS}
import akka.actor.ActorRef
import models.User
import modules.OSSClientProvider
import net.spy.memcached.MemcachedClient
import org.apache.commons.lang3.StringUtils
import play.api.Logger
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.Codecs
import play.api.libs.json.{JsString, JsBoolean, Json}
import play.api.mvc.{Action, Controller}
import utils.SystemService

case class ResetSubmitForm (phoneNum:String,code:String)

/**
 * Created by china_005 on 15/10/28.
 */
class ResetPassWordApplication @Inject() (oss_client : OSSClientProvider, cache_client: MemcachedClient, @Named("sms") sms:ActorRef) extends Controller{

  val sendCodeForm :Form[SendCodeForm] = Form(
    mapping(
      "phoneNum" -> text,
      "imageCode" -> text
    )(SendCodeForm.apply)(SendCodeForm.unapply)
  )
  val resetSubmitForm :Form[ResetSubmitForm] = Form(
    mapping(
      "phoneNum" -> text,
      "code" -> text
    )(ResetSubmitForm.apply)(ResetSubmitForm.unapply)
  )

  //发送短信验证码
  def sendCode  = Action { implicit request =>
    val data = sendCodeForm.bindFromRequest.get
    val imageCode:String = data.imageCode.trim
    val phoneNum:String = data.phoneNum.trim

    if(StringUtils.isEmpty(imageCode) || cache_client.get(imageCode.toUpperCase)==null){
      Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.IMAGE_CODE_ERROR)))
    }
    else if(!SystemService.checkPhoneNum(phoneNum)){//检查手机号码格式
      Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.PHONE_NUM_TYPE_ERROR)))
    }else{
      User.find_by_phone(phoneNum) match {
        case Some(user) =>
          val code = String.valueOf((100000 + Math.random * 900000).toInt)//生成code
          sms ! SMS(phoneNum,code,SMSType.comm)//actor发送短信
          cache_client.set(Codecs.md5(("resetPasswd"+phoneNum).getBytes()),180,code)//code存入缓存
          Logger.debug(s"重置密码手机:$phoneNum 发送验证码:$code")
          Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.SEND_SUCCESS)))
        case None =>
          Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.PHONE_NUM_NO_EXTISTS)))

      }
    }
  }

  //跳转注册页面
  def toResetPassword = Action {
    Ok(views.html.resetPassword.render())
  }

  def resetPasswordSubmit = Action { implicit request =>
    val data = resetSubmitForm.bindFromRequest.get
    val code:String = data.code.trim
    val phoneNum:String = data.phoneNum.trim
    val cacheCode = cache_client.get(Codecs.md5(("resetPasswd"+phoneNum).getBytes()))
    if(StringUtils.isEmpty(code)||StringUtils.isEmpty(phoneNum)){
      Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.PHONE_CODE_ERROR)))
    }else if(cacheCode!=null&&cacheCode.equals(code)){
      val passwd = String.valueOf((100000 + Math.random * 900000).toInt)//生产密码
      val rows = User.reset_password(phoneNum,passwd)//重置密码
      if(rows>0){
        sms ! SMS(phoneNum,passwd,SMSType.resetPasswd)//actor发送短信
        Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.CHANGE_SUCCESS)))
      }
      else{
        Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.CHANGE_FAILED)))
      }

    }else {
      Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.PHONE_CODE_ERROR)))
    }
  }
}
