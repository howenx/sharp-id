package controllers

import java.io.{IOException, ByteArrayOutputStream, InputStream}
import javax.inject.{Named, Inject}
import actor.{SMSType, SMS}
import akka.actor.ActorRef
import models.User
import modules.OSSClientProvider
import net.spy.memcached.MemcachedClient
import org.apache.commons.lang3.StringUtils
import play.api.Logger
import play.api.libs.Codecs
import play.api.libs.iteratee.Enumerator
import play.api.data._
import play.api.data.Forms._
import play.api.libs.json._
import play.api.mvc.{ResponseHeader, Result, Action, Controller}
import utils.{ImageCodeService, SystemService}

/**
  *
 * Created by china_005 on 15/10/23.
 */

case class SendCodeForm (phoneNum:String,imageCode:String)

case class RegSubmitForm (phoneNum:String,code:String)

class RegApplication @Inject() (oss_client : OSSClientProvider, cache_client: MemcachedClient, @Named("sms") sms:ActorRef) extends Controller {

  val sendCodeForm :Form[SendCodeForm] = Form(
    mapping(
      "phoneNum" -> text,
      "imageCode" -> text
    )(SendCodeForm.apply)(SendCodeForm.unapply)
  )

  val regSubmitForm :Form[RegSubmitForm] = Form(
    mapping(
      "phoneNum" -> text,
      "code" -> text
    )(RegSubmitForm.apply)(RegSubmitForm.unapply)
  )

  //获取图形验证码
  def getImageCodes(id: Integer) = Action{
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
      cache_client.set(code.toUpperCase, 60*60*3, code)
      Result(
        header = ResponseHeader(200, Map(CONTENT_TYPE -> "text/plain")),
        body = Enumerator(out.toByteArray)
      )
    }
    catch {
      case ex: IOException => Ok("error")

    }
  }

  //发送短信验证码
  def sendCode  = Action {implicit request =>
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
              Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.PHONE_NUM_EXTISTS)))
            case None =>
              val code = String.valueOf((100000 + Math.random * 900000).toInt)//生成code
              sms ! SMS(phoneNum,code,SMSType.comm)//actor发送短信
              cache_client.set(Codecs.md5(("reg"+phoneNum).getBytes()),180,code)//code存入缓存
              Logger.debug(s"注册手机:$phoneNum 发送验证码:$code")
              Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.SEND_SUCCESS)))
          }
        }
  }

  //跳转注册页面
  def toReg = Action { implicit request =>
    val url = request.getQueryString("url").getOrElse(Systemcontents.INDEX_PAGE)
    Ok(views.html.reg.render(String.valueOf(url)))
  }

  def regSubmit = Action { implicit request =>
    val data = regSubmitForm.bindFromRequest.get
    val code:String = data.code.trim
    val phoneNum:String = data.phoneNum.trim
    val cacheCode = cache_client.get(Codecs.md5(("reg"+phoneNum).getBytes()))
    if(StringUtils.isEmpty(code)||StringUtils.isEmpty(phoneNum)){
      Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.PHONE_CODE_ERROR)))
    }else if(cacheCode!=null&&cacheCode.equals(code)){
      val password = String.valueOf((100000 + Math.random * 900000).toInt)//生产密码
      User.insert(phoneNum,password,request.remoteAddress) match {
        case Some(id) =>
          sms ! SMS(phoneNum,password,SMSType.passwd)//actor发送短信
          Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.REG_SUCCESS)))
        case None =>
          Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.REG_FAILED)))
      }

    }else {
      Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.PHONE_CODE_ERROR)))
    }
  }



}
