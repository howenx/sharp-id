package controllers

import java.sql.Timestamp
import java.text.SimpleDateFormat
import javax.inject.{Named, Inject}
import actor._
import akka.actor.ActorRef
import models.User
import modules.OSSClientProvider
import net.spy.memcached.MemcachedClient
import org.apache.commons.lang3.StringUtils
import org.joda.time.DateTime
import play.api.{Configuration, Logger}
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.Codecs
import play.api.libs.json.{JsNumber, JsString, JsBoolean, Json}
import play.api.mvc._
import utils.SystemService


case class PhoneLoginForm (phoneNum:String,code:String)

case class NameLoginForm (name:String,passwd:String)

class LoginApplication @Inject() (oss_client : OSSClientProvider, cache_client: MemcachedClient, @Named("sms") sms:ActorRef,configuration: Configuration)  extends Controller{

  val sendCodeForm :Form[SendCodeForm] = Form(
    mapping(
      "phoneNum" -> text,
      "imageCode" -> text
    )(SendCodeForm.apply)(SendCodeForm.unapply)
  )

  val phoneLoginForm :Form[PhoneLoginForm] = Form(
    mapping(
      "phoneNum" -> text,
      "code" -> text
    )(PhoneLoginForm.apply)(PhoneLoginForm.unapply)
  )

  val nameLoginForm :Form[NameLoginForm] = Form(
    mapping(
      "name" -> text,
      "passwd" -> text
    )(NameLoginForm.apply)(NameLoginForm.unapply)
  )

  def toLogin = Action{req =>
    val url = req.getQueryString("url").getOrElse(Systemcontents.INDEX_PAGE)
    Ok(views.html.login.render(String.valueOf(url)))
  }

  //发送短信验证码
  def sendCode  = Action { implicit request =>
    val data = sendCodeForm.bindFromRequest.get
    val imageCode:String = data.imageCode.trim
    val phoneNum:String = data.phoneNum.trim

    if(StringUtils.isEmpty(imageCode) || cache_client.get(imageCode.toUpperCase)==null){//都用大写来比较验证码
      Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.IMAGE_CODE_ERROR)))
    }
    else if(!SystemService.checkPhoneNum(phoneNum)){//检查手机号码格式
      Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.PHONE_NUM_TYPE_ERROR)))
    }
    else{
      val code = String.valueOf((100000 + Math.random * 900000).toInt)//生成code
      sms ! SMS(phoneNum,code,SMSType.comm)//actor发送短信
      cache_client.set(Codecs.md5(("login"+phoneNum).getBytes()),180,code)//code存入缓存
      Logger.debug(s"登录手机:$phoneNum 发送验证码:$code")
      Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.SEND_SUCCESS)))
    }
  }

  def loginByTel = Action { implicit request =>
    val data = phoneLoginForm.bindFromRequest.get
    val code:String = data.code.trim
    val phoneNum:String = data.phoneNum.trim
    val cacheCode = cache_client.get(Codecs.md5(("login"+phoneNum).getBytes()))
    if(StringUtils.isEmpty(code)||StringUtils.isEmpty(phoneNum)){
      Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.PHONE_CODE_ERROR)))
    }
    else if(cacheCode!=null&&cacheCode.equals(code)){
      val timeNow = new SimpleDateFormat("YYYY-MM-dd hh:mm:ss").format(new Timestamp((new DateTime()).withMillisOfSecond(0).getMillis))
      val securityStr = phoneNum + timeNow + "webLogin"
      User.find_by_phone(phoneNum) match {
        //已经存在的用户自动登录
        case Some(userInfo) =>
          //actor异步 登录成功后跳转页面 导致cache里面没有存放用户信息 页面
          //userLogin!UserLoginSetCache(userInfo,request.remoteAddress,securityStr)
          setCache(userInfo,request.remoteAddress,securityStr)
          Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.LOGIN_SUCCESS))).
            withCookies(Cookie(Systemcontents.WEB_TOKEN, Codecs.md5(securityStr.getBytes()), Option(Int.MinValue),"/",Option(configuration.getString("domain").getOrElse(""))))

        //未注册的用户自动注册并登陆
        case None =>
          val passwd = String.valueOf((100000 + Math.random * 900000).toInt)//生产密码
          User.insert(phoneNum,passwd,request.remoteAddress) match{
            case Some(id)=>
              if(id>0){
                sms ! SMS(phoneNum,passwd,SMSType.passwd)//actor发送短信
                User.find_by_id(id:Long) match {
                  case Some(userInfo) =>
//                  userLogin!UserLoginSetCache(userInfo,request.remoteAddress,securityStr)
                    setCache(userInfo,request.remoteAddress,securityStr)
                    Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.LOGIN_SUCCESS))).
                      withCookies(Cookie(Systemcontents.WEB_TOKEN, Codecs.md5(securityStr.getBytes()), Option(Int.MinValue),"/",Option(configuration.getString("domain").getOrElse(""))))
                  case None =>
                    Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString("恭喜你你找到了一个NB的错误，手机号码已经注册成功了怎么还查询不到？!")))
                }
              }else{
                Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.REG_FAILED)))
              }
            case None =>
              Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.REG_FAILED)))
          }
      }
    }
    else {
      Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.PHONE_CODE_ERROR)))
    }
  }

  def loginByName =Action { implicit request =>
    val data = nameLoginForm.bindFromRequest.get
    val name: String = data.name.trim
    val passwd: String = data.passwd.trim
    if (StringUtils.isEmpty(name) || StringUtils.isEmpty(passwd)) {
      Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.USER_PASSWORD_ERROR)))
    }
    else {
      val timeNow = new SimpleDateFormat("YYYY-MM-dd hh:mm:ss").format(new Timestamp(new DateTime().withMillisOfSecond(0).getMillis))
      val securityStr = name + timeNow + "webLogin"
      val p = "^1[3-8][0-9]{9}".r
      val email = "\\b[a-zA-Z0-9.!#$%&\'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*\\b".r
      name match {
        case p() =>
          User.find_by_phone(name,passwd) match {
            case Some(userInfo) =>
//              userLogin ! UserLoginSetCache(userInfo, request.remoteAddress, securityStr)
              setCache(userInfo,request.remoteAddress,securityStr)
              Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.LOGIN_SUCCESS))).
                withCookies(Cookie(Systemcontents.WEB_TOKEN, Codecs.md5(securityStr.getBytes), Option(Int.MinValue), "/", Option(configuration.getString("domain").getOrElse(""))))
            case None =>
              Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.USER_PASSWORD_ERROR)))
          }
        case email() =>
          User.find_by_email(name,passwd)match{
            case Some(userInfo)=>
//              userLogin!UserLoginSetCache(userInfo,request.remoteAddress,securityStr)
              setCache(userInfo,request.remoteAddress,securityStr)
              Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.LOGIN_SUCCESS)))
            case None =>
              Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.USER_PASSWORD_ERROR)))
          }
        case _ =>
          User.find_by_nickname(name,passwd) match {
            case Some(userInfo) =>
//              userLogin ! UserLoginSetCache(userInfo, request.remoteAddress, securityStr)
              setCache(userInfo,request.remoteAddress,securityStr)
              Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.LOGIN_SUCCESS))).
                withCookies(Cookie(Systemcontents.WEB_TOKEN, Codecs.md5(securityStr.getBytes), Option(Int.MinValue), "/", Option(configuration.getString("domain").getOrElse(""))))
            case None =>
              Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(false), Systemcontents.RESULT_MESSAGE -> JsString(Systemcontents.USER_PASSWORD_ERROR)))
          }
      }
    }
  }

  def setCache (user:User,ip:String,securityStr:String)= {
    User.login(user.id,ip)//更新最后登录时间和最后登录ip
    //用户信息放入到缓存中 存放一天
    cache_client.set(Codecs.md5(securityStr.getBytes), 60 * 60 * 24, Json.stringify(Json.obj(
      "user_id"->JsNumber(user.id),//用户id
      "nickname"->JsString(user.nickname),//昵称
      "photo_url"->JsString(user.photo_url),//头像
      "gender"->JsString(user.gender)//性别
    )))
  }

}
