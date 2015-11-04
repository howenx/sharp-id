package controllers

import javax.inject.{Named, Inject}
import actor.OSS
import akka.actor.ActorRef
import models.User
import modules.OSSClientProvider
import net.spy.memcached.MemcachedClient
import org.apache.commons.lang3.StringUtils
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import play.api.{Logger, Configuration}
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc.{Action, Controller}
import utils.SystemService


/**
 * Created by china_005 on 15/11/2.
 */

case class UserChangeForm (id:Long,nickname:String,gender:String,email:String,birthday:String)

class UserChangeApplication @Inject() (oss_client : OSSClientProvider, cache_client: MemcachedClient, @Named("oss") oss:ActorRef,configuration: Configuration) extends  Controller{

  val userChangeForm :Form[UserChangeForm] = Form(
    mapping(
      "id"->longNumber,
      "nickname" -> text,
      "gender" -> text,
      "email" ->text,
      "birthday" ->text
    )(UserChangeForm.apply)(UserChangeForm.unapply)
  )


  def toUserChange(msg:String) = Action{ implicit request=>
    try{
      val cookie = request.cookies(Systemcontents.WEB_TOKEN)
      cookie match {
        case null =>
          Redirect("/toLogin?url=toUserChange/change")
        case _ =>
          val cacheInfo = cache_client.get(cookie.value.toString)
          cacheInfo match {
            case null =>
              Redirect("/toLogin?url=toUserChange/change")
            case _ =>
              User.find_by_id_more(play.libs.Json.parse(String.valueOf(cacheInfo)).get("user_id").intValue()) match{
                case Some(user) =>
                  if(msg.equals("change")){
                    Ok(views.html.change.render(user,null))
                  } else{
                    Ok(views.html.change.render(user,Systemcontents.getInstance().getErrorMsg(msg)))
                  }
                case None =>
                  Redirect("/toLogin?url=toUserChange/change")
              }
          }
      }
    }catch {
      case ex:RuntimeException =>
        Redirect("/toLogin?url=toUserChange/change")
    }
  }

  def changeFormSubmit = Action(parse.multipartFormData){ implicit request=>
    val data = userChangeForm.bindFromRequest().get
    val email = data.email.trim
    val id = data.id
    val nickname = data.nickname.trim
    val gender = data.gender.trim
    val birthday = data.birthday.trim
    var active = "N"
    if(!SystemService.checkEmail(email)){
      Redirect("/toUserChange/1000")
    }else if (!SystemService.checkBirthday(birthday)){
      Redirect("/toUserChange/1001")
    }else if(nickname.replaceAll("[^\\x00-\\xff]", "**").length()>20){
      Redirect("/toUserChange/1003")
    }else if(!gender.equals("M")&& !gender.equals("F")){
      Redirect("/toUserChange/1004")
    }else {
      try{
        val cookie = request.cookies(Systemcontents.WEB_TOKEN)
        cookie match {
          case null =>
            Redirect("/toLogin?url=toUserChange")
          case _ =>
            val cacheInfo = cache_client.get(cookie.value.toString)
            if(cacheInfo==null || id!=(play.libs.Json.parse(String.valueOf(cacheInfo))).get("user_id").intValue())
              Redirect("/toUserChange/1005")
        }
      }catch {
        case ex:RuntimeException =>
          Redirect("/toLogin?url=toUserChange")
      }
      User.find_by_id_more(id) match {
        case Some(user) =>
          var status = ""
          if(StringUtils.isEmpty(status)&&email.equals(user.email.trim)){
            active = user.active
          }else{
            active = "N"
            User.find_by_email(email) match {
              case Some(user) =>
                status = "1006"
              case None =>
            }
          }
          if(StringUtils.isEmpty(status) && !nickname.equals(user.nickname.trim)){
            User.find_by_nickname(nickname) match {
              case Some(user) =>
                status = "1007"
              case None =>
            }
          }
          if(!StringUtils.isEmpty(status)){
            Redirect(s"/toUserChange/$status")
          }
          else{
            val photoUrl = request.body.file("inputfile").map { picture =>
              var key = "style" + "/" + DateTimeFormat.forPattern("yyyy-MM-dd").print(new DateTime) + "/" + System.currentTimeMillis + picture.filename.replaceFirst("^[^.]*", "")
              oss!OSS(picture.ref,key)
              Logger.debug("/"+key)
              "/"+key
            }.getOrElse {""}
            if(User.change(id,email,active,nickname,birthday,gender,photoUrl)>0){
              Redirect("/toUserDetail")
            }else{
              Redirect("/toUserChange/1008")
            }
          }
        case None =>
          Redirect("/toUserChange/1009")
      }

    }

  }




}
