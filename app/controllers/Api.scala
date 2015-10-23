package controllers

import javax.inject._
import actor.{SMSType, SMS}
import akka.actor.ActorRef
import play.api.data._
import play.api.data.Forms._
import play.api.mvc.{Action, Controller}
import play.api.Logger
import models.User

import scala.concurrent.ExecutionContext


case class UserNameLoginInfo(name: String, passwd: String)

/**
 * Created by handy on 15/10/23.
 * Daumkakao china
 */
@Singleton
class Api @Inject() (@Named("sms") sms:ActorRef) extends Controller {

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
    "name" ->nonEmptyText,
    "passwd" -> nonEmptyText
  )(UserNameLoginInfo.apply)(UserNameLoginInfo.unapply))



  def login_user_name = Action { implicit request =>
    val user_data = user_name_login_form.bindFromRequest.get
    Logger.debug(sms.toString())
    val s = new SMS("test","test",SMSType.verify)
    sms ! s
    User.find_by_nickanme(user_data.name, user_data.passwd) match {
      case None =>
        Ok("error")
      case Some(user) =>
        Logger.debug(user.toString)
        Ok("find")
    }

  }

}
