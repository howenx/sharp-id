package controllers

import javax.inject.Inject

import models.User
import modules.OSSClientProvider
import play.api._
import play.api.mvc._

/**
 * Created by handy on 15/10/23.
 * Daumkakao china
 */

class Application @Inject() (oss_client : OSSClientProvider) extends Controller {

  def index = Action {
    val client = oss_client.get
    Logger.debug(client.toString)
    Logger.debug(User.find_by_id(1).toString)
    Ok(views.html.index("Your new application is ready."))
  }

}
