package controllers

/**
 * Created by handy on 15/10/20.
 * Daumkakao china
 */
import play.api._
import play.api.mvc._

object Application extends Controller {

  def index = Action { implicit request =>
    Ok(views.html.index("Your new application is ready."))
  }

  def login = Action { implicit request =>

    Ok("ok")
  }

}
