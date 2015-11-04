package controllers

import javax.inject.{Singleton, Named, Inject}

import akka.actor.ActorRef
import modules.OSSClientProvider
import net.spy.memcached.MemcachedClient
import play.api._
import play.api.libs.json.{JsBoolean, Json}
import play.api.mvc._


/**
 * Created by handy on 15/10/23.
 * Daumkakao china
 */
@Singleton
class Application @Inject() (oss_client : OSSClientProvider, cache_client: MemcachedClient, @Named("sms") sms:ActorRef,configuration: Configuration) extends Controller {

  def index = Action {
    Ok(views.html.login.render(Systemcontents.INDEX_PAGE))
  }

  def getUserName = Action {implicit request=>
    try{
      val cookie = request.cookies(Systemcontents.WEB_TOKEN)
      cookie match {
        case null =>
          Redirect("/toLogin")
        case _ =>
          val cacheInfo = cache_client.get(cookie.value.toString)
          cacheInfo match {
            case null =>
              Redirect("/toLogin")
            case _ =>
              Ok(Json.obj(Systemcontents.RESULT_BOOLEAN -> JsBoolean(true), Systemcontents.RESULT_MESSAGE -> play.libs.Json.parse(String.valueOf(cacheInfo)).get("nickname").textValue()))
          }
      }
    }catch {
      case ex:RuntimeException =>
        Redirect("/toLogin")
    }
  }

  def redirect = Action {req =>
    val url = req.getQueryString("url").getOrElse(Systemcontents.INDEX_PAGE)
    Logger.info(s"跳转页面 $url ")
    Redirect(url)
  }

  def loginOut = Action {implicit request=>
    try{
      val cookie = request.cookies(Systemcontents.WEB_TOKEN)
      cookie match {
        case null =>
          Redirect("/index")
        case _ =>
          val cacheInfo = cache_client.get(cookie.value.toString)
          cacheInfo match {
            case null =>
              Redirect("/index")
            case _ =>
              cache_client.delete(cookie.value.toString)
              Redirect("/index").discardingCookies(DiscardingCookie(Systemcontents.WEB_TOKEN,"/",Option(configuration.getString("domain").getOrElse(""))))
          }
      }
    }catch {
      case ex:RuntimeException =>
        Redirect("/index")
    }
  }

}
