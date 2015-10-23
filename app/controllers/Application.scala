package controllers

import javax.inject.{Singleton, Named, Inject}

import actor.{SMS, SMSType}
import akka.actor.ActorRef
import models.User
import modules.OSSClientProvider
import net.spy.memcached.MemcachedClient
import play.api._
import play.api.mvc._


/**
 * Created by handy on 15/10/23.
 * Daumkakao china
 */
@Singleton
class Application @Inject() (oss_client : OSSClientProvider, cache_client: MemcachedClient, @Named("sms") sms:ActorRef) extends Controller {

  def index = Action {
    val client = oss_client.get
    Logger.debug(client.toString)
    Logger.debug(User.find_by_id(1).toString)
    Logger.debug(sms.toString())
    Logger.debug(cache_client.toString)
    val s = new SMS("test","test",SMSType.verify)
    sms ! s
    Ok(views.html.index("Your new application is ready."))
  }

}
