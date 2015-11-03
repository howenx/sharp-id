package actor

import javax.inject.{Inject}
import akka.actor.{Actor}
import models.{User}
import net.spy.memcached.MemcachedClient
import play.api.libs.Codecs
import play.api.libs.json.{JsNumber, JsString, Json}


case class UserLoginSetCache(user: User,ip:String,securityStr:String)

case class UserLoginWithoutCache(id:Long,ip:String)


/**
 * Created by tony on 15/10/28.
 * kakao china
 */
class UserLoginActor @Inject() (cache_client: MemcachedClient) extends Actor{

  override def receive  =  {
    case user:UserLoginSetCache =>
      val securityStr = Codecs.md5(user.securityStr.getBytes)
      User.login(user.user.id,user.ip)//更新最后登录时间和最后登录ip
      //用户信息放入到缓存中 存放一天
      cache_client.set(securityStr, 60 * 60 * 24, Json.stringify(Json.obj(
        "user_id"->JsNumber(user.user.id),//用户id
        "nickname"->JsString(user.user.nickname),//昵称
        "photo_url"->JsString(user.user.photo_url),//头像
        "gender"->JsString(user.user.gender)//性别
      )))

    case user:UserLoginWithoutCache=>
      User.login(user.id,user.ip)//更新最后登录时间和最后登录ip

  }
}

