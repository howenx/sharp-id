package modules

import actor._
import com.google.inject.AbstractModule
import play.api.libs.concurrent.AkkaGuiceSupport


/**
 * Created by handy on 15/10/23.
 * kakao china
 */
class AkkaModule extends AbstractModule with AkkaGuiceSupport {
  override def configure() ={
    bindActor[SMSActor] ("sms")
    bindActor[OssActor] ("oss")
    bindActor[CouponsActor] ("coupons")
    bindActor[WechatUserInfoActor]("wechatUserInfoActor")
    bindActor[IdRunActor]("idRunActor")
  }
}
