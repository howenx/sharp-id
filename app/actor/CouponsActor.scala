package actor

import javax.inject.Inject

import akka.actor.Actor
import models.{Coupons, CouponsVo}
import play.api.Configuration

/**
  * 优惠券Actor
  * Created by howen on 15/12/9.
  */
class CouponsActor @Inject() (configuration: Configuration) extends Actor{

  override def receive = {
    case coupons:CouponsVo =>
      Coupons.updateCoupons(coupons)
  }
}
