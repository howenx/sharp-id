package models

import java.sql.Timestamp

import anorm.SqlParser._
import anorm._
import com.fasterxml.jackson.annotation.JsonIgnore
import org.joda.time.DateTime
import play.api.Play.current
import play.api.db.DB

/**
  * 用户优惠券
  * Created by howen on 15/12/9.
  */
case class CouponsVo(coupId: Option[String], @JsonIgnore userId: Option[Long],@JsonIgnore cateId: Option[Long],denomination: Option[BigDecimal],startAt: Option[Timestamp],endAt: Option[Timestamp],state: Option[String],@JsonIgnore orderId: Option[Long],@JsonIgnore useAt: Option[Timestamp], limitQuota: Option[BigDecimal], couponsCount:Option[Int])


object CouponsModel {

  val couponsMapping = {
    get[Option[String]]("coup_id") ~
      get[Option[Long]]("user_id") ~
      get[Option[Long]]("cate_id") ~
      get[Option[BigDecimal]]("denomination") ~
      get[Option[DateTime]]("start_at") ~
      get[Option[DateTime]]("end_at") ~
      get[Option[String]]("state") ~
      get[Option[Long]]("order_id") ~
      get[Option[DateTime]]("use_at")~
      get[Option[BigDecimal]] ("limit_quota")~
      get[Option[Int]] ("coupons_count") map {
      case coup_id ~ user_id ~ cate_id ~ denomination ~ start_at ~ end_at ~ state ~ order_id ~ use_at~ limit_quota~coupons_count=>
        CouponsVo(coup_id, user_id, cate_id, if(denomination.isDefined)Some(BigDecimal.apply(denomination.get.toBigInt())) else None, Some(new Timestamp(start_at.get.getMillis)), Some(new Timestamp(end_at.get.getMillis)), state, order_id, if (use_at.isDefined) Some(new Timestamp(use_at.get.getMillis)) else None,if (limit_quota.isDefined)Some(BigDecimal.apply(limit_quota.get.toBigInt())) else None,coupons_count)
    }
  }

  def updateCoupons(couponsVo: CouponsVo): Int = {
    val params: Seq[NamedParameter] = Seq("userId" -> couponsVo.userId.get, "state" -> couponsVo.state.get)
    DB.withConnection("shopping") { implicit conn =>
      SQL("""update sp_coupons set state={state} where user_id={userId} and state='N' and end_at < CURRENT_TIMESTAMP(0)""").on(params: _*).executeUpdate()
    }
  }

  def searchAllCoupons(couponsVo: CouponsVo): List[CouponsVo] = {
    val params: Seq[NamedParameter] = Seq("userId" -> couponsVo.userId.get, "state" -> couponsVo.state.get)
    DB.withConnection("shopping") { implicit conn =>
      SQL( """select count(*) OVER(order by cate_id) AS coupons_count, coup_id ,user_id,cate_id,denomination,start_at,end_at,state,order_id,use_at,limit_quota from sp_coupons WHERE user_id = {userId} and state={state} and start_at <= CURRENT_TIMESTAMP(0) and end_at >= CURRENT_TIMESTAMP(0)""").on(params: _*).as(couponsMapping.*)
    }
  }
}
