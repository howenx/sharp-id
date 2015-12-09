package models

import java.sql.Timestamp

import anorm.SqlParser._
import anorm._
import com.fasterxml.jackson.annotation.JsonIgnore
import org.joda.time.DateTime
import play.api.db.DB
import play.api.Play.current

/**
  * 用户优惠券
  * Created by howen on 15/12/9.
  */
case class CouponsVo(coupId: String, @JsonIgnore userId: Long,@JsonIgnore cateId: Long,denomination: BigDecimal,startAt: Timestamp,endAt: Timestamp,state: String,@JsonIgnore orderId: Long,@JsonIgnore useAt: Timestamp, limitQuota: BigDecimal, couponsCount:Int)


object Coupons {

  val couponsMapping = {
    get[String]("coup_id") ~
      get[Long]("user_id") ~
      get[Long]("cate_id") ~
      get[BigDecimal]("denomination") ~
      get[DateTime]("start_at") ~
      get[DateTime]("end_at") ~
      get[String]("state") ~
      get[Long]("order_id") ~
      get[DateTime]("use_at")~
      get[BigDecimal] ("limit_quota")~
      get[Int] ("coupons_count") map {
      case coup_id ~ user_id ~ cate_id ~ denomination ~ start_at ~ end_at ~ state ~ order_id ~ use_at~ limit_quota~coupons_count=>
        CouponsVo(coup_id.toString, user_id.toLong, cate_id.toLong, BigDecimal.apply(denomination.toBigInt()), Timestamp.valueOf(start_at.toString), Timestamp.valueOf(end_at.toString), state, order_id.toLong, Timestamp.valueOf(use_at.toString),BigDecimal.apply(limit_quota.toBigInt()),coupons_count.toInt)
    }
  }

  def updateCoupons(couponsVo: CouponsVo): Int = {
    val params: Seq[NamedParameter] = Seq("userId" -> couponsVo.userId, "state" -> couponsVo.state)
    DB.withConnection("shopping") { implicit conn =>
      SQL("""update sp_coupons set state={state} where user_id={userId} and state='N' and end_at < CURRENT_TIMESTAMP(0)""").on(params: _*).executeUpdate()
    }
  }

  def searchAllCoupons(couponsVo: CouponsVo): List[CouponsVo] = {
    val params: Seq[NamedParameter] = Seq("userId" -> couponsVo.userId, "state" -> couponsVo.state)
    DB.withConnection("shopping") { implicit conn =>
      SQL( """select count(*) OVER(order by cate_id) AS coupons_count, coup_id ,user_id,cate_id,denomination,start_at,end_at,state,order_id,use_at,limit_quota from sp_coupons WHERE user_id = {user_id} and state={state} and start_at <= CURRENT_TIMESTAMP(0) and end_at >= CURRENT_TIMESTAMP(0)""").on(params: _*).as(couponsMapping.*)
    }
  }
}
