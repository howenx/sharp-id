package models

import anorm.SqlParser._
import anorm.{SQL, ~, _}
import com.fasterxml.jackson.databind.JsonNode
import play.api.Play.current
import play.api.db.DB
import play.libs.Json

/**
  * 用户详细信息,以及用户收货地址增删改查
  * Created by howen on 15/11/30.
  */
case class Address(addId: Option[Long], tel: Option[String], name: Option[String], deliveryCity: Option[String], deliveryDetail: Option[String], userId: Option[Long], orDefault: Option[Int], idCardNum: Option[String], orDestroy: Option[Boolean],cityCode:Option[String])

case class UserDetail(userId: Option[Long], nickname: Option[String], phoneNum: Option[String], birthday: Option[String], activeYn: Option[String], realYN: Option[String], gender: Option[String], photoUrl: Option[String], status: Option[String])

object UserInfoModel {

  val addressMapping = {
    get[Long]("add_id") ~
      get[String]("tel") ~
      get[String]("name") ~
      get[String]("delivery_city") ~
      get[String]("delivery_detail") ~
      get[Long]("user_id") ~
      get[Int]("or_default") ~
      get[String]("id_card_num")~
      get[String]("city_code")map {
      case add_id ~ tel ~ name ~ delivery_city ~ delivery_detail ~ user_id ~ or_default ~ id_card_num ~ city_code => Address(Some(add_id), Some(tel), Some(name), Some(delivery_city), Some(delivery_detail), Some(user_id), Some(or_default), Some(id_card_num), Some(false),Some(city_code))
    }
  }

  val userInfoDetailMapping = {
    get[Long]("user_id") ~
      get[String]("nickname") ~
      get[String]("gender") ~
      get[String]("photo_url") ~
      get[String]("birthday") ~
      get[String]("phone_num") ~
      get[String]("active_YN") ~
      get[String]("real_YN") ~
      get[String]("status") map {
      case user_id ~ nickname ~ gender ~ photo_url ~ birthday ~ phone_num ~ active_YN ~ real_YN ~ status => UserDetail(Some(user_id), Some(nickname), Some(phone_num), Some(birthday), Some(active_YN), Some(real_YN), Some(gender), Some(photo_url), Some(status))
    }
  }

  def changeRealName(id: Long, cardNum: String, cardImg: JsonNode, realName: String, realYn: String): Int = {
    DB.withConnection() { implicit conn =>
      SQL( """ update "ID" set real_name={realName},card_num={cardNum},card_img={cardImg}::jsonb,"real_YN"={realYn} where user_id={user_id} """).on("realName" -> realName, "cardNum" -> cardNum, "cardImg" -> Json.stringify(cardImg), "realYn" -> realYn, "user_id" -> id).executeUpdate()
    }
  }

  def allAddress(address: Address): List[Address] = {
    var setString: String = "1 = 1"
    var params: collection.mutable.Seq[NamedParameter] = collection.mutable.Seq()
    if (address.orDefault.isDefined) {
      setString += """and or_default = {orDefault}"""
      params = params :+ NamedParameter("orDefault", if(address.orDefault.get==0)false else true)
    }
    if (address.userId.isDefined) {
      setString += """and user_id = {userId}"""
      params = params :+ NamedParameter("userId", address.userId.get)
    }
    DB.withConnection() { implicit conn =>
      SQL( """select i.add_id,i.tel,i."name",(i.delivery_city->>'province') ||(' ')|| (i.delivery_city->>'city')||(' ')||(i.delivery_city->>'area') as delivery_city,COALESCE((i.delivery_city->>'city_code'),'none') as city_code,i.delivery_detail,i.user_id,i.or_default::integer,i.id_card_num from id_address i WHERE """+ setString+ """ and i.or_destroy=false """).on(params: _*).as(addressMapping.*)
    }
  }

  def updateAddress(address: Address): Int = {

    var setString: String = "update_at = CURRENT_TIMESTAMP(0)"
    var params: collection.mutable.Seq[NamedParameter] = collection.mutable.Seq()

    if (address.tel.isDefined) {
      setString += """, tel = {tel}"""
      params = params :+ NamedParameter("tel", address.tel.get)
    }
    if (address.name.isDefined) {
      setString += ""","name" = {name}"""
      params = params :+ NamedParameter("name", address.name.get)
    }
    if (address.deliveryCity.isDefined) {
      setString += """,delivery_city = {deliveryCity}::jsonb"""
      params = params :+ NamedParameter("deliveryCity", address.deliveryCity.get)
    }
    if (address.deliveryDetail.isDefined) {
      setString += """,delivery_detail = {deliveryDetail}"""
      params = params :+ NamedParameter("deliveryDetail", address.deliveryDetail.get)
    }
    if (address.userId.isDefined) {
      setString += """,user_id = {userId}"""
      params = params :+ NamedParameter("userId", address.userId.get)
    }
    if (address.orDefault.isDefined) {
      setString += """,or_default = {orDefault}"""
      params = params :+ NamedParameter("orDefault", if(address.orDefault.get==0)false else true)
    }
    if (address.idCardNum.isDefined) {
      setString += """,id_card_num = {idCardNum}"""
      params = params :+ NamedParameter("idCardNum", address.idCardNum.get)
    }
    if (address.orDestroy.isDefined) {
      setString += """,or_destroy = {orDestroy}"""
      setString += """, destroy_at= CURRENT_TIMESTAMP(0)"""
      params = params :+ NamedParameter("orDestroy", address.orDestroy.get)
    }
    var whereString: String = ""
    if(address.userId.isDefined){
      whereString +=""" and user_id={userId} """
      whereString +=""" and or_destroy=false """
      params = params :+ NamedParameter("userId", address.userId.get)
    }
    if(address.addId.isDefined){
      whereString +=""" and add_id={addId} """
      params = params :+ NamedParameter("addId", address.addId.get)
    }

    DB.withConnection { implicit c =>
      SQL( """UPDATE id_address SET """ + setString+""" where 1=1 """+whereString).on(params: _*).executeUpdate()
    }
  }

  def insertAddress(address: Address): Option[Long] = {
    DB.withConnection() { implicit conn =>
      var params: Seq[NamedParameter] = Seq("tel" -> address.tel.get, "name" -> address.name.get
        , "deliveryCity" -> address.deliveryCity.get, "deliveryDetail" -> address.deliveryDetail.get, "userId" -> address.userId.get
        , "idCardNum" -> address.idCardNum.get
      )
      if (address.orDefault.isDefined) {
        if (address.orDefault.get==0){
          params = params :+ NamedParameter("orDefault", false)
        }else params = params :+ NamedParameter("orDefault", true)
      }

      SQL(
        """ insert into  id_address(user_id,tel, "name",delivery_city,delivery_detail,id_card_num,or_default,create_at)values({userId},{tel},{name},{deliveryCity}::jsonb,{deliveryDetail},{idCardNum},{orDefault},CURRENT_TIMESTAMP(0) )""").on(params: _*).executeInsert()
    }
  }

  def findByUserId(id: Long): Option[UserDetail] = {
    DB.withConnection() { implicit conn =>
      SQL( """select user_id, nickname, gender, photo_url,email,to_char(birthday,'YYYY-MM-DD') birthday,phone_num,"active_YN","real_YN",status from "ID" where user_id = {user_id}""").on("user_id" -> id).as(userInfoDetailMapping.*).headOption
    }
  }

  def updateUserDetail(userDetail: UserDetail): Int = {
    var setString: String = "alter_dt = CURRENT_TIMESTAMP(0)"
    var params: collection.mutable.Seq[NamedParameter] = collection.mutable.Seq(
      NamedParameter("userId", userDetail.userId.get)
    )
    if (userDetail.activeYn.isDefined) {
      setString += """, "active_YN" = {activeYn}"""
      params = params :+ NamedParameter("activeYn", userDetail.activeYn.get)
    }
    if (userDetail.birthday.isDefined) {
      setString += """,birthday = to_timestamp({birthday},'YYYY-MM-DD')"""
      params = params :+ NamedParameter("birthday", userDetail.birthday.get)
    }
    if (userDetail.gender.isDefined) {
      setString += """,gender = {gender}"""
      params = params :+ NamedParameter("gender", userDetail.gender.get)
    }
    if (userDetail.nickname.isDefined) {
      setString += """,nickname = {nickname}"""
      params = params :+ NamedParameter("nickname", userDetail.nickname.get)
    }
    if (userDetail.phoneNum.isDefined) {
      setString += """,phone_num = {phoneNum}"""
      params = params :+ NamedParameter("phoneNum", userDetail.phoneNum.get)
    }
    if (userDetail.photoUrl.isDefined) {
      setString += """,photo_url = {photoUrl}"""
      params = params :+ NamedParameter("photoUrl", userDetail.photoUrl.get)
    }
    if (userDetail.realYN.isDefined) {
      setString += ""","real_YN" = {realYN}"""
      params = params :+ NamedParameter("realYN", userDetail.realYN.get)
    }
    if (userDetail.status.isDefined) {
      setString += """,status = {status}"""
      params = params :+ NamedParameter("status", userDetail.status.get)
    }

    DB.withConnection { implicit c =>
      SQL( """UPDATE "ID" SET """ + setString + """ WHERE user_id = {userId}""").on(params: _*).executeUpdate()
    }
  }
}
