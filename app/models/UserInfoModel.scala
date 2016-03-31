package models

import anorm.SqlParser._
import anorm.{SQL, ~, _}
import com.fasterxml.jackson.databind.JsonNode
import play.Logger
import play.api.Play.current
import play.api.db.DB
import play.libs.Json

/**
  * 用户详细信息,以及用户收货地址增删改查
  * Created by howen on 15/11/30.
  */


object UserInfoModel {

  val addressMapping = {
    get[Long]("add_id") ~
      get[String]("tel") ~
      get[String]("name") ~
      get[String]("delivery_city") ~
      get[String]("delivery_detail") ~
      get[Long]("user_id") ~
      get[Int]("or_default") ~
      get[String]("id_card_num") ~
      get[String]("city_code") map {
      case add_id ~ tel ~ name ~ delivery_city ~ delivery_detail ~ user_id ~ or_default ~ id_card_num ~ city_code => Address(Some(add_id), Some(tel), Some(name), Some(delivery_city), Some(delivery_detail), Some(user_id), Some(or_default), Some(id_card_num), Some(false), Some(city_code))
    }
  }



  val userOpenMapping = {
    get[Option[Long]]("user_id") ~
      get[Option[String]]("nickname") ~
      get[Option[String]]("passwd") ~
      get[Option[String]]("phone_num") ~
      get[Option[String]]("gender") ~
      get[Option[String]]("birthday") ~
      get[Option[String]]("photo_url") ~
      get[Option[String]]("real_name") ~
      get[Option[String]]("card_type") ~
      get[Option[String]]("card_num") ~
      get[Option[String]]("card_img") ~
      get[Option[String]]("reg_ip") ~
      get[Option[String]]("reg_dt") ~
      get[Option[String]]("or_real") ~
      get[Option[String]]("lastlogin_dt") ~
      get[Option[String]]("lastlogin_ip") ~
      get[Option[String]]("status") ~
      get[Option[String]]("id_type") ~
      get[Option[String]]("open_id") ~
      get[Option[String]]("id_area") ~
      get[Option[Long]]("login_times") ~
      get[Option[String]]("email") ~
      get[Option[String]]("alter_dt") ~
      get[Option[String]]("or_active") map {
      case user_id ~
        nickname ~
        passwd ~
        phone_num ~
        gender ~
        birthday ~
        photo_url ~
        real_name ~
        card_type ~
        card_num ~
        card_img ~
        reg_ip ~
        reg_dt ~
        or_real ~
        lastlogin_dt ~
        lastlogin_ip ~
        status ~
        id_type ~
        open_id ~
        id_area ~
        login_times ~
        email ~
        alter_dt ~
        or_active =>
        UserOpen(
          user_id,
          nickname,
          passwd,
          phone_num,
          gender,
          birthday,
          photo_url,
          real_name,
          card_type,
          card_num,
          card_img,
          reg_ip,
          reg_dt,
          or_real,
          lastlogin_dt,
          lastlogin_ip,
          status,
          id_type,
          open_id,
          id_area,
          login_times,
          email,
          alter_dt,
          or_active
        )
    }
  }

  def changeRealName(id: Long, cardNum: String, cardImg: JsonNode, realName: String, orReal: String): Int = {
    DB.withConnection() { implicit conn =>
      SQL( """ update "ID" set real_name={realName},card_num={cardNum},card_img={cardImg}::jsonb,or_real={orReal} where user_id={user_id} """).on("realName" -> realName, "cardNum" -> cardNum, "cardImg" -> Json.stringify(cardImg), "or_real" -> orReal, "user_id" -> id).executeUpdate()
    }
  }

  def allAddress(address: Address): List[Address] = {
    var setString: String = "1 = 1"
    var params: collection.mutable.Seq[NamedParameter] = collection.mutable.Seq()
    if (address.orDefault.isDefined) {
      setString += """and or_default = {orDefault}"""
      params = params :+ NamedParameter("orDefault", if (address.orDefault.get == 0) false else true)
    }
    if (address.userId.isDefined) {
      setString += """and user_id = {userId}"""
      params = params :+ NamedParameter("userId", address.userId.get)
    }
    DB.withConnection() { implicit conn =>
      SQL( """select i.add_id,i.tel,i."name",(i.delivery_city->>'province') ||(' ')|| (i.delivery_city->>'city')||(' ')||(i.delivery_city->>'area') as delivery_city,COALESCE((i.delivery_city->>'city_code'),'none') as city_code,i.delivery_detail,i.user_id,i.or_default::integer,i.id_card_num from id_address i WHERE """ + setString + """ and i.or_destroy=false """).on(params: _*).as(addressMapping.*)
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
      params = params :+ NamedParameter("orDefault", if (address.orDefault.get == 0) false else true)
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
    if (address.userId.isDefined) {
      whereString +=""" and user_id={userId} """
      whereString +=""" and or_destroy=false """
      params = params :+ NamedParameter("userId", address.userId.get)
    }
    if (address.addId.isDefined) {
      whereString +=""" and add_id={addId} """
      params = params :+ NamedParameter("addId", address.addId.get)
    }

    DB.withConnection { implicit c =>
      SQL( """UPDATE id_address SET """ + setString +""" where 1=1 """ + whereString).on(params: _*).executeUpdate()
    }
  }

  def insertAddress(address: Address): Option[Long] = {
    DB.withConnection() { implicit conn =>
      var params: Seq[NamedParameter] = Seq("tel" -> address.tel.get, "name" -> address.name.get
        , "deliveryCity" -> address.deliveryCity.get, "deliveryDetail" -> address.deliveryDetail.get, "userId" -> address.userId.get
        , "idCardNum" -> address.idCardNum.get
      )
      if (address.orDefault.isDefined) {
        if (address.orDefault.get == 0) {
          params = params :+ NamedParameter("orDefault", false)
        } else params = params :+ NamedParameter("orDefault", true)
      }

      SQL(
        """ insert into  id_address(user_id,tel, "name",delivery_city,delivery_detail,id_card_num,or_default,create_at)values({userId},{tel},{name},{deliveryCity}::jsonb,{deliveryDetail},{idCardNum},{orDefault},CURRENT_TIMESTAMP(0) )""").on(params: _*).executeInsert()
    }
  }

  val sqlColumn: String = "" +
    "user_id," +
    "nickname," +
    "passwd," +
    "phone_num," +
    "gender," +
    "to_char(birthday,'YYYY-MM-DD') birthday," +
    "photo_url," +
    "real_name," +
    "card_type," +
    "card_num," +
    "card_img," +
    "reg_ip::text," +
    "to_char(reg_dt,'YYYY-MM-DD HH24:MI:SS') reg_dt," +
    "or_real," +
    "to_char(lastlogin_dt,'YYYY-MM-DD HH24:MI:SS') lastlogin_dt," +
    "lastlogin_ip::text," +
    "status," +
    "id_type," +
    "open_id," +
    "id_area," +
    "login_times," +
    "email," +
    "to_char(alter_dt,'YYYY-MM-DD HH24:MI:SS') alter_dt," +
    "or_active"

  def findByUserId(id: Long): Option[UserOpen] = {
    DB.withConnection() { implicit conn =>
      SQL( """select """ + sqlColumn +""" from "ID" where user_id = {user_id}""").on("user_id" -> id).as(userOpenMapping.*).headOption
    }
  }


  def queryUser(user: UserOpen): Option[UserOpen] = {
    var setString: String = "1 = 1"
    var params: collection.mutable.Seq[NamedParameter] = collection.mutable.Seq()
    if (user.userId.isDefined) {
      setString += """and user_id = {userId}"""
      params = params :+ NamedParameter("userId", user.userId.get)
    }
    if (user.phoneNum.isDefined) {
      setString += """and phone_num = {phoneNum}"""
      params = params :+ NamedParameter("phoneNum", user.phoneNum.get)
    }
    if (user.openId.isDefined) {
      setString += """and open_id = {openId}"""
      params = params :+ NamedParameter("openId", user.openId.get)
    }
    if (user.idType.isDefined) {
      setString += """and id_type = {idType}"""
      params = params :+ NamedParameter("idType", user.idType.get)
    }
    if (user.passwd.isDefined) {
      setString +="""and passwd = user_passwd(user_id,{passwd})"""
      params = params :+ NamedParameter("passwd", user.passwd.get)
    }
    DB.withConnection() { implicit conn =>
      SQL( """select """ + sqlColumn +""" from "ID" where """ + setString).on(params: _*).as(userOpenMapping.*).headOption
    }
  }

  def addUser(au: UserOpen): Option[Long] = {
    var columnString: String = "nickname"
    var valueString: String = "{nickname}"
    var params: collection.mutable.Seq[NamedParameter] = collection.mutable.Seq()
    if (au.nickname.isDefined) {
      params = params :+ NamedParameter("nickname", au.nickname.get)
    } else {
      val phone: String = if (au.phoneNum.isDefined) au.phoneNum.get else scala.util.Random.nextString(11)
      params = params :+ NamedParameter("nickname", "HMM " + phone.substring(0, 3) + "****" + phone.substring(7, 11))
    }
    if (au.gender.isDefined) {
      columnString += ",gender"
      valueString += ",{gender}"
      params = params :+ NamedParameter("gender", au.gender.get)
    }

    if (au.passwd.isDefined) {
      columnString += ",passwd"
      valueString +=""",user_passwd(currval('"ID_user_id_seq"'::regclass),{passwd})"""
      params = params :+ NamedParameter("passwd", au.passwd.get)
    }

    if (au.phoneNum.isDefined) {
      columnString += ",phone_num"
      valueString += ",{phoneNum}"
      params = params :+ NamedParameter("phoneNum", au.phoneNum.get)
    }

    if (au.birthday.isDefined) {
      columnString += ",birthday"
      valueString += ",to_timestamp({birthday},'YYYY-MM-DD')"
      params = params :+ NamedParameter("birthday", au.birthday.get)
    }

    if (au.photoUrl.isDefined) {
      columnString += ",photo_url"
      valueString += ",{photoUrl}"
      params = params :+ NamedParameter("photoUrl", au.photoUrl.get)
    }

    if (au.regIp.isDefined) {
      columnString += ",reg_ip"
      valueString += ",cidr({regIp})"
      params = params :+ NamedParameter("regIp", au.regIp.get)
    }

    columnString += ",reg_dt"
    valueString += ",CURRENT_TIMESTAMP(0)"

    if (au.idType.isDefined) {
      columnString += ",id_type"
      valueString += ",{idType}"
      params = params :+ NamedParameter("idType", au.idType.get)
    }
    if (au.openId.isDefined) {
      columnString += ",open_id"
      valueString += ",{openId}"
      params = params :+ NamedParameter("openId", au.openId.get)
    }
    if (au.idArea.isDefined) {
      columnString += ",id_area"
      valueString += ",{idArea}"
      params = params :+ NamedParameter("idArea", au.idArea.get)
    }
    if (au.email.isDefined) {
      columnString += ",email"
      valueString += ",{email}"
      params = params :+ NamedParameter("email", au.email.get)
    }

    DB.withConnection() { implicit conn =>
      SQL( """insert into "ID" (""" + columnString +""") values (""" + valueString +""") """).on(params: _*).executeInsert()
    }
  }

  def updateUser(user: UserOpen): Int = {
    var setString: String = "alter_dt = CURRENT_TIMESTAMP(0)"
    var whereString: String = "1=1"
    var params: collection.mutable.Seq[NamedParameter] = collection.mutable.Seq(
      NamedParameter("userId", user.userId.get)
    )
    if (user.orActive.isDefined) {
      setString += """, "or_active" = {orActive}"""
      params = params :+ NamedParameter("orActive", user.orActive.get)
    }
    if (user.birthday.isDefined) {
      setString += """,birthday = to_timestamp({birthday},'YYYY-MM-DD')"""
      params = params :+ NamedParameter("birthday", user.birthday.get)
    }
    if (user.gender.isDefined) {
      setString += """,gender = {gender}"""
      params = params :+ NamedParameter("gender", user.gender.get)
    }
    if (user.nickname.isDefined) {
      setString += """,nickname = {nickname}"""
      params = params :+ NamedParameter("nickname", user.nickname.get)
    }
    if (user.phoneNum.isDefined) {
      setString += """,phone_num = {phoneNum}"""
      params = params :+ NamedParameter("phoneNum", user.phoneNum.get)
    }
    if (user.photoUrl.isDefined) {
      setString += """,photo_url = {photoUrl}"""
      params = params :+ NamedParameter("photoUrl", user.photoUrl.get)
    }
    if (user.orReal.isDefined) {
      setString += ""","or_real" = {orReal}"""
      params = params :+ NamedParameter("orReal", user.orReal.get)
    }
    if (user.status.isDefined) {
      setString += """,status = {status}"""
      params = params :+ NamedParameter("status", user.status.get)
    }
    if (user.openId.isDefined) {
      whereString += "and open_id = {openId}"
      setString += """,open_id = {openId}"""
      params = params :+ NamedParameter("openId", user.openId.get)
    }
    if (user.idType.isDefined) {
      setString += """,id_type = {idType}"""
      params = params :+ NamedParameter("idType", user.idType.get)
    }
    if (user.lastloginDt.isDefined) {
      setString += """,lastlogin_dt = CURRENT_TIMESTAMP(0)"""
    }
    if (user.lastloginIp.isDefined) {
      setString += """,lastlogin_ip = cidr({lastloginIp})"""
      params = params :+ NamedParameter("lastloginIp", user.lastloginIp.get)
    }
    if (user.loginTimes.isDefined) {
      setString +="""COALESCE(login_times::int,0)+1"""
    }
    if (user.userId.isDefined) {
      whereString += "and user_id = {userId}"
    }

    DB.withConnection { implicit c =>
      SQL( """UPDATE "ID" SET """ + setString + """ WHERE """ + whereString).on(params: _*).executeUpdate()
    }
  }

  def updateUserDetail(userDetail: UserOpen): Int = {
    var setString: String = "alter_dt = CURRENT_TIMESTAMP(0)"
    var params: collection.mutable.Seq[NamedParameter] = collection.mutable.Seq(
      NamedParameter("userId", userDetail.userId.get)
    )
    if (userDetail.orActive.isDefined) {
      setString += """, "or_active" = {orActive}"""
      params = params :+ NamedParameter("orActive", userDetail.orActive.get)
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
    if (userDetail.orReal.isDefined) {
      setString += ""","or_real" = {orReal}"""
      params = params :+ NamedParameter("orReal", userDetail.orReal.get)
    }
    if (userDetail.status.isDefined) {
      setString += """,status = {status}"""
      params = params :+ NamedParameter("status", userDetail.status.get)
    }

    DB.withConnection { implicit c =>
      SQL( """UPDATE "ID" SET """ + setString + """ WHERE user_id = {userId}""").on(params: _*).executeUpdate()
    }
  }


  def reset_password(phone: String, passwd: String): Int = {
    DB.withConnection() { implicit conn =>
      SQL( """ update "ID" set  passwd = user_passwd(user_id,{passwd}) where phone_num = {phone_num} """).on("passwd" -> passwd, "phone_num" -> phone).executeUpdate()
    }
  }


  def insert(phone: String, passwd: String, ip: String): Option[Long] = {
    val nickname = "HMM "+phone.substring(0,3)+"****"+phone.substring(7,11)
    DB.withConnection() { implicit conn =>
      SQL( """insert into "ID" (nickname,passwd,phone_num,reg_ip,reg_dt) values ({nickname},user_passwd(currval('"ID_user_id_seq"'::regclass),{passwd}),{phone_num},cidr({reg_ip}),CURRENT_TIMESTAMP(0)) """).on("nickname" -> nickname, "passwd" -> passwd, "phone_num" -> phone, "reg_ip" -> ip).executeInsert()
    }
  }


  def login(id: Long, ip: String):Int = {
    DB.withConnection() { implicit conn =>
      SQL( """ update "ID" set lastlogin_dt = CURRENT_TIMESTAMP(0) ,login_times =COALESCE(login_times::int,0)+1, lastlogin_ip = cidr({lastlogin_ip}) where user_id={user_id} """).on("lastlogin_ip" -> ip, "user_id" -> id).executeUpdate()
    }
  }
}
