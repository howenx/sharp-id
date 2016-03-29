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
case class Address(addId: Option[Long], tel: Option[String], name: Option[String], deliveryCity: Option[String], deliveryDetail: Option[String], userId: Option[Long], orDefault: Option[Int], idCardNum: Option[String], orDestroy: Option[Boolean], cityCode: Option[String])

case class UserDetail(userId: Option[Long], nickname: Option[String], phoneNum: Option[String], birthday: Option[String], activeYn: Option[String], realYN: Option[String], gender: Option[String], photoUrl: Option[String], status: Option[String])

case class UserOpen(
                     userId: Option[Long], //用户ID
                     nickname: Option[String], //用户昵称
                     passwd: Option[String], //密码
                     phoneNum: Option[String], //手机号
                     gender: Option[String], //性别
                     birthday: Option[String], //生日
                     photoUrl: Option[String], //头像
                     realName: Option[String], //真实姓名
                     cardType: Option[String], //证件类型
                     cardNum: Option[String], //证件编号
                     cardImg: Option[String], //证件原图
                     regIp: Option[String], //注册IP
                     regDt: Option[String], //注册时间
                     realYn: Option[String], //是否实名认证
                     lastloginDt: Option[String], //最后登录时间
                     lastloginIp: Option[String], //最后登录IP
                     status: Option[String], //状态
                     idType: Option[String], //用户类型
                     openId: Option[String], //其它平台唯一识别用户ID
                     idArea: Option[String], //用户所在区域
                     loginTimes: Option[Long], //登录次数
                     email: Option[String], //邮箱
                     alterDt: Option[String], //更新时间
                     activeYn: Option[String] //激活时间
                   )

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

  val userOpenMapping = {
    get[Long]("user_id") ~
      get[String]("nickname") ~
      get[String]("passwd") ~
      get[String]("phone_num") ~
      get[String]("gender") ~
      get[String]("birthday") ~
      get[String]("photo_url") ~
      get[String]("real_name") ~
      get[String]("card_type") ~
      get[String]("card_num") ~
      get[String]("card_img") ~
      get[String]("reg_ip") ~
      get[String]("reg_dt") ~
      get[String]("real_YN") ~
      get[String]("lastlogin_dt") ~
      get[String]("lastlogin_ip") ~
      get[String]("status") ~
      get[String]("id_type") ~
      get[String]("open_id") ~
      get[String]("id_area") ~
      get[Long]("login_times") ~
      get[String]("email") ~
      get[String]("alter_dt") ~
      get[String]("active_YN") map {
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
        real_YN ~
        lastlogin_dt ~
        lastlogin_ip ~
        status ~
        id_type ~
        open_id ~
        id_area ~
        login_times ~
        email ~
        alter_dt ~
        active_YN =>
        UserOpen(
          Some(user_id),
          Some(nickname),
          Some(passwd),
          Some(phone_num),
          Some(gender),
          Some(birthday),
          Some(photo_url),
          Some(real_name),
          Some(card_type),
          Some(card_num),
          Some(card_img),
          Some(reg_ip),
          Some(reg_dt),
          Some(real_YN),
          Some(lastlogin_dt),
          Some(lastlogin_ip),
          Some(status),
          Some(id_type),
          Some(open_id),
          Some(id_area),
          Some(login_times),
          Some(email),
          Some(alter_dt),
          Some(active_YN)
        )
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
    "user_id" +
    "nickname" +
    "passwd" +
    "phone_num" +
    "gender" +
    "to_char(birthday,'YYYY-MM-DD') birthday" +
    "photo_url" +
    "real_name" +
    "card_type" +
    "card_num" +
    "card_img" +
    "reg_ip::text" +
    "reg_dt" +
    """"real_YN"""" +
    "to_char(lastlogin_dt,'YYYY-MM-DD HH24:MI:SS') lastlogin_dt" +
    "lastlogin_ip::text" +
    "status" +
    "id_type" +
    "open_id" +
    "id_area" +
    "login_times" +
    "email" +
    "alter_dt" +
    """"active_YN""""

  def findByUserId(id: Long): Option[UserDetail] = {
    DB.withConnection() { implicit conn =>
      SQL( """select """ + sqlColumn +""" from "ID" where user_id = {user_id}""").on("user_id" -> id).as(userInfoDetailMapping.*).headOption
    }
  }


  def queryUser(user: UserOpen): Option[UserOpen] = {
    var setString: String = "1 = 1"
    var params: collection.mutable.Seq[NamedParameter] = collection.mutable.Seq()
    if (user.userId.isDefined) {
      setString += """and user_id = {userId}"""
      params = params :+ NamedParameter("userId", user.userId.get)
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
    var params: collection.mutable.Seq[NamedParameter] = collection.mutable.Seq(
      NamedParameter("userId", user.userId.get)
    )
    if (user.activeYn.isDefined) {
      setString += """, "active_YN" = {activeYn}"""
      params = params :+ NamedParameter("activeYn", user.activeYn.get)
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
    if (user.realYn.isDefined) {
      setString += ""","real_YN" = {realYN}"""
      params = params :+ NamedParameter("realYN", user.realYn.get)
    }
    if (user.status.isDefined) {
      setString += """,status = {status}"""
      params = params :+ NamedParameter("status", user.status.get)
    }
    if (user.openId.isDefined) {
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

    DB.withConnection { implicit c =>
      SQL( """UPDATE "ID" SET """ + setString + """ WHERE user_id = {userId}""").on(params: _*).executeUpdate()
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
