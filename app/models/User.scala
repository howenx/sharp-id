package models

import java.sql.Timestamp
import java.text.SimpleDateFormat

import anorm.SqlParser._
import anorm.{NamedParameter, SQL, ~}
import com.fasterxml.jackson.databind.JsonNode
import org.joda.time.DateTime
import play.api.Play.current
import play.api.db.DB
import play.libs.Json

case class User(id: Long, nickname: String, gender: String, photo_url: String)

case class UserMore(id: Long, nickname: String, gender: String, photo_url: String, email: String, birthday: String, phone_num: String, active: String)

case class Address(addId: Option[Long], tel: Option[String], name: Option[String], deliveryCity: Option[String], deliveryDetail: Option[String], userId: Option[Long], orDefault: Option[Boolean])


/**
  * Created by handy on 15/10/23.
  * Daumkakao china
  */
object User {
  val user = {
    get[Long]("user_id") ~
      get[String]("nickname") ~
      get[String]("gender") ~
      get[String]("photo_url") map {
      case user_id ~ nickname ~ gender ~ photo_url => User(user_id, nickname, gender, photo_url)
    }
  }

  val address = {
    get[Long]("add_id") ~
      get[String]("tel") ~
      get[String]("name") ~
      get[String]("delivery_city") ~
      get[String]("delivery_detail") ~
      get[Long]("user_id") ~
      get[Boolean]("or_default") map {
      case add_id ~ tel ~ name ~ delivery_city ~ delivery_detail ~ user_id ~ or_default => Address(Some(add_id), Some(tel), Some(name), Some(delivery_city), Some(delivery_detail), Some(user_id), Some(or_default))
    }
  }

  val userMore = {
    get[Long]("user_id") ~
      get[String]("nickname") ~
      get[String]("gender") ~
      get[String]("photo_url") ~
      get[String]("email") ~
      get[String]("to_char") ~
      get[String]("phone_num") ~
      get[String]("active_YN") map {
      case user_id ~ nickname ~ gender ~ photo_url ~ email ~ birthday ~ phone_num ~ active_YN => UserMore(user_id, nickname, gender, photo_url, email, birthday, phone_num, active_YN)
    }
  }


  def find(): List[User] = {
    DB.withConnection() { implicit conn =>
      SQL( """select user_id, nickname, gender, photo_url from "ID" """).as(user *)
    }
  }

  def find_by_id(id: Long): Option[User] = {
    DB.withConnection() { implicit conn =>
      SQL( """select user_id, nickname, gender, photo_url from "ID" where user_id = {user_id}""").on("user_id" -> id).as(user *).headOption
    }
  }

  def find_by_id_more(id: Long): Option[UserMore] = {
    DB.withConnection() { implicit conn =>
      SQL( """select user_id, nickname, gender, photo_url,email,to_char(birthday,'YYYY-MM-DD'),phone_num,"active_YN" from "ID" where user_id = {user_id}""").on("user_id" -> id).as(userMore *).headOption
    }
  }

  def find_by_nickname(nickname: String): Option[User] = {
    DB.withConnection() { implicit conn =>
      SQL( """select user_id, nickname, gender, photo_url from "ID" where nickname = {nickname} """).on("nickname" -> nickname).as(user *).headOption
    }
  }

  def find_by_nickname(nickname: String, passwd: String): Option[User] = {
    DB.withConnection() { implicit conn =>
      SQL( """select user_id, nickname, gender, photo_url from "ID" where nickname = {nickname} and passwd = user_passwd(user_id,{passwd}) """).on("nickname" -> nickname, "passwd" -> passwd).as(user *).headOption
    }
  }

  def find_by_email(email: String): Option[User] = {
    DB.withConnection() { implicit conn =>
      SQL( """select user_id, nickname, gender, photo_url from "ID" where email = {email}  """).on("email" -> email).as(user *).headOption
    }
  }

  def find_by_email(email: String, passwd: String): Option[User] = {
    DB.withConnection() { implicit conn =>
      SQL( """select user_id, nickname, gender, photo_url from "ID" where email = {email} and passwd = user_passwd(user_id,{passwd}) """).on("email" -> email, "passwd" -> passwd).as(user *).headOption
    }
  }

  def find_by_phone(phone: String): Option[User] = {
    DB.withConnection() { implicit conn =>
      SQL( """select user_id, nickname, gender, photo_url from "ID" where phone_num = {phone_num}""").on("phone_num" -> phone).as(user *).headOption
    }
  }

  def find_by_phone(phone: String, passwd: String): Option[User] = {
    DB.withConnection() { implicit conn =>
      SQL( """select user_id, nickname, gender, photo_url from "ID" where phone_num = {phone_num} and passwd = user_passwd(user_id,{passwd}) """).on("phone_num" -> phone, "passwd" -> passwd).as(user *).headOption
    }
  }

  def reset_password(phone: String, passwd: String): Int = {
    DB.withConnection() { implicit conn =>
      SQL( """ update "ID" set  passwd = user_passwd(user_id,{passwd}) where phone_num = {phone_num} """).on("passwd" -> passwd, "phone_num" -> phone).executeUpdate()
    }
  }

  def change_phone(phone: String, id: Long): Int = {
    DB.withConnection() { implicit conn =>
      SQL( """ update "ID" set phone_num = {phone_num} where user_id={user_id}""").on("phone_num" -> phone, "user_id" -> id).executeUpdate()
    }
  }

  def change_password(password: String, id: Long, old: String): Int = {
    DB.withConnection() { implicit conn =>
      SQL( """ update "ID" set passwd = user_passwd(user_id,{passwd}) where user_id={user_id} and passwd =  user_passwd(user_id,{old}) """).on("passwd" -> password, "user_id" -> id, "old" -> old).executeUpdate()
    }
  }

  /**
    * 简单注册
    * @param phone
    * @param passwd
    * @return
    */
  def insert(phone: String, passwd: String, ip: String): Option[Long] = {
    val nickname = String.valueOf((100000 + Math.random * 900000).toInt)
    DB.withConnection() { implicit conn =>
      SQL( """insert into "ID" (nickname,passwd,email,phone_num,reg_ip,"active_YN",lastlogin_ip) values ({nickname},user_passwd(currval('"ID_user_id_seq"'::regclass),{passwd}),{email},{phone_num},cidr({reg_ip}),{active_YN},cidr({lastlogin_ip})) """).on("nickname" -> nickname, "passwd" -> passwd, "email" -> "", "phone_num" -> phone, "reg_ip" -> ip, "active_YN" -> "N", "lastlogin_ip" -> ip).executeInsert()
    }
  }

  def login(id: Long, ip: String) = {
    DB.withConnection() { implicit conn =>
      SQL( """ update "ID" set lastlogin_dt = {lastlogin_dt} , lastlogin_ip = cidr({lastlogin_ip}) where user_id={user_id} """)
        .on("lastlogin_dt" -> new Timestamp((new DateTime()).withMillisOfSecond(0).getMillis), "lastlogin_ip" -> ip, "user_id" -> id).executeUpdate()
    }
  }

  def change(id: Long, email: String, active: String, nickname: String, birthday: String, gender: String, photo: String): Int = {
    DB.withConnection() { implicit conn =>
      if (photo.equals("")) {
        SQL( """ update "ID" set email = {email} ,"active_YN"={active} , nickname = {nickname},birthday = {birthday},gender={gender} where user_id={user_id} """).on("email" -> email, "active" -> active, "nickname" -> nickname, "birthday" -> new SimpleDateFormat("yyyy-MM-dd").parse(birthday), "gender" -> gender, "user_id" -> id).executeUpdate()
      } else {
        SQL( """ update "ID" set email = {email} ,"active_YN"={active} , nickname = {nickname},birthday = {birthday},gender={gender},photo_url={photo} where user_id={user_id} """).on("email" -> email, "active" -> active, "nickname" -> nickname, "birthday" -> new SimpleDateFormat("yyyy-MM-dd").parse(birthday), "gender" -> gender, "photo" -> photo, "user_id" -> id).executeUpdate()
      }
    }
  }

  def active(id: Long): Int = {
    DB.withConnection() { implicit conn =>
      SQL( """ update "ID" set "active_YN"={active}  where user_id={user_id}""").on("active" -> "Y", "user_id" -> id).executeUpdate()
    }
  }

  def changeRealName(id: Long, cardNum: String, cardImg: JsonNode, realName: String,realYn:String): Int = {
    DB.withConnection() { implicit conn =>
      SQL( """ update "ID" set real_name={realName},card_num={cardNum},card_img={cardImg}::jsonb,"real_YN"={realYn} where user_id={user_id} """).on("realName" -> realName, "cardNum" -> cardNum, "cardImg" -> Json.stringify(cardImg),"realYn" -> realYn, "user_id" -> id).executeUpdate()
    }
  }

  def allAddress(): List[Address] = {
    DB.withConnection() { implicit conn =>
      SQL( """select i.add_id,i.tel,i."name",i.delivery_city,i.delivery_detail,i.user_id,i.or_default from id_address i WHERE i.or_destroy=false """).as(address *)
    }
  }

  def updateAddress(address: Address): Int = {
    DB.withConnection() { implicit conn =>
      val params: Seq[NamedParameter] = Seq("tel" -> address.tel.get, "name" -> address.name.get
        , "deliveryCity" -> address.deliveryCity.get, "deliveryDetail" -> address.deliveryDetail.get, "userId" -> address.userId.get
        , "orDefault" -> address.orDefault.get, "addId" -> address.addId.get
      )
      SQL( """ update id_address set tel={tel} ,"name" = {name}, delivery_city={deliveryCity} , delivery_detail={deliveryDetail}, user_id={userId} , or_default={orDefault},update_at=CURRENT_TIMESTAMP(0) where add_id={addId}""").on(params: _*).executeUpdate()
    }
  }

  def deleteAddress(address: Address): Int = {
    DB.withConnection() { implicit conn =>
      SQL( """ update id_address set or_destroy=true, destroy_at=CURRENT_TIMESTAMP(0) where add_id={addId}""").on("addId" -> address.addId).executeUpdate()
    }
  }

  def insertAddress(address: Address): Option[Long] = {
    DB.withConnection() { implicit conn =>
      val params: Seq[NamedParameter] = Seq("tel" -> address.tel.get, "name" -> address.name.get
        , "deliveryCity" -> address.deliveryCity.get, "deliveryDetail" -> address.deliveryDetail.get, "userId" -> address.userId.get
      )
      SQL(
        """ insert into  id_address(user_id,tel, "name",delivery_city,delivery_detail,create_at)values({userId},{tel},{name},{deliveryCity},{deliveryDetail},CURRENT_TIMESTAMP(0) )""").on(params: _*).executeInsert()
    }
  }

}
