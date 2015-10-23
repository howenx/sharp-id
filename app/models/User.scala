package models

import anorm.SqlParser._
import anorm.~
import anorm.SQL
import play.api.db.DB
import play.api.Play.current

case class User (id: Long, nickname: String, gender: String, photo_url: String)

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

  def find():List[User] = {
    DB.withConnection() {implicit conn =>
      SQL("""select user_id, nickname, gender, photo_url from "ID" """).as(user *)
    }
  }

  def find_by_id(id:Long):Option[User] = {
    DB.withConnection() {implicit conn =>
      SQL("""select user_id, nickname, gender, photo_url from "ID" where user_id = {user_id}""").on("user_id"->id).as(user *).headOption
    }
  }

  def find_by_nickanme(nickname:String, passwd:String):Option[User] = {
    DB.withConnection() {implicit conn =>
      SQL("""select user_id, nickname, gender, photo_url from "ID" where nickname={nickname} and passwd = {passwd}""").on("nickname"->nickname, "passwd"->passwd).as(user *).headOption
    }
  }

  def find_by_phone(phone:String, passwd:String):Option[User] = {
    DB.withConnection() {implicit conn =>
      SQL("""select user_id, nickname, gender, photo_url from "ID" """).as(user *).headOption
    }
  }

  def insert(user:User) ={
    DB.withConnection() {implicit conn =>
      SQL("""insert into "ID" (nickname, gender """)
    }
  }

}
