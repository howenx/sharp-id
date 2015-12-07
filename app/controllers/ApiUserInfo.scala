package controllers

import java.io.ByteArrayInputStream
import javax.inject.{Inject, Named}

import actor.OSSIS
import akka.actor.ActorRef
import models.{Address, UserDetail, UserInfo}
import net.spy.memcached.MemcachedClient
import org.apache.commons.codec.binary.Base64
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc.{Action, BodyParsers, Controller}
import play.api.{Configuration, Logger}

/**
  * 用户信息返回和收货地址CURD
  * Created by howen on 15/11/30.
  */

case class Message(var message: String, var code: Int)

object ChessPiece extends Enumeration {
  val SUCCESS = ChessPieceVal("成功", 200)
  val FAILURE = ChessPieceVal("失败", 400)
  val ERROR = ChessPieceVal("内部发生错误", 1001)
  val SERVER_EXCEPTION = ChessPieceVal("服务器异常", 1002)
  val BAD_PARAMETER = ChessPieceVal("参数不合法", 1003)
  val BAD_USER_TOKEN = ChessPieceVal("用户不存在", 1004)
  val DATABASE_EXCEPTION = ChessPieceVal("数据库操作异常", 1005)

  protected case class ChessPieceVal(string: String, pointValue: Int) extends super.Val()

  def convert(value: Value) = value.asInstanceOf[ChessPieceVal]
}

class ApiUserInfo @Inject()(cache_client: MemcachedClient, @Named("sms") sms: ActorRef, @Named("oss") oss: ActorRef, configuration: Configuration) extends Controller {


  case class RealNameForm(realName: Option[String], cardNum: Option[String], cardImgA: Option[String], cardImgB: Option[String])

  implicit lazy val realNameFormReads: Reads[RealNameForm] = (
    (__ \ "realName").readNullable[String] and
      (__ \ "cardNum").readNullable[String] and
      (__ \ "cardImgA").readNullable[String] and
      (__ \ "cardImgB").readNullable[String]
    ) (RealNameForm)

  implicit val realNameFormWrites: Writes[RealNameForm] = (
    (__ \ "realName").writeNullable[String] and
      (__ \ "cardNum").writeNullable[String] and
      (__ \ "cardImgA").writeNullable[String] and
      (__ \ "cardImgB").writeNullable[String]
    ) (unlift(RealNameForm.unapply))

  implicit lazy val messageReads: Reads[Message] = (
    (__ \ "message").read[String] and
      (__ \ "code").read[Int]
    ) (Message)

  implicit lazy val messageWrites: Writes[Message] = (
    (__ \ "message").write[String] and
      (__ \ "code").write[Int]
    ) (unlift(Message.unapply))

  implicit lazy val addressReads: Reads[Address] = (
    (__ \ "addId").readNullable[Long] and
      (__ \ "tel").readNullable[String] and
      (__ \ "name").readNullable[String] and
      (__ \ "deliveryCity").readNullable[String] and
      (__ \ "deliveryDetail").readNullable[String] and
      (__ \ "userId").readNullable[Long] and
      (__ \ "orDefault").readNullable[Boolean] and
      (__ \ "idCardNum").readNullable[String] and
      (__ \ "orDestroy").readNullable[Boolean] and
      (__ \ "cityCode").readNullable[String]
    ) (Address)

  implicit lazy val addressWrites: Writes[Address] = (
    (__ \ "addId").writeNullable[Long] and
      (__ \ "tel").writeNullable[String] and
      (__ \ "name").writeNullable[String] and
      (__ \ "deliveryCity").writeNullable[String] and
      (__ \ "deliveryDetail").writeNullable[String] and
      (__ \ "userId").writeNullable[Long] and
      (__ \ "orDefault").writeNullable[Boolean] and
      (__ \ "idCardNum").writeNullable[String] and
      (__ \ "orDestroy").writeNullable[Boolean] and
      (__ \ "cityCode").writeNullable[String]
    ) (unlift(Address.unapply))

  implicit lazy val userDetailReads: Reads[UserDetail] = (
    (__ \ "userId").readNullable[Long] and
      (__ \ "nickname").readNullable[String] and
      (__ \ "phoneNum").readNullable[String] and
      (__ \ "birthday").readNullable[String] and
      (__ \ "activeYn").readNullable[String] and
      (__ \ "realYN").readNullable[String] and
      (__ \ "gender").readNullable[String] and
      (__ \ "photoUrl").readNullable[String] and
      (__ \ "status").readNullable[String]
    ) (UserDetail)

  implicit lazy val userDetailWrites: Writes[UserDetail] = (
    (__ \ "userId").writeNullable[Long] and
      (__ \ "nickname").writeNullable[String] and
      (__ \ "phoneNum").writeNullable[String] and
      (__ \ "birthday").writeNullable[String] and
      (__ \ "activeYn").writeNullable[String] and
      (__ \ "realYN").writeNullable[String] and
      (__ \ "gender").writeNullable[String] and
      (__ \ "photoUrl").writeNullable[String] and
      (__ \ "status").writeNullable[String]
    ) (unlift(UserDetail.unapply))

  /**
    * 用户上传身份证信息
    * @return
    */
  def change_real_name = Action(BodyParsers.parse.json(10 * 1024 * 1024)) { request =>
    val cache = collection.mutable.Map[String, Object]()

    val data = request.body.validate[RealNameForm]
    data.fold(
      errors => {
        cache.put("message", Message(ChessPiece.ERROR.string, ChessPiece.ERROR.pointValue))
        Ok(JsonUtil.toJson(cache))
      },
      data => {
        if (request.headers.get("id-token").isDefined) {
          val id_token = cache_client.get(request.headers.get("id-token").get)
          val user_id = Json.parse(id_token.toString).\("id").asOpt[String]
          if (user_id.isDefined) {
            val bytea = Base64.decodeBase64(data.cardImgA.get.getBytes)
            val byteb = Base64.decodeBase64(data.cardImgB.get.getBytes)
            val isa = new ByteArrayInputStream(bytea)
            val isb = new ByteArrayInputStream(byteb)
            val keya = "style" + "/" + DateTimeFormat.forPattern("yyyy-MM-dd").print(new DateTime) + "/" + System.currentTimeMillis + scala.util.Random.nextInt(6) + ".jpg"
            val keyb = "style" + "/" + DateTimeFormat.forPattern("yyyy-MM-dd").print(new DateTime) + "/" + System.currentTimeMillis + scala.util.Random.nextInt(6) + ".jpg"
            val json = play.libs.Json.newObject()
            json.put("cardImgA", "/" + keya)
            json.put("cardImgB", "/" + keyb)
            oss ! OSSIS(isa, keya, bytea.length)
            oss ! OSSIS(isb, keyb, byteb.length)
            val realYn = "Y"
            if (UserInfo.changeRealName(user_id.get.toLong, data.cardNum.get, json, data.realName.get, realYn) > 0) {
              cache.put("message", Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue))
              Ok(JsonUtil.toJson(cache))
            }
            else {
              cache.put("message", Message(ChessPiece.BAD_USER_TOKEN.string, ChessPiece.BAD_USER_TOKEN.pointValue))
              Ok(JsonUtil.toJson(cache))
            }
          } else {
            cache.put("message", Message(ChessPiece.BAD_USER_TOKEN.string, ChessPiece.BAD_USER_TOKEN.pointValue))
            Ok(JsonUtil.toJson(cache))
          }
        }
        else {
          cache.put("message", Message(ChessPiece.BAD_USER_TOKEN.string, ChessPiece.BAD_USER_TOKEN.pointValue))
          Ok(JsonUtil.toJson(cache))
        }
      }
    )
  }

  /**
    * 查询用户当前的所有收获地址
    * @return
    */
  def all_address = Action { request =>
    val cache = collection.mutable.Map[String, Any]()
    if (request.headers.get("id-token").isDefined) {
      val id_token = cache_client.get(request.headers.get("id-token").get)
      val user_id = Json.parse(id_token.toString).\("id").asOpt[String]
      if (user_id.isDefined) {
        val addr: Address = new Address(None, None, None, None, None, Some(user_id.get.toLong), None, None, None,None)
        val adds: List[Address] = UserInfo.allAddress(addr)
        if (adds.nonEmpty) {
          cache.put("message", Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue))
          cache.put("address", adds)
          Ok(JsonUtil.toJson(cache))
        } else {
          cache.put("message", Message(ChessPiece.DATABASE_EXCEPTION.string, ChessPiece.DATABASE_EXCEPTION.pointValue))
          Ok(JsonUtil.toJson(cache))
        }
      } else {
        cache.put("message", Message(ChessPiece.BAD_USER_TOKEN.string, ChessPiece.BAD_USER_TOKEN.pointValue))
        Ok(JsonUtil.toJson(cache))
      }
    } else {
      cache.put("message", JsonUtil.toJson(new Message(ChessPiece.BAD_USER_TOKEN.string, ChessPiece.BAD_USER_TOKEN.pointValue)))
      Ok(JsonUtil.toJson(cache))
    }
  }

  /**
    * 保存用户收货地址
    * @return
    */
  def insert_address = Action(BodyParsers.parse.json) { request =>
    val data: JsResult[Address] = request.body.validate[Address]

    val cache = collection.mutable.Map[String, Any]()
    data.fold(
      errors => {
        cache.put("message", new Message(ChessPiece.ERROR.string, ChessPiece.ERROR.pointValue))
        Ok(JsonUtil.toJson(cache))
      },
      data => {
        if (request.headers.get("id-token").isDefined) {
          val id_token = cache_client.get(request.headers.get("id-token").get)
          val user_id = Json.parse(id_token.toString).\("id").asOpt[String]
          if (user_id.isDefined) {
            //在进行insert前必须先判断是否是默认,如果是要作为默认的,就需要先更新此用户的所有未删除的地址为非默认,否则去查看当前是否存在有默认的,如果有就insert,否则就设置当前的为默认的
            if (data.orDefault.isDefined) {

              //如果要设置当前新加入的地址为默认地址
              if (data.orDefault.get) {
                val address: Address = new Address(None, None, None, None, None, Some(user_id.get.toLong), Some(false), None, None, None)
                val result = UserInfo.updateAddress(address)
                if (result >= 0) {
                  val address: Address = new Address(data.addId, data.tel, data.name, data.deliveryCity, data.deliveryDetail, Some(user_id.get.toLong), Some(true), data.idCardNum, data.orDestroy, None)
                  val result = UserInfo.insertAddress(address)
                  result match {
                    case Some(content) =>
                      cache.put("address", new Address(Some(content), data.tel, data.name, data.deliveryCity, data.deliveryDetail, Some(user_id.get.toLong), Some(true), data.idCardNum, data.orDestroy, None))
                      cache.put("message", Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue))
                      Ok(JsonUtil.toJson(cache))
                    case None =>
                      cache.put("message", Message(ChessPiece.DATABASE_EXCEPTION.string, ChessPiece.DATABASE_EXCEPTION.pointValue))
                      Ok(JsonUtil.toJson(cache))
                  }
                } else {
                  cache.put("message", Message(ChessPiece.DATABASE_EXCEPTION.string, ChessPiece.DATABASE_EXCEPTION.pointValue))
                  Ok(JsonUtil.toJson(cache))
                }
              } else {
                val address: Address = new Address(None, None, None, None, None, Some(user_id.get.toLong), Some(true), None, None, None)
                //先判断在insert中的是非默认地址时,如果此用户已经存在默认地址,那么就插入非默认地址,否则就把当前地址作为默认地址插入
                if (UserInfo.allAddress(address).nonEmpty) {
                  val address: Address = new Address(data.addId, data.tel, data.name, data.deliveryCity, data.deliveryDetail, Some(user_id.get.toLong), Some(false), data.idCardNum, data.orDestroy, None)
                  val result = UserInfo.insertAddress(address)
                  result match {
                    case Some(content) =>
                      cache.put("address", new Address(Some(content), data.tel, data.name, data.deliveryCity, data.deliveryDetail, Some(user_id.get.toLong), Some(false), data.idCardNum, data.orDestroy, None))
                      cache.put("message", Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue))
                      Ok(JsonUtil.toJson(cache))
                    case None =>
                      cache.put("message", Message(ChessPiece.DATABASE_EXCEPTION.string, ChessPiece.DATABASE_EXCEPTION.pointValue))
                      Ok(JsonUtil.toJson(cache))
                  }
                } else {
                  val address: Address = new Address(data.addId, data.tel, data.name, data.deliveryCity, data.deliveryDetail, Some(user_id.get.toLong), Some(true), data.idCardNum, data.orDestroy, None)
                  val result = UserInfo.insertAddress(address)
                  result match {
                    case Some(content) =>
                      cache.put("address", new Address(Some(content), data.tel, data.name, data.deliveryCity, data.deliveryDetail, Some(user_id.get.toLong), Some(true), data.idCardNum, data.orDestroy, None))
                      cache.put("message", Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue))
                      Ok(JsonUtil.toJson(cache))
                    case None =>
                      cache.put("message", Message(ChessPiece.DATABASE_EXCEPTION.string, ChessPiece.DATABASE_EXCEPTION.pointValue))
                      Ok(JsonUtil.toJson(cache))
                  }
                }
              }
            } else {
              cache.put("message", Message(ChessPiece.BAD_PARAMETER.string, ChessPiece.BAD_PARAMETER.pointValue))
              Ok(JsonUtil.toJson(cache))
            }
          } else {
            cache.put("message", Message(ChessPiece.BAD_USER_TOKEN.string, ChessPiece.BAD_USER_TOKEN.pointValue))
            Ok(JsonUtil.toJson(cache))
          }
        } else {
          cache.put("message", Message(ChessPiece.BAD_USER_TOKEN.string, ChessPiece.BAD_USER_TOKEN.pointValue))
          Ok(JsonUtil.toJson(cache))
        }
      })
  }

  /**
    * 更新删除收货地址
    * @param handle 删除或者更新标志位
    * @return
    */
  def update_address(handle: Integer) = Action(BodyParsers.parse.json) { request =>
    val data: JsResult[Address] = request.body.validate[Address]
    val cache = collection.mutable.Map[String, Object]()
    data.fold(
      errors => {
        cache.put("message", Message(ChessPiece.ERROR.string, ChessPiece.ERROR.pointValue))
        Ok(JsonUtil.toJson(cache))
      },
      data => {
        if (request.headers.get("id-token").isDefined) {
          val id_token = cache_client.get(request.headers.get("id-token").get)
          val user_id = Json.parse(id_token.toString).\("id").asOpt[String]
          if (user_id.isDefined) {
            if (handle == 1) {
              Logger.error(data.toString)
              //更新时候,先看是否已经存在默认地址
              if (data.orDefault.isDefined) {
                if (data.orDefault.get) {
                  //如果要设置当前地址为默认地址,那么更新此用户下所有地址为非默认地址
                  val address: Address = new Address(None, None, None, None, None, Some(user_id.get.toLong), Some(false), None, None, None)
                  val result = UserInfo.updateAddress(address)
                  if (result >= 0) {
                    val address: Address = new Address(data.addId, data.tel, data.name, data.deliveryCity, data.deliveryDetail, Some(user_id.get.toLong), Some(true), data.idCardNum, data.orDestroy, None)
                    val result = UserInfo.updateAddress(address)
                    if (result >= 0) {
                      cache.put("message", Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue))
                      Ok(JsonUtil.toJson(cache))
                    } else {
                      cache.put("message", Message(ChessPiece.DATABASE_EXCEPTION.string, ChessPiece.DATABASE_EXCEPTION.pointValue))
                      Ok(JsonUtil.toJson(cache))
                    }
                  } else {
                    cache.put("message", Message(ChessPiece.DATABASE_EXCEPTION.string, ChessPiece.DATABASE_EXCEPTION.pointValue))
                    Ok(JsonUtil.toJson(cache))
                  }
                } else {
                  //如果用户要设置当前地址为非默认地址,那么就查询当前是否有其他地址为默认地址,否则仍旧设置当前地址为默认地址
                  val address: Address = new Address(None, None, None, None, None, Some(user_id.get.toLong), Some(true), None, Some(false), None)
                  if (UserInfo.allAddress(address).nonEmpty) {
                    val address: Address = new Address(data.addId, data.tel, data.name, data.deliveryCity, data.deliveryDetail, Some(user_id.get.toLong), Some(false), data.idCardNum, data.orDestroy, None)
                    val result = UserInfo.updateAddress(address)
                    if (result >= 0) {
                      cache.put("message", Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue))
                      Ok(JsonUtil.toJson(cache))
                    } else {
                      cache.put("message", Message(ChessPiece.DATABASE_EXCEPTION.string, ChessPiece.DATABASE_EXCEPTION.pointValue))
                      Ok(JsonUtil.toJson(cache))
                    }
                  } else {
                    val address: Address = new Address(data.addId, data.tel, data.name, data.deliveryCity, data.deliveryDetail, Some(user_id.get.toLong), Some(true), data.idCardNum, data.orDestroy, None)
                    val result = UserInfo.updateAddress(address)
                    if (result >= 0) {
                      cache.put("message", Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue))
                      Ok(JsonUtil.toJson(cache))
                    } else {
                      cache.put("message", Message(ChessPiece.DATABASE_EXCEPTION.string, ChessPiece.DATABASE_EXCEPTION.pointValue))
                      Ok(JsonUtil.toJson(cache))
                    }
                  }
                }
              } else {
                cache.put("message", Message(ChessPiece.BAD_PARAMETER.string, ChessPiece.BAD_PARAMETER.pointValue))
                Ok(JsonUtil.toJson(cache))
              }
            } else {

              //删除的时候也要判断,如果删除的是默认地址,那么如果用此用户去查询还有其他的地址,那么随便取一个设置为默认地址,如果没有其他地址,不管,如果删除的是非默认地址,也不管
              if (data.orDefault.isDefined) {
                if (data.orDefault.get) {
                  val address: Address = new Address(data.addId, data.tel, data.name, data.deliveryCity, data.deliveryDetail, Some(user_id.get.toLong), data.orDefault, data.idCardNum, Some(true), None)
                  val result = UserInfo.updateAddress(address)
                  if (result >= 0) {
                    val address: Address = new Address(None, None, None, None, None, Some(user_id.get.toLong), None, None, None, None)
                    val adds: List[Address] = UserInfo.allAddress(address)
                    if (adds.nonEmpty) {
                      val add_temp = adds.last
                      val new_add = new Address(add_temp.addId, None, None, None, None, Some(user_id.get.toLong), Some(true), None, None, None)
                      val result = UserInfo.updateAddress(new_add)
                      if (result >= 0) {
                        cache.put("message", Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue))
                        Ok(JsonUtil.toJson(cache))
                      } else {
                        cache.put("message", Message(ChessPiece.DATABASE_EXCEPTION.string, ChessPiece.DATABASE_EXCEPTION.pointValue))
                        Ok(JsonUtil.toJson(cache))
                      }
                    } else {
                      cache.put("message", Message(ChessPiece.DATABASE_EXCEPTION.string, ChessPiece.DATABASE_EXCEPTION.pointValue))
                      Ok(JsonUtil.toJson(cache))
                    }
                  }
                }
                val address_del: Address = new Address(data.addId, data.tel, data.name, data.deliveryCity, data.deliveryDetail, Some(user_id.get.toLong), data.orDefault, data.idCardNum, Some(true), None)
                val result = UserInfo.updateAddress(address_del)
                if (result >= 0) {
                  cache.put("message", Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue))
                  Ok(JsonUtil.toJson(cache))
                } else {
                  cache.put("message", Message(ChessPiece.DATABASE_EXCEPTION.string, ChessPiece.DATABASE_EXCEPTION.pointValue))
                  Ok(JsonUtil.toJson(cache))
                }
              } else {
                cache.put("message", Message(ChessPiece.BAD_PARAMETER.string, ChessPiece.BAD_PARAMETER.pointValue))
                Ok(JsonUtil.toJson(cache))
              }
            }
          } else {
            cache.put("message", Message(ChessPiece.BAD_USER_TOKEN.string, ChessPiece.BAD_USER_TOKEN.pointValue))
            Ok(JsonUtil.toJson(cache))
          }
        } else {
          cache.put("message", Message(ChessPiece.BAD_USER_TOKEN.string, ChessPiece.BAD_USER_TOKEN.pointValue))
          Ok(JsonUtil.toJson(cache))
        }
      }

    )
  }

  /**
    * 查询用户信息
    * @return
    */
  def select_user_info() = Action {
    request =>
      if (request.headers.get("id-token").isDefined) {
        val id_token = cache_client.get(request.headers.get("id-token").get)
        val user_id = Json.parse(id_token.toString).\("id").asOpt[String]
        if (user_id.isDefined) {
          UserInfo.findByUserId(user_id.get.toLong) match {
            case Some(user) => Ok(Json.obj(
              "message" -> Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue),
              "userInfo" -> Json.obj("name" -> JsString(user.nickname.get),
                "photo" -> JsString(configuration.getString("staticUrl").getOrElse("") + user.photoUrl.get),
                "realYn" -> JsString(user.realYN.get),
                "phoneNum" -> JsString(user.phoneNum.get),"gender"->JsString(user.gender.get)
              )
            ))
            case None => Ok(Json.obj("message" -> Message(ChessPiece.BAD_PARAMETER.string, ChessPiece.BAD_PARAMETER.pointValue)))
          }
        }
        else Ok(Json.obj("message" -> Message(ChessPiece.BAD_USER_TOKEN.string, ChessPiece.BAD_USER_TOKEN.pointValue)))
      } else Ok(Json.obj("message" -> Message(ChessPiece.BAD_USER_TOKEN.string, ChessPiece.BAD_USER_TOKEN.pointValue)))
  }

  /**
    * 更新用户个人信息
    * @return
    */
  def update_user_info() = Action(BodyParsers.parse.json(20 * 1024 * 1024)) {
    request =>
      val data: JsResult[UserDetail] = request.body.validate[UserDetail]
      data.fold(
        errors => {
          Ok(Json.obj("message" -> Message(ChessPiece.ERROR.string, ChessPiece.ERROR.pointValue)))
        },
        data => {
          Logger.error(request.headers.get("id-token").toString)
          if (request.headers.get("id-token").isDefined) {
            val id_token = cache_client.get(request.headers.get("id-token").get)
            val user_id = Json.parse(id_token.toString).\("id").asOpt[String]

            var userDetail: UserDetail = new UserDetail(Some(user_id.get.toLong), data.nickname, data.phoneNum, data.birthday, data.activeYn, data.realYN, data.gender, data.photoUrl, data.status)

            if (user_id.isDefined) {
              if (data.photoUrl.isDefined) {
                val bytea = Base64.decodeBase64(data.photoUrl.get.getBytes)
                val isa = new ByteArrayInputStream(bytea)
                val keya = "users/photo" + "/" + DateTimeFormat.forPattern("yyyy-MM-dd").print(new DateTime) + "/" + System.currentTimeMillis + scala.util.Random.nextInt(6) + ".jpg"
                oss ! OSSIS(isa, keya, bytea.length)
                userDetail = new UserDetail(Some(user_id.get.toLong), data.nickname, data.phoneNum, data.birthday, data.activeYn, data.realYN, data.gender, Some("/" + keya), data.status)
              }
              if (UserInfo.updateUserDetail(userDetail) >= 0)
                Ok(Json.obj("message" -> Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue)))
              else Ok(Json.obj("message" -> Message(ChessPiece.DATABASE_EXCEPTION.string, ChessPiece.DATABASE_EXCEPTION.pointValue)))
            } else Ok(Json.obj("message" -> Message(ChessPiece.BAD_USER_TOKEN.string, ChessPiece.BAD_USER_TOKEN.pointValue)))
          } else Ok(Json.obj("message" -> Message(ChessPiece.BAD_USER_TOKEN.string, ChessPiece.BAD_USER_TOKEN.pointValue)))
        })
  }
}
