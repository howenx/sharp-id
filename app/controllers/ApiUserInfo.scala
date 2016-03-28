package controllers

import java.io.ByteArrayInputStream
import javax.inject.{Inject, Named}

import actor.OSSIS
import akka.actor.ActorRef
import filters.Authentication
import models.FormConstModel._
import models.JsonConstModel._
import models._
import net.spy.memcached.MemcachedClient
import org.apache.commons.codec.binary.Base64
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import play.api.libs.json._
import play.api.mvc.{BodyParsers, Controller}
import play.api.{Configuration, Logger}
import utils.JsonUtil

import scala.language.postfixOps

/**
  * 用户信息返回和收货地址CURD
  * Created by howen on 15/11/30.
  */

class ApiUserInfo @Inject()(cache_client: MemcachedClient, @Named("sms") sms: ActorRef, @Named("oss") oss: ActorRef, @Named("coupons") couponsActor: ActorRef, configuration: Configuration) extends Controller {

  val auth = new Authentication(cache_client)

  import auth.Authenticated

  /**
    * 用户上传身份证信息
    *
    * @return
    */
  def change_real_name = Authenticated(BodyParsers.parse.json(10 * 1024 * 1024)) { request =>
    val cache = collection.mutable.Map[String, Object]()

    val data = request.body.validate[RealNameForm]
    data.fold(
      errors => {
        Logger.error("Json校验错误信息--->" + errors.toString())
        cache.put("message", new Message(ChessPiece.BAD_PARAMETER.string, ChessPiece.BAD_PARAMETER.pointValue))
        Ok(JsonUtil.toJson(cache))
      },
      data => {
        val user_id = request.userId
        val byteA = Base64.decodeBase64(data.cardImgA.get.getBytes)
        val byteB = Base64.decodeBase64(data.cardImgB.get.getBytes)
        val isa = new ByteArrayInputStream(byteA)
        val isb = new ByteArrayInputStream(byteB)
        val keyA = "style" + "/" + DateTimeFormat.forPattern("yyyyMMdd").print(new DateTime) + "/" + System.currentTimeMillis + scala.util.Random.nextInt(6) + ".jpg"
        val keyB = "style" + "/" + DateTimeFormat.forPattern("yyyyMMdd").print(new DateTime) + "/" + System.currentTimeMillis + scala.util.Random.nextInt(6) + ".jpg"
        val json = play.libs.Json.newObject()
        json.put("cardImgA", "/" + keyA)
        json.put("cardImgB", "/" + keyB)
        oss ! OSSIS(isa, keyA, byteA.length)
        oss ! OSSIS(isb, keyB, byteB.length)
        val realYn = "Y"
        if (UserInfoModel.changeRealName(user_id, data.cardNum.get, json, data.realName.get, realYn) > 0) {
          cache.put("message", Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue))
          Ok(JsonUtil.toJson(cache))
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
    *
    * @return
    */
  def all_address = Authenticated { request =>

    val result = collection.mutable.Map[String, Any]()
    val user_id = request.userId
    Address.tupled
    val addr: Address = new Address(None, None, None, None, None, Some(user_id.toLong), None, None, None, None)
    val adds: List[Address] = UserInfoModel.allAddress(addr)
    if (adds.nonEmpty) {
      Logger.info("user select all address.")
      result.put("message", Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue))
      result.put("address", adds)
      Ok(JsonUtil.toJson(result))
    } else {
      result.put("message", Message(ChessPiece.DATABASE_EXCEPTION.string, ChessPiece.DATABASE_EXCEPTION.pointValue))
      Ok(JsonUtil.toJson(result))
    }

  }

  /**
    * 保存用户收货地址
    *
    * @return
    */
  def insert_address = Authenticated(BodyParsers.parse.json) { request =>
    val data: JsResult[Address] = request.body.validate[Address]
    val cache = collection.mutable.Map[String, Any]()
    data.fold(
      errors => {
        Logger.error("Json校验错误信息--->" + errors.toString())
        cache.put("message", new Message(ChessPiece.BAD_PARAMETER.string, ChessPiece.BAD_PARAMETER.pointValue))
        Ok(JsonUtil.toJson(cache))
      },
      data => {
        val user_id = request.userId
        //在进行insert前必须先判断是否是默认,如果是要作为默认的,就需要先更新此用户的所有未删除的地址为非默认,否则去查看当前是否存在有默认的,如果有就insert,否则就设置当前的为默认的
        if (data.orDefault.isDefined) {

          //如果要设置当前新加入的地址为默认地址
          if (data.orDefault.get == 1) {
            val address: Address = new Address(None, None, None, None, None, Some(user_id.toLong), Some(0), None, None, None)
            val result = UserInfoModel.updateAddress(address)
            if (result >= 0) {
              val address: Address = new Address(data.addId, data.tel, data.name, data.deliveryCity, data.deliveryDetail, Some(user_id.toLong), Some(1), data.idCardNum, data.orDestroy, None)
              val result = UserInfoModel.insertAddress(address)
              result match {
                case Some(content) =>
                  cache.put("address", new Address(Some(content), data.tel, data.name, data.deliveryCity, data.deliveryDetail, Some(user_id.toLong), Some(1), data.idCardNum, data.orDestroy, None))
                  cache.put("message", Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue))
                  Logger.info("user insert address: " + address.name)
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
            val address: Address = new Address(None, None, None, None, None, Some(user_id.toLong), Some(1), None, None, None)
            //先判断在insert中的是非默认地址时,如果此用户已经存在默认地址,那么就插入非默认地址,否则就把当前地址作为默认地址插入
            if (UserInfoModel.allAddress(address).nonEmpty) {
              val address: Address = new Address(data.addId, data.tel, data.name, data.deliveryCity, data.deliveryDetail, Some(user_id.toLong), Some(0), data.idCardNum, data.orDestroy, None)
              val result = UserInfoModel.insertAddress(address)
              result match {
                case Some(content) =>
                  cache.put("address", new Address(Some(content), data.tel, data.name, data.deliveryCity, data.deliveryDetail, Some(user_id.toLong), Some(0), data.idCardNum, data.orDestroy, None))
                  cache.put("message", Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue))
                  Logger.info("user insert address: " + address.name)
                  Ok(JsonUtil.toJson(cache))
                case None =>
                  cache.put("message", Message(ChessPiece.DATABASE_EXCEPTION.string, ChessPiece.DATABASE_EXCEPTION.pointValue))
                  Ok(JsonUtil.toJson(cache))
              }
            } else {
              val address: Address = new Address(data.addId, data.tel, data.name, data.deliveryCity, data.deliveryDetail, Some(user_id.toLong), Some(1), data.idCardNum, data.orDestroy, None)
              val result = UserInfoModel.insertAddress(address)
              result match {
                case Some(content) =>
                  cache.put("address", new Address(Some(content), data.tel, data.name, data.deliveryCity, data.deliveryDetail, Some(user_id.toLong), Some(1), data.idCardNum, data.orDestroy, None))
                  cache.put("message", Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue))
                  Logger.info("user insert address: " + address.name)
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
      })
  }

  /**
    * 更新删除收货地址
    *
    * @param handle 删除或者更新标志位
    * @return
    */
  def update_address(handle: Integer) = Authenticated(BodyParsers.parse.json) { implicit request =>
    val data: JsResult[Address] = request.body.validate[Address]
    val cache = collection.mutable.Map[String, Object]()
    data.fold(
      errors => {
        Logger.error("Json校验错误信息--->" + errors.toString())
        cache.put("message", new Message(ChessPiece.BAD_PARAMETER.string, ChessPiece.BAD_PARAMETER.pointValue))
        Ok(JsonUtil.toJson(cache))
      },
      data => {
        val user_id = request.userId
        if (handle == 1) {
          //更新时候,先看是否已经存在默认地址
          if (data.orDefault.isDefined) {
            if (data.orDefault.get == 1) {
              //如果要设置当前地址为默认地址,那么更新此用户下所有地址为非默认地址
              val address: Address = new Address(None, None, None, None, None, Some(user_id.toLong), Some(0), None, None, None)
              val result = UserInfoModel.updateAddress(address)
              if (result >= 0) {
                val address: Address = new Address(data.addId, data.tel, data.name, data.deliveryCity, data.deliveryDetail, Some(user_id.toLong), Some(1), data.idCardNum, data.orDestroy, None)
                val result = UserInfoModel.updateAddress(address)
                if (result >= 0) {
                  Logger.info("user update address: " + address.name.getOrElse(""))
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
              val address: Address = new Address(None, None, None, None, None, Some(user_id.toLong), Some(1), None, Some(false), None)
              val addList: List[Address] = UserInfoModel.allAddress(address)
              if (addList.nonEmpty) {
                if (!addList.head.addId.equals(data.addId)) {
                  val address: Address = new Address(data.addId, data.tel, data.name, data.deliveryCity, data.deliveryDetail, Some(user_id.toLong), Some(0), data.idCardNum, data.orDestroy, None)
                  val result = UserInfoModel.updateAddress(address)
                  if (result >= 0) {
                    Logger.info("user update address: " + address.name.getOrElse(""))
                    cache.put("message", Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue))
                    Ok(JsonUtil.toJson(cache))
                  } else {
                    cache.put("message", Message(ChessPiece.DATABASE_EXCEPTION.string, ChessPiece.DATABASE_EXCEPTION.pointValue))
                    Ok(JsonUtil.toJson(cache))
                  }
                } else {
                  Logger.info("user update address: " + address.name.getOrElse(""))
                  cache.put("message", Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue))
                  Ok(JsonUtil.toJson(cache))
                }
              } else {
                val address: Address = new Address(data.addId, data.tel, data.name, data.deliveryCity, data.deliveryDetail, Some(user_id.toLong), Some(1), data.idCardNum, data.orDestroy, None)
                val result = UserInfoModel.updateAddress(address)
                if (result >= 0) {
                  Logger.info("user update address: " + address.name.getOrElse(""))
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
            if (data.orDefault.get == 1) {
              val address: Address = new Address(data.addId, data.tel, data.name, data.deliveryCity, data.deliveryDetail, Some(user_id.toLong), data.orDefault, data.idCardNum, Some(true), None)
              val result = UserInfoModel.updateAddress(address)
              if (result >= 0) {
                val address: Address = new Address(None, None, None, None, None, Some(user_id.toLong), None, None, None, None)
                val adds: List[Address] = UserInfoModel.allAddress(address)
                if (adds.nonEmpty) {
                  val add_temp = adds.last
                  val new_add = new Address(add_temp.addId, None, None, None, None, Some(user_id.toLong), Some(1), None, None, None)
                  val result = UserInfoModel.updateAddress(new_add)
                  if (result >= 0) {
                    Logger.info("user delete address: " + address.name.getOrElse(""))
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
            val address_del: Address = new Address(data.addId, data.tel, data.name, data.deliveryCity, data.deliveryDetail, Some(user_id.toLong), data.orDefault, data.idCardNum, Some(true), None)
            val result = UserInfoModel.updateAddress(address_del)
            if (result >= 0) {
              Logger.info("user delete address: " + address_del.name.getOrElse(""))
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
      }

    )
  }

  /**
    * 查询用户信息
    *
    * @return
    */
  def select_user_info() = Authenticated {
    request =>
      val user_id = request.userId
      couponsActor ! CouponsVo(None, Some(user_id.toLong), None, None, None, None, Some("S"), None, None, None, None)
      //查询当前用户下所有未使用的优惠券数量
      val couponVo: List[CouponsVo] = CouponsModel.searchAllCoupons(CouponsVo(None, Some(user_id.toLong), None, None, None, None, Some("N"), None, None, None, None))
      UserInfoModel.findByUserId(user_id.toLong) match {
        case Some(user) =>
          Logger.info("user select personal info: " + user.phoneNum)
          Ok(Json.obj(
            "message" -> Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue),
            "userInfo" -> Json.obj("name" -> JsString(user.nickname.get),
              "photo" -> JsString(configuration.getString("oss.url").getOrElse("") + user.photoUrl.get),
              "realYn" -> JsString(user.realYN.get),
              "phoneNum" -> JsString(user.phoneNum.get), "gender" -> JsString(user.gender.get), "couponsCount" -> JsNumber(couponVo.size)
            )
          ))
        case None => Ok(Json.obj("message" -> Message(ChessPiece.BAD_PARAMETER.string, ChessPiece.BAD_PARAMETER.pointValue)))
      }
  }

  /**
    * 更新用户个人信息
    *
    * @return
    */
  def update_user_info() = Authenticated(BodyParsers.parse.json(20 * 1024 * 1024)) {
    request =>
      val data: JsResult[UserDetail] = request.body.validate[UserDetail]
      data.fold(
        errors => {
          Logger.error("Json校验错误信息--->" + errors.toString())
          Ok(Json.obj("message" -> Message(ChessPiece.BAD_PARAMETER.string, ChessPiece.BAD_PARAMETER.pointValue)))
        },
        data => {
          val user_id = request.userId

          var userDetail: UserDetail = UserDetail(Some(user_id.toLong), data.nickname, data.phoneNum, data.birthday, data.activeYn, data.realYN, data.gender, data.photoUrl, data.status)

          if (data.photoUrl.isDefined) {
            val byteA = Base64.decodeBase64(data.photoUrl.get.getBytes)
            val isa = new ByteArrayInputStream(byteA)
            val keyA = "users/photo" + "/" + DateTimeFormat.forPattern("yyyyMMdd").print(new DateTime) + "/" + System.currentTimeMillis + scala.util.Random.nextInt(6) + ".jpg"
            oss ! OSSIS(isa, keyA, byteA.length)

            Logger.info("用户更新头像:" + user_id + " ---> " + keyA)
            userDetail = UserDetail(Some(user_id.toLong), data.nickname, data.phoneNum, data.birthday, data.activeYn, data.realYN, data.gender, Some(keyA), data.status)
          }
          if (UserInfoModel.updateUserDetail(userDetail) >= 0) {
            Logger.info("user update personal info: " + userDetail.userId.get)
            Ok(Json.obj("message" -> Message(ChessPiece.SUCCESS.string, ChessPiece.SUCCESS.pointValue)))
          }
          else Ok(Json.obj("message" -> Message(ChessPiece.DATABASE_EXCEPTION.string, ChessPiece.DATABASE_EXCEPTION.pointValue)))
        })
  }
}
