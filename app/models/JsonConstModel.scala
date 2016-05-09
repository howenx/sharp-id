package models

import util.IdVerifyUtil
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.Writes._
import play.api.libs.json.{util, _}


/**
  * 用于JSON表单提交的校验
  * Created by howen on 16/3/17.
  */

case class UserJsResult(id: Long, name: String, photo: String)

case class Address(addId: Option[Long], tel: Option[String], name: Option[String], deliveryCity: Option[String], deliveryDetail: Option[String], userId: Option[Long], orDefault: Option[Int], idCardNum: Option[String], orDestroy: Option[Boolean], cityCode: Option[String])


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
                     orReal: Option[String], //是否实名认证
                     lastloginDt: Option[String], //最后登录时间
                     lastloginIp: Option[String], //最后登录IP
                     status: Option[String], //状态
                     idType: Option[String], //用户类型
                     openId: Option[String], //其它平台唯一识别用户ID
                     idArea: Option[String], //用户所在区域
                     loginTimes: Option[Long], //登录次数
                     email: Option[String], //邮箱
                     alterDt: Option[String], //更新时间
                     orActive: Option[String] //激活时间
                   )

case class UserUpdateJson(
                       userId: Option[Long], //用户ID
                       nickname: Option[String], //用户昵称
                       passwd: Option[String], //密码
                       phoneNum: Option[String], //手机号
                       gender: Option[String], //性别
                       birthday: Option[String], //生日
                       photoUrl: Option[String], //头像
                       lastloginDt: Option[String], //最后登录时间
                       lastloginIp: Option[String], //最后登录IP
                       status: Option[String], //状态
                       idType: Option[String], //用户类型
                       openId: Option[String], //其它平台唯一识别用户ID
                       idArea: Option[String] //用户所在区域
                     )

object JsonConstModel {

  /**
    * 身份证校验
    *
    * @param error error
    * @param reads reads
    * @return
    */
  def id(error: String = "error.id")(implicit reads: Reads[String]) =
    Reads[String](js => reads.reads(js).flatMap { o =>
      if (IdVerifyUtil.IDCardValidate(o).equals("")) JsSuccess(o) else JsError(error)
    })


  implicit lazy val userJsResultReads: Reads[UserJsResult] = (
    (__ \ "id").read[Long] and
      (__ \ "name").read[String] and
      (__ \ "photo").read[String]
    ) (UserJsResult)

  implicit lazy val userJsResultWrites: Writes[UserJsResult] = (
    (__ \ "id").write[Long] and
      (__ \ "name").write[String] and
      (__ \ "photo").write[String]
    ) (unlift(UserJsResult.unapply))


  implicit lazy val addressReads: Reads[Address] = (
    (__ \ "addId").readNullable[Long] and
      (__ \ "tel").readNullable[String](minLength[String](11) keepAnd maxLength[String](11) keepAnd pattern("""[1][345678]\d{9}""".r, "tel phone error")) and
      (__ \ "name").readNullable[String](minLength[String](2) keepAnd maxLength[String](15) keepAnd pattern("""[a-zA-Z0-9\u4e00-\u9fa5]+""".r, "name error")) and
      (__ \ "deliveryCity").readNullable[String] and
      (__ \ "deliveryDetail").readNullable[String](minLength[String](5) keepAnd maxLength[String](50) keepAnd pattern("""[a-zA-Z0-9\u4e00-\u9fa5]+""".r, "deliveryDetail error")) and
      (__ \ "userId").readNullable[Long] and
      (__ \ "orDefault").readNullable[Int] and
      (__ \ "idCardNum").readNullable[String](id()) and
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
      (__ \ "orDefault").writeNullable[Int] and
      (__ \ "idCardNum").writeNullable[String] and
      (__ \ "orDestroy").writeNullable[Boolean] and
      (__ \ "cityCode").writeNullable[String]
    ) (unlift(Address.unapply))


  implicit lazy val userUpdateJsonReads: Reads[UserUpdateJson] = (
    (__ \ 'userId).readNullable[Long] and
      (__ \ 'nickname).readNullable[String](minLength[String](2) keepAnd maxLength[String](15) keepAnd pattern("""[a-zA-Z0-9\u4e00-\u9fa5]+""".r, "nickname error")) and
      (__ \ "passwd").readNullable[String](minLength[String](6) keepAnd maxLength[String](12) keepAnd pattern("""^(?![0-9]+$)(?![a-zA-Z]+$)[a-zA-Z0-9]{6,12}""".r, "passwd error")) and
      (__ \ "phoneNum").readNullable[String](minLength[String](11) keepAnd maxLength[String](11) keepAnd pattern("""[1][345678]\\d{9}""".r, "phoneNum error")) and
      (__ \ "gender").readNullable[String](minLength[String](1) keepAnd maxLength[String](1) keepAnd pattern("""[MF]""".r, "gender error")) and
      (__ \ "birthday").readNullable[String] and
      (__ \ "photoUrl").readNullable[String] and
      (__ \ "lastloginDt").readNullable[String] and
      (__ \ "lastloginIp").readNullable[String] and
      (__ \ "status").readNullable[String] and
      (__ \ "idType").readNullable[String] and
      (__ \ "openId").readNullable[String] and
      (__ \ "idArea").readNullable[String]
    ) (UserUpdateJson)

  implicit lazy val userUpdateJsonWrites: Writes[UserUpdateJson] = (
    (__ \ "userId").writeNullable[Long] and
      (__ \ "nickname").writeNullable[String] and
      (__ \ "passwd").writeNullable[String] and
      (__ \ "phoneNum").writeNullable[String] and
      (__ \ "gender").writeNullable[String] and
      (__ \ "birthday").writeNullable[String] and
      (__ \ "photoUrl").writeNullable[String] and
      (__ \ "lastloginDt").writeNullable[String] and
      (__ \ "lastloginIp").writeNullable[String] and
      (__ \ "status").writeNullable[String] and
      (__ \ "idType").writeNullable[String] and
      (__ \ "openId").writeNullable[String] and
      (__ \ "idArea").writeNullable[String]
    ) (unlift(UserUpdateJson.unapply))


}
