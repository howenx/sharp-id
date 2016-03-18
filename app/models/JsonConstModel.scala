package models

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.Writes._
import play.api.libs.json._
import utils.IdVerifyUtil

/**
  * 用于JSON表单提交的校验
  * Created by howen on 16/3/17.
  */

case class UserJsResult(id: Long, name: String, photo: String)

object JsonConstModel {

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

  implicit lazy val userDetailReads: Reads[UserDetail] = (
    (__ \ "userId").readNullable[Long] and
      (__ \ "nickname").readNullable[String](minLength[String](2) keepAnd maxLength[String](15) keepAnd pattern("""[a-zA-Z0-9\u4e00-\u9fa5]+""".r, "name error")) and
      (__ \ "phoneNum").readNullable[String](minLength[String](11) keepAnd maxLength[String](11) keepAnd pattern("""[1][345678]\\d{9}""".r, "phoneNum error")) and
      (__ \ "birthday").readNullable[String] and
      (__ \ "activeYn").readNullable[String] and
      (__ \ "realYN").readNullable[String] and
      (__ \ "gender").readNullable[String](minLength[String](1) keepAnd maxLength[String](1) keepAnd pattern("""[MF]""".r, "gender error")) and
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
}
