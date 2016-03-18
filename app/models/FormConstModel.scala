package models

import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import play.api.libs.Codecs
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.ImageCodeUtil

/**
  * 所有表单的校验和所有的Form对象
  * Created by howen on 16/3/16.
  */


case class UserPhoneLoginForm(phone: String, password: String, code: String)

case class ApiSendCodeForm(phone: String, msg: String)

case class ApiRegForm(phone: String, password: String, code: String)

case class VerifyPhoneForm(phone: String, code: String)

case class RefreshForm(token: String)

case class RealNameForm(realName: Option[String], cardNum: Option[String], cardImgA: Option[String], cardImgB: Option[String])

case class UserResultVo(id:Long,token:String,expired:Int)


case class Message(var message: String, var code: Int)

object FormConstModel {
  /**
    * 用户登录表单
    */
  val user_phone_login_form = Form(mapping(
    "phone" -> nonEmptyText.verifying(minLength(11), maxLength(11)).verifying("Bad phone regex", {
      _.matches("""[1][345678]\d{9}""")
    }),
    "password" -> nonEmptyText.verifying(minLength(6), maxLength(12)).verifying("Bad password regex", {
      _.matches("""^(?![0-9]+$)(?![a-zA-Z]+$)[a-zA-Z0-9]{6,12}""")
    }),
	"code" -> nonEmptyText
  )(UserPhoneLoginForm.apply)(UserPhoneLoginForm.unapply))

  /**
    * 校验用户手机是否注册表单
    */
  val verify_phone_form = Form(mapping(
    "phone" -> nonEmptyText.verifying(minLength(11), maxLength(11)).verifying("Bad phone regex", {
      _.matches("""[1][345678]\d{9}""")
    }),
    "code" -> nonEmptyText
  )(VerifyPhoneForm.apply)(VerifyPhoneForm.unapply))

  /**
    * 刷新token表单
    */
  val refresh_form = Form(mapping(
    "token" -> nonEmptyText
  )(RefreshForm.apply)(RefreshForm.unapply))

  /**
    * 发送短信验证码表单
    * msg为MD5加密信息
    */
  val send_code_form = Form(mapping(
    "phone" -> nonEmptyText.verifying(minLength(11), maxLength(11)).verifying("Bad phone regex", {
      _.matches("""[1][345678]\d{9}""")
    }),
    "msg" -> nonEmptyText
  )(ApiSendCodeForm.apply)(ApiSendCodeForm.unapply))

  /**
    * 用户注册,重置密码form
    * code为短信验证码
    */
  val api_reg_form = Form(mapping(
    "phone" -> nonEmptyText.verifying(minLength(11), maxLength(11)).verifying("Bad phone regex", {
      _.matches("""[1][345678]\d{9}""")
    }),
    "password" -> nonEmptyText.verifying(minLength(6), maxLength(12)).verifying("Bad password regex", {
      _.matches("""^(?![0-9]+$)(?![a-zA-Z]+$)[a-zA-Z0-9]{6,12}""")
    }),
    "code" -> nonEmptyText
  )(ApiRegForm.apply)(ApiRegForm.unapply))


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

  implicit lazy val userReads: Reads[User] = (
    (__ \ "id").read[Long] and
      (__ \ "nickname").read[String] and
      (__ \ "gender").read[String] and
      (__ \ "photo_url").read[String]
    ) (User)

  implicit lazy val userWrites: Writes[User] = (
    (__ \ "id").write[Long] and
      (__ \ "nickname").write[String] and
      (__ \ "gender").write[String] and
      (__ \ "photo_url").write[String]
    ) (unlift(User.unapply))

  implicit lazy val uerResultVoReads: Reads[UserResultVo] = (
    (__ \ "id").read[Long] and
      (__ \ "token").read[String] and
      (__ \ "expired").read[Int]
    ) (UserResultVo)

  implicit lazy val userResultVoWrites: Writes[UserResultVo] = (
    (__ \ "id").write[Long] and
      (__ \ "token").write[String] and
      (__ \ "expired").write[Int]
    ) (unlift(UserResultVo.unapply))
}

object ChessPiece extends Enumeration {

  val SUCCESS = ChessPieceVal("成功", 200)
  val FAILURE = ChessPieceVal("失败", 400)

  val FAILURE_REQUEST_ERROR = ChessPieceVal("失败", 441)
  val FAILURE_REQUEST_HANDLER_NOT_FOUND = ChessPieceVal("失败", 442)
  val FAILURE_BAD_REQUEST = ChessPieceVal("失败", 443)

  val ERROR = ChessPieceVal("内部发生错误", 1001)
  val SERVER_EXCEPTION = ChessPieceVal("服务器异常", 1002)
  val BAD_PARAMETER = ChessPieceVal("参数不合法", 1003)
  val BAD_USER_TOKEN = ChessPieceVal("用户不存在", 1004)
  val DATABASE_EXCEPTION = ChessPieceVal("数据库操作异常", 1005)
  val CREATE_ORDER_EXCEPTION = ChessPieceVal("创建订单异常", 1006)
  val REFUND_SUCCESS = ChessPieceVal("退款成功", 1007)
  val REFUND_FAILED = ChessPieceVal("退款失败", 1008)
  val ORDER_CANCEL_AUTO = ChessPieceVal("未支付订单超过24小时,已被自动取消", 1009)
  val ORDER_DEL = ChessPieceVal("交易未完成不能删除订单", 1010)
  val SLIDER_NULL_EXCEPTION = ChessPieceVal("获取滚动条数据空", 1011)
  val THEME_NULL_EXCEPTION = ChessPieceVal("获取主题数据空", 1012)
  val THEME_LIST_NULL_EXCEPTION = ChessPieceVal("获取主题列表数据空", 1013)
  val SKU_DETAIL_NULL_EXCEPTION = ChessPieceVal("获取产品详情数据空", 1014)
  val CART_LIST_NULL_EXCEPTION = ChessPieceVal("获取购物车数据空", 1015)
  val NOT_FOUND_SHIP_FEE_INFO = ChessPieceVal("未找到邮费信息", 1016)

  val SKU_AMOUNT_SHORTAGE = ChessPieceVal("亲,此件商品库存不足了", 2001)
  val SKU_INVALID = ChessPieceVal("亲,您已经长时间未操作,此商品已经失效,建议您刷新购物车", 2002)

  val PURCHASE_QUANTITY_LIMIT = ChessPieceVal("亲,您购买数量超过我们的限制了", 3001)
  val PURCHASE_QUANTITY_SUM_PRICE = ChessPieceVal("海关规定单次报关物品价值不能超过1000元", 3002)

  val PASSWORD_ERROR_TOO_MANY = ChessPieceVal("密码错误次数过多", 4001)

  val USERNAME_OR_PASSWORD_ERROR = ChessPieceVal("用户名或密码有误", 4002)

  val NOT_REGISTERED = ChessPieceVal("用户未注册", 4003)

  val INPUT_VERIFY_FAILED = ChessPieceVal("输入信息有误", 4004)

  val PASSWORD_ERROR_LOCKED = ChessPieceVal("密码输错次数过多账户将被锁1小时,请1小时后再来登录", 4005)

  val PASSWORD_ERROR_LOCKED_NOTIFY = ChessPieceVal("账户已被锁,请稍后再来登录", 4006)

  val IMAGE_CODE_ERROR = ChessPieceVal("验证码校验失败", 4007)

  val IMAGE_CODE_NULL = ChessPieceVal("验证码不能为空", 4008)

  val USER_EXISTS = ChessPieceVal("此手机已经注册", 5001)

  val SMS_CODE_ERROR = ChessPieceVal("短信验证码错误", 5002)

  val PASSWORD_VERIFY_ERROR = ChessPieceVal("密码不符合规则", 5003)

  val SECURITY_ERROR = ChessPieceVal("安全校验不通过", 5004)

  val SEND_SMS_TOO_MANY = ChessPieceVal("发送验证码次数过多,请明天再试", 5005)


  protected case class ChessPieceVal(string: String, pointValue: Int) extends super.Val()

  def convert(value: Value) = value.asInstanceOf[ChessPieceVal]
}
