package utils


import controllers.Systemcontents
import org.apache.commons.lang3.StringUtils
import sun.security.util.Password


/**
  *
 * Created by china_005 on 15/10/26.
 */
object SystemService {
  def checkPhoneNum (phoneNum:String) : Boolean= {
    val p = "^1[3-8][0-9]{9}".r
    if(StringUtils.isEmpty(phoneNum)){
       false
    }
    else if(phoneNum.length()!=Systemcontents.PHONENUM_LENGTH){
       false
    }
    else{
      phoneNum match{
        case p() =>
          true
        case _ =>
          false
      }
    }
  }

  def checkEmail(email:String) : Boolean = {
    val p = "\\b[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*\\b".r
    if(StringUtils.isEmpty(email)){
      true
    }
    else{
      email match{
        case p() =>
          true
        case _ =>
          false
      }
    }
  }

  def checkPassword(password:String) : Boolean = {
    val p = "^(?![0-9]+$)(?![a-zA-Z]+$)[a-zA-Z0-9]{6,12}".r
    if(StringUtils.isEmpty(password)){
      true
    }
    else{
      password match{
        case p() =>
          true
        case _ =>
          false
      }
    }
  }



  def checkBirthday(birthday:String) : Boolean = {
    val p = "^((\\d{2}(([02468][048])|([13579][26]))[\\-\\/\\s]?((((0?[13578])|(1[02]))[\\-\\/\\s]?((0?[1-9])|([1-2][0-9])|(3[01])))|(((0?[469])|(11))[\\-\\/\\s]?((0?[1-9])|([1-2][0-9])|(30)))|(0?2[\\-\\/\\s]?((0?[1-9])|([1-2][0-9])))))|(\\d{2}(([02468][1235679])|([13579][01345789]))[\\-\\/\\s]?((((0?[13578])|(1[02]))[\\-\\/\\s]?((0?[1-9])|([1-2][0-9])|(3[01])))|(((0?[469])|(11))[\\-\\/\\s]?((0?[1-9])|([1-2][0-9])|(30)))|(0?2[\\-\\/\\s]?((0?[1-9])|(1[0-9])|(2[0-8]))))))(\\s(((0?[0-9])|([1-2][0-3]))\\:([0-5]?[0-9])((\\s)|(\\:([0-5]?[0-9])))))?$".r
    if(StringUtils.isEmpty(birthday)){
      false
    }
    else{
      birthday match{
        case p() =>
          true
        case _ =>
          false
      }
    }
  }

  def  getEmailHtml (link : String):String = {
  "<html>" + "<head>" + "<meta charset=\"utf-8\"><title>5游网--WWW.5DSY.CN</title>" + "<meta name=\"keywords\" content=\"手游,网游,游戏\">" + "<style>" + "body{margin: 0;paddding:0;}" + ".mail{width: 600px;border: 1px solid #000;margin: 15px auto;padding: 0 0 30px;}" + ".header{width: 100%; padding: 15px 0;background: #393939;}" + ".header a{margin-left: 5%; display: block;}" + "p{margin: 0; color: #656566; padding: 0 5%;}" + ".p1{margin: 50px 0 10px;}.p2{margin: 30px 0 10px;}" + ".ft{margin-top: 100px;color: #656566;padding: 0 5%;}" + "</style>" + "</head>" + "<body>" + "<div class=\"mail\">" + "<div class=\"header\">" + "<a href=\"" + Systemcontents.INDEX_PAGE + "\"><img src=\"http://wodeshouyou.oss-cn-hangzhou.aliyuncs.com/img/small-logo.png\"></a>" + "</div>" + "<p class=\"p1\">尊敬的用户，您好!</p>" + "<p class=\"p2\">5游网已经收到了您激活邮箱的请求，请点击" + "<a href=\"" + link + "\">此链接完成激活</a></p>" + "<p class=\"p3\">（本链接30分钟内有效)</p>" + "<div class=\"ft\">公司网址：" + "<a href=\"http://www.5dsy.cn/\">WWW.5DSY.CN</a>" + "</div>" + "</div>" + "</body>" + "</html>"
}



}