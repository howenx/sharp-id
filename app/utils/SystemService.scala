package utils


import org.apache.commons.lang3.StringUtils


/**
 * Created by china_005 on 15/10/26.
 */
object SystemService {
  def checkPhoneNum (phoneNum:String) : Boolean= {
    val p = ("^1[3-8][0-9]{9}").r
    if(StringUtils.isEmpty(phoneNum)){
       false
    }
    else if(phoneNum.length()!=Systemcontents.PHONENUM_LENGTH){
       false
    }
    else{
      phoneNum match{
        case p =>
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
        case p =>
          true
        case _ =>
          false
      }
    }
  }

  def checkBirthday(birthday:String) : Boolean = {
    val p = "^((\\d{2}(([02468][048])|([13579][26]))[\\-\\/\\s]?((((0?[13578])|(1[02]))[\\-\\/\\s]?" + "((0?[1-9])|([1-2][0-9])|(3[01])))|(((0?[469])|(11))[\\-\\/\\s]?((0?[1-9])|([1-2][0-9])|(30)))|(0?2[\\-\\/\\s]?((0?[1-9])|([1-2][0-9])))))|" + "(\\d{2}(([02468][1235679])|([13579][01345789]))[\\-\\/\\s]?((((0?[13578])|(1[02]))[\\-\\/\\s]?((0?[1-9])|([1-2][0-9])|(3[01])))|(((0?[469])|(11))[\\-\\/\\s]?" + "((0?[1-9])|([1-2][0-9])|(30)))|(0?2[\\-\\/\\s]?((0?[1-9])|(1[0-9])|(2[0-8]))))))(\\s(((0?[0-9])|([1-2][0-3]))\\:([0-5]?[0-9])((\\s)|(\\:([0-5]?[0-9])))))?$".r
    if(StringUtils.isEmpty(birthday)){
      false
    }
    else{
      birthday match{
        case p =>
          true
        case _ =>
          false
      }
    }
  }



}