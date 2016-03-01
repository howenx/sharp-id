package controllers;
import play.Logger;
import play.Play;
import play.mvc.Controller;

import java.util.HashMap;
/**
 * Systemcontents
 * Created by howen on 16/2/28.
 */
@SuppressWarnings("unchecked")
public class Systemcontents extends Controller {

    public static final String STATIC_URL = Play.application().configuration().getString("staticUrl");

    public static final String INDEX_PAGE = Play.application().configuration().getString("indexPage");

    public static final String BILLING_PAGE = Play.application().configuration().getString("billingPage");

    public static final Integer PHONENUM_LENGTH=11;

    public static final String PHONE_NUM_TYPE_ERROR ="电话号码格式错误";
    public static final String EMAIL_TYPE_ERROR = "邮箱格式错误";
    public static final String PHONE_CODE_ERROR ="验证码错误";
    public static final String IMAGE_CODE_ERROR ="图片验证码错误";
    public static final String USER_PASSWORD_ERROR ="用户名或密码错误";
    public static final String USER_INFO_ERROR ="用户信息错误";
    public static final String CHANGE_SUCCESS ="修改成功";
    public static final String CHANGE_FAILED ="修改失败";
    public static final String PHONE_NUM_EXTISTS ="手机号码已经注册";
    public static final String PHONE_NUM_NO_EXTISTS ="手机号未注册";
    public static final String LOGIN_SUCCESS = "登录成功";
    public static final String REG_SUCCESS = "注册成功";
    public static final String REG_FAILED = "注册失败";
    public static final String SEND_SUCCESS = "发送成功";
    public static final String SEND_EMAIL_SUCCESS = "邮件发送成功";
    public static final String SEND_EMAIL_FAILED = "邮件发送失败";
    public static final String EMAIL_ACTIVE_OREADY = "邮箱已经激活";
    public static final String EMAIL_ACTIVE_FAILED= "邮箱激活失败";
    public static final String EMAIL_ACTIVE_SUCCESS= "邮箱激活成功";

    public static final String API_RESULT_REFRESH_SUCCESS ="刷新成功";
    public static final String API_RESULT_REFRESH_FAILED ="刷新失败";

    public static final String API_RESULT_SUCCESS ="成功";
    public static final String API_RESULT_FAILED ="失败";
    public static final String API_RESULT_SUCCESS_CODE="200";
    public static final String API_RESULT_FAILED_CODE="400";

    public static final String API_RESULT_BOOLEAN ="result";
    public static final String API_RESULT_MESSAGE ="message";
    public static final String API_RESULT_TOKEN ="token";
    public static final String API_RESULT_OVER_TIME ="expired";

    public static final String RESULT_BOOLEAN="if";
    public static final String RESULT_MESSAGE ="back";

    public static final String WEB_TOKEN = "web_token";



    public static final HashMap map = new HashMap();


    private static volatile Systemcontents systemcontents = null;

    public static Systemcontents getInstance() {
        Systemcontents result = systemcontents;
        if (result == null) {
            synchronized (Systemcontents.class) {
                result = systemcontents;
                if(result == null) {
                    systemcontents = result =  new Systemcontents();
                }
            }
        }
        return result;
    }

    public Systemcontents(){
        map.put("1001", "邮箱格式错误");
        map.put("1002", "生日格式错误");
        map.put("1003", "昵称不能超过十个汉字");
        map.put("1004", "性别格式错误");
        map.put("1005", "用户信息读取错误");
        map.put("1006", "邮箱已存在");
        map.put("1007", "昵称已存在");
        map.put("1008", "保存失败");
        map.put("1009", "未知错误");
        Logger.debug(map.toString());
    }

    public String getErrorMsg(String code){
        return (String) map.get(code);
    }
}
