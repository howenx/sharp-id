package utils;

import play.Logger;

import java.util.HashMap;

/**
 * Created by china_005 on 15/10/26.
 */
public class Systemcontents {

    public static final Integer PHONENUM_LENGTH=11;

    public static final HashMap<String,String> map = new HashMap<String,String>();


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
        map.put("1001", "邮件格式错误");
        map.put("1002", "生日格式错误");
        map.put("1003", "昵称不能超过十个汉字");
        map.put("1004", "性别格式错误");
        map.put("1005", "用户信息读取错误");
        map.put("1006", "邮件已存在");
        map.put("1007", "昵称已存在");
        map.put("1008", "保存失败");
        map.put("1009", "未知错误");
        Logger.debug(map.toString());
    }

    public String getErrorMsg(String code){
        return map.get(code);
    }
}
