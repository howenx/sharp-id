/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package utils;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.*;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.Hashtable;
import java.util.Random;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 *
 * @author lea
 */
public class ImageCodeService {
	
	private static volatile ImageCodeService imageCodeService = null;
	public static ImageCodeService getInstance() {
		ImageCodeService result = imageCodeService;
        if (result == null) {
            synchronized (ImageCodeService.class) {
                result = imageCodeService;
                if(result == null) {
                	imageCodeService = result =  new ImageCodeService();
                }
            }
        }
        return result;
    }

  private int width = 120;
    private int height = 50;
    private Random random = new Random();
    private BufferedImage image;
    /**
     * 验证码图片上显示的字符
     */
    private String code;
    /**
     * 波形的幅度倍数，越大扭曲的程序越高，一般为3
     */
    private int twistLevel = 3;
    /**
     * 干扰线数量
     */
    private int noiseLineNumber = 5;
    /**
     * 背景色
     */
    private Color backgroundColor = Color.WHITE;
    /**
     * 字体颜色
     */
    // private Color foregroundColor = Color.BLACK;  
    private Color[] colors = {Color.BLUE, Color.RED, Color.GREEN, Color.BLACK, Color.CYAN, Color.MAGENTA};

    private Color getRandomColor() {
        return colors[random.nextInt(colors.length)];
    }
    
  //使用到Algerian字体，系统里没有的话需要安装字体，字体只显示大写，去掉了1,0,i,o几个容易混淆的字符  
    public static final String VERIFY_CODES = "23456789ABCDEFGHJKLMNPQRSTUVWXYZ";  
  
    /** 
     * 使用系统默认字符源生成验证码 
     * @param verifySize    验证码长度 
     * @return 
     */  
    public String generateVerifyCode(int verifySize){  
        return generateVerifyCode(verifySize, VERIFY_CODES);  
    }  
    /** 
     * 使用指定源生成验证码 
     * @param verifySize    验证码长度 
     * @param sources   验证码字符源 
     * @return 
     */  
    public static String generateVerifyCode(int verifySize, String sources){  
        if(sources == null || sources.length() == 0){  
            sources = VERIFY_CODES;  
        }  
        int codesLen = sources.length();  
        Random rand = new Random(System.currentTimeMillis());  
        StringBuilder verifyCode = new StringBuilder(verifySize);  
        for(int i = 0; i < verifySize; i++){  
            verifyCode.append(sources.charAt(rand.nextInt(codesLen-1)));  
        }  
        return verifyCode.toString();  
    }

    /**
     * @param width - 验证码图片宽度
     * @param height - 验证码图片高度
     * @param randomStr - 随机字符串
     * @return BufferedImage
     */
    public BufferedImage generate(int width, int height, String randomStr) {
		try {
			this.width = width;
	        this.height = height;
	        this.code = randomStr;
	        if (code == null || code.isEmpty()) {
	            throw new RuntimeException("randomStr can not be empty.");
	        }
	        int xWidth = width / (code.length() + 2);
	        int yIndex = height - 4;
	        Graphics2D graphics = graphicsInit();
	        for (int i = 0; i < code.length(); i++) {
	            graphics.setColor(getRandomColor());
	            graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
	            graphics.drawString(code.charAt(i) + "", (i + 1) * xWidth, yIndex);
	        }
	        setBuffImg(disturb());
	        return image;
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
        
    }

    private Graphics2D graphicsInit() {
        Graphics2D graphics = buffImgInit().createGraphics();
        graphics.setColor(backgroundColor);
        graphics.fillRect(0, 0, width, height);
        graphics.setFont(new Font("Fixedsys", Font.ITALIC, height - 2));
        graphics.drawRect(0, 0, width - 1, height - 1);
        return graphics;
    }

    private BufferedImage buffImgInit() {
        image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
        return image;
    }

    private BufferedImage disturb() {
        drawNoiseLine(image.createGraphics());
        return twistImage();
    }

    private void drawNoiseLine(Graphics2D graphics) {
        int x = 0;
        int y = 0;
        int xl = 0;
        int yl = 0;
        for (int i = 0; i < noiseLineNumber; i++) {
            x = random.nextInt(width * 2 / 3);
            y = random.nextInt(height * 2 / 3);
            xl = random.nextInt(width / 2);
            yl = random.nextInt(height / 2);
            // graphics.setColor(foregroundColor);  
            graphics.setColor(getRandomColor());
            graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            graphics.drawLine(x, y, x + xl, y + yl);
        }
    }

    private BufferedImage twistImage() {
        double dMultValue = random.nextInt(9) + twistLevel;
        double dPhase = random.nextInt(6);// 波形的起始相位，取值区间（0-2＊PI）  
        BufferedImage destBi = new BufferedImage(image.getWidth(), image.getHeight(), BufferedImage.TYPE_INT_RGB);
        Graphics2D graphics = destBi.createGraphics();
        graphics.setColor(backgroundColor);
        graphics.fillRect(0, 0, width, height);
        for (int i = 0; i < destBi.getWidth(); i++) {
            for (int j = 0; j < destBi.getHeight(); j++) {
                int nOldX = getXPosition4Twist(dPhase, dMultValue, destBi.getHeight(), i, j);
                int nOldY = j;
                if (nOldX >= 0 && nOldX < destBi.getWidth() && nOldY >= 0 && nOldY < destBi.getHeight()) {
                    destBi.setRGB(nOldX, nOldY, image.getRGB(i, j));
                }
            }
        }
        return destBi;
    }

    private int getXPosition4Twist(double dPhase, double dMultValue, int height, int xPosition, int yPosition) {
        double PI = Math.PI; // 此值越大，扭曲程度越大  
        double dx = (double) (PI * yPosition) / height + dPhase;
        double dy = Math.sin(dx);
        return xPosition + (int) (dy * dMultValue);
    }

    public BufferedImage getImage() {
        return image;
    }

    public void setBuffImg(BufferedImage buffImg) {
        this.image = buffImg;
    }

    public int getWidth() {
        return width;
    }

    public void setWidth(int width) {
        this.width = width;
    }

    public int getHeight() {
        return height;
    }

    public void setHeight(int height) {
        this.height = height;
    }

    public InputStream getImageIo(String code) {
        BufferedImage bi = new ImageCodeService().generate(120, 40, code);
        OutputStream bos = new ByteArrayOutputStream();
        InputStream  input = null;
        try {
			ImageIO.write(bi, "png", bos);
			input = new ByteArrayInputStream(((ByteArrayOutputStream) bos).toByteArray());
			return input;
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}finally {
			try {
                bos.close();
			} catch (IOException ignored) {}

			try {
				if (input != null) input.close();
			} catch (IOException ignored) {}
		}
        
    }

    public static String IDCardValidate(String IDStr) {
        String errorInfo = "";// 记录错误信息
        String[] ValCodeArr = { "1", "0", "x", "9", "8", "7", "6", "5", "4",
                "3", "2" };
        String[] Wi = { "7", "9", "10", "5", "8", "4", "2", "1", "6", "3", "7",
                "9", "10", "5", "8", "4", "2" };
        String Ai = "";
        // ================ 号码的长度 15位或18位 ================
        if (IDStr.length() != 15 && IDStr.length() != 18) {
            errorInfo = "身份证号码长度应该为15位或18位。";
            return errorInfo;
        }
        // =======================(end)========================

        // ================ 数字 除最后以为都为数字 ================
        if (IDStr.length() == 18) {
            Ai = IDStr.substring(0, 17);
        } else if (IDStr.length() == 15) {
            Ai = IDStr.substring(0, 6) + "19" + IDStr.substring(6, 15);
        }
        if (!isNumeric(Ai)) {
            errorInfo = "身份证15位号码都应为数字 ; v";
            return errorInfo;
        }
        // =======================(end)========================

        // ================ 出生年月是否有效 ================
        String strYear = Ai.substring(6, 10);// 年份
        String strMonth = Ai.substring(10, 12);// 月份
        String strDay = Ai.substring(12, 14);// 天
        if (!isDate(strYear + "-" + strMonth + "-" + strDay)) {
            errorInfo = "身份证生日无效。";
            return errorInfo;
        }
        GregorianCalendar gc = new GregorianCalendar();
        SimpleDateFormat s = new SimpleDateFormat("yyyy-MM-dd");
        try {
            if ((gc.get(Calendar.YEAR) - Integer.parseInt(strYear)) > 150
                    || (gc.getTime().getTime() - s.parse(
                    strYear + "-" + strMonth + "-" + strDay).getTime()) < 0) {
                errorInfo = "身份证生日不在有效范围。";
                return errorInfo;
            }
        } catch (NumberFormatException | java.text.ParseException e) {
            e.printStackTrace();
        }
        if (Integer.parseInt(strMonth) > 12 || Integer.parseInt(strMonth) == 0) {
            errorInfo = "身份证月份无效";
            return errorInfo;
        }
        if (Integer.parseInt(strDay) > 31 || Integer.parseInt(strDay) == 0) {
            errorInfo = "身份证日期无效";
            return errorInfo;
        }
        // =====================(end)=====================

        // ================ 地区码时候有效 ================
        Hashtable h = GetAreaCode();
        if (h.get(Ai.substring(0, 2)) == null) {
            errorInfo = "身份证地区编码错误。";
            return errorInfo;
        }
        // ==============================================

        // ================ 判断最后一位的值 ================
        int TotalmulAiWi = 0;
        for (int i = 0; i < 17; i++) {
            TotalmulAiWi = TotalmulAiWi
                    + Integer.parseInt(String.valueOf(Ai.charAt(i)))
                    * Integer.parseInt(Wi[i]);
        }
        int modValue = TotalmulAiWi % 11;
        String strVerifyCode = ValCodeArr[modValue];
        Ai = Ai + strVerifyCode;

        if (IDStr.length() == 18) {
            if (!Ai.equals(IDStr)) {
                errorInfo = "身份证无效，不是合法的身份证号码";
                return errorInfo;
            }
        } else {
            return errorInfo;
        }
        // =====================(end)=====================
        return errorInfo;
    }

    private static boolean isNumeric(String str) {
        Pattern pattern = Pattern.compile("[0-9]*");
        Matcher isNum = pattern.matcher(str);
        if (isNum.matches()) {
            return true;
        } else {
            return false;
        }
    }

    public static boolean isDate(String strDate) {
        Pattern pattern = Pattern
                .compile("^((\\d{2}(([02468][048])|([13579][26]))[\\-\\/\\s]?((((0?[13578])|(1[02]))[\\-\\/\\s]?((0?[1-9])|([1-2][0-9])|(3[01])))|(((0?[469])|(11))[\\-\\/\\s]?((0?[1-9])|([1-2][0-9])|(30)))|(0?2[\\-\\/\\s]?((0?[1-9])|([1-2][0-9])))))|(\\d{2}(([02468][1235679])|([13579][01345789]))[\\-\\/\\s]?((((0?[13578])|(1[02]))[\\-\\/\\s]?((0?[1-9])|([1-2][0-9])|(3[01])))|(((0?[469])|(11))[\\-\\/\\s]?((0?[1-9])|([1-2][0-9])|(30)))|(0?2[\\-\\/\\s]?((0?[1-9])|(1[0-9])|(2[0-8]))))))(\\s(((0?[0-9])|([1-2][0-3]))\\:([0-5]?[0-9])((\\s)|(\\:([0-5]?[0-9])))))?$");
        Matcher m = pattern.matcher(strDate);
        if (m.matches()) {
            return true;
        } else {
            return false;
        }
    }

    private static Hashtable GetAreaCode() {
        Hashtable<String, String> hashtable = new Hashtable<String, String>();
        hashtable.put("11", "北京");
        hashtable.put("12", "天津");
        hashtable.put("13", "河北");
        hashtable.put("14", "山西");
        hashtable.put("15", "内蒙古");
        hashtable.put("21", "辽宁");
        hashtable.put("22", "吉林");
        hashtable.put("23", "黑龙江");
        hashtable.put("31", "上海");
        hashtable.put("32", "江苏");
        hashtable.put("33", "浙江");
        hashtable.put("34", "安徽");
        hashtable.put("35", "福建");
        hashtable.put("36", "江西");
        hashtable.put("37", "山东");
        hashtable.put("41", "河南");
        hashtable.put("42", "湖北");
        hashtable.put("43", "湖南");
        hashtable.put("44", "广东");
        hashtable.put("45", "广西");
        hashtable.put("46", "海南");
        hashtable.put("50", "重庆");
        hashtable.put("51", "四川");
        hashtable.put("52", "贵州");
        hashtable.put("53", "云南");
        hashtable.put("54", "西藏");
        hashtable.put("61", "陕西");
        hashtable.put("62", "甘肃");
        hashtable.put("63", "青海");
        hashtable.put("64", "宁夏");
        hashtable.put("65", "新疆");
        hashtable.put("71", "台湾");
        hashtable.put("81", "香港");
        hashtable.put("82", "澳门");
        hashtable.put("91", "国外");
        return hashtable;
    }
}
