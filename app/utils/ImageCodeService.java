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
import java.util.Random;

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
     * @return
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
				if (bos != null) bos.close();
			} catch (IOException ioex) {}

			try {
				if (input != null) input.close();
			} catch (IOException ioex) {}
		}
        
    }
}
