package actor;

import akka.actor.AbstractActor;
import akka.japi.pf.ReceiveBuilder;
import com.squareup.okhttp.OkHttpClient;
import com.squareup.okhttp.Request;
import com.squareup.okhttp.Response;
import domain.VersionVo;
import play.Configuration;
import play.Logger;

import javax.inject.Inject;
import java.io.*;
import java.net.URLEncoder;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * 用于处理压缩包下载和运行脚本
 * Created by howen on 16/04/18.
 */
public class IdRunActor extends AbstractActor {

    private final OkHttpClient client = new OkHttpClient();


    @Inject
    public IdRunActor(Configuration configuration) {
        receive(ReceiveBuilder.match(Object.class, message -> {

            if (message instanceof VersionVo) {
                final String fileName = ((VersionVo) message).getFileName();
                final String projectName = "style-" + ((VersionVo) message).getProductType();

                Request request = new Request.Builder()
                        .url(configuration.getString("zip.download.url") + URLEncoder.encode(((VersionVo) message).getDownloadLink(), "UTF-8"))
                        .build();
                client.setReadTimeout(3, TimeUnit.MINUTES);
                client.setConnectTimeout(3, TimeUnit.MINUTES);
                Response response = client.newCall(request).execute();
                if (!response.isSuccessful()) throw new IOException("Unexpected code " + response);
                else {
                    InputStream inputStream = null;
                    OutputStream outputStream = null;
                    try {
                        inputStream = response.body().byteStream();
                        String zipPath = configuration.getString("id.zip.path");

                        final File file = new File(zipPath);

                        Logger.error("id.zip.path文件为: " + file.getPath());

                        if (!file.exists()) {
                            if (!file.mkdirs()) Logger.error("创建文件目录出错");
                        }

                        outputStream = new FileOutputStream(new File(zipPath + fileName));

                        int read = 0;
                        byte[] buffer = new byte[1024];

                        while ((read = inputStream.read(buffer)) != -1) {
                            outputStream.write(buffer, 0, read);
                        }

                        callShell(zipPath, fileName, projectName);

                    } catch (IOException e) {
                        Logger.error(e.getMessage());
                        e.printStackTrace();
                    } finally {
                        if (inputStream != null) {
                            inputStream.close();
                        }
                        if (outputStream != null) {
                            outputStream.close();
                        }
                    }
                }
            }

        }).matchAny(s -> {
            Logger.error("AdminRunActor received messages not matched: {}", s.toString());
            unhandled(s);
        }).build());
    }

    private void callShell(String dist, String fileName, String projectName) {

        List<String> commands = Arrays.asList("bash", "-c", "ls -al ; unzip " + fileName);
        String projectDir = dist + fileName.replaceAll(".zip", "");
        Logger.error("项目目录-->" + projectDir);
        List<String> commands2 = Arrays.asList("bash", "-c", "sh run.sh " + projectDir + " " + projectName);

        String output = null;
        String output2 = null;
        try {
            output = exec(dist, null, commands);
            output2 = exec(dist, null, commands2);

        } catch (IOException | InterruptedException e) {
            e.printStackTrace();
            Logger.error(e.getMessage());
        }
        Logger.error("压缩---->\n" + output);
        Logger.error("执行脚本---->\n" + output2);
    }

    private String exec(String dist, String command, List<String> commands) throws IOException, InterruptedException {
        StringBuilder output = new StringBuilder();
        ProcessBuilder pb;
        if (command != null) {
            pb = new ProcessBuilder(command);

        } else {
            pb = new ProcessBuilder(commands);
        }

        pb.directory(new File(dist));
        Process p = pb.start();
        p.waitFor();
        BufferedReader reader =
                new BufferedReader(new InputStreamReader(p.getInputStream()));
        try {

            String line;
            while ((line = reader.readLine()) != null) {
                output.append(line).append("\n");
            }
            Logger.error("failed to execute:" + p.exitValue());

            return output.toString();
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        } finally {
            reader.close();
        }
    }

}
