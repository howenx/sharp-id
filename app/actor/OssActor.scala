package actor

import java.io.{File, FileInputStream}
import javax.inject.Inject

import akka.actor.Actor
import com.aliyun.oss.model.ObjectMetadata
import modules.OSSClientProvider
import play.Logger
import play.api.Configuration
import play.api.libs.Files.TemporaryFile

/**
 * Created by china_005 on 15/11/2.
 */


case class OSS (file:TemporaryFile,key:String)

class OssActor @Inject() (oss_client : OSSClientProvider,configuration: Configuration) extends Actor{

  override def receive = {
    case oss:OSS =>
      var is = new FileInputStream(oss.file.file.getPath)
      var objMetadata = new ObjectMetadata()
      objMetadata.setContentLength(oss.file.file.length())
      objMetadata.setContentType(oss.file.file.getName.replaceFirst("^[^.]*", ""))
      val result = oss_client.get.putObject(configuration.getString("oss.bucket").getOrElse(""), oss.key, is, objMetadata)
      Logger.info(result.getETag)

  }

}
