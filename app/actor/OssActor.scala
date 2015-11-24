package actor

import java.io.{ByteArrayInputStream, File, FileInputStream}
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

case class OSSIS (is:ByteArrayInputStream,key:String,len:Int)

class OssActor @Inject() (oss_client : OSSClientProvider,configuration: Configuration) extends Actor{

  override def receive = {
    case oss:OSS =>
      val is = new FileInputStream(oss.file.file.getPath)
      val objMetadata = new ObjectMetadata()
      objMetadata.setContentLength(oss.file.file.length())
      objMetadata.setContentType(oss.file.file.getName.replaceFirst("^[^.]*", ""))
      val result = oss_client.get.putObject(configuration.getString("oss.bucket").getOrElse(""), oss.key, is, objMetadata)
      Logger.info(result.getETag)

    case ossis:OSSIS =>
      val objMetadata = new ObjectMetadata()
      objMetadata.setContentLength(ossis.len)
      objMetadata.setContentType("jpg")
      val result = oss_client.get.putObject(configuration.getString("oss.bucket").getOrElse(""), ossis.key, ossis.is, objMetadata)
      Logger.info(result.getETag)

  }

}
