package modules

import javax.inject.{Provider, Inject, Singleton}

import com.aliyun.oss.OSSClient
import play.api.{Logger, Configuration}
import play.api.inject.ApplicationLifecycle

import scala.concurrent.Future

/**
 * Created by handy on 15/10/23.
 * Daumkakao china
 */
@Singleton
class OSSClientProvider @Inject()(configuration: Configuration, lifecycle:ApplicationLifecycle) extends Provider[OSSClient]{

  lazy val get : OSSClient = {
    val client = {
      configuration.getString("oss.endpoint").map { endpoint =>
        configuration.getString("oss.access_key").map { access_key =>
          configuration.getString("oss.access_secret").map { access_secret =>
            new OSSClient(endpoint, access_key, access_secret)
          }.getOrElse {
            throw new RuntimeException("config oss error")
          }

        }.getOrElse {
          throw new RuntimeException("config oss error")
        }
      }.getOrElse {
        throw new RuntimeException("config oss error")
      }

    }
    Logger.info("start oss ....")

    lifecycle.addStopHook(() => Future.successful {
      Logger.info("stop oss ...")
    })
    client
  }

}
