package modules

import play.api.inject.{Binding, Module}
import play.api.{Configuration, Environment}

/**
 * Created by handy on 15/10/23.
 * kakao china
 */
class OSSModule extends Module{
  override def bindings(environment: Environment, configuration: Configuration) : Seq[Binding[_]] = {
    Seq (
      bind[OSSClientProvider].toSelf.eagerly()
    )
  }
}
