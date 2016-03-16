package modules

import controllers.FormConstraint
import play.api.{Configuration, Environment}
import play.api.inject.{Binding, Module}

/**
 * Created by handy on 15/10/23.
 * kakao china
 */
class OSSModule extends Module{
  override def bindings(environment: Environment, configuration: Configuration) : Seq[Binding[_]] = {
    Seq (
      bind[OSSClientProvider].toSelf.eagerly(),
      bind[FormConstraint].toSelf
    )
  }
}
