package br.unb.cic.flang

import cats.MonadError
import cats.instances.either._

object MErr {
  type MError[A] = Either[String, A]

  val eh = MonadError[MError, String]
}
