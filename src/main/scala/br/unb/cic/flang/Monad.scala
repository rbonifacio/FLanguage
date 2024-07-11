package br.unb.cic.flang

import cats.MonadError
import cats.instances.either._

object MErr {
  type MError[A] = Either[String, A]

  val eh = MonadError[MError, String]

  def assertError[A](m: MError[A]) : Boolean = m match {
    case Left(_) => true
    case Right(_) => false  
  }
}
