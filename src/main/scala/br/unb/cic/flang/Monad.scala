package br.unb.cic.flang

package object ErrorMonad {
  sealed trait M[A]

  case class Raise[A](msg: String) extends M[A]
  case class Return[A](a: A) extends M[A]

  def pure[A](a: A): M[A] = Return(a)

  def flatMap[A, B](m: M[A])(f: A => M[B]): M[B] = m match {
    case Raise(s)  => Raise(s) /* in this case, we just propagate the error */
    case Return(a) => f(a) /* in this case we just apply f */
  }

  def err[A](s: String): M[A] = Raise(s)

  def assertError[A](v: M[A]): Boolean = v match {
    case Raise(_) => true
    case _        => false
  }
}
