package br.unb.cic.flang

sealed trait M[A] {
  def error() : Boolean = this match {
    case Left(_) => true
    case _       => false     
  }
}

case class Left[A](msg: String) extends M[A]
case class Right[A](a: A) extends M[A]

object M {

  def pure[A](a: A) : M[A] = Right(a)

  def bind[A,B](m: M[A])(f : A => M[B]) : M[B] = m match {
    case Left(s) => Left(s) /* in this case, we just propagate the error */
    case Right(a) => f(a)   /* in this case we just apply f */   
  }

  def err[A](s: String) : M[A] = Left(s)
}
