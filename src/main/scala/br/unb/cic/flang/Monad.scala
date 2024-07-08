package br.unb.cic.flang

package object StateMonad {
  type S = List[(String, Integer)]

  case class M[A](f: S => (A, S))

  def pure[A](a: A): M[A] = M[A] { s => (a, s) }

  def bind[A, B](m: M[A])(f: A => M[B]): M[B] = M({ s: S =>
    {
      val (a, s1) = runState(m)(s)
      val (b, s2) = runState(f(a))(s1)
      (b, s2)
    }
  })

  def runState[A](s: M[A]): (S => (A, S)) = s.f

  def put(s: S): M[Unit] = M({ _: S => ((), s) })

  def get[A](): M[S] = M({ s: S => (s, s) })

  def declareVar(name: String, value: Integer, state: S): S =
    (name, value) :: state

  def lookupVar(name: String, state: S): Integer = state match {
    case List()                      => ???
    case (n, v) :: tail if n == name => v
    case _ :: tail                   => lookupVar(name, tail)
  }

}
