package br.unb.cic.flang

import ErrorMonad._

case class FDeclaration(name: String, arg: String, body: Expr)

object Declarations {

  def lookup(
      name: String,
      declarations: List[FDeclaration]
  ): M[FDeclaration] = declarations match {
    case List() => err(s"Function $name not declared.")
    case ((f @ FDeclaration(n, a, b)) :: _) if n == name => pure(f)
    case (_ :: fs)                                       => lookup(name, fs)
  }

}
