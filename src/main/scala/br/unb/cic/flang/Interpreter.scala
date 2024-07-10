package br.unb.cic.flang

import MErr._

import Declarations._
import Substitution._

import cats.syntax.applicative._       // for pure
import cats.syntax.applicativeError._  // for raiseError

object Interpreter {
  def eval(expr: Expr, declarations: List[FDeclaration]): MError[Integer] = expr match {
    case CInt(v) => eh.pure(v)
    case Add(lhs, rhs) => for {
      l <- eval(lhs, declarations)
      r <- eval(rhs, declarations)
    } yield l + r
    case Mul(lhs, rhs) => for {
      l <- eval(lhs, declarations)
      r <- eval(rhs, declarations)
    } yield l * r
    case Id(v) => eh.raiseError("Error evaluating an identifier.")
    case App(n, arg) => for {
      fdecl <- lookup(n, declarations)
      bodyS = substitute(arg, fdecl.arg, fdecl.body)
      res <- eval(bodyS, declarations)
    } yield res
  }
}
