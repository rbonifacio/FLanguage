package br.unb.cic.flang

import Declarations._
import StateWithCats._
import cats.data.State
import cats.implicits._
import Substitution._

object Interpreter {
  def eval(expr: Expr): StateM[Integer] = expr match {
    case CInt(v) => pure(v)
    case Add(lhs, rhs) => for {
      l <- eval(lhs)
      r <- eval(rhs)
    } yield l + r
    case Mul(lhs, rhs) => for {
      l <- eval(lhs)
      r <- eval(rhs)
    } yield l * r
    case Id(v) => for {
      state <- get
      value = lookupVar(v, state)
    } yield value
    case App(n, arg) => for {
      state <- get
      fdecl = lookupFunction(n, state)
      bodyS = substitute(arg, fdecl.arg, fdecl.body)
      res <- eval(bodyS)
    } yield res
  }

  def lookupFunction(name: String, state: S): FDeclaration = {
    state.collectFirst { case (n, FuncValue(d)) if n == name => d }
      .getOrElse(throw new RuntimeException(s"Function $name not found"))
  }

  def evalWithDeclarations(expr: Expr, declarations: List[FDeclaration]): StateM[Integer] = {
    val initialState: S = declarations.map(d => (d.name, FuncValue(d)))
    for {
      _ <- put(initialState)
      res <- eval(expr)
    } yield res
  }

  def run(expr: Expr, initialState: S): (Integer, S) = {
    runState(eval(expr), initialState)
  }
}
