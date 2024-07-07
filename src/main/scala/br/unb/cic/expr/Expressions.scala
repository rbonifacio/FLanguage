package br.unb.cic.expr

sealed trait Expr

case class CInt(v: Integer) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Mul(lhs: Expr, rhs: Expr) extends Expr
case class Id(name: String) extends Expr
case class App(name: String, arg: Expr) extends Expr
