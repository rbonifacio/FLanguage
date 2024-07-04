package br.unb.cic.expr

object Interpreter {
  def eval(expr: Expr) : Integer = expr match {
    case CInt(v) => v
    case Add(v,u) => eval(v) + eval(u)
    case Mul(v,u) => eval(v) * eval(u)
  }
}
