package br.unb.cic.expr

import br.unb.cic.expr.FDeclaration
import br.unb.cic.expr.Declarations._
import br.unb.cic.expr.Substitution._

object Interpreter {
  def eval(expr: Expr, declarations: List[FDeclaration]): Integer = expr match {
    case CInt(v)       => v
    case Add(lhs, rhs) => eval(lhs, declarations) + eval(rhs, declarations)
    case Mul(lhs, rhs) => eval(lhs, declarations) * eval(rhs, declarations)
    case Id(_)         => ???
    case App(n, e) =>
      lookup(n, declarations) match {
        case Some(FDeclaration(_, arg, body)) => {
          val bodyS = substitute(e, arg, body)
          eval(bodyS, declarations)
        }
        case None => ???
      }
  }
}
