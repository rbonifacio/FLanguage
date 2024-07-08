package br.unb.cic.flang

import br.unb.cic.flang.FDeclaration
import br.unb.cic.flang.Declarations._
import br.unb.cic.flang.Substitution._

object Interpreter {
  def eval(expr: Expr, declarations: List[FDeclaration]): Integer = expr match {
    case CInt(v)       => v
    case Add(lhs, rhs) => eval(lhs, declarations) + eval(rhs, declarations)
    case Mul(lhs, rhs) => eval(lhs, declarations) * eval(rhs, declarations)
    case Id(_)         => ???
    case App(n, e) => {
      val fdecl = lookup(n, declarations)
      val bodyS = substitute(e, fdecl.arg, fdecl.body)
      eval(bodyS, declarations)
    }
  }
}
