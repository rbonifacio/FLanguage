package br.unb.cic.flang

import br.unb.cic.flang.FDeclaration
import br.unb.cic.flang.Declarations._
import br.unb.cic.flang.Substitution._

import M._

object Interpreter {
  def eval(expr: Expr, declarations: List[FDeclaration]): M[Integer] = expr match {
    case CInt(v)       => pure(v)
    case Add(lhs, rhs) => bind(eval(lhs, declarations))({a: Integer => bind(eval(rhs, declarations))({b: Integer => pure(a+b)})})
    case Mul(lhs, rhs) => bind(eval(lhs, declarations))({a: Integer => bind(eval(rhs, declarations))({b: Integer => pure(a*b)})})
    case Id(_)         => err("Not expecting a variable while executing the interpreter")
    case App(n, e)     =>
      bind(lookup(n, declarations)) ({ f : FDeclaration =>  {
        val bodyS = substitute(e, f.arg, f.body)
        eval(bodyS, declarations)
      }})
  }
}
