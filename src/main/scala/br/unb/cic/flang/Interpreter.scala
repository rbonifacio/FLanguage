package br.unb.cic.flang

import Declarations._
import StateWithCats._


object Interpreter {

  /** This implementation relies on a state monad.
   *
   * Here we replace the substitution function (that needs to traverse the AST
   * twice during interpretation), by a 'global' state that contains the
   * current 'bindings'. The bindings are pairs from names to integers.
   *
   * We only update the state when we are interpreting a function application.
   * This implementation deals with sections 6.1 and 6.2 of the book
   * "Programming Languages: Application and Interpretation". However, here we
   * use a monad state, instead of passing the state explicitly as an agument
   * to the eval function.
   *
   * Sections 6.3 and 6.4 improves this implementation. We will left such an
   * improvements as an exercise.
   */
  def eval(expr: Expr, declarations: List[FDeclaration]): StateM[Integer] =
    expr match {
      case CInt(v) => pure(v)
      case Add(lhs, rhs) =>
        for {
          l <- eval(lhs, declarations)
          r <- eval(rhs, declarations)
        } yield l + r
      case Mul(lhs, rhs) =>
        for {
          l <- eval(lhs, declarations)
          r <- eval(rhs, declarations)
        } yield l * r
      case Id(name) =>
        for {
          state <- get
          value = lookupVar(name, state)
        } yield value
      case App(name, arg) =>
        val fdecl = lookup(name, declarations)
        for {
          value <- eval(arg, declarations)
          s1 <- get
          _ <- put(declareVar(fdecl.arg, value, s1))
          result <- eval(fdecl.body, declarations)
        } yield result
    }
}
