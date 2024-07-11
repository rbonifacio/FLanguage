package br.unb.cic.flang

import org.scalatest._
import flatspec._
import matchers._

import Interpreter._
import Declarations._
import MErr._

class InterpreterTest extends AnyFlatSpec with should.Matchers {

  val inc = FDeclaration("inc", "x", Add(Id("x"), CInt(1)))
  val bug = FDeclaration("bug", "x", Add(Id("y"), CInt(1)))

  val declarations = List(inc, bug)

  "eval CInt(5)" should "return an integer value 5." in {
    val c5 = CInt(5)
    eval(c5, declarations) should be (Right(5))
  }

  "eval Add(CInt(5), CInt(10)) " should "return an integer value 15." in {
    val c5  = CInt(5)
    val c10 = CInt(10)
    val add = Add(c5, c10)
    eval(add, declarations) should be (Right(15))
  }

  "eval Add(CInt(5), Add(CInt(5), CInt(10))) " should "return an integer value 20." in {
    val c5 = CInt(5)
    val c10 = CInt(10)
    val add = Add(c5, Add(c5, c10))
    eval(add, declarations) should be(Right(20))
  }

  "eval Mul(CInt(5), CInt(10))" should "return an integer value 50" in {
    val c5 = CInt(5)
    val c10 = CInt(10)
    val mul = Mul(c5, CInt(10))
    eval(mul, declarations) should be(Right(50))
  }

  "eval App(inc, 99) " should "return an integer value 100" in {
    val app = App("inc", CInt(99))
    eval(app, declarations) should be (Right(100))
  }

  "eval App(foo, 10) " should "raise an error." in {
    val app = App("foo", CInt(10))
    assertError(eval(app, declarations)) should be (true)
  }

  "eval Add(5, App(bug, 10)) " should "raise an error." in {
    val c5  = CInt(5)
    val app = App("bug", CInt(10))
    val add = Add(c5, app)
    assertError(eval(add, declarations)) should be (true)
  }
  
}
