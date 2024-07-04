package br.unb.cic.expr

import org.scalatest._
import flatspec._
import matchers._
import Interpreter._
class InterpreterTest extends AnyFlatSpec with should.Matchers {

  "eval CInt(5)" should "return an integer value 5." in {
    val c5 = CInt(5)
    eval(c5) should be (5)
  }

  "eval Add(CInt(5), CInt(10)) " should "return an integer value 15." in {
    val c5  = CInt(5)
    val c10 = CInt(10)
    val add = Add(c5, c10)
    eval(add) should be (15)
  }

  "eval Add(CInt(5), Add(CInt(5), CInt(10))) " should "return an integer value 20." in {
    val c5 = CInt(5)
    val c10 = CInt(10)
    val add = Add(c5, Add(c5, c10))
    eval(add) should be(20)
  }

  "eval Mul(CInt(5), CInt(10))" should "return an integer value 50" in {
    val c5 = CInt(5)
    val c10 = CInt(10)
    val mul = Mul(c5, CInt(10))
    eval(mul) should be(50)
  }
}
