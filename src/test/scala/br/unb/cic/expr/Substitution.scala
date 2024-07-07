package br.unb.cic.expr

import org.scalatest._
import flatspec._
import matchers._
import Substitution._
class SubstitutionTest extends AnyFlatSpec with should.Matchers {

  "substitute x by CInt(5) in CInt(10)" should "return the int constant 10." in {
    val c5  = CInt(5)
    val c10 = CInt(10)
    substitute(c5, "x", c10) should be (CInt(10))
  }

  "substitute x by CInt(5) in Id('x')" should "return the int constant 5." in {
    val c5 = CInt(5)
    val id = Id("x")
    substitute(c5, "x", id) should be (CInt(5))
  }

  "substitute x by CInt(5) in Id('y')" should "return Id('y')." in {
    val c5 = CInt(5)
    val id = Id("y")
    substitute(c5, "x", id) should be (Id("y"))
  }

  "substitute x by CInt(5) in Add(CInt(10), Id('x'))" should "return Add(CInt(10), CInt(5))" in {
    val c5 = CInt(5)
    val c10 = CInt(10)
    val add = Add(CInt(10), Id("x"))
    substitute(c5, "x", add) should be (Add(c10, c5))
  }


  "substitute x by CInt(5) in Add(CInt(10), Add(Id('x'), Id('x')))" should "return Add(CInt(10), Add(CInt(5), CInt(5)))" in {
    val c5 = CInt(5)
    val c10 = CInt(10)
    val add = Add(CInt(10), Add(Id("x"), Id("x")))
    substitute(c5, "x", add) should be (Add(CInt(10), Add(CInt(5), CInt(5))))
  }
}
