package br.unb.cic.flang
import cats.data.State
import cats.implicits._

package object StateWithCats {
  type S = List[(String, StateValue)]

  // Define a State Monad
  type StateM[A] = State[S, A]

  def pure[A](a: A): StateM[A] = State.pure(a)

  def bind[A, B](m: StateM[A])(f: A => StateM[B]): StateM[B] = m.flatMap(f)

  def put(s: S): StateM[Unit] = State.set(s)

  def get: StateM[S] = State.get

  def modify(f: S => S): StateM[Unit] = State.modify(f)

  def runState[A](state: StateM[A], initial: S): (A, S) = {
    state.run(initial).value.swap
  }

  // Declare a variable in the state
  def declareVar(name: String, value: Integer, state: S): S =
    (name, IntValue(value)) :: state

  // Look up a variable's value in the state
  def lookupVar(name: String, state: S): Integer = state match {
    case List() => throw new RuntimeException(s"Variable $name not found")
    case (n, IntValue(v)) :: tail if n == name => v
    case _ :: tail => lookupVar(name, tail)
  }
}
