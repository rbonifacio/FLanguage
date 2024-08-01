package br.unb.cic.flang
import cats.data.State
import cats.implicits._

package object StateWithCats {
  type S = List[(String, Integer)]

  // recebe uma função que mapeia um estado para um par de um valor e um novo estado
  type StateM[A] = State[S, A] // State[S, A] é um tipo que representa uma função que mapeia um estado para um par de um valor e um novo estado

  def pure[A](a: A): StateM[A] = State.pure(a) // cria um estado que mapeia um estado para um par de um valor e um novo estado, onde o valor é a

  def bind[A, B](m: StateM[A])(f: A => StateM[B]): StateM[B] = m.flatMap(f) // cria um estado que mapeia um estado para um par de um valor e um novo estado, onde o valor é obtido aplicando a função f ao valor de m

  def put(s: S): StateM[Unit] = State.set(s) // cria um estado que mapeia um estado para um par de um valor e um novo estado, onde o valor é uma tupla vazia e o novo estado é s

  def get: StateM[S] = State.get // cria um estado que mapeia um estado para um par de um valor e um novo estado, onde o valor é o estado e o novo estado é o mesmo estado

  def modify(f: S => S): StateM[Unit] = State.modify(f) // cria um estado que mapeia um estado para um par de um valor e um novo estado, onde o valor é uma tupla vazia e o novo estado é o resultado de aplicar a função f ao estado

  def runState[A](state: StateM[A], initial: S): (A, S) = {
    val (s, a) = state.run(initial).value
    (a, s)
  }// executa o estado state com o estado inicial initial e retorna o valor e o novo estado

  // recebe uma string, um inteiro e um estado (S: state do tipo inteiro) e retorna um novo estado com a variável name associada ao valor value
  def declareVar(name: String, value: Integer, state: S): S =
    (name, value) :: state

  // recebe uma string e um estado (S: state do tipo inteiro) e retorna o valor associado a uma variável
  def lookupVar(name: String, state: S): Integer = state match {
    case List()                      => ???
    case (n, v) :: tail if n == name => v
    case _ :: tail                   => lookupVar(name, tail)
  }

}
