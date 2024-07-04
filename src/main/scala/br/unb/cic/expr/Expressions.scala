package br.unb.cic.expr

sealed trait Expr

case class CInt (v: Integer) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Mul(lhs: Expr, rhs: Expr) extends Expr


// TODO: Implementar o suporte a funções.

// (a) como representar declaracoes de funcoes?
// (b) como "avaliar" funcoes pre-definidas?
// (c) qual o impacto no interpretaror? 
