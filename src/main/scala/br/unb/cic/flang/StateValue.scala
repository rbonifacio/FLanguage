package br.unb.cic.flang

sealed trait StateValue
case class IntValue(value: Integer) extends StateValue
case class FuncValue(fdecl: FDeclaration) extends StateValue
