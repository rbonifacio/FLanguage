package br.unb.cic.flang

case class FDeclaration(name: String, arg: String, body: Expr)

object Declarations {

  def lookup(
      name: String,
      declarations: List[FDeclaration]
  ): FDeclaration = declarations match {
    case List()                                          => ???
    case ((f @ FDeclaration(n, a, b)) :: _) if n == name => f
    case (_ :: fs)                                       => lookup(name, fs)
  }

}
