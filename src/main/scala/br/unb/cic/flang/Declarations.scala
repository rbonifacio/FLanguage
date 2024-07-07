package br.unb.cic.flang

case class FDeclaration(name: String, arg: String, body: Expr)

object Declarations {

  def lookup(
      name: String,
      declarations: List[FDeclaration]
  ): Option[FDeclaration] = declarations match {
    case List()                                          => None
    case ((f @ FDeclaration(n, a, b)) :: _) if n == name => Some(f)
    case (_ :: fs)                                       => lookup(name, fs)
  }

}
