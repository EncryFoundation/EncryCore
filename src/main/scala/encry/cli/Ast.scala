package encry.cli

object Ast {

  case class Command(category: Identifier, name: Identifier, params: List[Param], flags: List[Flag])

  sealed trait Value
  case class Num(i: Long) extends Value
  case class Str(s: String) extends Value

  sealed trait Bool extends Value
  case object True extends Value with Bool
  case object False extends Value with Bool

  case class Identifier(ident: String)
  case class Param(ident: Identifier, value: Value)
  case class Flag(ident: Identifier)
}
