package encry.cli

package object commands {

  object LocalCommand

  object AddKey extends ViewCommand

  object GetBalance extends ViewCommand

  object PrintMyAddrs extends ViewCommand

  //TODO This cmd is unsafe.
  object PrintPrivKeys extends ViewCommand

  case object PrintPubKeys extends ViewCommand

  object Help extends Help

}
