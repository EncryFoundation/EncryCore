package encry.cli.commands

import scala.util.Try

trait Command {

  def execute(args: String): Try[Unit]
}
