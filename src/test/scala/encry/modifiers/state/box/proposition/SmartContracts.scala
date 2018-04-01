package encry.modifiers.state.box.proposition

import encrywm.common.{ESContract, SourceProcessor}

trait SmartContracts {

  val DummyContract: ESContract = {
    val source =
      """
        |unlock if true
    """.stripMargin

    SourceProcessor.source2Contract(source).get
  }

  val HLContract: ESContract = {
    val source =
      """
        |let unlockStart = 1000
        |let unlockFinish = 2000
        |unlock if context.state.height >= unlockStart and context.state.height <= unlockFinish
      """.stripMargin

    SourceProcessor.source2Contract(source).get
  }
}
