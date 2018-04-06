package encry.view.history.storage

import encry.modifiers.history.block.header.EncryBlockHeader
import encry.settings.NodeSettings
import encry.view.history.Height

case class FullBlockDownloadProcessor(nodeSettings: NodeSettings){

  private[history] var minimalBlockHeightVar: Height = Height @@ -1

  def minimalBlockHeight: Height = minimalBlockHeightVar

  def updateMinimalHeightOfBlock(header: EncryBlockHeader): Unit =
    minimalBlockHeightVar = minimalFullBlockAfter(header)

  private def minimalFullBlockAfter(header: EncryBlockHeader): Height = Height @@ {
    if (!nodeSettings.verifyTransactions) {
      Int.MaxValue
    } else if (minimalBlockHeightVar == Int.MaxValue) {
      // just synced with the headers chain - determine first full block to apply
      if (nodeSettings.blocksToKeep < 0) 0
      else if (!nodeSettings.stateMode.isDigest) 0
      else Math.max(0, header.height - nodeSettings.blocksToKeep + 1)

    } else if (nodeSettings.blocksToKeep >= 0) {
      Math.max(header.height - nodeSettings.blocksToKeep + 1, minimalBlockHeightVar)
    } else {
      0
    }
  }
}
