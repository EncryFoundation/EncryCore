package encry.view.history.storage

import encry.modifiers.history.block.header.EncryBlockHeader
import encry.settings.NodeSettings
import encry.view.history.Height

case class FullBlockDownloadProcessor(nodeSettings: NodeSettings){

  private[history] var minimalHeightOfBlock: Height = Height @@ -1

  def getMinimalHeightOfBlock: Height = minimalHeightOfBlock

  def setMinimalHeightOfBlock(header: EncryBlockHeader): Unit =
    minimalHeightOfBlock = minimalFullBlockAfter(header)

  private def minimalFullBlockAfter(header: EncryBlockHeader): Height = {
    if(minimalHeightOfBlock == -1) {
      if (nodeSettings.blocksToKeep < 0) Height @@ 0
      else Height @@ Math.max(header.height - nodeSettings.blocksToKeep + 1, 0)
    } else Height @@ Math.max(header.height - nodeSettings.blocksToKeep + 1, minimalHeightOfBlock)
  }
}
