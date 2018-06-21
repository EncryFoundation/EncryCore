package encry.view.state

import encry.view.wallet.NodeViewComponent
import scorex.core.VersionTag

trait StateReader extends NodeViewComponent {

  //must be ID of last applied modifier
  def version: VersionTag

  def maxRollbackDepth: Int

}
