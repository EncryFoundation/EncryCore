package encry.view.state

import encry.VersionTag
import encry.view.wallet.NodeViewComponent

trait StateReader extends NodeViewComponent {

  //must be ID of last applied modifier
  def version: VersionTag

  def maxRollbackDepth: Int

}
