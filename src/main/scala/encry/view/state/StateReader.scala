package encry.view.state

import encry.utils.CoreTaggedTypes.VersionTag
import encry.view.NodeViewComponent

trait StateReader extends NodeViewComponent {

  //must be ID of last applied modifier
  def version: VersionTag

  def maxRollbackDepth: Int

}
