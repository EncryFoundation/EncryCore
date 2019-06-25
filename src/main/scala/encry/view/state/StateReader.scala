package encry.view.state

import encry.utils.CoreTaggedTypes.VersionTag
import encry.view.NodeViewComponent

trait StateReader extends NodeViewComponent {

  def version: VersionTag

}
