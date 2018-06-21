package encry.view.wallet

trait NodeViewComponent {
  self =>

  type NVCT >: self.type <: NodeViewComponent
}
