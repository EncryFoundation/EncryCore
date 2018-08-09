package encry.view

trait NodeViewComponent {
  self =>

  type NVCT >: self.type <: NodeViewComponent
}
