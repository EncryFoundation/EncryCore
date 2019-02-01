package encry.it.docker

import com.typesafe.config.Config
import monix.eval.Coeval
import org.scalatest.Suite

trait NodesFromDocker extends DockerAfterAll {
  _: Suite =>

  protected def nodeConfigs: Seq[Config]

  protected val dockerNodes: Coeval[Seq[Node]] = dockerSingleton
    .map(_.startNodes(nodeConfigs))
    .memoize

  protected def nodes: Seq[Node] = dockerNodes()
}