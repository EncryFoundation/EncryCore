package encry.it.docker

import monix.eval.Coeval
import org.scalatest.BeforeAndAfterAll
import org.scalatest.Suite

trait DockerAfterAll extends BeforeAndAfterAll {

  this: Suite =>

  protected val dockerSingleton: Coeval[Docker] = Coeval.evalOnce(createDocker)

  protected final def docker: Docker = dockerSingleton()

  protected def createDocker: Docker = Docker(getClass)

  override protected def afterAll(): Unit = {
    super.afterAll()
    docker.close()
  }
}