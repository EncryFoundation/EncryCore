package encry.api.http

import java.security.SecureRandom

import akka.actor.ActorRefFactory
import akka.http.scaladsl.server.Route
import encry.settings.RESTApiSettings
import io.swagger.v3.oas.annotations.enums.ParameterIn
import io.swagger.v3.oas.annotations.{Operation, Parameter}
import io.swagger.v3.oas.annotations.media.{Content, ExampleObject, Schema}
import io.swagger.v3.oas.annotations.parameters.RequestBody
import io.swagger.v3.oas.annotations.responses.ApiResponse
import javax.ws.rs.{GET, POST, Path}
import org.encryfoundation.common.Algos
import scorex.crypto.hash.Blake2b256

@Path("utils")
case class UtilsApiRoute(override val settings: RESTApiSettings)(implicit val context: ActorRefFactory) extends ApiRoute {
  private val SeedSize: Int = 32

  private def seed(length: Int): String = {
    val seed: Array[Byte] = new Array[Byte](length)
    new SecureRandom().nextBytes(seed) //seed mutated here!
    Algos.encode(seed)
  }

  override val route: Route = pathPrefix("utils") {
    seedRoute ~ length ~ hashBlake2b
  }

  @GET
  @Path("/seed")
  @Operation(summary = "Return random seed of length 32",
    responses = Array(
      new ApiResponse(responseCode = "200", description = "Random seed response",
        content = Array(new Content(schema = new Schema(implementation = classOf[String])))),
      new ApiResponse(responseCode = "500", description = "Internal server error"))
  )
  def seedRoute: Route = (path("seed") & get) (complete(seed(SeedSize)))

  @GET
  @Path("/seed/{length}")
  @Operation(summary = "Return random seed of length {length}",
    responses = Array(
      new ApiResponse(responseCode = "200", description = "Random seed response",
        content = Array(new Content(schema = new Schema(implementation = classOf[String])))),
      new ApiResponse(responseCode = "500", description = "Internal server error")),
    parameters = Array(
      new Parameter(name = "length", required = true, schema = new Schema(implementation = classOf[Int]), in = ParameterIn.PATH),
    )
  )
  def length: Route = (path("seed" / IntNumber) & get) (length => complete(seed(length)))

  @POST
  @Path("/hash/blake2b")
  @Operation(summary = "Return Blake2b hash of specified message",
    requestBody = new RequestBody(content =
      Array(new Content(schema =
        new Schema(implementation = classOf[String]), examples = Array(new ExampleObject(value = "7yaASMijGEGTbttYHg1MrXnWB8EbzjJnFLSWvmNoHrXV")))),
      required = true
    ),
    responses = Array(
      new ApiResponse(responseCode = "200", description = "Base58-encoded 32 byte hash",
        content = Array(new Content(schema = new Schema(implementation = classOf[String], example = "6QLZkR1RdHvF7gUw7oms1XdQM6kc9kxpmyHRADN5x7uQ")))),
      new ApiResponse(responseCode = "500", description = "Internal server error"))
  )
  def hashBlake2b: Route = (path("hash" / "blake2b") & post & entity(as[String])) { message =>
    complete(Algos.encode(Blake2b256(message)))
  }
}