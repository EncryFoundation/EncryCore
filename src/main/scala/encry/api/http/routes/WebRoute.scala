package encry.api.http.routes

import java.net.URLDecoder
import java.security.Security
import java.util.concurrent.TimeUnit
import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.headers.{HttpCookie, RawHeader}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.server.{Route, ValidationRejection}
import akka.pattern._
import com.typesafe.scalalogging.StrictLogging
import encry.api.http.DataHolderForApi.{GetAllInfoHelper, GetBlockChainSync, GetMinerStatus, GetNodePassHashAndSalt}
import encry.local.miner.Miner.MinerStatus
import encry.settings.{NodeSettings, RESTApiSettings}
import io.circe.generic.auto._
import io.circe.{Json, parser}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import pdi.jwt.{JwtAlgorithm, JwtClaim, JwtSprayJson}
import scalatags.Text
import scalatags.Text.all.{div, td, _}
import scorex.utils.Random
import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.{Failure, Success}

case class InfoApi(name: String,
                   stateType: String,
                   difficulty: String,
                   bestFullHeaderId: String,
                   bestHeaderId: String,
                   peersCount: Int,
                   unconfirmedCount: Int,
                   previousFullHeaderId: String,
                   fullHeight: Int,
                   headersHeight: Int,
                   stateVersion: String,
                   uptime: Long,
                   storage: String,
                   isConnectedWithKnownPeers: Boolean,
                   isMining: Boolean,
                   knownPeers: Seq[String],
                   stateRoot: String)

case class WebRoute(override val settings: RESTApiSettings, nodeSettings: NodeSettings, dataHolder: ActorRef)(
  implicit val context: ActorRefFactory
) extends EncryBaseApiRoute with StrictLogging {

  def getPass: Future[String => Boolean] =
    (dataHolder ? GetNodePassHashAndSalt).mapTo[String => Boolean]

  def syncIsDoneF: Future[Boolean] = (dataHolder ? GetBlockChainSync).mapTo[Boolean]

  def signUp: Text.TypedTag[String] = html(
    scalatags.Text.all.head(
      meta(charset := "utf-8"),
      meta(name := "viewport", content := "width=device-width, initial-scale=1, shrink-to-fit=no"),

      tag("title")(
        "Encry Foundation"
      ),
      // Favicon
      link(href := "argon/assets/img/brand/favicon.png", rel := "icon", tpe := "image/png"),
      // Fonts
      link(href := "https://fonts.googleapis.com/css?family=Open+Sans:300,400,600,700", rel := "stylesheet"),
      // Icons
      link(href := "argon/assets/vendor/nucleo/css/nucleo.css", rel := "stylesheet"),
      link(href := "argon/assets/vendor/@fortawesome/fontawesome-free/css/all.min.css", rel := "stylesheet"),
      // Argon CSS
      link(tpe := "text/css", href := "argon/assets/css/argon.css?v=1.0.0", rel := "stylesheet")
    ),
    body(cls := "bg-default",
      div(cls := "main-content",

        // Header
        div(cls := "header bg-gradient-primary py-7 py-lg-8",
          div(cls := "container",
            div(cls := "header-body text-center mb-7",
              div(cls := "row justify-content-center",
                div(cls := "col-lg-5 col-md-6",
                  h1(cls := "text-white", "Welcome!"),
                  p(cls := "text-lead text-light",
                    "Use these to enter Encry node."
                  )
                )
              )
            )
          ),
          div(cls := "separator separator-bottom separator-skew zindex-100",
            tag("svg")(attr("x") := "0", attr("y") := "0", attr("viewBox") := "0 0 2560 100", attr("preserveAspectRatio") := "none", attr("version") := "1.1", xmlns := "http://www.w3.org/2000/svg",
              tag("polygon")(cls := "fill-default", attr("points") := "2560 0 2560 100 0 100")
            )
          )
        ),
        // Page content
        div(cls := "container mt--8 pb-5",
          div(cls := "row justify-content-center",
            div(cls := "col-lg-5 col-md-7",
              div(cls := "card bg-secondary shadow border-0",
                div(cls := "card-body px-lg-5 py-lg-5",
                  form(role := "form", id:="myForm2", action := "/token", attr("method") := "post",
                    div(cls := "form-group",
                      div(cls := "input-group input-group-alternative",
                        div(cls := "input-group-prepend",
                          scalatags.Text.tags.span(cls := "input-group-text",
                            i(cls := "ni ni-lock-circle-open")
                          )
                        ),
                        input(cls := "form-control", placeholder := "Password", tpe := "password", id:="password", name:="password")
                      )
                    ),
                    div(cls := "text-center",
                      button(tpe := "submit", cls := "btn btn-primary my-4", "Sign in")
                    )
                  )
                )
              )
            )
          )
        )
      ),
      // Footer
      footer(cls := "py-5",
        div(cls := "container",
          div(cls := "row align-items-center justify-content-xl-between",
            div(cls := "col-xl-6",
              div(cls := "copyright text-center text-xl-left text-muted", "© 2018",
                a(href := "https://www.creative-tim.com", cls := "font-weight-bold ml-1", target := "_blank", "Creative Tim")
              )
            ),
            div(cls := "col-xl-6",
              ul(cls := "nav nav-footer justify-content-center justify-content-xl-end",
                li(cls := "nav-item",
                  a(href := "https://www.encry.com", cls := "nav-link", target := "_blank", "Encry Foundation")
                )
              )
            )
          )
        )
      ),
      // Argon Scripts
      // Core
      script(src := "argon/assets/vendor/jquery/dist/jquery.min.js"),
      script(src := "argon/assets/vendor/bootstrap/dist/js/bootstrap.bundle.min.js"),
      // Optional JS
      script(src := "argon/assets/vendor/chart.js/dist/Chart.min.js"),
      script(src := "argon/assets/vendor/chart.js/dist/Chart.extension.js"),
      Seq(
        link(rel := "stylesheet", tpe := "text/css", href := "https://cdn.datatables.net/1.10.20/css/jquery.dataTables.min.css"),
        script(tpe := "text/javascript", src := "https://cdn.datatables.net/1.10.20/js/jquery.dataTables.min.js")
      ),
      // Argon JS
      script(src := "argon/assets/js/argon.js?v=1.0.0")
    )
  )

  def login: Route = path("login") {
        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, signUp.render))
  }
  def loginRoute: Route =
    (path("token") & post) {
      onComplete(getPass) {
        case Success(verifyPass) =>
          entity(as[String]) { urlPass =>
              val decodedStr = URLDecoder.decode(urlPass, "UTF-8")
              val receivedPass = decodedStr.substring(9)
              if (verifyPass(receivedPass)) {
                val token = WebRoute.createToken(receivedPass, 1)
                respondWithHeader(RawHeader("Access-Token", token)) {
                  setCookie(HttpCookie("JWT", value = token)) {
                    redirect("/web", StatusCodes.PermanentRedirect)
                  }
                }
              } else complete(s"Incorrect password: $receivedPass / ${URLDecoder.decode(urlPass, "UTF-8")}")
          }
        case Failure(exception) => complete(exception)
      }
    }

  def authenticatedRoute: Route =
    path("web") {
        WebRoute.authRoute(
          onComplete(currentInfoF) {
            case Success((nodeInfo, minerStatus, isSynced)) =>
              complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, webResponse(nodeInfo, minerStatus, isSynced).render))
            case Failure(exception) => complete(exception)
          }, settings)
    }

  //  JWT
  def statusF: Future[MinerStatus] = (dataHolder ? GetMinerStatus).mapTo[MinerStatus]

  def infoHelper: Future[Json] = (dataHolder ? GetAllInfoHelper).mapTo[Json]

  def currentInfoF: Future[(Json, MinerStatus, Boolean)] = for {
    info <- infoHelper
    status <- statusF
    sync   <- syncIsDoneF
  } yield (info, status, sync)

  def webResponse(json: Json, minerStatus: MinerStatus, sync: Boolean): Text.TypedTag[String] = {
    val nodeInfo = parser.decode[InfoApi](json.toString())

    html(
      scalatags.Text.all.head(
        meta(charset := "utf-8"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1, shrink-to-fit=no"),
        meta(name := "description", content := "Encry Core"),
        meta(name := "author", content := "Creative Tim"),
        script(
          raw("""function shutdown(){
    var request = new XMLHttpRequest();
    request.open('GET', "/node/shutdown");
    request.send();
    window.alert("The node was disconnected...");
  }""")
        ),
        script(
          raw("""function start(){
    var request = new XMLHttpRequest();
    request.open('GET', "/node/startMining");
    request.send();
    setTimeout(location.reload.bind(location), 5000);
    window.alert("Start mining... \n Reloading page in 5s.");
  }""")
        ),
        script(
          raw("""function stop(){
    var request = new XMLHttpRequest();
    request.open('GET', "/node/stopMining");
    request.send();
    setTimeout(location.reload.bind(location), 5000);
    window.alert("Stop mining... \n Reloading page in 5s.");
  }""")
        ),

        tag("title")(
          "Encry Core"
        ),
        // Favicon
        link(href := "argon/assets/img/brand/favicon.png", rel := "icon", tpe := "image/png"),
        // Fonts
        link(href := "https://fonts.googleapis.com/css?family=Open+Sans:300,400,600,700", rel := "stylesheet"),
        // Icons
        link(href := "argon/assets/vendor/nucleo/css/nucleo.css", rel := "stylesheet"),
        link(href := "argon/assets/vendor/@fortawesome/fontawesome-free/css/all.min.css", rel := "stylesheet"),
        // Argon CSS
        link(tpe := "text/css", href := "argon/assets/css/argon.css?v=1.0.0", rel := "stylesheet")
      ),
      body(
        // Sidenav
        tag("nav")(cls := "navbar navbar-vertical fixed-left navbar-expand-md navbar-light bg-white", id := "sidenav-main",
          div(cls := "container-fluid",
            // Toggler
            button(cls := "navbar-toggler", tpe := "button", data("toggle") := "collapse", data("target") := "#sidenav-collapse-main", aria.controls := "sidenav-main", aria.expanded := "false", aria.label := "Toggle navigation",
              scalatags.Text.tags.span(cls := "navbar-toggler-icon")
            ),
            // Brand
            a(cls := "navbar-brand pt-0", href := "/web",
              img(src := "argon/assets/img/brand/encry-logo.png", cls := "navbar-brand-img", alt := "...")
            ),
            // User

            // Collapse
            div(cls := "collapse navbar-collapse", id := "sidenav-collapse-main",
              // Collapse header
              div(cls := "navbar-collapse-header d-md-none",
                div(cls := "row",
                  div(cls := "col-6 collapse-brand",
                    a(href := "/web",
                      img(src := "argon/assets/img/brand/encry-logo.png")
                    )
                  ),
                  div(cls := "col-6 collapse-close",
                    button(tpe := "button", cls := "navbar-toggler", data("toggle") := "collapse", data("target") := "#sidenav-collapse-main", aria.controls := "sidenav-main", aria.expanded := "false", aria.label := "Toggle sidenav",
                      scalatags.Text.tags.span(),
                      scalatags.Text.tags.span()
                    )
                  )
                )
              ),
              // Navigation
              ul(cls := "navbar-nav",
                li(cls := "nav-item",
                  a(cls := "nav-link", href := "./web",
                    i(cls := "ni ni-tv-2 text-primary"), "Info"
                  )
                ),
                if (sync) {
                  li(cls := "nav-item",
                    a(tpe := "button", cls := "nav-link", disabled := "disabled",
                      i(cls := "ni ni-planet text-blue"), "Wallet (n.a during sync)"
                    )
                  )
                }
                else {
                  li(cls := "nav-item",
                    a(cls := "nav-link", href := "./webWallet",
                      i(cls := "ni ni-planet text-blue"), "Wallet"
                    )
                  )
                },
                div(cls := "dropdown",
                  a(cls := "nav-link", href := "#", role := "button", data("toggle") := "dropdown", aria.haspopup := "true", aria.expanded := "false",
                    i(cls := "ni ni-bullet-list-67 text-orange"), "Peers"
                  ),
                  div(cls := "dropdown-menu dropdown-menu-right dropdown-menu-arrow",
                    a(cls := "dropdown-item", href := "./allPeers", "All peers"),
                    a(cls := "dropdown-item", href := "./connectedPeers", "Connected peers"),
                    a(cls := "dropdown-item", href := "./bannedPeers", "Banned peers")
                  )
                )
              ),
            )
          )
        ),
        // Main content
        div(cls := "main-content",
          // Top navbar
          tag("nav")(cls := "navbar navbar-top navbar-expand-md navbar-dark", id := "navbar-main",
            div(cls := "container-fluid",
              // Brand
              a(cls := "h4 mb-0 text-white text-uppercase d-none d-lg-inline-block", href := "/web", "Encry"),


            )
          ),
          // Header

          div(cls := "header bg-gradient-primary pb-8 pt-5 pt-md-8",
            div(cls := "container-fluid",
              div(cls := "header-body"),
              //Cards

              div(cls := "row",
                div(cls := "col-xl-3 col-lg-6",
                  div(cls := "card card-stats mb-4 mb-xl-0",
                    div(cls := "card-body",
                      div(cls := "row",
                        div(cls := "col",
                          h5(cls := "card-title text-uppercase text-muted mb-0", "Node status"),
                          if(nodeInfo.right.get.headersHeight == 0 && nodeInfo.right.get.fullHeight == 0) {
                            scalatags.Text.tags.span(cls := "h3 font-weight-bold mb-0", "Not Synced (calculating...)")
                          }
                          else if (nodeInfo.right.get.headersHeight == nodeInfo.right.get.fullHeight ) {
                            scalatags.Text.tags.span(cls := "h3 font-weight-bold mb-0", "Synced")
                          }
                          else {
                            val int = (nodeInfo.right.get.fullHeight.toDouble/nodeInfo.right.get.headersHeight.toDouble)*100
                            val percentage = BigDecimal(int).setScale(0, BigDecimal.RoundingMode.HALF_UP).toDouble.toInt.toString
                            scalatags.Text.tags.span(cls := "h5 font-weight-bold mb-0", s"Sync in progress - $percentage%")
                          }
                        ),
                        div(cls := "col-auto",
                          div(cls := "icon icon-shape bg-danger text-white rounded-circle shadow",
                            i(cls := "ni ni-vector")
                          )
                        )
                      )
                    )
                  )
                ),
                div(cls := "col-xl-3 col-lg-6",
                  div(cls := "card card-stats mb-4 mb-xl-0",
                    div(cls := "card-body",
                      div(cls := "row",
                        div(cls := "col",
                          h5(cls := "card-title text-uppercase text-muted mb-0", "Blocks in chain"),
                          scalatags.Text.tags.span(cls := "h2 font-weight-bold mb-0", nodeInfo.right.get.headersHeight)
                        ),
                        div(cls := "col-auto",
                          div(cls := "icon icon-shape bg-warning text-white rounded-circle shadow",
                            i(cls := "ni ni-app")
                          )
                        )
                      )
                    )
                  )
                ),
                div(cls := "col-xl-3 col-lg-6",
                  div(cls := "card card-stats mb-4 mb-xl-0",
                    div(cls := "card-body",
                      div(cls := "row",
                        div(cls := "col",
                          h5(cls := "card-title text-uppercase text-muted mb-0", "Mining status"),
                          if(nodeInfo.right.get.isMining) {
                            scalatags.Text.tags.span(cls := "text-success mr-2", "Enabled")
                          }
                          else {
                            scalatags.Text.tags.span(cls := "text-warning mr-2", "Disabled")
                          }
                        ),
                        div(cls := "col-auto",
                          div(cls := "icon icon-shape bg-yellow text-white rounded-circle shadow",
                            i(cls := "ni ni-air-baloon")
                          )
                        )
                      )
                    )
                  )
                ),
                div(cls := "col-xl-3 col-lg-6",
                  div(cls := "card card-stats mb-4 mb-xl-0",
                    div(cls := "card-body",
                      div(cls := "row",
                        div(cls := "col",
                          h5(cls := "card-title text-uppercase text-muted mb-0", "Node name"),
                          scalatags.Text.tags.span(cls := "h3 font-weight-bold mb-0", nodeInfo.right.get.name)
                        ),
                        div(cls := "col-auto",
                          div(cls := "icon icon-shape bg-info text-white rounded-circle shadow",
                            i(cls := "ni ni-circle-08")
                          )
                        )
                      )
                    )
                  )
                )
              ),
// CARDS END
              div(cls:="row",

   if(minerStatus.isMining) {
              Seq(
                div(cls := "col-lg-3 col-md-6",
                  button(tpe := "button",  cls := "btn-icon-clipboard", onclick:="stop();", id:="myButtonn", data("clipboard-text") := "button-play", title := "", data("original-title") := "Stop Mining",
                    div(
                      i(cls := "ni ni-button-pause"),
                      scalatags.Text.tags.span("Stop Mining")
                    )
                  )
                ),
              )
   } else {
     Seq(
       div(cls := "col-lg-3 col-md-6",
         button(tpe := "button", cls := "btn-icon-clipboard", onclick:="start();", id:="myButton", data("clipboard-text") := "button-play", title := "", data("original-title") := "Start Mining",
           div(
             i(cls := "ni ni-button-play"),
             scalatags.Text.tags.span("Start Mining")
           )
         )
       ),
     )
   },
     Seq(
     div(cls := "col-lg-3 col-md-6",
       button(tpe := "button", cls := "btn-icon-clipboard", onclick:="shutdown()", id:="aaa", data("clipboard-text") := "button-play", title := "", data("original-title") := "Node Shutdown",
         div(
           i(cls := "ni ni-button-power"),
           scalatags.Text.tags.span("Node Shudown")
         )
       )
     ),
   )
    )
            )
          ),
          // Page content
          div(cls := "container-fluid mt--7",
            div(cls := "row",
              div(cls := "col",
                div(cls := "card shadow",
                  div(cls := "card-header border-0",
                    div(cls := "row align-items-center",
                      div(cls := "col",
                        h3(cls := "mb-0", "Node Info")
                      )
                    )
                  ),
                  div(cls := "table-responsive",
                    // Projects table
                    table(cls := "table align-items-center table-flush", id :="myTable",
                      thead(cls := "thead-light",
                        tr(th(attr("scope") := "row", "Difficulty"),
                          td(attr("scope") := "row", nodeInfo.right.get.difficulty)
                        ),
                        tr(th(attr("scope") := "row", "Best Full Header Id"),
                          td(attr("scope") := "row", nodeInfo.right.get.bestFullHeaderId)
                        ),
                        tr(th(attr("scope") := "row", "bestHeaderId"),
                          td(attr("scope") := "row", nodeInfo.right.get.bestHeaderId)
                        ),
                        tr(th(attr("scope") := "row", "peersCount"),
                          td(attr("scope") := "row", nodeInfo.right.get.peersCount)
                        ),
                        tr(th(attr("scope") := "row", "Transactions in mempool"),
                          td(attr("scope") := "row", nodeInfo.right.get.unconfirmedCount)
                        ),
                        tr(th(attr("scope") := "row", "previousFullHeaderId"),
                          td(attr("scope") := "row", nodeInfo.right.get.previousFullHeaderId)
                        ),
                        tr(th(attr("scope") := "row", "stateVersion"),
                          td(attr("scope") := "row", nodeInfo.right.get.stateVersion)
                        ),
                        tr(th(attr("scope") := "row", "uptime"),
                          td(attr("scope") := "row", WebRoute.mills2Time(nodeInfo.right.get.uptime))
                        ),
                        tr(th(attr("scope") := "row", "isConnectedWithKnownPeers"),
                          td(attr("scope") := "row", nodeInfo.right.get.isConnectedWithKnownPeers.toString())
                        ),
                        tr(th(attr("scope") := "row", "isMining"),
                          td(attr("scope") := "row", nodeInfo.right.get.isMining.toString())
                        ),
                      )
                    )
                  )
                )
              )
            ),
            // Footer
            footer(cls := "footer",
              div(cls := "row align-items-center justify-content-xl-between",
                div(cls := "col-xl-6",
                  div(cls := "copyright text-center text-xl-left text-muted", "© 2018",
                    a(href := "https://www.creative-tim.com", cls := "font-weight-bold ml-1", target := "_blank", "Creative Tim")
                  )
                ),
                div(cls := "col-xl-6",
                  ul(cls := "nav nav-footer justify-content-center justify-content-xl-end",
                    li(cls := "nav-item",
                      a(href := "https://www.creative-tim.com", cls := "nav-link", target := "_blank", "Creative Tim")
                    ),
                    li(cls := "nav-item",
                      a(href := "https://www.creative-tim.com/presentation", cls := "nav-link", target := "_blank", "About Us")
                    ),
                    li(cls := "nav-item",
                      a(href := "http://blog.creative-tim.com", cls := "nav-link", target := "_blank", "Blog")
                    ),
                    li(cls := "nav-item",
                      a(href := "https://github.com/creativetimofficial/argon-dashboard/blob/master/LICENSE.md", cls := "nav-link", target := "_blank", "MIT License")
                    )
                  )
                )
              )
            )
          ),
        ),
        // Argon Scripts
        // Core
        script(src := "argon/assets/vendor/jquery/dist/jquery.min.js"),
        script(src := "argon/assets/vendor/bootstrap/dist/js/bootstrap.bundle.min.js"),
        // Optional JS
        script(src := "argon/assets/vendor/chart.js/dist/Chart.min.js"),
        script(src := "argon/assets/vendor/chart.js/dist/Chart.extension.js"),
        Seq(
          link(rel := "stylesheet", tpe := "text/css", href := "https://cdn.datatables.net/1.10.20/css/jquery.dataTables.min.css"),
          script(tpe := "text/javascript", src := "https://cdn.datatables.net/1.10.20/js/jquery.dataTables.min.js")
        ),
        // Argon JS
        script(src := "argon/assets/js/argon.js?v=1.0.0")

      )
    )
  }


  override def route: Route = login ~ loginRoute ~ authenticatedRoute
}

object WebRoute  {
  import akka.http.scaladsl.model._
  import akka.http.scaladsl.server.Directives._
  // JWT
  if (Security.getProvider("BC") == null) {
    Security.addProvider(new BouncyCastleProvider())
  }

  val algorithm = JwtAlgorithm.HS256
  val secretKey = new String(Random.randomBytes())

  def mills2Time(millis: Long): String = s" ${TimeUnit.MILLISECONDS.toDays(millis)} days, " +
    s"${TimeUnit.MILLISECONDS.toMinutes(millis) - TimeUnit.DAYS.toMinutes(TimeUnit.MILLISECONDS.toDays(millis))} minutes, " +
    s"${TimeUnit.MILLISECONDS.toSeconds(millis) - TimeUnit.MINUTES.toSeconds(TimeUnit.MILLISECONDS.toMinutes(millis))} seconds"

  def createToken(username: String, expirationPeriodInDays: Int): String = {
      val claims = JwtClaim(
        expiration = Some(System.currentTimeMillis() / 1000 + TimeUnit.DAYS.toSeconds(expirationPeriodInDays)),
        issuedAt = Some(System.currentTimeMillis() / 1000),
        issuer = Some("encry.com"),
        subject = Some(username)
      )

      JwtSprayJson.encode(claims, secretKey, algorithm) // JWT string
    }

  def isTokenExpired(token: String): Boolean = JwtSprayJson.decode(token, secretKey, Seq(algorithm)) match {
    case Success(claims) => claims.expiration.getOrElse(0L) < System.currentTimeMillis() / 1000
    case Failure(_) => true
  }

  def isTokenValid(token: String): Boolean = JwtSprayJson.isValid(token, secretKey, Seq(algorithm))

  def authRoute(pingedRoute: Route, restApi: RESTApiSettings): Route =
    routeWithJWT(extractIp(pingedRoute, restApi))

  private def routeWithJWT(onSuccess: Route): Route = {
    optionalCookie("JWT") {
      case Some(token) =>
        val tokenValidity: Boolean = isTokenValid(token.value)
        val tokenExpiration: Boolean = isTokenExpired(token.value)
        if (tokenValidity && !tokenExpiration) onSuccess
        else if (tokenExpiration) complete(HttpResponse(status = StatusCodes.Unauthorized, entity = "Token expired."))
        else complete(HttpResponse(status = StatusCodes.Unauthorized, entity = "Token is invalid."))
      case _ => complete(HttpResponse(status = StatusCodes.Unauthorized, entity = "No token provided!"))
    }
  }

  def extractIp(pingedRoute: Route, restApi: RESTApiSettings): Route = extractClientIP { ip =>
    if (ip.toOption.exists(x => restApi.allowedPeers.contains(x.getHostAddress))) pingedRoute
    else reject(ValidationRejection("Access denied"))
  }

}
