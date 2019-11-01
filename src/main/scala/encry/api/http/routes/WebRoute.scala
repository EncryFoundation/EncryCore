package encry.api.http.routes

import java.util.concurrent.TimeUnit

import akka.actor.{ActorRef, ActorRefFactory, ActorSystem}
import akka.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpMethods, HttpRequest, HttpResponse, StatusCodes}
import akka.http.scaladsl.server.{Directive, Directive1, HttpApp, Route, StandardRoute, ValidationRejection}
import akka.pattern._
import com.typesafe.scalalogging.StrictLogging
import encry.api.http.DataHolderForApi.{GetAllInfoHelper, GetAllPermissionedUsers, GetMinerStatus, StartMiner, StopMiner}
import encry.local.miner.Miner.MinerStatus
import scalatags.Text.all.{div, span, td, _}
import encry.settings.{NodeSettings, RESTApiSettings}
import io.circe.Decoder.Result
import io.circe.Json
import scalatags.{Text, generic}
import io.circe.parser
import io.circe.generic.auto._
import spray.json._
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.headers.{HttpCookie, RawHeader}
import akka.http.scaladsl.server.Directives.{complete, optionalCookie}
import encry.api.http.routes.WebRoute.{isTokenExpired, isTokenValid}
import pdi.jwt.{JwtAlgorithm, JwtClaim, JwtSprayJson}

import scala.concurrent.{Await, Future}
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

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
                   uptime: Int,
                   storage: String,
                   isConnectedWithKnownPeers: Boolean,
                   isMining: Boolean,
                   knownPeers: Seq[String]
                  )

case class LoginRequest(a: String, b: String)

case class WebRoute(override val settings: RESTApiSettings, nodeSettings: NodeSettings, dataHolder: ActorRef)(
  implicit val context: ActorRefFactory
) extends EncryBaseApiRoute with StrictLogging {
// JWT
  val superSecretPasswordDb = Map(
    "admin" -> "admin",
    "daniel" -> "Rockthejvm1!"
  )

  val algorithm = JwtAlgorithm.HS256
  val secretKey = "encry"

  def checkPassword(username: String, password: String): Boolean =
    superSecretPasswordDb.contains(username) && superSecretPasswordDb(username) == password

  def createToken(username: String, expirationPeriodInDays: Int): String = {
    val claims = JwtClaim(
      expiration = Some(System.currentTimeMillis() / 1000 + TimeUnit.DAYS.toSeconds(expirationPeriodInDays)),
      issuedAt = Some(System.currentTimeMillis() / 1000),
      issuer = Some("rockthejvm.com"),
      subject = Some(username)
    )

    JwtSprayJson.encode(claims, secretKey, algorithm) // JWT string
  }

  def isTokenExpired(token: String): Boolean = JwtSprayJson.decode(token, secretKey, Seq(algorithm)) match {
    case Success(claims) => claims.expiration.getOrElse(0L) < System.currentTimeMillis() / 1000
    case Failure(_) => true
  }

  def isTokenValid(token: String): Boolean = JwtSprayJson.isValid(token, secretKey, Seq(algorithm))

def loginForm = html(
  scalatags.Text.all.head(
    tag("title")("Login")
  ),
  body(
    form(name := "input", action := "/token", attr("method") := "post", id := "loginForm",
      label(`for` := "password", "Password:"),
      input(tpe := "password", value := "", id := "password", name := "password"),
      div(cls := "error"),
      input(tpe := "submit", value := "Send")
    )
  )
)
  
  def sasjf = html(
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
                  form(role := "form", action := "/token", attr("method") := "post",
                    div(cls := "form-group",
                      div(cls := "input-group input-group-alternative",
                        div(cls := "input-group-prepend",
                          span(cls := "input-group-text",
                            i(cls := "ni ni-lock-circle-open")
                          )
                        ),
                        input(cls := "form-control", placeholder := "Password", tpe := "password", id:="password", name:="password")
                      )
                    ),
                    div(cls := "text-center",
                      button(tpe := "button", cls := "btn btn-primary my-4", "Sign in")
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
//                li(cls := "nav-item",
//                  a(href := "https://www.creative-tim.com/presentation", cls := "nav-link", target := "_blank", "About Us")
//                ),
//                li(cls := "nav-item",
//                  a(href := "http://blog.creative-tim.com", cls := "nav-link", target := "_blank", "Blog")
//                ),
//                li(cls := "nav-item",
//                  a(href := "https://github.com/creativetimofficial/argon-dashboard/blob/master/LICENSE.md", cls := "nav-link", target := "_blank", "MIT License")
//                )
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

  def login = (path("login") ) {
    complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, sasjf.render))
  }

  def loginRoute =
    (path("token") & post) {
      println("111")
      entity(as[String]) {
        case x: String if checkPassword("admin", x.substring(9)) =>
          println("222")
          val token = createToken(x, 1)
          respondWithHeader(RawHeader("Access-Token", token)) {
            setCookie(HttpCookie("JWT", value = token)) {
              redirect("/web", StatusCodes.PermanentRedirect)
            }
          }
        case hui =>
          println(s"HUI = $hui")
          println(hui.substring(10))
          println("333")
          complete(StatusCodes.Unauthorized)
      }
    }

  def authenticatedRoute: Route =
    path("web") {
      WebRoute.extractIp(
          WebRoute.authRoute(
            onComplete(currentInfoF) {
            case Success(info) => complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, woqdl(info._1, info._2).render))
          })
      )
    }

  //  JWT
  def statusF: Future[MinerStatus] = (dataHolder ? GetMinerStatus).mapTo[MinerStatus]

  def infoHelper: Future[Json] = (dataHolder ? GetAllInfoHelper).mapTo[Json]

  def currentInfoF: Future[(Json, MinerStatus)] = for {
    info <- infoHelper
    status <- statusF
  } yield (info, status)

  def woqdl(json: Json, minerStatus: MinerStatus): Text.TypedTag[String] = {
    val nodeInfo = parser.decode[InfoApi](json.toString())
    html(
      scalatags.Text.all.head(
        meta(charset := "utf-8"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1, shrink-to-fit=no"),
        meta(name := "description", content := "Start your development with a Dashboard for Bootstrap 4."),
        meta(name := "author", content := "Creative Tim"),
        script(
          raw("""function shutdown(){
    var input1 = document.getElementById("input1");
    var request = new XMLHttpRequest();
    request.open('GET', "http://localhost:9051/node/shutdown");
//    request.setRequestHeader('content-type', 'application/json');
    request.send();
  }""")
        ),
        script(
          raw("""function start(){
    var request = new XMLHttpRequest();
    request.open('GET', "http://localhost:9051/node/startMining");
//    request.setRequestHeader('content-type', 'application/json');
    request.send();
    setTimeout(location.reload.bind(location), 5000);
    window.alert("Start mining... \n Reloading page in 5s.");
  }""")
        ),
        script(
          raw("""function stop(){
    var request = new XMLHttpRequest();
    request.open('GET', "http://localhost:9051/node/stopMining");
//    request.setRequestHeader('content-type', 'application/json');
    request.send();
    setTimeout(location.reload.bind(location), 5000);
    window.alert("Stop mining... \n Reloading page in 5s.");
  }""")
        ),

        tag("title")(
          "Argon Dashboard - Free Dashboard for Bootstrap 4"
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
              span(cls := "navbar-toggler-icon")
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
                    a(href := "./index.html",
                      img(src := "argon/assets/img/brand/blue.png")
                    )
                  ),
                  div(cls := "col-6 collapse-close",
                    button(tpe := "button", cls := "navbar-toggler", data("toggle") := "collapse", data("target") := "#sidenav-collapse-main", aria.controls := "sidenav-main", aria.expanded := "false", aria.label := "Toggle sidenav",
                      span(),
                      span()
                    )
                  )
                )
              ),
              // Form
              form(cls := "mt-4 mb-3 d-md-none",
                div(cls := "input-group input-group-rounded input-group-merge",
                  input(tpe := "search", cls := "form-control form-control-rounded form-control-prepended", placeholder := "Search", aria.label := "Search"),
                  div(cls := "input-group-prepend",
                    div(cls := "input-group-text",
                      span(cls := "fa fa-search")
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
                li(cls := "nav-item",
                  a(cls := "nav-link", href := "./webWallet",
                    i(cls := "ni ni-planet text-blue"), "Wallet"
                  )
                ),
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
                          if(nodeInfo.right.get.headersHeight == nodeInfo.right.get.fullHeight) {
                            span(cls := "h2 font-weight-bold mb-0", "Synced")
                          }
                          else {
                            span(cls := "h2 font-weight-bold mb-0", "Sync in progress")
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
                          span(cls := "h2 font-weight-bold mb-0", nodeInfo.right.get.fullHeight)
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
                            span(cls := "text-success mr-2", "Enabled")
                          }
                          else {
                            span(cls := "text-warning mr-2", "Disabled")
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
                          span(cls := "h3 font-weight-bold mb-0", nodeInfo.right.get.name)
                        ),
                        div(cls := "col-auto",
                          div(cls := "icon icon-shape bg-info text-white rounded-circle shadow",
                            i(cls := "fas fa-percent")
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
                      span("Stop Mining")
                    )
                  )
                ),
//                script(tpe := "text/javascript",
//                  raw("""document.getElementById("myButton").onclick = function () {
//        document.getElementById("myButton").disabled = true;
//    };""")
//                )
              )
   } else {
     Seq(
       div(cls := "col-lg-3 col-md-6",
         button(tpe := "button", cls := "btn-icon-clipboard", onclick:="start();", id:="myButton", data("clipboard-text") := "button-play", title := "", data("original-title") := "Start Mining",
           div(
             i(cls := "ni ni-button-play"),
             span("Start Mining")
           )
         )
       ),
//       script(tpe := "text/javascript",
//         raw(
//           """document.getElementById("myButton").onclick = function () {
//        location.href = "/node/startMining";
//    };""")
//       )
     )
   },
     Seq(
     div(cls := "col-lg-3 col-md-6",
       button(tpe := "button", cls := "btn-icon-clipboard", onclick:="shutdown()", id:="aaa", data("clipboard-text") := "button-play", title := "", data("original-title") := "Node Shutdown",
         div(
           i(cls := "ni ni-button-power"),
           span("Node Shudown")
         )
       )
     ),
//     script(tpe := "text/javascript",
//       raw(
//         """document.getElementById("aaa").onclick = function () {
//        location.href = "/node/shutdown";
//    };""")
//     )
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

//                        tr(th(attr("scope") := "row", "State Type"),
//                          td(attr("scope") := "row", nodeInfo.right.get.stateType)
//                        ),
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
                        ), // print above
                        tr(th(attr("scope") := "row", "Transactions in mempool"),
                          td(attr("scope") := "row", nodeInfo.right.get.unconfirmedCount)
                        ),
                        tr(th(attr("scope") := "row", "previousFullHeaderId"),
                          td(attr("scope") := "row", nodeInfo.right.get.previousFullHeaderId)
                        )
                        ,
//                        tr(th(attr("scope") := "row", "fullHeight"),
//                          td(attr("scope") := "row", nodeInfo.right.get.fullHeight)
//                        ),
//                        tr(th(attr("scope") := "row", "headersHeight"),
//                          td(attr("scope") := "row", nodeInfo.right.get.headersHeight)
//                        ),
                        tr(th(attr("scope") := "row", "stateVersion"),
                          td(attr("scope") := "row", nodeInfo.right.get.stateVersion)
                        ),
                        tr(th(attr("scope") := "row", "uptime"),
                          td(attr("scope") := "row", nodeInfo.right.get.uptime)
                        ),
//                        tr(th(attr("scope") := "row", "storage"),
//                          td(attr("scope") := "row", nodeInfo.right.get.storage)
//                        ),
                        tr(th(attr("scope") := "row", "isConnectedWithKnownPeers"),
                          td(attr("scope") := "row", nodeInfo.right.get.isConnectedWithKnownPeers.toString())
                        ),
                        tr(th(attr("scope") := "row", "isMining"),
                          td(attr("scope") := "row", nodeInfo.right.get.isMining.toString())
                        ),
//                        tr(th(attr("scope") := "row", "knownPeers"),
//                          td(attr("scope") := "row", nodeInfo.right.get.knownPeers)
//                        )
                        //                        th(attr("scope") := "row", "Unique users"),
                        //                        th(attr("scope") := "row", "Bounce rate"))
                      )
                      //                    tbody(
                      //                      tr(
                      //                        th(attr("scope") := "col", "/argon/"),
                      //                        td(currentInfo.findAllByKey("name").head.toString()),
                      //                        td("340"),
                      //                        td(
                      //                          i(cls := "fas fa-arrow-up text-success mr-3"), "46,53%"
                      //                        )
                      //                      ),
                      //                      tr(
                      //                        th(attr("scope") := "col", "/argon/index.html"),
                      //                        td("3,985"),
                      //                        td("319"),
                      //                        td(
                      //                          i(cls := "fas fa-arrow-down text-warning mr-3"), "46,53%"
                      //                        )
                      //                      )
                      ////                      tr(
                      ////                        th(attr("scope") := "col", "/argon/charts.html"),
                      ////                        td("3,513"),
                      ////                        td("294"),
                      ////                        td(
                      ////                          i(cls := "fas fa-arrow-down text-warning mr-3"), "36,49%"
                      ////                        )
                      ////                      ),
                      ////                      tr(
                      ////                        th(attr("scope") := "col", "/argon/tables.html"),
                      ////                        td("2,050"),
                      ////                        td("147"),
                      ////                        td(
                      ////                          i(cls := "fas fa-arrow-up text-success mr-3"), "50,87%"
                      ////                        )
                      ////                      ),
                      ////                      tr(
                      ////                        th(attr("scope") := "col", "/argon/profile.html"),
                      ////                        td("1,795"),
                      ////                        td("190"),
                      ////                        td(
                      ////                          i(cls := "fas fa-arrow-down text-danger mr-3"), "46,53%"
                      ////                        )
                      ////                      )
                      //                    )
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
  import akka.http.scaladsl.Http
  import akka.http.scaladsl.model._
  import akka.http.scaladsl.server.Directives._
  // JWT
  val superSecretPasswordDb = Map(
    "admin" -> "admin",
    "daniel" -> "Rockthejvm1!"
  )

  val algorithm = JwtAlgorithm.HS256
  val secretKey = "encry"

  def checkPassword(username: String, password: String): Boolean =
    superSecretPasswordDb.contains(username) && superSecretPasswordDb(username) == password

  def createToken(username: String, expirationPeriodInDays: Int): String = {
    val claims = JwtClaim(
      expiration = Some(System.currentTimeMillis() / 1000 + TimeUnit.DAYS.toSeconds(expirationPeriodInDays)),
      issuedAt = Some(System.currentTimeMillis() / 1000),
      issuer = Some("rockthejvm.com"),
      subject = Some(username)
    )

    JwtSprayJson.encode(claims, secretKey, algorithm) // JWT string
  }

  def isTokenExpired(token: String): Boolean = JwtSprayJson.decode(token, secretKey, Seq(algorithm)) match {
    case Success(claims) => claims.expiration.getOrElse(0L) < System.currentTimeMillis() / 1000
    case Failure(_) => true
  }

  def isTokenValid(token: String): Boolean = JwtSprayJson.isValid(token, secretKey, Seq(algorithm))

  def authRoute(onSuccess: Route) = {
    optionalCookie("JWT") {
      case Some(token) =>
        if (isTokenValid(token.value)) {
          if (isTokenExpired(token.value)) {
            complete(HttpResponse(status = StatusCodes.Unauthorized, entity = "Token expired."))
          } else {
            onSuccess
          }
        } else {
          complete(HttpResponse(status = StatusCodes.Unauthorized, entity = "Token is invalid, or has been tampered with."))
        }
      case _ => complete(HttpResponse(status = StatusCodes.Unauthorized, entity = "No token provided!"))
    }
  }

  def extractIp(pingedRoute: Route) = extractClientIP { ip =>
    if (ip.toOption.map(x => x.getHostAddress).getOrElse("unknown") == "127.0.0.1") {
      pingedRoute
  } else reject(ValidationRejection("Access denied"))

  }

}
