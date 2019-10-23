package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Route
import akka.pattern._
import com.typesafe.scalalogging.StrictLogging
import encry.api.http.DataHolderForApi.{GetAllInfoHelper, GetMinerStatus, StartMiner, StopMiner}
import encry.local.miner.Miner.MinerStatus
import scalatags.Text.all.{div, span, td, _}
import encry.settings.{NodeSettings, RESTApiSettings}
import io.circe.Decoder.Result
import io.circe.Json
import scalatags.{Text, generic}
import io.circe.parser
import io.circe.generic.auto._
import scalatags.text.Builder

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
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
                   uptime: Int,
                   storage: String,
                   isConnectedWithKnownPeers: Boolean,
                   isMining: Boolean,
                   knownPeers: Seq[String]
                  )

case class WebRoute(override val settings: RESTApiSettings, nodeSettings: NodeSettings, dataHolder: ActorRef)(
  implicit val context: ActorRefFactory
) extends EncryBaseApiRoute with StrictLogging {


  def statusF = (dataHolder ? GetMinerStatus).mapTo[MinerStatus]

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
        link(href := "/argon/assets/img/brand/favicon.png", rel := "icon", tpe := "image/png"),
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
            a(cls := "navbar-brand pt-0", href := "https://www.w3schools.com",
              img(src := "argon/assets/img/brand/blue.png", cls := "navbar-brand-img", alt := "...")
            ),
            // User
            ul(cls := "nav align-items-center d-md-none",
              li(cls := "nav-item dropdown",
                a(cls := "nav-link nav-link-icon", href := "#", role := "button", data("toggle") := "dropdown", aria.haspopup := "true", aria.expanded := "false",
                  i(cls := "ni ni-bell-55")
                ),
                div(cls := "dropdown-menu dropdown-menu-arrow dropdown-menu-right", aria.labelledby := "navbar-default_dropdown_1",
                  a(cls := "dropdown-item", href := "#", "Action"),
                  a(cls := "dropdown-item", href := "#", "Another action"),
                  div(cls := "dropdown-divider"),
                  a(cls := "dropdown-item", href := "#", "Something else here")
                )
              ),
              li(cls := "nav-item dropdown",
                a(cls := "nav-link", href := "#", role := "button", data("toggle") := "dropdown", aria.haspopup := "true", aria.expanded := "false",
                  div(cls := "media align-items-center",
                    span(cls := "avatar avatar-sm rounded-circle",
                      img(alt := "Image placeholder", src := "argon/assets/img/theme/team-1-800x800.jpg")
                    )
                  )
                ),
                div(cls := "dropdown-menu dropdown-menu-arrow dropdown-menu-right",
                  div(cls := " dropdown-header noti-title",
                    h6(cls := "text-overflow m-0", "Welcome!")
                  ),
                  a(href := "argon/examples/profile.html", cls := "dropdown-item",
                    i(cls := "ni ni-single-02"),
                    span("My profile")
                  ),
                  a(href := "argon/examples/profile.html", cls := "dropdown-item",
                    i(cls := "ni ni-settings-gear-65"),
                    span("Settings")
                  ),
                  a(href := "argon/examples/profile.html", cls := "dropdown-item",
                    i(cls := "ni ni-calendar-grid-58"),
                    span("Activity")
                  ),
                  a(href := "argon/examples/profile.html", cls := "dropdown-item",
                    i(cls := "ni ni-support-16"),
                    span("Support")
                  ),
                  div(cls := "dropdown-divider"),
                  a(href := "#!", cls := "dropdown-item",
                    i(cls := "ni ni-user-run"),
                    span("Logout")
                  )
                )
              )
            ),
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
                  a(cls := "nav-link", href := "./wallet",
                    i(cls := "ni ni-planet text-blue"), "Wallet"
                  )
                ),
                div(cls := "dropdown",
                  a(cls := "nav-link", href := "#", role := "button", data("toggle") := "dropdown", aria.haspopup := "true", aria.expanded := "false",
                    i(cls := "ni ni-bullet-list-67 text-orange"), "Peers"
                  ),
                  div(cls := "dropdown-menu dropdown-menu-right dropdown-menu-arrow",
                    a(cls := "dropdown-item", href := "./peers", "All peers"),
                    a(cls := "dropdown-item", href := "./connectedPeers", "Connected peers"),
                    a(cls := "dropdown-item", href := "./bannedPeers", "Banned peers")
                  )
                ),
                li(cls := "nav-item",
                  a(cls := "nav-link", href := "argon/examples/profile.html",
                    i(cls := "ni ni-single-02 text-yellow"), "User profile"
                  )
                ),
                li(cls := "nav-item",
                  a(cls := "nav-link", href := "argon/examples/tables.html",
                    i(cls := "ni ni-bullet-list-67 text-red"), "Tables"
                  )
                ),
                li(cls := "nav-item",
                  a(cls := "nav-link", href := "argon/examples/login.html",
                    i(cls := "ni ni-key-25 text-info"), "Start Mining"
                  )
                ),
                li(cls := "nav-item",
                  a(cls := "nav-link", href := "argon/examples/register.html",
                    i(cls := "ni ni-circle-08 text-pink"), "Register"
                  )
                )
              ),
              // Divider
              hr(cls := "my-3"),
              // Heading
              h6(cls := "navbar-heading text-muted", "Documentation"),
              // Navigation
              ul(cls := "navbar-nav mb-md-3",
                li(cls := "nav-item",
                  a(cls := "nav-link", href := "https://demos.creative-tim.com/argon-dashboard/docs/getting-started/overview.html",
                    i(cls := "ni ni-spaceship"), "Getting started"
                  )
                ),
                li(cls := "nav-item",
                  a(cls := "nav-link", href := "https://demos.creative-tim.com/argon-dashboard/docs/foundation/colors.html",
                    i(cls := "ni ni-palette"), "Foundation"
                  )
                ),
                li(cls := "nav-item",
                  a(cls := "nav-link", href := "https://demos.creative-tim.com/argon-dashboard/docs/components/alerts.html",
                    i(cls := "ni ni-ui-04"), "Components"
                  )
                )
              )
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
              // Form
              form(cls := "navbar-search navbar-search-dark form-inline mr-3 d-none d-md-flex ml-lg-auto",
                div(cls := "form-group mb-0",
                  div(cls := "input-group input-group-alternative",
                    div(cls := "input-group-prepend",
                      span(cls := "input-group-text",
                        i(cls := "fas fa-search")
                      )
                    ),
                    input(cls := "form-control", placeholder := "Search", tpe := "text")
                  )
                )
              ),
              // User
              ul(cls := "navbar-nav align-items-center d-none d-md-flex",
                li(cls := "nav-item dropdown",
                  a(cls := "nav-link pr-0", href := "#", role := "button", data("toggle") := "dropdown", aria.haspopup := "true", aria.expanded := "false",
                    div(cls := "media align-items-center",
                      span(cls := "avatar avatar-sm rounded-circle",
                        img(alt := "Image placeholder", src := "argon/assets/img/theme/team-4-800x800.jpg")
                      ),
                      div(cls := "media-body ml-2 d-none d-lg-block",
                        span(cls := "mb-0 text-sm  font-weight-bold", "Jessica Jones")
                      )
                    )
                  ),
                  div(cls := "dropdown-menu dropdown-menu-arrow dropdown-menu-right",
                    div(cls := " dropdown-header noti-title",
                      h6(cls := "text-overflow m-0", "Welcome!")
                    ),
                    a(href := "argon/examples/profile.html", cls := "dropdown-item",
                      i(cls := "ni ni-single-02"),
                      span("My profile")
                    ),
                    a(href := "argon/examples/profile.html", cls := "dropdown-item",
                      i(cls := "ni ni-settings-gear-65"),
                      span("Settings")
                    ),
                    a(href := "argon/examples/profile.html", cls := "dropdown-item",
                      i(cls := "ni ni-calendar-grid-58"),
                      span("Activity")
                    ),
                    a(href := "argon/examples/profile.html", cls := "dropdown-item",
                      i(cls := "ni ni-support-16"),
                      span("Support")
                    ),
                    div(cls := "dropdown-divider"),
                    a(href := "#!", cls := "dropdown-item",
                      i(cls := "ni ni-user-run"),
                      span("Logout")
                    )
                  )
                )
              )
            )
          ),
          // Header

          div(cls := "header bg-gradient-primary pb-8 pt-5 pt-md-8",
            div(cls := "container-fluid",
              div(cls := "header-body"),
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
                        tr(
                          th(attr("scope") := "row", "Name"),
                          td(attr("scope") := "row", nodeInfo.right.get.name)
                        ),
                        tr(th(attr("scope") := "row", "State Type"),
                          td(attr("scope") := "row", nodeInfo.right.get.stateType)
                        ),
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
                        tr(th(attr("scope") := "row", "unconfirmedCount"),
                          td(attr("scope") := "row", nodeInfo.right.get.unconfirmedCount)
                        ),
                        tr(th(attr("scope") := "row", "previousFullHeaderId"),
                          td(attr("scope") := "row", nodeInfo.right.get.previousFullHeaderId)
                        )
                        ,
                        tr(th(attr("scope") := "row", "fullHeight"),
                          td(attr("scope") := "row", nodeInfo.right.get.fullHeight)
                        ),
                        tr(th(attr("scope") := "row", "headersHeight"),
                          td(attr("scope") := "row", nodeInfo.right.get.headersHeight)
                        ),
                        tr(th(attr("scope") := "row", "stateVersion"),
                          td(attr("scope") := "row", nodeInfo.right.get.stateVersion)
                        ),
                        tr(th(attr("scope") := "row", "uptime"),
                          td(attr("scope") := "row", nodeInfo.right.get.uptime)
                        ),
                        tr(th(attr("scope") := "row", "storage"),
                          td(attr("scope") := "row", nodeInfo.right.get.storage)
                        ),
                        tr(th(attr("scope") := "row", "isConnectedWithKnownPeers"),
                          td(attr("scope") := "row", nodeInfo.right.get.isConnectedWithKnownPeers.toString())
                        ),
                        tr(th(attr("scope") := "row", "isMining"),
                          td(attr("scope") := "row", nodeInfo.right.get.isMining.toString())
                        ),
                        tr(th(attr("scope") := "row", "knownPeers"),
                          td(attr("scope") := "row", nodeInfo.right.get.knownPeers)
                        )
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

//  override def route: Route = (path("web") & get) {
//    //    complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, woqdl.render))
//    onComplete(currentInfoF) {
//      case Success(value) => complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, woqdl(value._1, value._2).render))
//    }
//  }

  override def route: Route = (path("web") & get) {
    //    complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, woqdl.render))
    onComplete(currentInfoF) {
      case Success(value) => complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, woqdl(value._1, value._2).render))
    }
  }


}
