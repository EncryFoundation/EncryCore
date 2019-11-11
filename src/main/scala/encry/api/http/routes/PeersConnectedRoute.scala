package encry.api.http.routes

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.{Route, ValidationRejection}
import akka.pattern._
import com.typesafe.scalalogging.StrictLogging
import encry.api.http.DataHolderForApi.{ConnectedPeersConnectionHelper, GetAllInfoHelper, GetAllPeers, GetConnectedPeersHelper, GetConnections, GetViewCreateKey, GetViewGetBalance, GetViewPrintPubKeys, StartMiner, StopMiner}
import encry.api.http.routes.PeersApiRoute.PeerInfoResponse
import encry.local.miner.Miner.MinerStatus
import encry.network.ConnectedPeersCollection
import encry.network.ConnectedPeersCollection.PeerInfo
import scalatags.Text.all.{div, span, td, _}
import encry.settings.{NodeSettings, RESTApiSettings}
import io.circe.Json
import scalatags.{Text, generic}
import io.circe.parser
import io.circe.generic.auto._
import org.encryfoundation.common.crypto.PrivateKey25519
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.utils.Algos

import scala.concurrent.{Await, Future}
import scala.language.implicitConversions
import scala.util.{Failure, Success}


case class PeersConnectedRoute(override val settings: RESTApiSettings, nodeSettings: NodeSettings, dataHolder: ActorRef)(
  implicit val context: ActorRefFactory
) extends EncryBaseApiRoute with StrictLogging {


  def walletF: Future[Map[String, Amount]] = (dataHolder ? GetViewGetBalance).mapTo[Map[String, Amount]]

  def pubKeysF: Future[List[String]] = (dataHolder ? GetViewPrintPubKeys).mapTo[List[String]]

  def connectedPeers = (dataHolder ? GetConnections).mapTo[ConnectedPeersCollection]

  def info: Future[ConnectedPeersCollection] = for {
    peerAll <- connectedPeers
  } yield peerAll

  def infoU = for {
    peerAll <- connectedPeers
  } yield println("AAA = " + peerAll)

  def infoHelper: Future[Json] = (dataHolder ? GetAllInfoHelper).mapTo[Json]

  def peerScript(peersR: ConnectedPeersCollection): Text.TypedTag[String] = {

    html(
      scalatags.Text.all.head(
        meta(charset := "utf-8"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1, shrink-to-fit=no"),
        meta(name := "description", content := "Start your development with a Dashboard for Bootstrap 4."),
        meta(name := "author", content := "Creative Tim"),
        script(
          raw("""function validateForm() {
  var y = document.forms["myForm"]["fee"].value;
  var z = document.forms["myForm"]["amount"].value;
  if (y == "") {
     alert("Address must be filled out");
     return false;
   }
 if (z == "") {
    alert("Port must be filled out");
    return false;
  }
}""")
        ),
        script(
          raw("""function wallet(){
                 var b = document.forms["myForm"]["fee"].value;
                 var x = document.forms["myForm"]["amount"].value;
                    var request = new XMLHttpRequest();
                    request.open('POST', "http://localhost:9051/peers/add", true);
                //    request.setRequestHeader('content-type', 'application/json');
                    request.send(b.toString() + ':' + x.toString());
                     window.alert("Info about adding peer was sent to node");
                    setTimeout(location.reload.bind(location), 1500);

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

            )
          ),
          // Header

          div(cls := "header bg-gradient-primary pb-8 pt-5 pt-md-8",
            div(cls := "container-fluid",
              div(cls := "header-body"),
              div(cls := "row",

              )
            )
          ),
          // Page content
          div(cls := "container-fluid mt--7",
            div(cls := "row mt-5",
              div(cls := "col-xl-12 mb-5 mb-xl-0",
                div(cls := "card shadow",
                  div(cls := "card-header border-0",
                    div(cls := "row align-items-center",
                      div(cls := "col",
                        h3(cls := "mb-0", "Connected peers")
                      ),
                      div(cls := "col-md-4",
                        div(cls := "row",
                          div(cls := "col",
                            button(tpe := "button", cls := "btn btn-block btn-primary mb-3", data("toggle") := "modal", data("target") := "#modal-form", "Add peer"),
                          )
                        ),
                          div(cls := "modal fade", id := "modal-form", tabindex := "-1", role := "dialog", aria.labelledby := "modal-form", aria.hidden := "true",
                          div(cls := "modal-dialog modal- modal-dialog-centered modal-sm", role := "document",
                            div(cls := "modal-content",
                              div(cls := "modal-body p-0",
                                div(cls := "card bg-secondary shadow border-0",
                                  div(cls := "card-body px-lg-5 py-lg-5",
                                    form(role := "form", onsubmit:="return validateForm()", id:="myForm",

                                      div(cls := "form-group",
                                        div(cls := "input-group input-group-alternative mb-3",
                                          div(cls := "input-group-prepend",
                                            span(cls := "input-group-text",
                                              i(cls := "ni ni-money-coins")
                                            )
                                          ),
                                          input(cls := "form-control", id:="bibo", name:="fee", placeholder := "Address", tpe := "text"),
//                                          script(
//                                            raw(
//                                              """
//                                                 function setInputFilter(textbox, inputFilter) {
//      ["input", "keydown", "keyup", "mousedown", "mouseup", "select", "contextmenu", "drop"].forEach(function(event) {
//        textbox.oldValue = "";
//         textbox.addEventListener(event, function() {
//       if (inputFilter(this.value)) {
//         this.oldValue = this.value;
//         this.oldSelectionStart = this.selectionStart;
//         this.oldSelectionEnd = this.selectionEnd;
//       } else if (this.hasOwnProperty("oldValue")) {
//         this.value = this.oldValue;
//         this.setSelectionRange(this.oldSelectionStart, this.oldSelectionEnd);
//       }
//     });
//   });
// }
//              setInputFilter(document.getElementById("bibo"), function(value) {
//                return /^-?\d*[.,]?\d*$/.test(value);
//              });
//            """.stripMargin)
//                                          )
                                        )

                                      ),
                                      div(cls := "form-group",
                                        div(cls := "input-group input-group-alternative",
                                          div(cls := "input-group-prepend",
                                            span(cls := "input-group-text",
                                              i(cls := "ni ni-credit-card")
                                            )
                                          ),
                                          input(cls := "form-control", id:="bobo", name:="amount", placeholder := "Port"),
                                          script(
                                            raw(
                                              """
                                                 function setInputFilter(textbox, inputFilter) {
      ["input", "keydown", "keyup", "mousedown", "mouseup", "select", "contextmenu", "drop"].forEach(function(event) {
        textbox.oldValue = "";
         textbox.addEventListener(event, function() {
       if (inputFilter(this.value)) {
         this.oldValue = this.value;
         this.oldSelectionStart = this.selectionStart;
         this.oldSelectionEnd = this.selectionEnd;
       } else if (this.hasOwnProperty("oldValue")) {
         this.value = this.oldValue;
         this.setSelectionRange(this.oldSelectionStart, this.oldSelectionEnd);
       }
     });
   });
 }
              setInputFilter(document.getElementById("bobo"), function(value) {
                return /^\d*$/.test(value) && (value === "" || parseInt(value) <= 10000);
              });
            """.stripMargin)
                                          ),
                                        )
                                      ),

                                      div(cls := "text-center",
                                        button(tpe := "button", onclick:="wallet()", cls := "btn btn-primary mt-4", "Add")
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  ),
                  div(cls := "table-responsive",
                    // Projects table
                    table(cls := "table align-items-center table-flush", id:="myTable",
                      thead(cls := "thead-light",
                        tr(
                          th(attr("scope") := "row", "Name"),
                          th(attr("scope") := "row", "Ip"),
                          th(attr("scope") := "row", "Connection type"),
                          th(attr("scope") := "row", "History comparison"),
                          th(attr("scope") := "row", "Last uptime"),
                          th(attr("scope") := "row", "Priority status")
                        )
                      ),
                      tbody(

                        (for (p <- peersR.peers) yield {
                          tr(
                            th(p._1.getHostName
                            ),
                            th(p._2.connectedPeer.socketAddress.getAddress.getHostAddress
                            ),
                            th(p._2.connectionType.toString
                            ),
                            th(p._2.historyComparisonResult.toString
                            ),
                            th(p._2.lastUptime.time
                            ),
                            th(p._2.peerPriorityStatus.toString
                            )
                          )
                        }).toSeq: _*
                      )
                    )
                  )
                )
              )
            )
            ,
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

  override def route: Route = (path("connectedPeers") & get) {
    extractClientIP { ip =>
      if (ip.toOption.map(x => x.getHostAddress).getOrElse("unknown") == "127.0.0.1") {
        //        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, walletScript.render))
        onComplete(info) {
          case Success(info) =>
            println(info)
            complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, peerScript(info).render))
          case Failure(e) => complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, peerScript(ConnectedPeersCollection()).render))
        }
      } else reject(ValidationRejection("Restricted!"))
    }
  }

}
