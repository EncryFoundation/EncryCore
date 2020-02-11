package encry.api.http.routes

import java.net.InetSocketAddress
import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Route
import akka.pattern._
import com.typesafe.scalalogging.StrictLogging
import encry.api.http.DataHolderForApi.{GetAllPeers, GetBlockChainSync}
import encry.settings.{NodeSettings, RESTApiSettings}
import io.circe.generic.auto._
import scalatags.Text
import scalatags.Text.all.{div, span, _}
import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.{Failure, Success}

case class PeersRoute(settings: RESTApiSettings, nodeSettings: NodeSettings, dataHolder: ActorRef)(
  implicit val context: ActorRefFactory
) extends EncryBaseApiRoute with StrictLogging {

  def peersAllF: Future[Seq[InetSocketAddress]] = (dataHolder ? GetAllPeers).mapTo[Seq[InetSocketAddress]]

  def syncIsDoneF: Future[Boolean] = (dataHolder ? GetBlockChainSync).mapTo[Boolean]

  def info: Future[(Seq[InetSocketAddress], Boolean)] = for {
    peers <- peersAllF
    sync <- syncIsDoneF
  } yield (peers, sync)

  def peerScript(peers: Seq[InetSocketAddress], sync: Boolean): Text.TypedTag[String] = {

    html(
      scalatags.Text.all.head(
        meta(charset := "utf-8"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1, shrink-to-fit=no"),
        meta(name := "description", content := "Start your development with a Dashboard for Bootstrap 4."),
        meta(name := "author", content := "Creative Tim"),

        tag("title")(
          "Encry Foundation"
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
                    a(href := "/web",
                      img(src := "argon/assets/img/brand/encry-logo.png")
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
                        h3(cls := "mb-0", "All peers")
                      )
                    )
                  ),
                  div(cls := "table-responsive",
                    // Projects table
                    table(cls := "table align-items-center table-flush", id:="myTable",
                      thead(cls := "thead-light",
                        tr(
                          th(attr("scope") := "row", "Address")
                        )
                      ),
                      tbody(
                        for (p <- peers) yield {
                          tr(
                            th(p.getHostName
                            )
                          )
                        }
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

  override def route: Route = (path("allPeers") & get) {
    WebRoute.authRoute(
        onComplete(info) {
          case Success(value) =>
            complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, peerScript(value._1, value._2).render))
          case Failure(_) => complete("Couldn't load page with peers cause of inner system is overloaded. Try it later!")
        },
      settings
    )
  }

}
