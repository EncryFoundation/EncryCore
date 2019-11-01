package encry.api.http.routes

import java.util.concurrent.TimeUnit

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse, StatusCodes}
import akka.http.scaladsl.server.{Route, ValidationRejection}
import akka.pattern._
import com.typesafe.scalalogging.StrictLogging
import encry.api.http.DataHolderForApi.{GetAllInfoHelper, GetViewCreateKey, GetViewGetBalance, GetViewPrintPubKeys, StartMiner, StopMiner}
import encry.local.miner.Miner.MinerStatus
import scalatags.Text.all.{div, span, td, _}
import encry.settings.{NodeSettings, RESTApiSettings}
import io.circe.Json
import pdi.jwt.{JwtAlgorithm, JwtClaim, JwtSprayJson}
import scalatags.{Text, generic}
import spray.json.DefaultJsonProtocol
//import io.circe.parser
//import io.circe.generic.auto._
import org.encryfoundation.common.crypto.PrivateKey25519
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.utils.Algos

import scala.concurrent.{Await, Future}
import scala.language.implicitConversions
import scala.util.{Failure, Success}


case class WalletRoute(override val settings: RESTApiSettings, nodeSettings: NodeSettings, dataHolder: ActorRef)(
  implicit val context: ActorRefFactory
) extends EncryBaseApiRoute with StrictLogging {


  def walletF: Future[Map[String, Amount]] = (dataHolder ? GetViewGetBalance).mapTo[Map[String, Amount]]

  def pubKeysF: Future[List[String]] = (dataHolder ? GetViewPrintPubKeys).mapTo[List[String]]

  def info: Future[(Map[String, Amount], List[String])] = for {
    wallet <- walletF
    pubKeys <- pubKeysF
  } yield (wallet, pubKeys)

  def infoHelper: Future[Json] = (dataHolder ? GetAllInfoHelper).mapTo[Json]

  def walletScript(balances: Map[String, Amount], pubKeysList: List[String]): Text.TypedTag[String] = {

    val IntrinsicTokenId: Array[Byte] = Algos.hash("intrinsic_token")

    val EttTokenId: String = Algos.encode(IntrinsicTokenId)

    val aaa = balances.values.head.toString
    html(
      scalatags.Text.all.head(
        meta(charset := "utf-8"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1, shrink-to-fit=no"),
        meta(name := "description", content := "Start your development with a Dashboard for Bootstrap 4."),
        meta(name := "author", content := "Creative Tim"),
        script(
          raw(
            """function validateForm2() {
  var y = document.forms["myForm2"]["fee"].value;
  var z = document.forms["myForm2"]["data`"].value;
  if (y == "") {
     alert("Fee must be filled out");
     return false;
   }
 if (z == "") {
    alert("Data must be filled out");
    return false;
  }
}""")
        ),
        script(
          raw(
            """function validateForm() {
  var x = document.forms["myForm"]["addr"].value;
  var y = document.forms["myForm"]["fee"].value;
  var z = document.forms["myForm"]["amount"].value;
  if (x == "") {
    alert("Address must be filled out");
    return false;
  }
  if (y == "") {
     alert("Fee must be filled out");
     return false;
   }
 if (z == "") {
    alert("Amount must be filled out");
    return false;
  }
}""")
        ),
        script(
          raw(
            """function validateForm1() {
  var y = document.forms["myForm1"]["fee"].value;
  var z = document.forms["myForm1"]["amount"].value;
  if (y == "") {
     alert("Fee must be filled out");
     return false;
   }
 if (z == "") {
    alert("Amount must be filled out");
    return false;
  }
}""")
        ),
        script(
          raw(
            """function wallet(){
                 var a = document.forms["myForm"]["addr"].value;
                 var b = document.forms["myForm"]["fee"].value;
                 var x = document.forms["myForm"]["amount"].value;
                    var request = new XMLHttpRequest();
                    request.open('GET', "http://localhost:9051/wallet/transfer?addr="+a+"&fee="+b+"&amount="+x);
                //    request.setRequestHeader('content-type', 'application/json');
                    request.send();
                     window.alert("Transaction has been sent successfully");
                    setTimeout(location.reload.bind(location), 3000);

                  }""")
        ),
        script(
          raw(
            """function token(){
                 var b = document.forms["myForm1"]["fee"].value;
                 var x = document.forms["myForm1"]["amount"].value;
                    var request = new XMLHttpRequest();
                    request.open('GET', "http://localhost:9051/wallet/createToken?fee="+b+"&amount="+x);
                //    request.setRequestHeader('content-type', 'application/json');
                    request.send();
                     window.alert("Token has been created successfully");
                    setTimeout(location.reload.bind(location), 3000);

                  }""")
        ),
        script(
          raw(
            """function dataTx(){
                 var b = document.forms["myForm2"]["fee"].value;
                 var x = document.forms["myForm2"]["data"].value;
                    var request = new XMLHttpRequest();
                    request.open('GET', "http://localhost:9051/wallet/data?fee="+b+"&data="+x);
                //    request.setRequestHeader('content-type', 'application/json');
                    request.send();
                     window.alert("Data has been created successfully");
                    setTimeout(location.reload.bind(location), 3000);

                  }""")
        ),
        script(
          raw(
            """function keyCreate() {
                   var request = new XMLHttpRequest();
                     request.open('GET', "http://localhost:9051/wallet/createKey");
                 //    request.setRequestHeader('content-type', 'application/json');
                     request.send();
                      window.alert("Key created successfully");
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
                        h3(cls := "mb-0", "Tokens")
                      ),
                      div(cls := "col-md-4",
                        div(cls := "row",
                          div(cls := "col",
                            button(tpe := "button", cls := "btn btn-block btn-primary mb-3", data("toggle") := "modal", data("target") := "#modal-form", "Transfer"),
                          ),
                          div(cls := "col",
                            button(tpe := "button", onclick := "keyCreate()", cls := "btn btn-block btn-primary mb-3", "Create key")
                          ),
                          div(cls := "col",
                            button(tpe := "button", cls := "btn btn-block btn-primary mb-3", data("toggle") := "modal", data("target") := "#modal-form2", "Create token")
                          ),
                          div(cls := "col",
                            button(tpe := "button", cls := "btn btn-block btn-primary mb-3", data("toggle") := "modal", data("target") := "#modal-form3", "Create data tx")
                          )
                        ),

                        //Create Data Tx Form
                        div(cls := "modal fade", id := "modal-form3", tabindex := "-1", role := "dialog", aria.labelledby := "modal-form", aria.hidden := "true",
                          div(cls := "modal-dialog modal- modal-dialog-centered modal-sm", role := "document",
                            div(cls := "modal-content",
                              div(cls := "modal-body p-0",
                                div(cls := "card bg-secondary shadow border-0",
                                  div(cls := "card-body px-lg-5 py-lg-5",
                                    form(role := "form", onsubmit := "return validateForm2()", id := "myForm2",

                                      div(cls := "form-group",
                                        div(cls := "input-group input-group-alternative mb-3",
                                          div(cls := "input-group-prepend",
                                            span(cls := "input-group-text",
                                              i(cls := "ni ni-money-coins")
                                            )
                                          ),
                                          input(cls := "form-control", id := "bibo2", name := "fee", placeholder := "Fee (min = 0)", tpe := "text"),
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
              setInputFilter(document.getElementById("bibo2"), function(value) {
              var x = document.getElementById("myTable").rows[1].cells[1].innerHTML;
                return /^\d*$/.test(value) && (value === "" || parseInt(value) <= x);
              });
            """.stripMargin)
                                          )
                                        )

                                      ),
                                      div(cls := "form-group",
                                        div(cls := "input-group input-group-alternative",
                                          div(cls := "input-group-prepend",
                                            span(cls := "input-group-text",
                                              i(cls := "ni ni-credit-card")
                                            )
                                          ),
                                          input(cls := "form-control", id := "bobo2", name := "data", placeholder := "Data"),
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
              setInputFilter(document.getElementById("bobo2"), function(value) {
              var x = document.getElementById("myTable").rows[1].cells[1].innerHTML;
                return /^\d*$/.test(value) && (value === "" || parseInt(value) <= x);
              });
            """.stripMargin)
                                          ),
                                        )
                                      ),

                                      div(cls := "text-center",
                                        button(tpe := "button", onclick := "dataTx()", cls := "btn btn-primary mt-4", "Send data tx")
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        ),
                        //
                        //Create Token Form
                        div(cls := "modal fade", id := "modal-form2", tabindex := "-1", role := "dialog", aria.labelledby := "modal-form", aria.hidden := "true",
                          div(cls := "modal-dialog modal- modal-dialog-centered modal-sm", role := "document",
                            div(cls := "modal-content",
                              div(cls := "modal-body p-0",
                                div(cls := "card bg-secondary shadow border-0",
                                  div(cls := "card-body px-lg-5 py-lg-5",
                                    form(role := "form", onsubmit := "return validateForm1()", id := "myForm1",

                                      div(cls := "form-group",
                                        div(cls := "input-group input-group-alternative mb-3",
                                          div(cls := "input-group-prepend",
                                            span(cls := "input-group-text",
                                              i(cls := "ni ni-money-coins")
                                            )
                                          ),
                                          input(cls := "form-control", id := "bibo1", name := "fee", placeholder := "Fee (min = 0)", tpe := "text"),
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
              setInputFilter(document.getElementById("bibo1"), function(value) {
              var x = document.getElementById("myTable").rows[1].cells[1].innerHTML;
                return /^\d*$/.test(value) && (value === "" || parseInt(value) <= x);
              });
            """.stripMargin)
                                          )
                                        )

                                      ),
                                      div(cls := "form-group",
                                        div(cls := "input-group input-group-alternative",
                                          div(cls := "input-group-prepend",
                                            span(cls := "input-group-text",
                                              i(cls := "ni ni-credit-card")
                                            )
                                          ),
                                          input(cls := "form-control", id := "bobo1", name := "amount", placeholder := "Amount"),
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
              setInputFilter(document.getElementById("bobo1"), function(value) {
              var x = document.getElementById("myTable").rows[1].cells[1].innerHTML;
                return /^\d*$/.test(value) && (value === "" || parseInt(value) <= x);
              });
            """.stripMargin)
                                          ),
                                        )
                                      ),

                                      div(cls := "text-center",
                                        button(tpe := "button", onclick := "token()", cls := "btn btn-primary mt-4", "Create token")
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        ),
                        //
                        //Create Transfer Form
                        div(cls := "modal fade", id := "modal-form", tabindex := "-1", role := "dialog", aria.labelledby := "modal-form", aria.hidden := "true",
                          div(cls := "modal-dialog modal- modal-dialog-centered modal-sm", role := "document",
                            div(cls := "modal-content",
                              div(cls := "modal-body p-0",
                                div(cls := "card bg-secondary shadow border-0",
                                  div(cls := "card-body px-lg-5 py-lg-5",
                                    form(role := "form", onsubmit := "return validateForm()", id := "myForm",

                                      div(cls := "form-group",
                                        div(cls := "input-group input-group-alternative mb-3",
                                          div(cls := "input-group-prepend",
                                            span(cls := "input-group-text",
                                              i(cls := "ni ni-email-83")
                                            )
                                          ),
                                          input(cls := "form-control", id := "babo", name := "addr", placeholder := "Address", tpe := "text")
                                        )
                                      ),
                                      div(cls := "form-group",
                                        div(cls := "input-group input-group-alternative mb-3",
                                          div(cls := "input-group-prepend",
                                            span(cls := "input-group-text",
                                              i(cls := "ni ni-money-coins")
                                            )
                                          ),
                                          input(cls := "form-control", id := "bibo", name := "fee", placeholder := "Fee (min = 0)", tpe := "text"),
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
              setInputFilter(document.getElementById("bibo"), function(value) {
              var x = document.getElementById("myTable").rows[1].cells[1].innerHTML;
                return /^\d*$/.test(value) && (value === "" || parseInt(value) <= x);
              });
            """.stripMargin)
                                          )
                                        )

                                      ),
                                      div(cls := "form-group",
                                        div(cls := "input-group input-group-alternative",
                                          div(cls := "input-group-prepend",
                                            span(cls := "input-group-text",
                                              i(cls := "ni ni-credit-card")
                                            )
                                          ),
                                          input(cls := "form-control", id := "bobo", name := "amount", placeholder := "Amount"),
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
              var x = document.getElementById("myTable").rows[1].cells[1].innerHTML;
                return /^\d*$/.test(value) && (value === "" || parseInt(value) <= x);
              });
            """.stripMargin)
                                          ),
                                        )
                                      ),
                                      div(cls := "form-group",
                                        select(cls := "form-control",
                                          for (coinIds <- balances.keys.toList) yield {
                                            option(value := coinIds, if (coinIds == EttTokenId) "ETT" else coinIds)
                                          }
                                        )
                                      ),
                                      div(cls := "text-center",
                                        button(tpe := "button", onclick := "wallet()", cls := "btn btn-primary mt-4", "Send Money")
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
                  //
                  div(cls := "table-responsive",
                    // Projects table
                    table(cls := "table align-items-center table-flush", id := "myTable",
                      thead(cls := "thead-light",
                        tr(
                          th(attr("scope") := "row", "TokenId"),
                          th(attr("scope") := "row", "Balance")
                        )
                      ),
                      tbody(

                        (for (b <- balances) yield {
                          val tknStr = b._1 match {
                            case encry if b._1 == EttTokenId => "ETT"
                            case _ => b._1
                          }
                          tr(
                            th(tknStr
                            ),
                            th(b._2
                            )
                          )
                        }).toSeq: _*
                      )
                    )
                  )
                )
              )
            ),

            div(cls := "row mt-5",
              div(cls := "col-xl-12 mb-5 mb-xl-0",
                div(cls := "card shadow",
                  div(cls := "card-header border-0",
                    div(cls := "row align-items-center",
                      div(cls := "col",
                        h3(cls := "mb-0", "Public keys")
                      )
                    )
                  ),
                  div(cls := "table-responsive",
                    // Projects table
                    table(cls := "table align-items-center table-flush",
                      thead(cls := "thead-light",
                        tr(
                          th(attr("scope") := "col", "Key")
                        )
                      ),
                      tbody(
                        for (p <- pubKeysList) yield {
                          tr(
                            th(attr("scope") := "row", p)
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

  //  def authRoute(onSuccess: HttpEntity) = {
  //    optionalCookie("JWT") {
  //      case Some(token) =>
  //        if (isTokenValid(token.value)) {
  //          if (isTokenExpired(token.value)) {
  //            complete(HttpResponse(status = StatusCodes.Unauthorized, entity = "Token expired."))
  //          } else {
  //            complete(onSuccess)
  //          }
  //        } else {
  //          complete(HttpResponse(status = StatusCodes.Unauthorized, entity = "Token is invalid, or has been tampered with."))
  //        }
  //      case _ => complete(HttpResponse(status = StatusCodes.Unauthorized, entity = "No token provided!"))
  //    }
  //  }

  override def route: Route = (path("webWallet") & get) {
    //        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, walletScript.render))
    WebRoute.extractIp(
      WebRoute.authRoute(
        onComplete(info) {
          case Success(info) =>
            complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, walletScript(info._1, info._2).render))
        }
      )
    )
  }

}
