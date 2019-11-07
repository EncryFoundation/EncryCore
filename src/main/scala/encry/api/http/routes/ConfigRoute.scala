package encry.api.http.routes

import java.net.InetSocketAddress

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.server.{Route, StandardRoute, ValidationRejection}
import cats.syntax.validated._
import cats.syntax.apply._
import cats.data.{NonEmptyChain, Validated}
import cats.effect.IO
import com.typesafe.scalalogging.StrictLogging
import encry.Starter.InitNodeResult
import encry.api.http.DataHolderForApi.{GetAllInfoHelper, GetAllPeers, GetViewCreateKey, GetViewGetBalance, GetViewPrintPubKeys, StartMiner, StopMiner}
import encry.local.miner.Miner.MinerStatus
import scalatags.Text.all.{div, form, h3, span, td, _}
import encry.settings.{NodeSettings, RESTApiSettings}
import encry.utils.Mnemonic
import io.circe.Json
import scalatags.{Text, generic}
import io.circe.parser
import io.circe.generic.auto._

import scala.language.implicitConversions

case class ConfigRoute(override val settings: RESTApiSettings, starter: ActorRef)(
  implicit val context: ActorRefFactory
) extends EncryBaseApiRoute with StrictLogging {

val phrase: String =
  Mnemonic.entropyToMnemonicCode(scorex.utils.Random.randomBytes(16))

  val words = Mnemonic.getWords.toList.mkString(",")

  def peerScript(): Text.TypedTag[String] = {

    html(
      scalatags.Text.all.head(
        meta(charset := "utf-8"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1, shrink-to-fit=no"),
        meta(name := "description", content := "Start your development with a Dashboard for Bootstrap 4."),
        meta(name := "author", content := "Creative Tim"),
        script(
          raw(
            """
              function ValidateIPaddress(address) {
                if (/^(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$/.test(address)) {
                  return true;
                }
                alert("You have entered an invalid IP address!");
                return false;
              }
            """.stripMargin)
        ),
        script(
          raw(s"""function configure(){
                  var words = "${words}";
                  var str = words.split(",");
                 var pass = document.getElementById("password").value;
                 var mnem = document.getElementById("mnemonic").value;
                 var check1 = document.querySelector('input[name="custom-radio-1"]:checked').value;
                 var check2 = document.querySelector('input[name="custom-radio-2"]:checked').value;
                 var check3 = document.querySelector('input[name="custom-radio-3"]:checked').value;
                 var host = document.forms["myForm"]["host"].value;
                 var peer = document.forms["myForm"]["port"].value;
                 var node = document.getElementById("nodepass").value;
                 var nodeName= document.getElementById("nodename").value;
                 var declared= document.getElementById("declared").value;
                 var bind= document.getElementById("bind").value;
                 var mnemToList = mnem.split(" ");

                 function arrayContainsArray (superset, subset) {
                   if (0 === subset.length) {
                     return false;
                   }
                   return subset.every(function (value) {
                     return (superset.indexOf(value) >= 0);
                   });
                 }

                 if (pass == "") {
      alert("Password must be filled out");
      return false;
    }

                if (!(ValidateIPaddress(host))){
                return false;
                };

    if (mnem == "") {
       alert("${phrase}");
      var request = new XMLHttpRequest();
      request.open('GET', "http://0.0.0.0:9051/collect?password="+pass+"&mnem="+"${phrase}"+"&chain="+check1+"&sync="+check2+"&host="+host+"&peer="+peer+"&cwp="+check3+"&nodePass="+node+"&nodeName="+nodeName+"&declared="+declared+"&bind="+bind);
      request.send();
      window.alert("Configuration completed. URL: http://0.0.0.0:9051/login will be available.");
      //                    setTimeout(location.reload.bind(location), 3000);
     } else {
                 if ((mnemToList.length == 12) && arrayContainsArray(str, mnemToList)) {
                    var request = new XMLHttpRequest();
                    request.open('GET', "http://0.0.0.0:9051/collect?password="+pass+"&mnem="+mnem+"&chain="+check1+"&sync="+check2+"&host="+host+"&peer="+peer+"&cwp="+check3+"&nodePass="+node+"&nodeName="+nodeName+"&declared="+declared+"&bind="+bind);
                    request.send();
                    window.alert("Configuration completed. URL: http://0.0.0.0:9051/login will be available.");
} else {
                 window.alert("Invalid mnemonic");
}
}
                  }""".stripMargin)
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

        // Main content
        div(cls := "main-content",
          // Top navbar
          tag("nav")(cls := "navbar navbar-top navbar-expand-md navbar-dark", id := "navbar-main",
            div(cls := "container-fluid",
              // Brand
              a(cls := "h4 mb-0 text-white text-uppercase d-none d-lg-inline-block",  "Node Configuration"),

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
                          form(role:="form", id:="myForm", onsubmit:="return validateForm2()",
                            //1. Node name
                            h3("1. Set up your node name"),
                            div(cls := "form-group",
                              div(cls := "input-group input-group-alternative",
                                div(cls := "input-group-prepend",
                                  span(cls := "input-group-text",
                                    i(cls := "ni ni-lock-circle-open")
                                  )
                                ),
                                input(cls := "form-control", placeholder := "Node name", id:="nodename", name:="nodename")
                              )
                            ),
                              //
                            //2. Node name
                            h3("2. Set up your declared address"),
                            div(cls := "form-group",
                              div(cls := "input-group input-group-alternative",
                                div(cls := "input-group-prepend",
                                  span(cls := "input-group-text",
                                    i(cls := "ni ni-lock-circle-open")
                                  )
                                ),
                                input(cls := "form-control", placeholder := "Declared address", id:="declared", name:="declared")
                              )
                            ),
                            //
                            //3. Node name
                            h3("3. Set up your bind address"),
                            div(cls := "form-group",
                              div(cls := "input-group input-group-alternative",
                                div(cls := "input-group-prepend",
                                  span(cls := "input-group-text",
                                    i(cls := "ni ni-lock-circle-open")
                                  )
                                ),
                                input(cls := "form-control", placeholder := "Bind address", id:="bind", name:="bind")
                              )
                            ),
                            //
                        // 2. Password
                            h3("4. Set up your password for node"),
                            div(cls := "form-group",
                              div(cls := "input-group input-group-alternative",
                                div(cls := "input-group-prepend",
                                  span(cls := "input-group-text",
                                    i(cls := "ni ni-lock-circle-open")
                                  )
                                ),
                                input(cls := "form-control", placeholder := "Password", tpe := "password", id:="nodepass", name:="nodepass")
                              )
                            ),
                            // 3
                            h3("5. Set up your password for wallet"),
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
                            // 3.
                            // 4. Enter mnemonic
                            h3("6. Enter your mnemonic if available (leave empty and we'll generate new one for you)"),
                              div(cls := "form-group",
                                div(cls := "input-group input-group-alternative",
                                  div(cls := "input-group-prepend",
                                    span(cls := "input-group-text"
                                    )
                                  ),
                                  input(cls := "form-control", value:="", placeholder := "Mnemonic", id:="mnemonic", name:="mnemonic")
                                )
                              ),
                            // 4.
                            // 5. Would you like to start new chain?
                            h3("7. Would you like to start your own chain?"),
                              Seq(
                              div(cls := "custom-control custom-radio mb-3",
                                input(name := "custom-radio-1", value:="Yes", cls := "custom-control-input", id := "customRadio1", tpe := "radio"),
                                label(cls := "custom-control-label",  `for` := "customRadio1", "Yes")
                              ),
                              div(cls := "custom-control custom-radio mb-3",
                                input(name := "custom-radio-1", value:="No", cls := "custom-control-input", id := "customRadio2", tpe := "radio"),
                            label(cls := "custom-control-label", `for` := "customRadio2", "No")
                            )
                          ),
                            // 5.
                            // 6. Choose sync type (fast / normal)
                            h3("8. Choose sync type (fast / normal)"),
                              Seq(
                              div(cls := "custom-control custom-radio mb-3",
                                input(name := "custom-radio-2", value:="Fast", cls := "custom-control-input", id := "customRadio3", tpe := "radio"),
                                label(cls := "custom-control-label", `for` := "customRadio3", "Fast")
                              ),
                              div(cls := "custom-control custom-radio mb-3",
                                input(name := "custom-radio-2", value:="Normal", cls := "custom-control-input", id := "customRadio4", tpe := "radio"),
                                label(cls := "custom-control-label", `for` := "customRadio4", "Normal")
                              )
                            ),
                            // 6.
                            // 7. Add peer to connect
                            h3("9. Add peer to set connection with"),

                              div(cls := "form-group",
                                div(cls := "input-group input-group-alternative mb-3",
                                  div(cls := "input-group-prepend",
                                    span(cls := "input-group-text",
                                      i(cls := "ni ni-money-coins")
                                    )
                                  ),
                                  input(cls := "form-control", id:="bibo", name:="host", placeholder := "Address", tpe := "text"),
                                )
                              ),
                              div(cls := "form-group",
                                div(cls := "input-group input-group-alternative",
                                  div(cls := "input-group-prepend",
                                    span(cls := "input-group-text",
                                      i(cls := "ni ni-credit-card")
                                    )
                                  ),
                                  input(cls := "form-control", id:="bobo", name:="port", placeholder := "Port"),
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
                            // 10.
                            h3("9. Set up max connections"),
                            div(cls := "form-group",
                              div(cls := "input-group input-group-alternative",
                                div(cls := "input-group-prepend",
                                  span(cls := "input-group-text",
                                    i(cls := "ni ni-credit-card")
                                  )
                                ),
                                input(cls := "form-control", id:="maxconnect", name:="maxconnect", placeholder := "Max connections (max = 30)"),
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
              setInputFilter(document.getElementById("maxconnect"), function(value) {
                return /^\d*$/.test(value) && (value === "" || parseInt(value) < 31);
              });
            """.stripMargin)
                                ),
                              )
                            ),
                            //
                            h3("10. Set up amount of workers"),
                            div(cls := "form-group",
                              div(cls := "input-group input-group-alternative",
                                div(cls := "input-group-prepend",
                                  span(cls := "input-group-text",
                                    i(cls := "ni ni-credit-card")
                                  )
                                ),
                                input(cls := "form-control", id:="worker", name:="worker", placeholder := "Workers (max = 10)"),
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
              setInputFilter(document.getElementById("worker"), function(value) {
                return /^\d*$/.test(value) && (value === "" || parseInt(value) < 11);
              });
            """.stripMargin)
                                ),
                              )
                            ),
                            // 8. Would you like to connect with only known peers?
                            h3("10. Would you like to connect with only known peers?"),
                            Seq(
                              div(cls := "custom-control custom-radio mb-3",
                                input(name := "custom-radio-3", value:="true", cls := "custom-control-input", id := "customRadio5", tpe := "radio"),
                                label(cls := "custom-control-label",  `for` := "customRadio5", "Yes")
                              ),
                              div(cls := "custom-control custom-radio mb-3",
                                input(name := "custom-radio-3", value:="false", cls := "custom-control-input", id := "customRadio6", tpe := "radio"),
                                label(cls := "custom-control-label", `for` := "customRadio6", "No")
                              )
                            ),
                            // 8.

                              div(cls := "text-center",
                              button(tpe := "button", onclick:="configure()", cls := "btn btn-primary mt-4", "Configure")
                            )
                        )
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

//  def validateInfo = (path("validatate") & get) {
//    parameters('mnem, 'host, 'peer) { (mnemonic, host, peer) =>
//
//    }
//  }

  def sendAllInfo: Route = (path("collect") & get){
    parameters('password, 'mnem, 'chain, 'sync, 'host, 'peer, 'cwp, 'nodePass, 'nodeName, 'declared, 'bind) {
      (password, mnemonic, chain, sync, host, peer, cwp, nodePass, nodeName, declaredAddr, bindAddr) =>

      val passwordR: String = password
      val mnemonicR: String = mnemonic match {
        case x: String => x
      }
      val chainR: Boolean = chain match {
        case "Yes" => true
        case "No"  => false
      }
      val syncR: Boolean = sync match {
        case "Fast"   => true
        case "Normal" => false
      }
      val peerR: InetSocketAddress = new InetSocketAddress(host, peer.toInt)
      val cwpR: Boolean = cwp match {
        case "true" => true
        case "false" => false
      }
        val nodePassR = nodePass

    println(passwordR + " " + mnemonicR + " " + chainR + " " + syncR + " " + peerR + " " + cwpR + " " + nodePassR)
      starter ! InitNodeResult(mnemonicR, passwordR, chainR, syncR, List(peerR), cwpR, nodePassR, nodeName, declaredAddr, bindAddr)
      complete("OK")
    }
  }

  def configR: Route = (path("config") & get) {
    extractClientIP { ip =>
      if (ip.toOption.map(x => x.getHostAddress).getOrElse("unknown") == "127.0.0.1") {
        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, peerScript().render))

      } else reject(ValidationRejection("Restricted!"))
    }
  }

  override def route: Route = configR ~ sendAllInfo


}
