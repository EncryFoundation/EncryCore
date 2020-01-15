package encry.api.http.routes

import java.net.InetSocketAddress
import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Route
import com.typesafe.scalalogging.StrictLogging
import encry.Starter.InitNodeResult
import encry.api.http.ApiRoute
import encry.settings.RESTApiSettings
import encry.utils.Mnemonic
import io.circe.generic.auto._
import scalatags.Text
import scalatags.Text.all.{div, form, h3, span, _}
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

case class ConfigRoute(settings: RESTApiSettings, starter: ActorRef)(
  implicit val context: ActorRefFactory
) extends ApiRoute with StrictLogging {

  val phrase: String = Mnemonic.entropyToMnemonicCode(scorex.utils.Random.randomBytes(16))

  val apiPort: Int = settings.bindAddress.getPort

  def peerScript(): Text.TypedTag[String] = {

    html(
      scalatags.Text.all.head(
        meta(charset := "utf-8"),
        meta(name := "viewport", content := "width=device-width, initial-scale=1, shrink-to-fit=no"),
        meta(name := "description", content := "Encry Core"),
        meta(name := "author", content := "Creative Tim"),
        script(
          raw(
            """
              function ValidateIPaddress(address) {
                if (/^(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?):[0-9]{1,5}$/.test(address)) {
                  return true;
                }
                alert("You have entered an invalid IP address!");
                return false;
              }
            """.stripMargin)
        ),
        script(
          raw(
            """
              function disableChecks() {
                document.getElementById("customRadio3").disabled = true;
                document.getElementById("customRadio4").disabled = true;
                document.getElementById("host").disabled = true;
              }
            """.stripMargin)
        ),
        script(
          raw(
            """
              function enableChecks() {
                document.getElementById("customRadio3").disabled = false;
                document.getElementById("customRadio4").disabled = false;
                document.getElementById("host").disabled = false;
              }
            """.stripMargin)
        ),
        script(
          raw(
            """
              function AvoidSpace(event) {
                  var k = event ? event.which : window.event.keyCode;
                  if (k == 32) return false;
                  if(k == 37) return false;
              }
            """.stripMargin)
        ),script(
          raw(
            s"""
              function checkPorts(declaredR, bindR) {
                                var declaredPort = declaredR.split(":")[1];
                                var bindPort = bindR.split(":")[1];
                                if (declaredPort == "${apiPort.toString}") {
                                  window.alert("Please check your port in declared address. Seems like it's the same as API port.")
                                  return false;
                                } else if (bindPort == "${apiPort.toString}") {
                                  window.alert("Please check your port in bind address. Seems like it's the same as API port.")
                                  return false;
                                } else return true;
                               }
            """.stripMargin)
        ),
        script(
          raw(
            s"""function configure(){
                  var words = "${Mnemonic.getWords.toList.mkString(",")}";
                  var str = words.split(",");
                  var pass = document.getElementById("password").value;
                  var mnem = document.getElementById("mnemonic").value;
                  var check1 = document.querySelector('input[name="custom-radio-1"]:checked').value;
                  var check2 = document.querySelector('input[name="custom-radio-2"]:checked').value;
                  var check3 = document.querySelector('input[name="custom-radio-3"]:checked').value;
                  var host = document.forms["myForm"]["host"].value;
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

                 if(nodeName == null) {
                 window.alert("null")
                 }

                 if (!(checkPorts(declared, bind))) {
                 return false;
                 }

                 if (pass == "") {
                    alert("Password must be filled out");
                    return false;
                  }

                 if (host!="" && !(ValidateIPaddress(host))){
                    return false;
                  };
                  if (!(ValidateIPaddress(declared))){
                    return false;
                  };
                  if (!(ValidateIPaddress(bind))){
                    return false;
                  };

                if (mnem == "") {
                    window.alert("Please, save it and don't show to anyone! ${phrase}");
                    var request = new XMLHttpRequest();
                    request.open('GET', "/collect?password="+pass+"&mnem="+"${phrase}"+"&chain="+check1+"&sync="+check2+"&host="+host+"&cwp="+check3+"&nodePass="+node+"&nodeName="+nodeName+"&declared="+declared+"&bind="+bind);
                    request.send();
                    window.alert("Configuration completed. URL: http://0.0.0.0:9051/login will be available.");
                } else {
                 if ((mnemToList.length == 12) && arrayContainsArray(str, mnemToList)) {
                    var request = new XMLHttpRequest();
                    request.open('GET', "/collect?password="+pass+"&mnem="+mnem+"&chain="+check1+"&sync="+check2+"&host="+host+"&cwp="+check3+"&nodePass="+node+"&nodeName="+nodeName+"&declared="+declared+"&bind="+bind);
                    request.send();
                    window.alert("Configuration completed. URL: http://0.0.0.0:9051/login will be available.");
                    } else {
                 window.alert("Invalid mnemonic");
                       }
                    }
                  }""".stripMargin)
        ),

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
                          form(role:="form", id:="myForm",
                            //1. Node name
                            h3("1. Set up your node name"),
                            div(cls := "form-group",
                              div(cls := "input-group input-group-alternative",
                                div(cls := "input-group-prepend",
                                ),
                                input(cls := "form-control", placeholder := " Node name", id:="nodename", name:="nodename")
                              )
                            ),
                            //1.
                            //2. Declared address
                            h3("2. Set up your declared address"),
                            p("The address at which other nodes connect to yours."),
                            div(cls := "form-group",
                              div(cls := "input-group input-group-alternative",
                                div(cls := "input-group-prepend",
                                ),
                                input(cls := "form-control", placeholder := " Declared address", id:="declared", name:="declared", onkeypress:="return AvoidSpace(event)")
                              )
                            ),
                            //2.
                            //3. Bind address
                            h3("3. Set up your bind address"),
                            p("The address on which the node starts on the local machine. (required)"),
                            div(cls := "form-group",
                              div(cls := "input-group input-group-alternative",
                                div(cls := "input-group-prepend",
                                ),
                                input(cls := "form-control", placeholder := " Bind address", id:="bind", name:="bind", onkeypress:="return AvoidSpace(event)")
                              )
                            ),
                            //3.
                            // 4. Password
                            h3("4. Set up your password for node"),
                            p("This password gives you access to front API."),
                            div(cls := "form-group",
                              div(cls := "input-group input-group-alternative",
                                div(cls := "input-group-prepend",
                                  span(cls := "input-group-text",
                                    i(cls := "ni ni-lock-circle-open")
                                  )
                                ),
                                input(cls := "form-control", placeholder := "Password", tpe := "password", id:="nodepass", name:="nodepass", onkeypress:="return AvoidSpace(event)")
                              )
                            ),
                            //4.
                            // 5. Password
                            h3("5. Set up your password for wallet"),
                            p("This password gives you access to your wallet."),
                          div(cls := "form-group",
                            div(cls := "input-group input-group-alternative",
                              div(cls := "input-group-prepend",
                                span(cls := "input-group-text",
                                  i(cls := "ni ni-lock-circle-open")
                                )
                              ),
                              input(cls := "form-control", placeholder := "Password", tpe := "password", id:="password", name:="password", onkeypress:="return AvoidSpace(event)")
                            )
                          ),
                            // 5.
                            // 6. Enter mnemonic
                            h3("6. Enter your mnemonic if available (leave empty and we'll generate new one for you)"),
                            p("A mnemonic phrase is a list of 12 words which store all the information needed to recover your wallet."),
                              div(cls := "form-group",
                                div(cls := "input-group input-group-alternative",
                                  div(cls := "input-group-prepend",
                                    span(cls := "input-group-text"
                                    )
                                  ),
                                  input(cls := "form-control", value:="", placeholder := "Mnemonic", id:="mnemonic", name:="mnemonic")
                                )
                              ),
                            //6.
                            // 7. Would you like to start new chain?
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
                            //7.
                            // 8. Choose sync type (fast / normal)
                            h3("8. Choose sync type (fast / normal)"),
                            p("Choose fast if you want to use fast synchronization and normal if you want to download all blocks from 0."),
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
                            //8.
                            // 9. Add peer to connect
                            h3("9. Add peer to set connection with"),
                            p("The address of peer with which you want to connect to."),
                              div(cls := "form-group",
                                div(cls := "input-group input-group-alternative mb-3",
                                  div(cls := "input-group-prepend",
                                  ),
                                  input(cls := "form-control", id:="host", name:="host", placeholder := " Address", tpe := "text", onkeypress:="return AvoidSpace(event)"),
                                )
                              ),
                            //9.
                            // 10. max connections
                            h3("10. Set up max connections"),
                            p("Maximum number of connections with other nodes."),
                            div(cls := "form-group",
                              div(cls := "input-group input-group-alternative",
                                div(cls := "input-group-prepend",
                                ),
                                input(cls := "form-control", id:="maxconnect", name:="maxconnect", placeholder := " Max connections (max = 30)", onkeypress:="return AvoidSpace(event)"),
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
                            //10.
                            // 11. Workers quantity
                            h3("11. Set up amount of workers"),
                            p("How many threads do you want to allocate for your mining."),
                            div(cls := "form-group",
                              div(cls := "input-group input-group-alternative",
                                div(cls := "input-group-prepend",
                                ),
                                input(cls := "form-control", id:="worker", name:="worker", placeholder := " Workers (max = 10)"),  onkeypress:="return AvoidSpace(event)",
                                script(
                                  raw(
                                    """function setInputFilter(textbox, inputFilter) {
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
                            //11.
                            // 12. Would you like to connect with only known peers?
                            h3("12. Would you like to connect with only known peers?"),
                            p("Do you want to establish a connection with peers about which you initially knew nothing."),
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
                            //12.
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
        // Argon JS
        script(src := "argon/assets/js/argon.js?v=1.0.0")

      )
    )
  }

  def sendAllInfo: Route = (path("collect") & get) {
    parameters('password, 'mnem, 'chain, 'sync, 'host, 'cwp, 'nodePass, 'nodeName, 'declared, 'bind) {
      (password, mnemonic, chain, sync, host, cwp, nodePass, nodeName, declaredAddr, bindAddr) =>

        val chainR: Boolean = chain match {
          case "Yes" => true
          case "No" => false
        }
        val syncR: Boolean = sync match {
          case "Fast" => true
          case "Normal" => false
        }
        val peerR = Try {
          val peerSpl = host.split(":")
          (peerSpl(0), peerSpl(1).toInt)
        } match {
          case Success((hostS, port)) => List(new InetSocketAddress(hostS, port))
          case Failure(_) =>  List.empty[InetSocketAddress]
        }

        val cwpR: Boolean = cwp match {
          case "true" => true
          case "false" => false
        }

        val declared = Try {
          val declaredSpl = declaredAddr.split(":")
          (declaredSpl(0), declaredSpl(1).toInt)
        } match {
          case Success((hostS, port)) => Some(new InetSocketAddress(hostS, port))
          case Failure(_) => None
        }
        val bind = Try {
          val bindSpl = bindAddr.split(":")
          (bindSpl(0), bindSpl(1).toInt)
        } match {
          case Success((hostS, port)) => new InetSocketAddress(hostS, port)
          case Failure(_) => new InetSocketAddress("0.0.0.0", 9001)
        }

        starter ! InitNodeResult(
          mnemonic,
          password,
          chainR,
          syncR,
          snapshotCreation = false, //todo incorrect
          peerR,
          connectWithOnlyKnownPeers = cwpR,
          nodePass,
          nodeName,
          declared,
          bind
        )
        complete("OK")
    }
  }

  def configR: Route = (path("config") & get) {
    complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, peerScript().render))
  }

  override def route: Route = configR ~ sendAllInfo

}
