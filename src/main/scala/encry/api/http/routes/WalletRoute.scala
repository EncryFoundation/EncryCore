package encry.api.http.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Route
import akka.pattern._
import com.typesafe.scalalogging.StrictLogging
import encry.api.http.DataHolderForApi.{GetViewGetBalance, GetViewPrintPubKeys}
import encry.settings.{EncryAppSettings, RESTApiSettings}
import org.encryfoundation.common.crypto.PublicKey25519
import org.encryfoundation.common.modifiers.state.box.Box.Amount
import org.encryfoundation.common.modifiers.state.box.TokenIssuingBox.TokenId
import org.encryfoundation.common.utils.Algos
import org.encryfoundation.common.utils.TaggedTypes.ADKey
import scalatags.Text
import scalatags.Text.all.{div, span, _}

import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.Success

case class WalletRoute(settings: RESTApiSettings,
                       dataHolder: ActorRef,
                       encrySettings: EncryAppSettings)(
   implicit val context: ActorRefFactory
) extends EncryBaseApiRoute with StrictLogging {

  val EttTokenId: String = Algos.encode(encrySettings.constants.IntrinsicTokenId)

 def walletF: Future[Map[(PublicKey25519, TokenId), Amount]] =
   (dataHolder ? GetViewGetBalance)
     .mapTo[Map[(PublicKey25519, TokenId), Amount]]

 def pubKeysF: Future[List[String]] = (dataHolder ? GetViewPrintPubKeys).mapTo[List[String]]

 def info: Future[(Map[(PublicKey25519, TokenId), Amount], List[String])] = for {
   wallet <- walletF
   pubKeys <- pubKeysF
 } yield (wallet, pubKeys)

 def walletScript(balances: Map[(PublicKey25519, TokenId), Amount]): Text.TypedTag[String] = {
   html(
     scalatags.Text.all.head(
       meta(charset := "utf-8"),
       meta(name := "viewport", content := "width=device-width, initial-scale=1, shrink-to-fit=no"),
       meta(name := "description", content := "Encry Core."),
       meta(name := "author", content := "Creative Tim"),
       script(
         raw(
           """
              function AvoidSpace(event) {
                  var k = event ? event.which : window.event.keyCode;
                  if (k == 32) return false;
              }
            """.stripMargin)
       ),
       script(
         raw(
           """function validateForm2() {
                var fee = document.forms["myForm2"]["fee"].value;
                var data = document.forms["myForm2"]["data"].value;
                  if (fee == "") {
                     alert("Fee must be filled out");
                     return false;
                   } else return true;
                  if (data == "") {
                     alert("Data must be filled out");
                     return false;
                   } else return true;
                  }""")
       ),
       script(
         raw(
           """function validateTransferForm() {
                var addr = document.forms["myForm"]["addr"].value;
                var fee = document.forms["myForm"]["fee"].value;
                var amount = document.forms["myForm"]["amount"].value;
              if (addr == "") {
              alert("Address must be filled out");
              return false;
              } else return true;
              if (fee == "") {
                 alert("Fee must be filled out");
                 return false;
               } else return true;
               if (amount == "") {
                  alert("Amount must be filled out");
                  return false;
                } else return true;
              }""")
       ),
       script(
         raw(
           s"""function wallet(){
               if(validateTransferForm()){
                 var addr = document.forms["myForm"]["addr"].value;
                 var fee = document.forms["myForm"]["fee"].value;
                 var amount = document.forms["myForm"]["amount"].value;
                 var coin = document.forms["myForm"]["coin"].value;
                    var request = new XMLHttpRequest();
                    if (coin == "$EttTokenId") {
                    request.open('GET', "/wallet/transfer?addr="+addr+"&fee="+fee+"&amount="+amount);
                    } else {
                    request.open('GET', "/wallet/transfer?addr="+addr+"&fee="+fee+"&amount="+amount+"&token="+coin);
                    }
                    request.send();
                     window.alert("Transaction was created and sent to node");
                    setTimeout(location.reload.bind(location), 3000);
                  }
                  }""")
       ),
       script(
         raw(
           """function validateContractForm() {
                var contract = document.forms["myForm4"]["contract"].value;
                var fee = document.forms["myForm4"]["fee"].value;
                var amount = document.forms["myForm4"]["amount"].value;
              if (contract == "") {
              alert("Contract must be filled out");
              return false;
              } else return true;
              if (fee == "") {
                 alert("Fee must be filled out");
                 return false;
               } else return true;
               if (amount == "") {
                  alert("Amount must be filled out");
                  return false;
                } else return true;
              }""")
       ),
       script(
         raw(
           s"""
               function contractF(){
               if(validateContractForm()){
                 var contract = document.forms["myForm4"]["contract"].value;
                 var fee = document.forms["myForm4"]["fee"].value;
                 var amount = document.forms["myForm4"]["amount"].value;
                 var coin = document.forms["myForm4"]["coin"].value;
                    var request = new XMLHttpRequest();
                    if (coin == "$EttTokenId") {
                    request.open('GET', "/wallet/transferContract?contract="+contract+"&fee="+fee+"&amount="+amount);
                    } else {
                    request.open('GET', "/wallet/transferContract?contract="+contract+"&fee="+fee+"&amount="+amount+"&token="+coin);
                    }
                    request.send();
                     window.alert("Transaction was created and sent to node");
                    setTimeout(location.reload.bind(location), 3000);
                  }
                  }""")
       ),
       script(
         raw(
           """function validateForm1() {
                var fee = document.forms["myForm1"]["fee"].value;
                var amount = document.forms["myForm1"]["amount"].value;
              if (fee == "") {
              alert("Fee must be filled out");
              return false;
              } else return true;
              if (amount == "") {
                 alert("Amount must be filled out");
                 return false;
               } else return true;
              }""")
       ),
       script(
         raw(
           """function token(){
              if (validateForm1()){
                 var fee = document.forms["myForm1"]["fee"].value;
                 var amount = document.forms["myForm1"]["amount"].value;
                    var request = new XMLHttpRequest();
                    request.open('GET', "/wallet/createToken?fee="+fee+"&amount="+amount);
                    request.send();
                     window.alert("Transaction with token creation was created and sent to node");
                    setTimeout(location.reload.bind(location), 3000);
                  }
                  }""")
        ),
       script(
         raw(
           """function validateDataForm() {
                var fee = document.forms["myForm2"]["fee"].value;
                var data = document.forms["myForm2"]["data"].value;
              if (fee == "") {
              alert("Fee must be filled out");
              return false;
              } else return true;
              if (data == "") {
                 alert("Data must be filled out");
                 return false;
               } else return true;
              }""")
       ),
        script(
          raw(
            """function dataTx(){
               if(validateDataForm()) {
                 var fee = document.forms["myForm2"]["fee"].value;
                 var data = document.forms["myForm2"]["data"].value;
                    var request = new XMLHttpRequest();
                    request.open('GET', "/wallet/data?fee="+fee+"&data="+data);
                    request.send();
                     window.alert("Data transaction was created and sent to node");
                    setTimeout(location.reload.bind(location), 3000);
                  }
                  }""")
       ),
       script(
         raw(
           """function keyCreate() {
                   var request = new XMLHttpRequest();
                     request.open('GET', "/wallet/createKey");
                     request.send();
                      window.alert("Key created successfully");
                     setTimeout(location.reload.bind(location), 1500);
}""")
        ),


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
                          ),
                          div(cls := "col",
                            button(tpe := "button", cls := "btn btn-block btn-primary mb-3", data("toggle") := "modal", data("target") := "#modal-form4", "Create contract tx")
                          )
                        ),

                        // Create contract form
                        div(cls := "modal fade", id := "modal-form4", tabindex := "-1", role := "dialog", aria.labelledby := "modal-form", aria.hidden := "true",
                          div(cls := "modal-dialog modal- modal-dialog-centered modal-sm", role := "document",
                            div(cls := "modal-content",
                              div(cls := "modal-body p-0",
                                div(cls := "card bg-secondary shadow border-0",
                                  div(cls := "card-body px-lg-5 py-lg-5",
                                    form(role := "form", onsubmit := "return validateForm()", id := "myForm4",

                                      div(cls := "form-group",
                                        div(cls := "input-group input-group-alternative mb-3",
                                          div(cls := "input-group-prepend",
                                            span(cls := "input-group-text",
                                              i(cls := "ni ni-email-83")
                                            )
                                          ),
                                          input(cls := "form-control", id := "contract", name := "contract", placeholder := "Contract", tpe := "text", onkeypress:="return AvoidSpace(event)")
                                        )
                                      ),
                                      div(cls := "form-group",
                                        div(cls := "input-group input-group-alternative mb-3",
                                          div(cls := "input-group-prepend",
                                            span(cls := "input-group-text",
                                              i(cls := "ni ni-money-coins")
                                            )
                                          ),
                                          input(cls := "form-control", id := "contractfee", name := "fee", placeholder := "Fee (min = 0)", tpe := "text", onkeypress:="return AvoidSpace(event)"),
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
              setInputFilter(document.getElementById("contractfee"), function(value) {
                return /^\d*$/.test(value) && (value === "" || parseInt(value) >= 0);
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
                                          input(cls := "form-control", id := "contractamount", name := "amount", placeholder := "Amount", onkeypress:="return AvoidSpace(event)"),
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
                               setInputFilter(document.getElementById("contractamount"), function(value) {
                                 return /^\d*$/.test(value) && (value === "" || parseInt(value) > 0);
                               });
                             """.stripMargin)
                                          ),
                                        )
                                      ),
                                      div(cls := "form-group",
                                        select(cls := "form-control", id :="coin", name:="coin",
                                          for {
                                            coinI <- balances.toList
                                          } yield {
                                            option(value := coinI._1._2.toString, if (Algos.encode(coinI._1._2) == EttTokenId) s"ETT (${coinI._2/100000000})" else "something else")
                                          }
                                        )
                                      ),
                                      div(cls := "text-center",
                                        button(tpe := "button", onclick := "contractF()", cls := "btn btn-primary mt-4", "Send tx")
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        ),
                        //
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
                                          input(cls := "form-control", id := "datafee", name := "fee", placeholder := "Fee (min = 0)", tpe := "text", onkeypress:="return AvoidSpace(event)"),
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
              setInputFilter(document.getElementById("datafee"), function(value) {
                return /^\d*$/.test(value) && (value === "" || parseInt(value) >= 0);
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
                                          input(cls := "form-control", id := "data", name := "data", placeholder := "Data", onkeypress:="return AvoidSpace(event)"),
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
                                          input(cls := "form-control", id := "tokenfee", name := "fee", placeholder := "Fee (min = 0)", tpe := "text", onkeypress:="return AvoidSpace(event)"),
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
             setInputFilter(document.getElementById("tokenfee"), function(value) {
               return /^\d*$/.test(value) && (value === "" || parseInt(value) >= 0);
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
                                          input(cls := "form-control", id := "tokenamount", name := "amount", placeholder := "Amount", onkeypress:="return AvoidSpace(event)"),
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
             setInputFilter(document.getElementById("tokenamount"), function(value) {
               return /^\d*$/.test(value) && (value === "" || parseInt(value) > 0);
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
                                          input(cls := "form-control", id := "transfer", name := "addr", placeholder := "Address", tpe := "text", onkeypress:="return AvoidSpace(event)")
                                        )
                                      ),
                                      div(cls := "form-group",
                                        div(cls := "input-group input-group-alternative mb-3",
                                          div(cls := "input-group-prepend",
                                            span(cls := "input-group-text",
                                              i(cls := "ni ni-money-coins")
                                            )
                                          ),
                                          input(cls := "form-control", id := "transferfee", name := "fee", placeholder := "Fee (min = 0)", tpe := "text", onkeypress:="return AvoidSpace(event)"),
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
              setInputFilter(document.getElementById("transferfee"), function(value) {
                return /^\d*$/.test(value) && (value === "" || parseInt(value) >= 0);
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
                                          input(cls := "form-control", id := "transferamount", name := "amount", placeholder := "Amount", onkeypress:="return AvoidSpace(event)"),
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
              setInputFilter(document.getElementById("transferamount"), function(value) {
                return /^\d*$/.test(value) && (value === "" || parseInt(value) > 0);
              });
            """.stripMargin)
                                          ),
                                        )
                                      ),
//                                      div(cls := "form-group",
//                                        select(cls := "form-control", id :="coin", name:="coin",
//                                          if (balances.nonEmpty) {
//                                            balances.values.flatten.toList.map( coinIds =>
//                                              option(value := coinIds._1, if (coinIds._1 == EttTokenId) s"ETT (${coinIds._2/100000000})" else coinIds._1)
//                                            )
//                                          } else {
//                                            option(value := "", "")
//                                          }
//                                        )
//                                      ),
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
                          th(attr("scope") := "row", "Public keys"),
                          th(attr("scope") := "row", "TokenId"),
                          th(attr("scope") := "row", "Balance")
                        )
                      ),
                      tbody(
                        if (balances.nonEmpty) {
                          (for {
                            mapKeyValue <- balances.toList
//                            tokenAmount <- mapKeyValue._1
                          } yield {
                            val tknStr = mapKeyValue._1._2 match {
                              case tokenId if Algos.encode(tokenId) == EttTokenId => "ETT"
                              case tokenId => tokenId
                            }
                            tr(
                              th(mapKeyValue._1._1.toString()),
                              th(tknStr.toString),
                              if (Algos.encode(mapKeyValue._1._2) == EttTokenId ) th(mapKeyValue._2/100000000) else th(mapKeyValue._2)
                            )
                          }).toList
                        } else {
                          tr()
                        }
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
//                  div(cls := "table-responsive",
//                    // Projects table
//                    table(cls := "table align-items-center table-flush",
//                      thead(cls := "thead-light",
//                        tr(
//                          th(attr("scope") := "col", "Key")
//                        )
//                      ),
//                      tbody(
//                        if(balances.keys.nonEmpty) {
//                          for (p <- balances.keys.toList) yield {
//                            tr(th(attr("scope") := "row", p))
//                          }
//                        } else {
//                          tr()
//                        }
//                      )
//                    )
//                  )
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

  override def route: Route = path("webWallet") {
      WebRoute.authRoute(
        onComplete(info) {
          case Success(info) =>
            complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, walletScript(info._1).render))
        }, settings
      )
  }

}
