package encry.api.http

object ScriptHelper {

  def validateForm(name: String,
                   form: String): String =
    """function validateForm() {
  var addr = document.forms["myForm"]["addr"].value;
  var port = document.forms["myForm"]["port"].value;
  if (addr == "") {
     alert("Address must be filled out");
     return false;
   }
 if (port == "") {
    alert("Port must be filled out");
    return false;
  }
}"""

  def setInputFilter(value: String): String =
    s"""function setInputFilter(textbox, inputFilter) {
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
      setInputFilter(document.getElementById("$value"), function(value) { """ + """
          return /^\d*$/.test(value) && (value === "" || parseInt(value) <= 10000);
             });
            """.stripMargin
}
