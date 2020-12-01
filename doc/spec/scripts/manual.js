function genFullGrammar() {
  var fullLex = "";
  var fullParse = "";
  var divLex = document.getElementById("full-lexical");
  var divGrammar = document.getElementById("full-grammar");
  if (!divLex || !divGrammar) return; 

  var gs = document.getElementsByClassName("grammar");
  for(var i = 0; i < gs.length; i++) {
    var elem = gs[i];
    if (elem.tagName.toLowerCase() != "table") continue;
    if (elem.className.indexOf("lex") >= 0) fullLex += elem.innerHTML;
    else if (elem.className.indexOf("parse") >= 0) fullParse += elem.innerHTML;
  }
  divLex.innerHTML="<table class='grammar'>\n"+ fullLex + "</table>"; 
  divGrammar.innerHTML="<table class='grammar'>\n" + fullParse + "</table>";
}

window.onload = function() { genFullGrammar() };
