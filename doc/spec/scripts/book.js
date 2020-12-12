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

window.onload = function() {   
  genFullGrammar() 
  const locals = document.querySelectorAll("a:not(.localref)");
  locals.forEach( function(link){
    const child = link.firstElementChild
    const ctag = (child==null ? "" : child.tagName)
    if (link.className.indexOf("bib") < 0 && ctag != "IMG" && ctag != "SPAN") {  // no bib, image or code links
      link.innerHTML = link.innerHTML + "<i class=\"fa fa-external-link\"></i>";
    }
  });
};
