
function initTOC() {
  var toc = document.getElementById('toc');    
  if (toc == null) return;
  var tocToggle = document.getElementById('toc-toggle');
  
  function showToc() {
    toc.style.display = 'block';
    if (tocToggle){
      tocToggle.innerHTML = '&#x25BC;'
      tocToggle.style.fontSize = '12pt'
    }
  }
  function hideToc() {
    toc.style.display = 'none';
    if (tocToggle) {
      tocToggle.innerHTML = '&#x25B6;';    
      tocToggle.style.fontSize = '16pt';
    }
  }
  function switchToc() {
    (toc.style.display != 'none' ? hideToc() : showToc());
  }

  if (tocToggle) tocToggle.onclick = switchToc;
  hideToc()
}

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

window.onload = function() { initTOC(); genFullGrammar() };
