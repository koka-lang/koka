// ---------------------------------------------------
// grammar
// ---------------------------------------------------

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

// ---------------------------------------------------
// external link symbol
// ---------------------------------------------------

function addExternalLinkIcons() {
  const locals = document.querySelectorAll("a:not(.localref)");
  const currentUrl = window.location.href.replace(/#.*$/,"");
  locals.forEach( function(link){
    const child = link.firstElementChild;
    const ctag = (child==null ? "" : child.tagName);
    const linkUrl = link.href.replace(/#.*$/,"");
    if (linkUrl != currentUrl && link.className.indexOf("bib") < 0 && ctag != "IMG" && ctag != "SPAN") {  // no bib, image or code links
      link.innerHTML = link.innerHTML + "<i class=\"fa fa-external-link\"></i>";
    }
  });
}

// ---------------------------------------------------
// copy button
// ---------------------------------------------------

function addClass(elem,cls) {
  elem.className = elem.className + " " + cls;
}

function removeClass(elem,cls) {
  elem.className = elem.className.split(/\s+/).filter( function(c){ return (c != cls); } ).join(" ");
}

function removeCopiedTooltip( target ) {
  removeClass(target,"tooltip-copied");
  target.setAttribute("title","Copy");  
}

function addCopiedTooltip(target,ok) {
  target.setAttribute("data-tooltip",ok ? "Copied!" : "Failed to copy!" );
  addClass(target,"tooltip-copied");
  target.setAttribute("title","");
  setTimeout( function(){ removeCopiedTooltip(target); }, 2000 );
}


function commandCopyToClipboard(text) {
  // use hidden text element to select and copy
  const elem = document.createElement('textarea');  
  elem.style.fontSize = '12pt';
  elem.style.border = '0';
  elem.style.padding = '0';
  elem.style.margin = '0';
  elem.style.position = 'absolute';
  const isRtl = document.documentElement.getAttribute('dir') === 'rtl';
  elem.style[isRtl ? 'right' : 'left'] = '-10000px';
  let ypos = window.pageYOffset || document.documentElement.scrollTop;
  elem.style.top = "" + ypos + "px";    
  elem.setAttribute('readonly', '');
  elem.value = text;
  document.body.appendChild(elem);
  
  // select and copy
  elem.focus();
  elem.select();
  var ok;
  try {
    ok = document.execCommand("copy");
  }
  catch(exn) {
    ok = false;
  }
  elem.remove();
  return ok;
}  

function copyToClipboard( event ) {
  const target = event.delegateTarget || event.currentTarget;
  const text = target.getAttribute( "data-value" );
  if (!text) return;

  if (navigator.clipboard) {
    navigator.clipboard.writeText(text).then( function(){
      addCopiedTooltip(target,true);
    }, function(err) {
      addCopiedTooltip(target,false);      
    });
  }
  else {
    const ok = commandCopyToClipboard(text);
    addCopiedTooltip(target,ok);
  }
}
 
function enableCopyButtons() {
  const buttons = document.querySelectorAll(".copy.button");
  buttons.forEach( function(button){
    button.addEventListener("click", copyToClipboard);
    button.addEventListener("mouseleave", function(event) {
      const target = event.delegateTarget || event.currentTarget;
      removeCopiedTooltip(target); 
    });
  });
}

// ---------------------------------------------------
// onload
// ---------------------------------------------------

window.onload = function() {   
  genFullGrammar();  
  addExternalLinkIcons();
  enableCopyButtons();
}
