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
      const ok = commandCopyToClipboard(text);
      addCopiedTooltip(target,ok);      
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
    /*
    button.addEventListener("mouseleave", function(event) {
      const target = event.delegateTarget || event.currentTarget;
      removeCopiedTooltip(target); 
    });
    */
  });
}

// ---------------------------------------------------
// runnable examples
// ---------------------------------------------------

// Code blocks marked {.run} (written ```koka {.run}) get a Run button that
// turns the block into an editable <koka-editor>, loaded from the playground
// on first use. Every editor on a page shares one compiler worker. A browser
// starts that worker only from a same-origin script, so the playground must be
// on the same origin as the book: ../playground/ by default, or
// <meta name="koka-playground">. Relative, because each published version keeps
// its doc/ and playground/ side by side, and its examples must run with its own
// compiler and standard library.
var kokaPlayground = "../playground/";
var kokaEditorLoading = null;

function loadKokaEditor() {
  if (!kokaEditorLoading) {
    const meta = document.querySelector("meta[name='koka-playground']");
    const base = new URL(meta ? meta.content : kokaPlayground, window.location.href).href;
    [["koka-wasm-url", base + "koka-playground.wasm"], ["koka-stdlib-url", base]].forEach( function(kv){
      if (document.querySelector("meta[name='" + kv[0] + "']")) return;
      const m = document.createElement("meta");
      m.name = kv[0];
      m.content = kv[1];
      document.head.appendChild(m);
    });
    kokaEditorLoading = new Promise( function(resolve,reject){
      const script = document.createElement("script");
      script.type = "module";
      script.src = base + "koka-editor.js";
      script.onerror = reject;
      document.head.appendChild(script);
      customElements.whenDefined("koka-editor").then(resolve);
    });
  }
  return kokaEditorLoading;
}

// The code of a block: the compiler's HTML holds the source twice, as
// .plaincode and highlighted with type tooltips as .nicecode.
function kokaSourceOf(pre) {
  const plain = pre.querySelector(".plaincode");
  return (plain ? plain.textContent : pre.textContent).replace(/\s+$/, "");
}

function enableRunButtons() {
  const blocks = document.querySelectorAll("pre.run");
  blocks.forEach( function(pre){
    const button = document.createElement("button");
    button.className = "run-button";
    button.textContent = "▶ Run";
    button.setAttribute("title", "Edit and run this example");
    button.addEventListener("click", function() {
      button.disabled = true;
      button.textContent = "⌛ Loading...";
      const editor = document.createElement("koka-editor");
      editor.setAttribute("theme", "light");
      editor.textContent = kokaSourceOf(pre);
      loadKokaEditor().then( function(){
        pre.replaceWith(editor);
        editor.run();
      }, function() {
        button.textContent = "Playground unavailable";
      });
    });
    pre.appendChild(button);
  });
}

// ---------------------------------------------------
// onload
// ---------------------------------------------------

window.onload = function() {   
  genFullGrammar();  
  addExternalLinkIcons();
  enableCopyButtons();
  enableRunButtons();
}
