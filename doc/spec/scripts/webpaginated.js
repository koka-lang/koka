// ---------------------------------------------
// Classname helpers
// ---------------------------------------------
function hasClassName(elem,cname) {
  if (elem==null) return false;
  var regex = new RegExp("\\s*\\b" + cname + "\\b","g");
  return regex.test(elem.className);
}

function removeClassName(elem,cname) {
  var regex = new RegExp("\\s*\\b" + cname + "\\b","g");
  elem.className = elem.className.replace( regex, "" );
}

function addClassName(elem,cname) {
  if (!hasClassName(elem,cname)) elem.className = elem.className + " " + cname;
}

function toggleClassName(elem,cname) {
  var regex = new RegExp("\\s*\\b" + cname + "\\b","g");
  var classes = elem.className;
  if (regex.test(classes)) {
    elem.className = classes.replace( regex, "" );
    return false;
  }
  else {
    elem.className = classes + " " + cname;
    return true;
  }
}

// ---------------------------------------------
// Reliable offset determination
// ---------------------------------------------

function getWindowOffset(elem) {
  var box;
  if (elem.getBoundingClientRect) {
    box = elem.getBoundingClientRect();
  }
  else if (elem.offsetParent && elem.offsetParent.getBoundingClientRect) {
    // text node
    box = elem.offsetParent.getBoundingClientRect();
    box.top = box.top + elem.offsetTop;
    box.left = box.left + elem.offsetLeft;
  }
  else {
    box = { top: 0, left : 0 };
  }
  return box;
}

// Return the viewport position: -1 (before), 0 (visible), 1 (after)
function viewportPosition(elem) {
  var pos = getWindowOffset(elem)
  if (pos.top < 0 || pos.left < 0) 
    return -1;
  else if (pos.top > (window.innerHeight || document.documentElement.clientHeight) ||
           pos.left > (window.innerWidth || document.documentElement.clientWidth)) 
    return 1;
  else 
    return 0;
}

// ---------------------------------------------
// Expand the toc sections and align headers with the toc.
// ---------------------------------------------

var side = document.getElementsByClassName("sidepanel")[0];
var afterScroll = null;

// Align a heading at the top of the page with the toc
function alignHeading( elem ) {
  var ofs     = getWindowOffset(elem).top;
  var sideofs = getWindowOffset(side).top;
  if (ofs >= 0 && ofs < sideofs) {
    window.scrollBy(0, ofs - sideofs);
  } 
}

// Expand, or unexpand, one toc item
// The class is 'auto-expands' or 'click-expands'; the latter is sticky
// as it is user induced and will not be automatically unexpanded.
function itemExpand(item,cls,expand) {
  // get possible toc block (that follows the item)
  var tocblock = item.nextElementSibling;
  if (tocblock==null || !hasClassName(tocblock,"tocblock")) return;
  
  // set expand class
  if (expand===undefined) expand = !hasClassName(tocblock,"expands");
  if (cls===undefined) cls = "auto-expands" 
  if (cls==="click-expands") removeClassName(tocblock,"auto-expands");
  if (expand && !hasClassName(tocblock,cls)) {
    toggleClassName(tocblock,cls); 
  }
  else if (!expand && hasClassName(tocblock,cls)) {
    toggleClassName(tocblock,cls);
    item.firstElementChild.className = "unexpanded";
  }
  
  // set expansion icon
  if (hasClassName(tocblock,"auto-expands") || hasClassName(tocblock,"click-expands")) {
    item.firstElementChild.className = "expanded";   
  }
  else {
    item.firstElementChild.className = "unexpanded";
  }
}


// Expand a single item in the toc (and unexpand others).
function itemExpandOne(item) {
  // unexpand anything that was expanded
  [].forEach.call( document.querySelectorAll(".tocitem"), function(item) {
    removeClassName(item,"current");
    itemExpand(item,"auto-expands",false);      
  });
  addClassName(item,"current");
  // expand the chain of parent blocks
  var tocblock = null;
  var toc = item.nextElementSibling;
  if (toc && hasClassName(toc,"tocblock")) { 
    tocblock = toc;
  }
  else {
    tocblock = item.parentElement;
  }
  while(tocblock != null && !hasClassName(tocblock,"toc")) {
    if (hasClassName(tocblock,"tocblock")) {
      item = tocblock.previousElementSibling;
      if (item != null) itemExpand(item,"auto-expands",true);
    }
    tocblock = tocblock.parentElement;
  }
}

// Auto expand the toc at current  position in the document 
function expandToc() {
  // find the first section heading that is visible in the viewport 
  var current = null;
  [].every.call( document.querySelectorAll(".tocitem"), function(item) {
    var target = document.getElementById( item.getAttribute("data-toc-target") );
    var pos = viewportPosition(target);
    if (pos <= 0) current = item;
    return (pos < 0);
  });
  // if found, expand the corresponding item
  if (current != null) itemExpandOne(current);
}


document.addEventListener("load", function(ev) { expandToc(); });
document.addEventListener("resize", function(ev) { expandToc(); });
document.addEventListener("scroll", function(ev) {
  if (afterScroll) {
    afterScroll();
    afterScroll = null;
  }
  else {
    expandToc();
  }
});

// ---------------------------------------------
// Install event handlers for all items in the TOC
// ---------------------------------------------

[].forEach.call( document.querySelectorAll(".tocitem"), function(item) {
  // only for toc items with a target
  var target = document.getElementById( item.getAttribute("data-toc-target") );
  if (!target) return;
  
  // ensure every tocblock has a expansion icon in front
  // (the optional nested tocblock follows the item)
  var tocblock = null;
  var toc = item.nextElementSibling;  
  if (toc && hasClassName(toc,"tocblock")) { 
    tocblock = toc;
    item.innerHTML = "<span class='unexpanded'></span>" + item.innerHTML;   
  } 
  
  // on a click
  item.addEventListener( "click", function() {
    // expand this toc item and set expansion icon
    itemExpand(item,"click-expands");
    // after navigation, align the heading with the toc
    afterScroll = (function() {
      alignHeading(target);
    });    
  }); 
});
 