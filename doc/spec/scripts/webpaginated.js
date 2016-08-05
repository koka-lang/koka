// ---------------------------------------------
// Helpers
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

function isInViewport (elem) {
  var pos = getWindowOffset(elem)
  return (pos.top >= 0 && pos.left >= 0 &&
      pos.top <= (window.innerHeight || document.documentElement.clientHeight) &&
      pos.right <= (window.innerWidth || document.documentElement.clientWidth)
  );
}

// ---------------------------------------------
// Expand the toc sections and align headers with the toc.
// ---------------------------------------------

var side = document.getElementsByClassName("sidepanel")[0];
var afterScroll = null;


function alignHeading( elem ) {
  var ofs     = getWindowOffset(elem).top;
  var sideofs = getWindowOffset(side).top;
  if (ofs >= 0 && ofs < sideofs) {
    window.scrollBy(0, ofs - sideofs);
  } 
}

function itemExpand(item,cls,expand) {
  // get toc block
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
  // set triangle
  if (hasClassName(tocblock,"auto-expands") || hasClassName(tocblock,"click-expands")) {
    item.firstElementChild.className = "expanded";   
  }
  else {
    item.firstElementChild.className = "unexpanded";
  }
}


function itemExpandOne(item) {
  // unexpand anything that was expanded
  [].forEach.call( document.querySelectorAll(".tocitem"), function(item) {
    itemExpand(item,"auto-expands",false);      
  });
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

document.addEventListener("scroll", function(ev) {
  if (afterScroll) {
    afterScroll();
    afterScroll = null;
  }
  else {   
    [].every.call( document.querySelectorAll(".tocitem"), function(item) {
      var target = document.getElementById( item.getAttribute("data-toc-target") );
      if (target != null && isInViewport(target)) {
        itemExpandOne(item,true);
        return false;
      }
      else return true;
    });
  }
});


// ---------------------------------------------
// Install event handlers for all items in the TOC
// ---------------------------------------------

[].forEach.call( document.querySelectorAll(".tocitem"), function(item) {
  var target = document.getElementById( item.getAttribute("data-toc-target") );
  if (!target) return;
  var itemContent = item.innerHTML;
  var tocblock = null;
  var toc = item.nextElementSibling;
  if (toc && hasClassName(toc,"tocblock")) { 
    tocblock = toc;
    item.innerHTML = "<span class='unexpanded'></span>" + itemContent;   
  } 
  
  // on a click
  item.addEventListener( "click", function() {
    // toggle expands class, and set expansion icon
    itemExpand(item,"click-expands");
    /*
    if (tocblock) {
      if (toggleClassName(tocblock,"expands")) {
        item.innerHTML = "<span class='expanded'></span>" + itemContent;  
      }
      else {
        item.innerHTML = "<span class='unexpanded'></span>" + itemContent;
      }
      tocBlockExpand(item,tocblock);
    }
    */
    // after navigation, align the heading with the toc
    afterScroll = (function() {
      alignHeading(target);
    });    
  }); 
});
 