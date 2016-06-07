/*---------------------------------------------------------------------------
  Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/
var _print = function(s) { };
var _trace = function(s) { };

if (typeof console !== undefined && typeof console.log === "function") {
  _trace = function(s) {
    console.log(s);
  };
  _print = _trace;
}

function _println(msg) {
  _print(msg + "\n");
}


/*------------------------------------------------
  Console for Node
------------------------------------------------*/
if (_host === "node") {
  _print = function(s) { 
    process.stdout.write(s); 
  };
}

/*------------------------------------------------
  Console for Browser
------------------------------------------------*/
if (_host === "browser") {
  (function(){
    var escapes = {
        '&': '&amp;', // & first!
        '<': '&lt;',
        '>': '&gt;',
        '\'': '&apos;',
        '"': '&quot;',
        '\n': '<br>',
        '\r': '',
    };
    var escapes_regex = new RegExp("[" + Object.keys(escapes).join("") + "]", "g");

    function html_escape(txt) {
      return txt.replace(escapes_regex, function (s) {
        var r = escapes[s];
        return (r ? r : "");
      });
    }

    function get_console() {
      var cons = document.getElementById("koka-console");
      if (cons==null) {
        cons = document.createElement("div");
        cons.id = "koka-console";
        cons.style.fontFamily = "Consolas,Monaco,'Ubuntu Mono','Droid Sans Mono','Source Code Pro',monospace"
        cons.style.fontSize = "12pt";
        cons.style.width = "99%";
        document.body.appendChild(cons);
      }
      if (cons.display == "none") return null;
      return cons;
    }

    var output = null;
    function get_console_out() 
    {
      if (output) return output;
      
      output = document.getElementById("koka-console-out");
      if (!output) {
        var cons = get_console();
        if (!cons) return null;

        output = document.createElement("div");
        output.id = "koka-console-out";
        output.style.fontFamily = "Consolas,Monaco,'Ubuntu Mono','Droid Sans Mono','Source Code Pro',monospace"
        output.style.fontSize = "12pt";
        output.style.width = "99%";
        output.style.height = "30ex";
        output.style.border = "gray solid 1px";
        output.wrap="off";
        output.style.overflow = "auto";
        output.style.whiteSpace = "pre";
        output.style.padding = "2px";
        output.style.margin = "2px";
        output.style.paddingBottom = "4px";
        output.readOnly = true;
        cons.appendChild(output);
      }

      if (!output.print) {
        output.print_html = function(s) {
          output.innerHTML = output.innerHTML + s;          
          // try to scroll to the end
          if (output.createTextRange) {
            output.createTextRange().scrollIntoView(false);
          }
          else if (output.scrollTop !== undefined) {
            output.scrollTop = output.scrollHeight;
          }    
        };
        output.print = function(s) {
          output.print_html(html_escape(s));
        };        
      }

      return output;
    }

    _print = function(s) {
      var out = get_console_out();
      if (out && out.print) {
        out.print(s);
      }
    };

    _print.print_html = function(s) {
      var out = get_console_out();
      if (out && out.print_html) {
        out.print_html(s);
      }
    };
  })();
}

