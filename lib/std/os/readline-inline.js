/*---------------------------------------------------------------------------
  Copyright 2012-2016 Microsoft Corporation.

  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

var _readline = function(action) {
  $std_core.error("std/readline/readline is not supported on this platform.");
  return (function(){ });
};

var _cancel_readline = function() {
  return;
}

/*------------------------------------------------
  Console for Node
------------------------------------------------*/
if ($std_core.host() === "node") {
  (function(){
    var nodein = null;

    function get_nodein() {
      if (!nodein) {
        var readline = require("readline");
        nodein = readline.createInterface( { input: process.stdin /*, output: process.stdout */ });
        nodein.listeners = [];
        nodein.on('line', function(s) {
          //console.log("line event");
          if (nodein.listeners.length > 0) {
            var action = nodein.listeners.shift();
            action(null,s);
          }
          if (nodein.listeners.length === 0) {
            nodein.close(); // no more readlines -> close the app
            nodein = null;
          }
        });
      }
      return nodein;
    }

    _cancel_readline = function() {
      var nodein = get_nodein();
      if (!nodein) return;
      if (nodein.listeners.length > 0) {
        nodein.listeners.shift();
      }
      if (nodein.listeners.length === 0) {
        nodein.close(); // no more readlines -> close the app
        nodein = null;
      }
    }

    _readline = function(action) {
      var rl = get_nodein();
      if (rl) {
        rl.listeners.push(action);
      }
    }
  })();
}

/*------------------------------------------------
  Console for Browser
------------------------------------------------*/
if ($std_core.host() === "browser") {
  (function() {
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

    var prompt = "> ";

    function caret_to_end(elem) {
      if (!elem || !elem.value || !elem.value.length) return;
      var pos = elem.value.length;
      if (pos===0) return;

      if (elem.createTextRange) { /* ie */
        var rng = elem.createTextRange();
        rng.collapse(true);
        rng.moveEnd("character",pos);
        rng.moveStart("character",pos);
        rng.select();
      }
      else if (elem.setSelectionRange) {  /* the rest */
        elem.setSelectionRange(pos,pos);
      }
    }

    function overlap(s,t) {
      if (!s || !t) return 0;
      var len = s.length;
      if (len < t.length) len = t.length;
      var i = 0;
      while(i < len) {
        if (s[i] !== t[i]) return i;
        i++;
      }
      return i;
    }

    var input = null;
    var console_input_init = false;
    function get_console_input()
    {
      if (input) return input;

      input = document.getElementById("koka-console-in");
      if (!input) {
        $std_core.print(""); // ensure there is an output pane
        var cons = document.getElementById("koka-console");
        if (!cons) return null;

        input = document.createElement("input");
        input.type = "text";
        input.id = "koka-console-in";
        input.style.fontFamily = "Consolas,Monaco,'Ubuntu Mono','Droid Sans Mono','Source Code Pro',monospace"
        input.style.fontSize = "12pt";
        input.style.width = "99%";
        input.style.border = "gray solid 1px";
        input.style.padding = "2px";
        input.style.margin = "2px";
        cons.appendChild(input);
      }

      if (!console_input_init) {
        console_input_init = true;
        var output = document.getElementById("koka-console-out");
        input.value = prompt;
        input.listeners = [];
        input.onfocus = function() {
          caret_to_end(input); /* needed on IE 10 */
        }
        input.onkeypress = function(ev) {
          ev = ev || window.event;
          if(ev.keyCode == 13) {
            var content = input.value;
            input.value = prompt;
            var i = overlap(prompt,content);            // remove prompt prefix (if present)
            if (i > 0) content = content.substring(i);
            if (output != null && typeof output.print_html === "function") {
              output.print_html("<span style='color: gray'>" + html_escape(prompt + content) + "</span><br>")
            }
            if (input.listeners.length > 0) {
              var action = input.listeners.shift();
              action(null,content);
            }
          }
        };
      }

      return input;
    }

    _readline = function(action) {
      var inp = get_console_input();
      if (inp) {
        inp.listeners.push(action);
        if (input.focus) input.focus();
      }
    }

    _cancel_readline = function() {
      var inp = get_console_input();
      if (inp) {
		 if (inp.listeners.length > 0) {
	        inp.listeners.shift();
			inp.value = prompt;
	     }
      }
    };
  })();
}
