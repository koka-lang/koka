/*---------------------------------------------------------------------------
  Copyright 2012 Microsoft Corporation.
 
  This is free software; you can redistribute it and/or modify it under the
  terms of the Apache License, Version 2.0. A copy of the License can be
  found in the file "license.txt" at the root of this distribution.
---------------------------------------------------------------------------*/

function $regexCreate( s, ignoreCase, multiLine ) 
{
  // we always use "g" flag. 
  // to re-use safely, we always need to explicitly set 'lastIndex' on each use!
  var flags = (ignoreCase!==0 ? "i" : "") + (multiLine!==0 ? "m" : "");
  return { regex: new RegExp( s, "g" + flags), flags: flags }
}

function $regexExec( r,  s, start ) 
{
  r.regex.lastIndex = start;
  var match = r.regex.exec(s);
  return (match==null ? Matched(-1,"",[""]) : Matched(match.index, match[0], match) ); 
}  

function $regexExecAll( r,  s, start ) 
{
  r.regex.lastIndex = start;
  var result = [];
  var match;
  while (match = r.regex.exec(s)) {
    result.push( Matched( match.index, match[0], match ) );
    if (r.regex.lastIndex === match.index) r.regex.lastIndex++; // avoid loop on zero-length match
  }
  return result;
}  

function $regexSearch( r, s, start ) {
  var res = $regexExec(r,s,start);
  return res.index;
}

function $regexSplit( r, s, n, start ) {
  r.regex.lastIndex = start;
  return (n <= 0 ? s.split( r.regex ) : s.split( r.regex, n ));
}

function $regexReplaceFun( r,  s, repl, all, start) 
{
  var regex = r.regex;
  if (all === 0) {                                       // only apply once?
    var regex = new RegExp( r.regex.source, r.flags );   //  then we need a recompile without 'g'
  }    
  regex.lastIndex = start;  
  return s.replace( regex, function() {
    if (arguments.length < 3) return ""; // should never happen!
    var index = arguments[arguments.length-2];
    var groups = [];
    for(var i = 0; i < arguments.length-2; i++) {
      groups[i] = arguments[i];
    }
    return repl( Matched( index, groups[0], groups) );      
  });
}

function $regexReplace( r, s, repl, all, start ) 
{
  var regex = r.regex;
  if (all === 0) {  // only once?
    var regex = new RegExp( r.regex.source, r.flags ); // then recompile without 'g'
  }
  regex.lastIndex = start; 
  return s.replace( regex, repl );
}

