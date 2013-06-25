#!/usr/bin/env node
//----------------------------------------------------------------------------
// Copyright 2012 Microsoft Corporation, Daan Leijen
//
// This is free software; you can redistribute it and/or modify it under the
// terms of the Apache License, Version 2.0. A copy of the License can be
// found in the file "license.txt" at the root of this distribution.
//----------------------------------------------------------------------------
var fs = require("fs");
var path = require("path");
var marked = require("../../../marked/lib/marked");
var commander = require("commander");
            
commander.version("0.1")
        .usage(["[options] <files...>"
               ,""
               ,"  Process markdown on content between <xmp> tags in the given files."
               ,"  The <xmp> tags are removed and replaced by the processed markdown."
               ,""
               ,"  For a file <base>.xmp.html, the output file is by default <base>.html."
               ,"  For other files <file>, the output file is by default <file>.html."]
               .join("\n"))
        .option("-o, --output <file>", "Specify output file")
        .option("-b, --breaks", "Use Github style line breaks")
        .parse(process.argv)

markdownFiles(commander.args,commander.output)

// Process markdown between <xmp> tags in the given input files.
// Output is written to the given outputFile name, or a default is used.
function markdownFiles(inputFiles,outputFile,cont) 
{
  if (inputFiles && inputFiles instanceof Array && inputFiles.length > 0) {
    var inputFile = inputFiles.shift();
    markdownFile(inputFile,outputFile,function() {
      markdownFiles(inputFiles,outputFile,cont);
    })
  }
  else if (inputFiles && typeof inputFiles === "string") {
    markdownFile(inputFiles,outputFile,cont);
  }
  else if (cont) cont();
}

// Process markdown between <xmp> tags for a given input file writing it to outputFile
function markdownFile(inputFile,outputFile,cont) {
  if (inputFile) {
    if (!outputFile) {
      var match = inputFile.match(/(.*)\.xmp(\.html?)$/);
      outputFile = (match ? match[1] + match[2] : inputFile + ".html");              
    } 
    // console.log("apply markdown: " + inputFile)
    fs.readFile(inputFile, { encoding: "utf8" }, function(err,input) {
      if (err) {
        console.log("markedxmp: unable to read: " + inputFile);
        if (cont) cont();
      }
      else {
        var output = markdown(input);
        fs.writeFile(outputFile,output,cont);
      }
    });
  } 
  else if (cont) cont();  
}

// Given input as a string, process markdown between <xmp> tags.
function markdown(input) 
{
  if (!input) return;
  var regXmp = /<xmp\b[^>]*>((?:[^<]|<(?!\/xmp))*)<\/xmp>/g;
  return input.replace(regXmp,function(all,content) { 
    return marked(content, { smartypants: true, extra: true, breaks: (commander.breaks ? true : false) });
  });
}
