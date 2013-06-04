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
var marked = require("marked");
var commander = require("commander");
            
commander.version("0.1")
        .usage("[options] <files...>")
        .option("-o, --output <file>", "Specify output file")
        .parse(process.argv)

markdownFiles(commander.args,commander.output)

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

function markdownFile(inputFile,outputFile,cont) {
  if (inputFile) {
    if (!outputFile) {
      var match = inputFile.match(/(.*)\.xmp\.html$/);
      outputFile = (match ? match[1] : inputFile) + ".html";              
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

function markdown(input) 
{
  if (!input) return;
  var regXmp = /<xmp\b[^>]*>((?:[^<]|<(?!\/xmp))*)<\/xmp>/g;
  return input.replace(regXmp,function(all,content) { 
    return marked(content);
  });
}
