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
var child = require("child_process");

var Colors = require("colors");
var Diff   = require("diff");

//-----------------------------------------------------
// Get the version from the package.json file
//-----------------------------------------------------
var version = "<unknown>";

function setVersion() {
  var content = fs.readFileSync("package.json",{encoding: "utf8"});
  if (content) {
    var matches = content.match(/"version"\s*\:\s*"([\w\.\-]+)"/);
    if (matches && matches.length >= 2) version = matches[1];
  }
}
setVersion();

//-----------------------------------------------------
// Configuration
//-----------------------------------------------------
var exeExt      = (path.sep==="\\" ? ".exe" : "");

var platformVariant = "cpp"; // other options: hugs and haddock
var platformVariantPath = path.join("Platform",platformVariant,"Platform")+path.sep;

var hsCompiler  = "ghc";
var hsFlags     = "-fwarn-incomplete-patterns";
var hsRunFlags  = "";
var hsPackages  = ["text","parsec"];
var hsLinkFlags = (["base","containers","directory","process","mtl"].concat(hsPackages)).map(function(p){ return "-package " + p; }).join(" ");

var alexCompiler= "alex";
var alexFlags   = "--latin1";

var main      = "koka";
var sourceDir = "src";
var outputDir = "out";
var libraryDir= "lib";
var testDir   = "test";

var variant   = process.env.variant || "debug";
var buildDir  = path.join(outputDir, variant);
var depFile   = path.join(buildDir,"dependencies");
var mainExe   = path.join(buildDir,main + "-" + version + exeExt);

var kokaFlags = "-i" + libraryDir + " -itest/algeff -itest/async -itest/resource -itest/lib --core --checkcore " + (process.env.kokaFlags || "");

if (variant === "profile") {
  hsFlags += " -prof -fprof-auto -O2";
  hsLinkFlags += " -rtsopts";
  hsRunFlags += " +RTS -p -RTS"
}
else if (variant === "release") {
  hsFlags += " -O2";
}
else if (variant === "trace") {
  // add profile information so failure can use Debug.traceStack
  hsFlags += " -prof -fprof-auto"
  hsLinkFlags += " -prof -rtsopts"
  // for now a bit useless since 'internal exceptions' are raised on things like doesFileExist...
  hsRunFlags  += " +RTS -xc -RTS"
}



//-----------------------------------------------------
// Tasks: compilation
//-----------------------------------------------------
task("default",["interactive"]);

desc("build and run the compiler (default)");
task("interactive", ["config","compiler"], function(rebuild) {
  var libDir = path.join(outputDir,libraryDir);
  jake.mkdirP(libDir);
  var cmd = mainExe + " " + hsRunFlags + " --outdir=" + libDir + " " + kokaFlags;
  jake.logger.log("> " + cmd);
  jake.exec(cmd + " 2>&1", {interactive: true});
},{async:true});


desc(["build the compiler (" + mainExe + ")",
      "     compiler[rebuild]  # force a rebuild (where dependencies are ignored)"].join("\n"));
task("compiler", [], function(rebuild) {
  rebuild = rebuild || false;
  initializeDeps(allItems,false,function(items) {
    build("koka " + version + " (" + variant + " version)",rebuild,items);
  });
},{async:true});


desc("load the compiler in ghci");
task("ghci", ["compiler"], function(module) {
  var cmd = "ghci " + path.join(outputDir,variant,"Platform","cconsole.o") + hsRunFlags
                + " -i" + sourceDir + " -i" + path.join(sourceDir,"Platform","cpp")
                  + " " + path.join(sourceDir,(module ? module + ".hs" : "Main.hs"));
  jake.logger.log("> " + cmd);
  jake.exec(cmd + " 2>&1", {interactive: true});
});


desc("run 'cabal install' to install prerequisites");
task("config", [], function () {
  jake.logger.log("check for packages: " + hsPackages.join(" "))
  child.exec("ghc-pkg list", function (error, stdout, stderr) {
    if (error) stdout = "";
    var foundall = hsPackages.every( function(pkg) {
      return ((new RegExp("\\b" + pkg + "-")).test(stdout));
    });
    if (foundall) {
      complete();
    }
    else {
      var cmd = "cabal install text random parsec"
      jake.logger.log("> " + cmd)
      jake.exec(cmd + " 2>&1", {interactive: true}, function() { complete(); });
    }
  });
},{async:true});


//-----------------------------------------------------
// Tasks: clean
//-----------------------------------------------------
desc("remove all generated files");
task("clean", function() {
  jake.logger.log("remove all generated files");
  jake.rmRf(outputDir);
});

desc("remove just koka generated interface files");
task("iclean", function() {
  jake.logger.log("remove koka generated files");
  var outdirs = new jake.FileList()
                 .include(path.join(outputDir,"*"))
                 .exclude(["trace","debug","release","profile"].map(function(s){ return path.join(outputDir,s); }))
                 .toArray();
  outdirs.forEach( function(dirName) { jake.rmRf(dirName); } );
});


//-----------------------------------------------------
// Tasks: test
//-----------------------------------------------------
desc(["run tests.",
      "     test[<dir|file>,<mode>]  # use <dir|file> to run the tests ('test')",
      "                              # optional <mode> is 'update','new',or 'verbose'"].join("\n"));
task("test", ["compiler"], function(testdir,testmode) {
  testdir=testdir||"test";
  testmode=testmode||"";
  // jake.rmRf(path.join(outputDir,"test"))
  jake.mkdirP("out/test/config");
  runTests(pathnorm(testdir),testmode);
});

//-----------------------------------------------------
// Tasks: c-grammar specification
//-----------------------------------------------------
desc(["compile flex/bison grammar",
      "     grammar[<file>]    # run parser on <file>"].join("\n"));
task("grammar",[],function(testfile)
{
  var outdir = path.join(outputDir,"grammar");
  var gdir = path.join("doc","spec","grammar");
  jake.cpR(gdir,outputDir);

  command("cd " + outdir + " && bison -vd -W parser.y 2>&1", function() {
    command("cd " + outdir + " && flex -v8 lexer.lex 2>&1", function() {
      command( "cd " + outdir + " && ghc -no-hs-main -o koka-parser lex.yy.c parser.tab.c", function () {
        if (testfile==null) complete();
         else {
            console.log("testing..")
            command( path.join(outdir,"koka-parser") + " --nosemi " + testfile, function() {
              complete();
            });
         };
      });
    });
  });
});

//-----------------------------------------------------
// Tasks: documentation generation & editor support
//-----------------------------------------------------
var cmdMarkdown = "node ../../../madoko/lib/cli.js"; // "madoko";
var docsite  = (process.env.docsite || "https://koka-lang.github.io/koka/doc/"); // http://research.microsoft.com/en-us/um/people/daan/koka/doc/");
var doclocal = (process.env.doclocal || "..\\koka-pages\\doc"); // \\\\research\\root\\web\\external\\en-us\\UM\\People\\daan\\koka\\doc");

desc("generate the language specification")
task("spec", ["compiler"], function(mode) {
  jake.logger.log("build language specification");
  var outspec   = path.join(outputDir,"spec");
  var outstyles = path.join(outspec,"styles");
  var outscripts = path.join(outspec,"scripts");
  var specdir   = path.join("doc","spec");
  var docflags  = "--htmlcss=styles/madoko.css;styles/koka.css " + ((mode === "publish") ? "--htmlbases=" + docsite + " " : "");
  var cmd = mainExe + " -c -l --outdir=" + outspec +  " -i" + specdir + " --html " + docflags + kokaFlags + " ";
  command(cmd + "kokaspec.kk.md spec.kk.md getstarted.kk.md overview.kk.md", function() {
    command(cmd + "toc.kk", function() {
      // fix up includes
      patchFile(path.join(outspec,"kokaspec.md"),/^\[INCLUDE=(\w+)\.kk\.md\]/mg, "[INCLUDE=$1.md]");
      // copy style files
      jake.mkdirP(outstyles);
      jake.mkdirP(outscripts);
      jake.cpR(path.join("doc","koka.css"),outstyles);
      var files = new jake.FileList().include(path.join(specdir,"styles/*.css"))
                                     .include(path.join(specdir,"styles/*.mdk"))
                                     .toArray();
      copyFiles(specdir,files,outspec);
      files = new jake.FileList().include(path.join(specdir,"scripts/*.js"))
                                 .toArray();
      copyFiles(specdir,files,outspec);
      files = new jake.FileList().include(path.join(specdir,"*.bib"))
                                 .toArray();
      copyFiles(specdir,files,outspec);
      // copy images
      var imgs1 = new jake.FileList().include("lib/std/time/*.png").toArray();
      copyFiles("lib/std/time",imgs1,outspec);
      // process xmp.html to html using madoko
      var xmpFiles = new jake.FileList().include(path.join(outspec,"*.xmp.html"))
                                        .include(path.join(outspec,"kokaspec.md"))
                                        .toArray().join(" ").replace(new RegExp("out[\\/\\\\]spec[\\/\\\\]","g"),"");
      console.log(xmpFiles)
      command("cd " + outspec + " && " + cmdMarkdown + " --odir=." + " -v -mline-no:false -mlogo:false " + xmpFiles, function () {
        jake.cpR(path.join(outspec,"madoko.css"),outstyles);
        if (mode === "publish") {
          // copy to website
          files = new jake.FileList().include(path.join(outspec,"*.html"))
                                     .include(path.join(outstyles,"*.css"))
                                     .exclude(path.join(outspec,"*.xmp.html"))
                                     .toArray();
          copyFiles(outspec,files,doclocal);
        }
        complete();
      });
    });
  });
});

desc("generate the tutorial");
task("guide", ["compiler"], function(publish) {
  jake.logger.log("build rise4fun tutorial");
  var outguide  = path.join(outputDir,"guide");
  var outstyles = path.join(outguide,"styles");
  var guidedir  = path.join("doc","rise4fun");
  var docflags  = publish ? "--htmlbases=" + docsite + " " : "";
  var cmd = mainExe + " -c -l --outdir=" + outguide + " -i" + guidedir + " --html " + docflags + kokaFlags + " ";
  command(cmd + "guide.kk.md", function() {
    // convert markdown
    command(cmdMarkdown + " --odir=" + outguide + " -v " + path.join(outguide,"guide.md"), function() {
      // copy style files
      jake.mkdirP(outstyles);
      jake.cpR(path.join("doc","koka.css"),outstyles);
      jake.cpR(path.join("doc","rise4fun","risekoka.css"),outstyles);
    });
  });
});

desc(["install Sublime Text 3 support files for Koka",
     "     sublime[<version>]  # install for <version> instead (2 or 3)"].join("\n")
    );
task("sublime", function(sversion) {
  jake.logger.log("install Sublime Text support");
  var sublime =　"";
  var sversion = sversion || "3"
  if (process.env.APPDATA) {
    sublime = path.join(process.env.APPDATA,"Sublime Text " + sversion);
  }
  else if (process.env.HOME) {
    if (path.platform === "darwin")
      sublime = path.join(process.env.HOME,"Library","Application Support","Sublime Text " + sversion);
    else
      sublime = path.join(process.env.HOME,".config","sublime-text-" + sversion);
  }
  sublime = path.join(sublime,"Packages");

  if (!fileExist(sublime)) {
    jake.logger.error("error: cannot find sublime package directory: " + sublime);
  }
  else {
    var dirCS = "Color Scheme - Default";
    var sublimeCS = path.join(sublime,dirCS);

    jake.mkdirP(sublimeCS);
    jake.cpR(path.join("support","sublime-text","Koka"),sublime);
    jake.cpR(path.join("support","sublime-text",dirCS,"Snow.tmTheme"),sublimeCS);
    jake.cpR(path.join("support","sublime-text","Jake-Haskell.sublime-build"),path.join(sublime,"User"));
  }
});

// Help
var usageInfo = [
  "usage: jake target[options] [variant=<build>]",
  "  <options>        are target specific, i.e. jake test[./test/type]",
  "  variant=<build>  any of 'release', 'profile', 'trace', or 'debug' (default)",
  ""].join("\n");

function showHelp() {
  jake.logger.log(usageInfo);
  jake.showAllTaskDescriptions(jake.program.opts.tasks);
  process.exit();
}

desc("show this information");
task("help",[],function() {
  showHelp();
});
task("?",["help"]);

if (process.argv.indexOf("-?") >= 0 || process.argv.indexOf("?") >= 0) {
  showHelp();
}
else if (jake.program.opts.tasks) {
  jake.logger.log(usageInfo);
};



//-----------------------------------------------------
// Sources
// Note: the sources must be given in a canonical build order.
//-----------------------------------------------------
var cSources = [
  cSource("Platform/cconsole.c",[sourcePath("Platform/cconsole.h")],"-fPIC"),
];

// small helper to quote pre-processor define options in Platform.Config
function defD(name,val) { return ("-D" + name + (val ? "=\\\"" + val + "\\\" " : " ")); }

var defWindows = (/^win.*$/.test(process.platform) ? " -DWINDOWS" : "");

var hsModules = [
  { name: "Platform.Config",
      flags: defD("MAIN",main) + defD("VERSION",version) + defD("VARIANT",variant) + defWindows,
      deps: ["package.json"] }, // dependent on this build file (due to version)
  "Platform.Runtime",
  "Platform.Var",
  { name: "Platform.Console", deps: [sourcePath("Platform/cconsole.c")] },
  "Platform.ReadLine",
  "Platform.GetOptions",
  "Platform.Filetime",
//  "Platform.IntMap",
//  "Platform.IntSet",

  "Lib.Printer",
  "Lib.PPrint",
//  "Lib.Set",
//  "Lib.Map",
//  "Lib.MultiSet",
//  "Lib.Seq",
  "Lib.Scc",
  "Lib.Trace",
  "Lib.JSON",

  "Common.Failure",
  "Common.ColorScheme",
  "Common.File",
  "Common.Syntax",
  "Common.Name",
  "Common.NameMap",
  "Common.NameSet",
  "Common.NamePrim",
  "Common.QNameMap",
  "Common.Id",
  "Common.IdMap",
  "Common.IdSet",
  "Common.IdNice",
  "Common.Range",
  "Common.Message",
  "Common.Unique",
  "Common.Error",

  "Syntax.Lexeme",
  alexModule("Syntax.Lexer"),
  { name: "Syntax.Lexer", deps: [sourcePath("Syntax/Lexer.x")] },
  "Syntax.Layout",
  "Syntax.Highlight",
  "Syntax.Syntax",
  "Syntax.Promote",
  "Syntax.Parse",

  "Kind.ImportMap",
  "Kind.Kind",
  "Kind.Pretty",
  "Type.Type",
  "Type.Kind",
  "Type.TypeVar",
  "Type.Pretty",

  "Static.FixityResolve",
  "Static.BindingGroups",
  "Core.Core",

  "Kind.Synonym",
  "Kind.Constructors",
  "Kind.Newtypes",
  "Kind.Assumption",
  "Core.Pretty",
  "Core.CoreVar",
  "Type.Assumption",
  "Syntax.RangeMap",

  "Kind.InferKind",
  "Kind.InferMonad",
  "Kind.Unify",
  "Kind.Infer",

  "Type.Operations",
  "Type.Unify",
  "Type.InfGamma",
  "Type.InferMonad",

  "Core.AnalysisMatch",
  "Core.Uniquefy",
  "Core.Divergent",
  "Core.BindingGroups",
  "Core.UnReturn",
  "Core.Monadic",
  "Core.AnalysisResume",
  "Core.Simplify",

  "Type.Infer",

  "Backend.CSharp.FromCore",
  "Backend.JavaScript.FromCore",

  "Syntax.Colorize",
  "Core.GenDoc",
  "Core.Parse",
  "Core.Check",

  "Compiler.Package",
  "Compiler.Options",
  "Compiler.Module",
  "Compiler.Compile",
  "Interpreter.Command",
  "Interpreter.Interpret",
  "Main",
];

var hsSources  = hsModules.map( function(mod) { return hsModule(mod); } );
var allSources = cSources.concat(hsSources);

var exeItems = [
  exeItem(mainExe,objectsFromSources(allSources)),
];

var allItems = allSources.concat(exeItems);


//-----------------------------------------------------
// Action items
//-----------------------------------------------------

function hsModule(mod,deps) {
  var src = (typeof mod === "string" ? src = { name: mod } : src = mod);
  if (src.cmds) return src; // already fully initialized (like alexModule)

  src.source = sourcePathFromModule(src.name,".hs");
  src.deps = src.deps || [];
  src.deps.unshift(src.source);
  src.outputs = [objectFile(src.source),objectFile(src.source,".hi")];
  src.cmds = hsCompile(src.source,src.outputs[0],src.flags);
  src.infdeps = null;
  src.infdepsUpdate = function(callback) { hsUpdateDeps(src.source,src,callback); };
  return src;
}

function alexModule(modName,deps,flags) {
  var src = { name: modName + ".x", source: sourcePathFromModule(modName,".x"), deps: deps || [] };
  src.deps.unshift(src.source);
  src.outputs = [sourcePathFromModule(modName,".hs")];
  src.cmds = alexCompile(src.source,src.outputs[0],flags);
  src.infdeps = []; // do not infer
  //src.infdepsUpdate = null; // function(callback) { hsUpdateDeps(src.source,src,callback); };
  return src;
}

function cSource(srcName,deps,flags) {
  var src = { name: srcName, source: sourcePath(srcName), deps: deps || [] };
  src.deps.unshift(src.source);
  src.outputs = [objectFile(src.source)];
  src.cmds = cCompile(src.source,src.outputs[0],flags);
  return src;
}

function exeItem(target,objectFiles,flags) {
  var src = { name: target, deps: objectFiles };
  if (path.extname(target) !== exeExt) target += exeExt;
  src.outputs = [target];
  src.cmds = exeLink(objectFiles,target,flags);
  return src;
}


//-----------------------------------------------------
// Compilation commands
//-----------------------------------------------------
function hsCompile(fileName,targetName,flags) {
  return ghcCompile(fileName,targetName,flags);
}

function ghcCompile(filename,targetName,flags) {
  flags = flags || "";
  return [hsCompiler,"-c " + filename,hsFlags,"-i" + buildDir,"-odir " + buildDir,"-hidir " + buildDir,flags].join(" ");
}

function cCompile(filename,targetName,flags) {
  flags = flags || "";
  var ensureDir = function(callback) { jake.mkdirP(path.dirname(targetName)); callback(); };
  ensureDir.msg = "mkdir -p " + path.dirname(targetName);
  return [ensureDir, [hsCompiler,"-c " + filename,hsFlags,"-i" + buildDir,"-o " + targetName,flags].join(" ") ];
}

function exeLink(objNames,targetName,flags) {
  flags = flags || "";
  return [hsCompiler,"-o " + targetName,hsFlags,hsLinkFlags,flags,objNames.join(" ")].join(" ");
}

function alexCompile(fileName,targetName,flags) {
  flags = flags || "";
  return [alexCompiler,"-o " + targetName,alexFlags,flags,fileName].join(" ");
}


//-----------------------------------------------------
// Utility functions
//-----------------------------------------------------
function pathnorm(fname) {
  return fname.replace(/[/\\]/g,path.sep);
}

function sourcePathFromModule(moduleName,extension) {
  extension = extension || ".hs";
  var sourceName = moduleName.replace(/\./g,path.sep) + extension;
  return sourcePath(sourceName);
}

function sourcePath(sourceName) {
  sourceName = pathnorm(sourceName).replace(new RegExp("^Platform\\" + path.sep), platformVariantPath );
  return path.join(sourceDir,sourceName);
}

function objectFile(fname,extension) {
  // header files have no object file
  if (/\.h$/.test(fname)) {
    return fname;
  }
  var filename = fname.replace(platformVariantPath, "Platform" + path.sep);
  // otherwise return the corresponding object file
  extension = extension || ".o";
  return filename.replace(sourceDir,buildDir).replace(/\.[a-z]+$/,extension);
}

function objectsFromSources(sources) {
  var objs = [];
  sources.forEach(function(src) {
    src.outputs.forEach(function(out){
      if (path.extname(out) === ".o") objs.push(out);
    });
  });
  return objs;
}

function fileExist(fileName) {
  var stats = null;
  try {
    stats = fs.statSync(fileName);
  }
  catch(e) {};
  return (stats != null);
}

function normalize(filename) {
  if (filename==null) return "";
  return filename.replace(/\\/g,"/");
}

// copyFiles 'files' to 'destdir' where the files in destdir are named relative to 'rootdir'
// i.e. copyFiles('A',['A/B/c.txt'],'D')  creates 'D/B/c.txt'
function copyFiles(rootdir,files,destdir) {
  rootdir = normalize(rootdir || "");
  files.forEach(function(filename) {
    filename = normalize(filename);
    // make relative
    var destname = path.join(destdir,(rootdir && filename.lastIndexOf(rootdir,0)===0 ? filename.substr(rootdir.length) : filename));
    var logfilename = (filename.length > 30 ? "..." + filename.substr(filename.length-30) : filename);
    var logdestname = (destname.length > 30 ? "..." + destname.substr(destname.length-30) : destname);
    jake.logger.log("cp -r " + logfilename + " " + logdestname);
    jake.cpR(filename,path.dirname(destname));
  })
}

//-----------------------------------------------------
// Manage inferred depencies:
// usually calculated when an source is compiled
//-----------------------------------------------------

function hsUpdateDeps(sourceName,item,callback) {
  importDeps(sourceName, function(imports) {
    var infdeps = imports.map( function(s){ return objectFile(sourcePathFromModule(s),".hi"); });
    updateDeps(item,infdeps,callback);
  });
}


// Return the imported module names from a haskell (or alex) file
function importDeps(fileName, callback) {
  var src = fs.readFileSync(fileName,{encoding:"utf8"}); // somehow must be synchronous or interactive processes afterwards read the console wrongly :-(
  var imports = [];
  var r   = /^\s*import\s+(qualified\s+)?([\w\'\.]+)/gm;
  var matches;
  while ((matches = r.exec(src)) !== null) {
    var mod = matches[2];
    if (findItem(hsModules,mod)) {
      imports.push(mod);
    }
  }
  callback(imports);
}

function updateDeps(item,infdeps,callback) {
  if (item.infdeps && item.infdeps.join() === infdeps.join()) {
    //jake.logger.log("saved write")
    callback();
  }
  else {
    //jake.logger.log("deps: " + targetName + ": " + infdeps.join(","));
    item.infdeps = infdeps;
    saveDeps(allItems,callback);  // hack: since the global allItems is updated in-place we can save from here
  }
}

function saveDeps(items,callback) {
  var content = items.map( function(item) {
    if (item.infdeps) {
      return item.outputs[0] + ":" + item.infdeps.join(",");
    }
    else return "";
  }).join("\n");
  //jake.logger.log("write: " + depFile);
  fs.writeFileSync(depFile,content,{encoding: "utf8"}); // again, must be synchronous :-(
  callback();
}

function initializeDeps(items,nodepend,callback) {
  if (nodepend) {
    if (callback) callback(items);
  }
  else {
    var content = null;
    try { content = fs.readFileSync(depFile,{encoding:"utf8"}); } catch(e) {}; // again, must be synchronous :-(
      var lines = (content ? content.split("\n") : []);
      lines.forEach( function(line) {
        if (!line)  return;
        line = line.trim();
        if (line.length === 0) return;
        var parts = line.split(":");
        var target = parts[0];
        var deps   = (parts.length === 2 && parts[1].length > 0 ? parts[1].split(",") : []);
        var item = findItem(items,target);
        if (item) {
          item.infdeps = (deps ? deps : []);
          // jake.logger.log("set " + item.name + ": " + "[" + deps.join(",") + "]");
        }
      });
      if (callback) callback(items);
  }
}

function findItem(items,name) {
  var i;
  for(i = 0; i < items.length; i++) {
    var item = items[i];
    if (item === name || item.name === name || (item.outputs && item.outputs[0] === name)) return item;
  }
  //jake.logger.log("could not find: " + name)
  return null;
}

//-----------------------------------------------------
// Build from a list of actions
// { name: string
//   outputs : [string]
//   inputs : [string]
//   cmds : [string]
// }
//-----------------------------------------------------
function fileTime( fname ) {
  try {
    return fs.statSync(fname).mtime;
  }
  catch(e) {
    return null;
  }
}

function shouldRun(outputs,inputs,infdeps) {
  // if no inferred depencies, we must run too
  // note: null = no inferred depencies yet. undefined = we never infer for this kind of target
  if (infdeps===null) return true;

  // find oldest output
  var outTime = null;
  var i;
  for(i=0; i<outputs.length; i++) {
    var tm = fileTime(outputs[i]);
    if (tm===null) {
      // jake.logger.log("output does not exist: " + outputs[i]);
      return true;
    }
    if (outTime===null) outTime = tm; // only first entry determines, this is for .hi files
  }

  // check all inputs
  if (infdeps)  inputs = inputs.concat(infdeps);
  for (i=0; i<inputs.length; i++) {
    var tm = fileTime(inputs[i]);
    if (tm===null) {
      jake.logger.log("require: " + inputs[i] + ": but no action to build it?");
      return true;  // hopefully, some other task will provide it as output?
    }
    if (tm > outTime) return true;
  }

  return false;
}

function build( msg, rebuild, srcs, callback, current ) {
  current = current || 0;

  if (current===0) {
    jake.logger.log("build: " + msg);
  }
  if (!srcs || current >= srcs.length) {
    jake.logger.log("build ok.");
    if (callback) {
      callback();
    }
    else {
      complete();
    }
  }
  else {
    var src = srcs[current];
    var buildrest = function(){ build(msg,rebuild,srcs,callback,current+1); }
    if (rebuild || shouldRun(src.outputs,src.deps,src.infdeps)) {
      var run = function() { command(src.cmds, buildrest ); };
      if (src.infdepsUpdate)
        src.infdepsUpdate(run);
      else
        run();
    }
    else {
      buildrest();
    }
  }
}


//-----------------------------------------------------
// Execute a command
//-----------------------------------------------------
function command(cmds,callback,current) {
  current = current || 0;
  if (typeof cmds === "string" || typeof cmds === "function") cmds = [cmds];

  if (!cmds || current >= cmds.length) {
    if (callback) callback();
    return;
  }

  var cmd = cmds[current];
  var fullmsg = (cmd.msg ? cmd.msg : (typeof cmd === "string" ? cmd : "")).trim();
  var msg = fullmsg;
  var msglen = 74;
  if (msg && msg.length > msglen) {
    msg = msg.substr(0,msglen) + "..."
  }
  if (msg) jake.logger.log("> " + msg);
  if (typeof cmd === "string") {
    child.exec(cmd, function (error, stdout, stderr) {
      var logout = (error ? console.log : jake.logger.log);
      function logerr(msg) {
        if (/^.*\berror:/.test(msg)) {
          msg = Colors.bold(Colors.red(msg));
        }
        else if (/^.*\bwarning:/.test(msg)) {
          msg = Colors.green(msg);
        }
        else {
          msg = Colors.bold(msg);
        }
        if (error) console.error(msg); else jake.logger.error(msg);
      }
      if (error !== null && fullmsg && fullmsg.length > msglen) {
        logout(jake.program.opts.quiet ? fullmsg : fullmsg.substr(msglen)); // show rest of command on error
      }
      if (stdout && stdout.length > 0) logout(stdout.trim());
      if (stderr && stderr.length > 0) logerr(stderr.trim());
      if (error !== null) {
        logerr(("> " + cmd))
        logerr(("\ncommand failed with exit code " + error.code + "."));
        if (/error:\s+Failed to load interface for/.test(stderr)) {
          logerr(["","---------------------------------------------------------------",
                     "Perhaps you did not install all required Haskell packages?",
                     "Run  \"jake config\"  first to install required Haskell packages.",
                     "---------------------------------------------------------------"].join("\n"));
        }
        process.exit(1);
      }
      command(cmds,callback,current+1);
    });
  }
  else {
    cmd(function() { command(cmds,callback,current+1); })
  }
}

//-----------------------------------------------------
// Tests
//-----------------------------------------------------
var testMessage = "total time ";

function runTests(test,testMode,flags,callback) {
  testMode = testMode||"";
  flags = flags || ("-i" + libraryDir + " -i" + testDir + " --outdir=" + path.join(outputDir,"test"));
  flags = "--checkcore " + flags
  fs.stat(test,function(err,stats) {
    if (err) {
      //jake.logger.error("file or directory does not exist: " + test);
      //process.exit(1);
    }
    var tests = [];
    if (!err && stats.isDirectory()) {
      var testList = new jake.FileList();
      testList.include(path.join(test,"*.kk"));
      testList.include(path.join(test,"*","*.kk"));
      testList.include(path.join(test,"*","*","*.kk"));
      tests = testList.toArray();
    }
    else {
      var testList = new jake.FileList();
      testList.include(test);
      // testList.include(test + ".kk");
      tests = testList.toArray();
    }
    if (err && (tests == null || tests == [] )) {
      jake.logger.error("file or directory does not exist: " + test);
      process.exit(1);
    }
    // jake.logger.log("run " + (tests.length===1 ? "test" : tests.length + " tests over") + ": " + test);
    console.time(testMessage);
    runTestList(tests.length,testMode,tests,flags,callback);
  })
}

function runTestList(n,testMode,tests,flags,callback,nooutCount,failedCount) {
  nooutCount = nooutCount || 0;
  failedCount = failedCount || 0;
  if (n <= 0) {
    var total = tests.length;
    jake.logger.log("");
    jake.logger.log("total tests: " + total);
    jake.logger.log("tested     : " + (total - nooutCount));
    jake.logger.log("failed     : " + failedCount);
    console.timeEnd(testMessage);
    if (callback) callback();
             else complete();
  }
  else {
    runTest(n,testMode,tests[tests.length - n],flags,function(status) {
      if (status === 2) failedCount++;
      if (status === 1) nooutCount++;
      runTestList(n-1,testMode,tests,flags,callback,nooutCount,failedCount);
    });
  }
}


function runTest(n,testMode,testFile,flags,callback) {
  callback = callback || function(status) { complete(); };
  runTestFile(n,testFile,testMode,flags,function(output) {
    var expectFile = testFile + ".out";
    fs.readFile(expectFile,{encoding: "utf8"},function(err,content) {
      if (err) {
        if (testMode==="new") {
          fs.writeFile(expectFile,output,{encoding: "utf8"},function(errout) {
            jake.logger.log( n + ": " + (errout ? errout.message : "wrote output file.") );
            callback(1);
          });
        }
        else {
          if (testMode==="verbose") {
            jake.logger.log(output)
          }
          jake.logger.log( n + ": no out file." );
          callback(1);
        }
      }
      else if (testMode==="update") {
        fs.writeFile(expectFile,output,{encoding: "utf8"},function(errout) {
          jake.logger.log( n + ": " + (errout ? errout.message : "updated output file.") );
          callback(1);
        });
      }
      else {
        if (testMode==="new") {
          jake.logger.log(n + ": output file already exists: use mode 'update' instead of 'new'" );
        }
        else if (testMode==="verbose") {
          var fcontent = fs.readFileSync(testFile);
          jake.logger.log( "------ verbose test file ------------\n" + fcontent + "------ end test file ----------------" );
        }
        content = testSanitize(content);
        if (content == output) {
          jake.logger.log( n + ": ok.");
          callback(0);
        }
        else {
          //jake.logger.log( n + ": failed!" );
          /*
          var msg = "----- expected output -----\n" + content
                      + "\n----- actual output -----\n" + output
                      + "\n-------------------------";
          msg = msg.split("\n").map(function(line) { return "    " + line }).join("\n");
          jake.logger.log( msg );
          */
          process.stderr.write("----- difference -----\n");
          var diff = Diff.diffTrimmedLines(content,output);
          diff.forEach(function(part){
            // green for additions, red for deletions
            // grey for common parts
            var color = (part.added ? 'green' : (part.removed ? 'red' : 'grey'));
            process.stderr.write(part.value[color]);
            if (part.removed) process.stderr.write("\n");
          });
          process.stderr.write("\n");

          jake.logger.log( n + ": " + testFile + " failed.\n" );
          callback(2);
        }
      }
    })
  });
}

function runTestFile(n,testFile,testMode,flags,callback) {
  var testDir = path.dirname(testFile);
  var flags = flags || "";
  fs.readFile(path.join(testDir,".flags"), { encoding: "utf8" }, function(err,content) {
    if (!err) flags += " " + content.trim().replace("\n"," ");
    fs.readFile(testFile + ".flags", { encoding: "utf8" }, function(err,content) {
      if (!err) flags += " " + content.trim().replace("\n"," ");
      var cmd = [mainExe,hsRunFlags," -c --console=raw",flags,testFile].join(" ");
      jake.logger.log(n + ": " + testFile);
      if (testMode==="verbose") jake.logger.log("> " + cmd);
      child.exec(cmd, function (error, stdout, stderr) {
        var output = "";
        if (stdout && stdout.length > 0) output += stdout;
        if (stderr && stderr.length > 0) output += stderr;
        if (error !== null) {
          // output += "command failed with exit code " + error.code + ".";
        }
        if (testMode) jake.logger.log(output);
        output = testSanitize(output);
        if (callback) {
          callback(output);
        }
        else {
          jake.logger.log(output);
          complete();
        }
      });
    });
  });
}

function patchFile( fname, regex, replacer ) {
  var contents1 = fs.readFileSync( fname, {encoding:"utf8"});
  var contents2 = contents1.replace(regex,replacer);
  fs.writeFileSync(fname,contents2,{encoding:"utf8"});
}

function testSanitize(s) {
  return s.replace(__dirname,"...")  // no local directories
          .replace(/\\/g,"/")        // unix style slashes
          .replace(/[ \t]+/g, " ")   // compress whitespace
          .replace(/[\r\n]+/g, "\n") // compress newlines sequences
          .replace(/(std_core\.js\:)\d+/, "$1" )  // hide line number of an exception
          .replace(/\n\s+at .*/g, "") // hide stack trace in exceptions
          .trim();                     // and trim
}
