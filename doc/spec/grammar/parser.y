/* Copyright 2012-2021, Microsoft Research, Daan Leijen
   This is free software; you can redistribute it and/or modify it under the
   terms of the Apache License, Version 2.0.
*/
/* Use the "Yash" extension in vscode for nice syntax highlighting.
   Requires at least Bison 3+; you can get a version for windows from
   https://sourceforge.net/projects/winflexbison
   (use the "latest" zip package)
*/

/* %pure-parser */
%define api.pure

%{
typedef void* yyscan_t;
%}

%parse-param { yyscan_t scanner }
%lex-param { yyscan_t scanner }


/* token structure. The memory for Id and String is kept in a list and deallocated after parsing (in 'doneScanState') */
%union {
  const char*   Id;      /* used for operators OP too */
  const char*   String;  /* 'modified' UTF-8 string (\0 chars are encoded as \xC0\x80) */
  double        Float;
  unsigned long Int;
  unsigned int  Char;
}

%{
#include <stdio.h>
#define yylex        mylex


typedef int bool;
#define true (1==1)
#define false (!true)

void yyerror(YYLTYPE* loc, yyscan_t scanner, char* s, ...);
int  mylex( YYSTYPE* val, YYLTYPE* loc, yyscan_t scanner );
bool verbose(yyscan_t scanner);

#define printDecl(sort,name) printDeclEx(sort,name,verbose(scanner))
void printDeclEx( const char* sort, const char* name, bool verbose );
%}


%token <Id>     ID CONID OP IDOP QID  QCONID QIDOP WILDCARDID IMPLICITID '(' ')' '[' ']'
%token <Int>    INT
%token <Float>  FLOAT
%token <String> STRING
%token <Char>   CHAR

/* %token APP  */ /* '(' for applications */
/* %token IDX  */ /* '[' for indexing */

%token IF THEN ELSE ELIF
%token WITH IN
%token MATCH
%token RARROW LARROW

%token FUN FN VAL VAR
%token TYPE STRUCT EFFECT
%token ALIAS CON
%token FORALL EXISTS SOME

%token IMPORT IMPORT_EXTERN AS MODULE
%token PUB ABSTRACT
%token EXTERN
%token INFIX INFIXL INFIXR

%token LEX_WHITE LEX_COMMENT
%token INSERTED_SEMI EXPR_SEMI
%token LE ASSIGN DCOLON EXTEND
%token RETURN CTX CTX_HOLE

%token HANDLER HANDLE NAMED MASK OVERRIDE
%token CTL FINAL RAW
%token IFACE UNSAFE BREAK CONTINUE

%token ID_CO ID_DIV
%token ID_INLINE ID_NOINLINE
%token ID_C ID_CS ID_JS ID_FILE
%token ID_LINEAR ID_OPEN ID_EXTEND
%token ID_BEHIND
%token ID_VALUE ID_REFERENCE ID_SCOPED
%token ID_INITIALLY ID_FINALLY
%token ID_FIP ID_FBIP ID_TAIL
%token ID_LAZY

%type <Id>  varid conid qvarid qconid op
%type <Id>  identifier qidentifier qoperator qconstructor
%type <Id>  typeid modulepath binder
%type <Id>  fundecl aliasdecl typedecl externdecl puredecl

/* precedence declarations are in increasing order,
   i.e. the last precedence declaration has the highest precedence.
*/

/* resolve s/r conflict by shifting on ELSE so the ELSE binds to the closest IF.*/
%precedence THEN
%precedence ELSE ELIF

/* resolve s/r conflict to have a `FN funparams -> expr` span as far as possible,
   e.g. `fn(x) -> x + 1` is `(fn(x) -> x + 1)` and not `(fn(x) -> x) + 1`
   and  `fn(x) -> x.foo` is `(fn(x) -> x.foo)` and not `(fn(x) -> x).foo`
   note: we could avoid these rules by disallowing the `->` form in trailing lambdas.
*/
%precedence RARROW                     /* -> */
%precedence '(' '[' FN '{' '.'         /* applications */
%precedence OP ASSIGN '>' '<' '|'      /* operators */

/* %precedence '?' */

%%

/* ---------------------------------------------------------
-- Program
----------------------------------------------------------*/

program     : semis MODULE modulepath moduledecl  { printDecl("module",$3); }
            | moduledecl                          { printDecl("module","main"); }
            ;

moduledecl  : '{' semis imports '}' semis
            | semis imports
            ;

imports     : importdecl semis1 imports
            | eimports
            ;

eimports    : IMPORT_EXTERN externimpbody semis1 eimports
            | declarations
            ;

importdecl  : pub IMPORT modulepath
            | pub IMPORT modulepath '=' modulepath
            ;

modulepath  : varid                       { $$ = $1; }
            | qvarid                      { $$ = $1; }
            ;

pub         : PUB
            | /* empty */
            ;

semis1      : semis1 semi
            | semi
            ;

semis       : semis semi
            | /* empty */
            ;

semi        : ';'
            | INSERTED_SEMI
            ;


/* ---------------------------------------------------------
-- Top level declarations
----------------------------------------------------------*/

declarations: fixitydecl semis1 declarations
            | topdecls
            ;

fixitydecl  : pub fixity oplist1
            ;

fixity      : INFIX INT
            | INFIXR INT
            | INFIXL INT
            ;

oplist1     : oplist1 ',' identifier
            | identifier
            ;


topdecls    : topdecls1
            | /* empty */
            ;

topdecls1   : topdecls1 topdecl semis1
            | topdecl semis1
            /* error recovery */
            | topdecls1 error semis1
            | error semis1                                    { yyerror(&@1,scanner,"skipped top-level declaration");  }
            ;

topdecl     : pub puredecl                             { printDecl("value",$2); }
            | pub aliasdecl                            { printDecl("alias",$2); }
            | pub externdecl                           { printDecl("extern",$2); }
            | pub typedecl                             { printDecl("type",$2); }
            | ABSTRACT typedecl                        { printDecl("type",$2); }
            ;


/* ---------------------------------------------------------
-- External declarations
----------------------------------------------------------*/

externdecl  : inlinemod fipmod EXTERN qidentifier externtype externbody   { $$ = $4; }
            /* | IMPORT EXTERN externimpbody                                 { $$ = "<extern import>"; } */
            ;

externtype  : ':' typescheme
            | typeparams '(' parameters ')' annotres
            ;

externbody  : '{' semis externstats1 '}'
            | '{' semis '}'
            ;

externstats1: externstats1 externstat semis1
            | externstat semis1
            ;

externstat  : externtarget externinline STRING
            | externinline STRING
            ;

externinline: ID_INLINE
            | /* empty */
            ;


externimpbody: '=' externimp
            | '{' semis externimps1 '}'
            ;

externimps1 : externimps1 externimp semis1
            | externimp semis1
            ;

externimp   : externtarget varid STRING
            | externtarget '{' externvals1 '}'
            ;

externvals1 : externvals1 externval semis1
            | externval semis1
            ;

externval   : varid '=' STRING
            ;

externtarget: ID_CS
            | ID_JS
            | ID_C
            ;


/* ---------------------------------------------------------
-- Type declarations
----------------------------------------------------------*/
aliasdecl   : ALIAS typeid typeparams kannot '=' type     { $$ = $2; }
            ;

typedecl    : typemod TYPE typeid typeparams kannot typebody      { $$ = $3; }
            | structmod STRUCT typeid typeparams kannot conparams { $$ = $3; }
            | effectmod EFFECT varid typeparams kannot opdecls          { $$ = $3; }
            | effectmod EFFECT typeparams kannot operation              { $$ = "<operation>"; }
            | NAMED effectmod EFFECT varid typeparams kannot opdecls           { $$ = $4; }
            | NAMED effectmod EFFECT typeparams kannot operation               { $$ = "<operation>"; }
            | NAMED effectmod EFFECT varid typeparams kannot IN type opdecls   { $$ = $4; }  /* error on SCOPED (?) */
            ;

typemod     : structmod
            | ID_OPEN
            | ID_EXTEND
            | ID_CO
            | ID_DIV
            | ID_LAZY
            ;

structmod   : ID_VALUE
            | ID_REFERENCE
            | /* empty */
            ;

effectmod   : ID_DIV
            | ID_LINEAR
            | ID_LINEAR ID_DIV
            | /* empty */
            ;



typebody    : '{' semis constructors '}'
            | /* empty */
            ;

typeid      : '(' commas ')'      { $$ = "(,)"; }       /* tuples */
            | '[' ']'             { $$ = "[]"; }        /* lists */
            | '<' '>'             { $$ = "<>"; }        /* total effect */
            | '<' '|' '>'         { $$ = "<|>"; }       /* effect extension */
            | varid               { $$ = $1; }
            | CTX                 { $$ = "ctx"; }
            ;

commas      : commas1
            | /* empty */
            ;

commas1     : commas ','
            ;


constructors: constructors1 semis1
            | /* empty */
            ;

constructors1: constructors1 semis1 constructor
            | constructor
            ;

constructor : pub con conid typeparams conparams
            | pub ID_LAZY con conid typeparams conparams RARROW blockexpr
            ;

con         : CON
            | /* empty */
            ;

conparams   : '(' parameters1 ')'          /* deprecated */
            | '{' semis sconparams '}'
            | /* empty */
            ;

sconparams  : sconparams parameter semis1
            | /* empty */
            ;


/* ---------------------------------------------------------
-- Effect declarations
----------------------------------------------------------*/


opdecls     : '{' semis operations '}'
            ;

operations  : operations operation semis1
            | /* empty */
            ;

operation   : pub VAL identifier typeparams ':' tatomic
            | pub FUN identifier typeparams '(' pparameters ')' ':' tatomic
            | pub controlmod CTL identifier typeparams '(' pparameters ')' ':' tatomic
            ;


/* ---------------------------------------------------------
-- Pure (top-level) Declarations
----------------------------------------------------------*/
puredecl    : inlinemod VAL binder '=' blockexpr        { $$ = $3; }
            | inlinemod fipmod FUN qidentifier funbody  { $$ = $4; }
            ;

inlinemod   : ID_INLINE
            | ID_NOINLINE
            | /* empty */
            ;

fipmod      : tailmod ID_FIP fiplimit
            | tailmod ID_FBIP fiplimit
            | tailmod /* empty */
            ;

fiplimit    : '(' INT ')'
            | '(' '_' ')'
            | /* empty */
            ;

tailmod     : ID_TAIL
            | /* empty */
            ;

fundecl     : identifier funbody            { $$ = $1; }
            ;

binder      : identifier                    { $$ = $1; }
            | identifier ':' type           { $$ = $1; }
            ;

funbody     : typeparams '(' pparameters ')' bodyexpr
            | typeparams '(' pparameters ')' ':' tresult qualifier block
            ;

annotres    : ':' tresult
            | /* empty */
            ;


/* ---------------------------------------------------------
-- Statements
----------------------------------------------------------*/

block       : '{' semis statements1 '}'    /* must end with an expression statement (and not a declaration) */
            ;

statements1 : statements1 statement semis1
            | statement semis1
            | error semis1
            ;

statement   : decl
            | withstat
            | withstat IN blockexpr
            | returnexpr
            | basicexpr
            ;

decl        : FUN fundecl
            | VAL apattern '=' blockexpr    /* local value declaration can use a pattern binding */
            | VAR binder ASSIGN blockexpr   /* local variable declaration */
            ;


/* ---------------------------------------------------------
-- Expressions
----------------------------------------------------------*/
bodyexpr    : blockexpr
            | RARROW blockexpr  /* deprecated */
            ;

blockexpr   : expr              /* a `block` is not interpreted as an anonymous function but as statement grouping */
            ;

expr        : withexpr
            | block             /* interpreted as an anonymous function (except if coming from `blockexpr`) */
            | returnexpr
            | valexpr
            // | basicexpr '?' expr ':' expr
            | basicexpr
            ;

basicexpr   : ifexpr
            | matchexpr
            | handlerexpr
            | fnexpr
            | opexpr             %prec RARROW
            ;


/* keyword expressions */

matchexpr   : MATCH ntlexpr '{' semis matchrules '}'
            | ID_LAZY MATCH ntlexpr '{' semis matchrules '}'
            ;

fnexpr      : FN funbody                     /* anonymous function */
            ;

returnexpr  : RETURN expr
            ;

ifexpr      : IF ntlexpr THEN blockexpr elifs
            | IF ntlexpr THEN blockexpr
            | IF ntlexpr RETURN expr
            ;

elifs       : ELIF ntlexpr THEN blockexpr elifs
            | ELSE blockexpr
            ;

valexpr     : VAL apattern '=' blockexpr IN expr
            ;


/* operator expression */

opexpr      : opexpr qoperator prefixexpr
            | prefixexpr
            ;

prefixexpr  : '!' prefixexpr
            | '~' prefixexpr
            | appexpr               %prec RARROW
            ;

appexpr     : appexpr '(' arguments ')'             /* application */
            | appexpr '[' arguments ']'             /* index expression */
            | appexpr '.' name                      /* dot application */
            | appexpr '.' '(' arguments ')'         /* dot application */
            | appexpr block                         /* trailing function application */
            | appexpr fnexpr                        /* trailing function application */
            | atom
            ;


/* non-trailing-lambda expression */
ntlexpr     : ntlopexpr
            ;

ntlopexpr   : ntlopexpr qoperator ntlprefixexpr
            | ntlprefixexpr
            ;

ntlprefixexpr: '!' ntlprefixexpr
            | '~' ntlprefixexpr
            | ntlappexpr
            ;

ntlappexpr  : ntlappexpr '(' arguments ')'             /* application */
            | ntlappexpr '[' arguments ']'             /* index expression */
            | ntlappexpr '.' name                      /* dot application */
            | ntlappexpr '.' '(' arguments ')'         /* dot application */
            | atom
            ;

/* atomic expressions */

atom        : name
            | literal
            | mask
            | '(' aexprs ')'             /* unit, parenthesized (possibly annotated) expression, tuple expression */
            | '[' cexprs ']'             /* list expression (elements may be terminated with comma instead of separated) */
            | ctxexpr                    /* ctx is an atom and not an expr so we can write `acc ++ ctx Cons(1,_)` */
            | ctxhole
            | etahole               /* hole for missing (eta-expanded) parameters */
            ;

name        : qidentifier
            | qconstructor
            | qimplicit
            ;

literal     : INT | FLOAT | CHAR | STRING
            ;

mask        : MASK behind '<' tbasic '>'
            ;

behind      : ID_BEHIND
            | /* empty */
            ;

etahole     : '_'                       /* hole for missing (eta-expanded) parameters */
            ;

ctxexpr     : CTX atom                    /* should contain a hole */
            ;

ctxhole     : CTX_HOLE
            ;

/* arguments: separated by comma */


arguments   : arguments1
            | /* empty */
            ;

arguments1  : arguments1 ',' argument
            | argument
            ;

argument    : expr
            | identifier '=' expr                 /* named arguments */
            | qimplicit '=' expr
            ;

/* parameters: separated by comma, must have a type */

parameters  : parameters1
            | /* empty */
            ;

parameters1 : parameters1 ',' parameter
            | parameter
            ;

parameter   : borrow paramid ':' type
            | borrow paramid ':' type '=' expr
            ;

paramid     : identifier
            | wildcard
            ;

borrow      : '^'
            | /* empty */
            ;

/* pattern matching parameters: separated by comma */

pparameters : pparameters1
            | /* empty */
            ;

pparameters1: pparameters1 ',' pparameter
            | pparameter
            ;

pparameter  : borrow pattern
            | borrow pattern ':' type
            | borrow pattern ':' type '=' expr
            | borrow pattern '=' expr
            | borrow qimplicit
            | borrow qimplicit ':' type
            ;

/* annotated expressions: separated or terminated by comma */

aexprs      : aexprs1                               /* separated by comma */
            | /* empty */
            ;

aexprs1     : aexprs1 ',' aexpr
            | aexpr
            ;

cexprs      : cexprs0                              /* terminated or separated by comma */
            | cexprs0 aexpr
            ;

cexprs0     : cexprs0 aexpr ','
            | /* empty */
            ;

aexpr       : expr annot
            ;

annot       : ':' typescheme
            | /* empty */
            ;



/* ---------------------------------------------------------
-- Identifiers and operators
----------------------------------------------------------*/

qoperator   : op
            ;

qidentifier : qvarid
            | QIDOP
            | identifier
            ;

identifier  : varid
            | IDOP
            ;

wildcard    : WILDCARDID
            | '_'
            ;

qimplicit   : IMPLICITID
            ;

qvarid      : QID
            ;

varid       : ID
            | ID_C            { $$ = "c"; }
            | ID_CS           { $$ = "cs"; }
            | ID_JS           { $$ = "js"; }
            | ID_FILE         { $$ = "file"; }
            | ID_INLINE       { $$ = "inline"; }
            | ID_NOINLINE     { $$ = "noinline"; }
            | ID_OPEN         { $$ = "open"; }
            | ID_EXTEND       { $$ = "extend"; }
            | ID_LINEAR       { $$ = "linear"; }
            | ID_BEHIND       { $$ = "behind"; }
            | ID_VALUE        { $$ = "value"; }
            | ID_REFERENCE    { $$ = "reference"; }
            | ID_SCOPED       { $$ = "scoped"; }
            | ID_INITIALLY    { $$ = "initially"; }
            | ID_FINALLY      { $$ = "finally"; }
            | ID_DIV          { $$ = "div"; }
            | ID_CO           { $$ = "co"; }
            | ID_FIP          { $$ = "fip"; }
            | ID_FBIP         { $$ = "fbip"; }
            | ID_TAIL         { $$ = "tail"; }
            | ID_LAZY         { $$ = "lazy"; }
            /* | ID_NAMED        { $$ = "named"; } */
            ;

qconstructor: conid
            | qconid
            ;

qconid      : QCONID { $$ = $1; }
            ;

conid       : CONID  { $$ = $1; }
            ;

op          : OP
            | '>'       { $$ = ">";  }
            | '<'       { $$ = "<";  }
            | '|'       { $$ = "|";  }
            | ASSIGN    { $$ = ":="; }
            ;


/* ---------------------------------------------------------
-- Matching
----------------------------------------------------------*/

matchrules  : matchrules1 semis1
            | /* empty */
            ;

matchrules1 : matchrules1 semis1 matchrule
            | matchrule
            ;

matchrule   : patterns1 '|' expr RARROW blockexpr
            | patterns1 RARROW blockexpr
            ;

patterns1   : patterns1 ',' pattern
            | pattern
            ;

apatterns   : apatterns1
            | /* empty */
            ;

apatterns1  : apatterns1 ',' apattern
            | apattern
            ;

apattern    : pattern annot                    /* annotated pattern */
            ;

pattern     : identifier
            | identifier AS pattern              /* named pattern */
            | conid
            | conid '(' patargs ')'
            | '(' apatterns ')'                  /* unit, parenthesized, and tuple pattern */
            | '[' apatterns ']'                  /* list pattern */
            | literal
            | wildcard
            ;

patargs     : patargs1
            | /* empty */
            ;

patargs1    : patargs ',' patarg
            | patarg
            ;

patarg      : identifier '=' apattern            /* named argument */
            | apattern
            ;


/* ---------------------------------------------------------
-- Handlers
----------------------------------------------------------*/
handlerexpr : override HANDLER witheff opclauses
            | override HANDLE witheff ntlexpr opclauses
            | NAMED HANDLER witheff opclauses
            | NAMED HANDLE witheff ntlexpr opclauses
            ;

override    : OVERRIDE
            | /* empty */
            ;

witheff     : '<' anntype '>'
            | /* empty */
            ;

withstat    : WITH basicexpr
            | WITH binder LARROW basicexpr
            /* single operation shorthands */
            | WITH override witheff opclause
            | WITH binder LARROW witheff opclause
            ;

withexpr    : withstat IN blockexpr
            /* | withstat */
            ;

opclauses   : '{' semis opclauses1 semis1 '}'
            | '{' semis '}'
            ;

opclauses1  : opclauses1 semis1 opclausex
            | opclausex
            ;

opclausex   : ID_FINALLY bodyexpr
            | ID_INITIALLY '(' opparam ')' bodyexpr
            | opclause
            ;

opclause    : VAL qidentifier '=' blockexpr
            | VAL qidentifier ':' type '=' blockexpr
            | FUN qidentifier opparams bodyexpr
            | controlmod CTL qidentifier opparams bodyexpr
            | RETURN '(' opparam ')' bodyexpr
            ;

controlmod  : FINAL
            | RAW
            | /* empty */
            ;

opparams    : '(' opparams0 ')'
            ;

opparams0   : opparams1
            | /* empty */
            ;

opparams1   : opparams1 ',' opparam
            | opparam
            ;

opparam     : paramid
            | paramid ':' type
            ;


/* ---------------------------------------------------------
-- Types
----------------------------------------------------------*/
tbinders    : tbinders1
            | /* empty */
            ;

tbinders1   : tbinders1 ',' tbinder
            | tbinder
            ;

tbinder     : varid kannot
            ;


/* full type */
typescheme  : someforalls tarrow qualifier        /* used for type annotations */
            ;

type        : FORALL typeparams1 tarrow qualifier
            | tarrow qualifier
            ;

someforalls : SOME typeparams1 FORALL typeparams1
            | SOME typeparams1
            | FORALL typeparams1
            | /* empty */
            ;

typeparams  : typeparams1
            | /* empty */
            ;

typeparams1 : '<' tbinders '>'
            ;

qualifier   : WITH '(' predicates1 ')'
            | /* empty */
            ;

predicates1 : predicates1 ',' predicate
            | predicate
            ;


predicate   : typeapp                     /* interface:  identifier '<' targuments '>' */
            ;


/* mono types */
tarrow      : tatomic RARROW tresult
            | tatomic
            ;

tresult     : tatomic tbasic                 /* effect and result type */
            | tatomic                        /* just a result type (with a default total effect) */
            ;

tatomic     : tbasic
            | '<' targuments1 '|' tatomic '>' /* extensible effect type */
            | '<' targuments '>'             /* fixed effect type */
            ;

tbasic      : typeapp
            | '(' tparams ')'                /* unit, parenthesis, tuple, named parameters */
            | '[' anntype ']'                /* list type */
            ;

typeapp     : typecon
            | typecon '<' targuments '>'
            ;

typecon     : varid | qvarid                 /* type name */
            | wildcard                       /* wildcard type variable */
            | '(' commas1 ')'                /* tuple constructor */
            | '[' ']'                        /* list constructor */
            | '(' RARROW ')'                 /* function constructor */
            | CTX
            ;


tparams     : tparams1
            | /* empty */
            ;

tparams1    : tparams1 ',' tparam
            | tparam
            ;

tparam      : identifier ':' anntype              /* named parameter */
            | anntype
            ;


targuments  : targuments1
            | /* empty */
            ;

targuments1 : targuments1 ',' anntype
            | anntype
            ;

anntype     : type kannot
            ;


/* ---------------------------------------------------------
-- Kinds
----------------------------------------------------------*/
kannot      : DCOLON kind
            | /* empty */
            ;

kind        : '(' kinds1 ')' RARROW katom
            | katom RARROW kind
            | katom
            ;

kinds1      : kinds1 ',' kind
            | kind
            ;

katom       : conid
            ;

%%

void printDeclEx( const char* sort, const char* name, bool verbose )
{
  if (verbose) {
    printf( "parsed %s declaration: %s\n", sort, name );
  }
}
