/* Copyright 2012-2020 Microsoft Corporation, Daan Leijen
   This is free software; you can redistribute it and/or modify it under the
   terms of the Apache License, Version 2.0.
*/
/* Requires at least Bison 3+; you can get a version for windows from
   https://sourceforge.net/projects/winflexbison
   (use the "latest" zip package)
*/

/* %pure-parser */
%define api.pure

%{
typedef void*        yyscan_t;
%}

%parse-param { yyscan_t scanner }
%lex-param { yyscan_t scanner }


/* token structure. The memory for Id and String is kept in a list and deallocated after parsing (in 'doneScanState') */
%union {
  const char*   Id;      /* used for operators OP too */
  const char*   String;  /* 'modified' UTF-8 string (\0 chars are encoded as \xC0\x80) */
  double        Float;
  unsigned long Nat;
  unsigned int  Char;
}

%{
#include <stdio.h>
#define yylex        mylex

void yyerror(YYLTYPE* loc, yyscan_t scanner, char* s, ...);
int  mylex( YYSTYPE* val, YYLTYPE* loc, yyscan_t scanner );

typedef int bool;
#define true (1==1)
#define false (!true)

void printDecl( const char* sort, const char* name );
%}


%token <Id>     ID CONID OP IDOP QID  QCONID QIDOP WILDCARD '(' ')' '[' ']'
%token <Nat>    NAT
%token <Float>  FLOAT
%token <String> STRING
%token <Char>   CHAR

/* %token APP  */ /* '(' for applications */
/* %token IDX  */ /* '[' for indexing */

%token IF THEN ELSE ELIF
%token WITH IN
%token MATCH
%token RARROW

%token FUN FN VAL VAR CONTROL RCONTROL EXCEPT
%token TYPE STRUCT EFFECT
%token ALIAS CON
%token FORALL EXISTS SOME

%token IMPORT AS MODULE
%token PUBLIC PRIVATE ABSTRACT
%token EXTERN
%token INFIX INFIXL INFIXR

%token LEX_WHITE LEX_COMMENT
%token SEMI
%token LE ASSIGN DCOLON EXTEND
%token RETURN

%token HANDLER HANDLE NAMED MASK OVERRIDE
%token IFACE UNSAFE

%token ID_CO ID_REC
%token ID_INLINE ID_NOINLINE ID_INCLUDE
%token ID_C ID_CS ID_JS ID_FILE
%token ID_LINEAR ID_OPEN ID_EXTEND
%token ID_BEHIND
%token ID_VALUE ID_REFERENCE ID_SCOPED
%token ID_INITIALLY ID_FINALLY


%type <Id>  varid conid qvarid qconid op
%type <Id>  identifier qidentifier qoperator qconstructor
%type <Id>  funid typeid modulepath binder
%type <Id>  fundecl aliasdecl typedecl externdecl puredecl

/* precedence declarations are in increasing order, i.e. the last precedence declaration has the highest precedence. */
/* resolve s/r conflict by shifting on ELSE so the ELSE binds to the closest IF.*/
%precedence THEN
%precedence ELSE ELIF

%%

/* ---------------------------------------------------------
-- Program
----------------------------------------------------------*/
program     : semis visibility MODULE modulepath moduledecl  { printDecl("module",$4); }
            | moduledecl                                     { printDecl("module","main"); }
            ;

moduledecl  : '{' semis modulebody '}' semis
            | semis modulebody
            ;

modulebody  : importdecl semis1 modulebody
            | declarations
            ;

importdecl  : visibility IMPORT modulepath
            | visibility IMPORT modulepath '=' modulepath
            ;

modulepath  : varid                       { $$ = $1; }
            | qvarid                      { $$ = $1; }
            ;

visibility  : PUBLIC
            | PRIVATE
            | /* empty */
            ;

semis1      : semis1 semi
            | semi
            ;

semis       : semis semi
            | /* empty */
            ;

semi        : ';'
            | SEMI
            ;


/* ---------------------------------------------------------
-- Top level declarations
----------------------------------------------------------*/

declarations: fixitydecl semis1 declarations
            | topdecls
            ;

fixitydecl  : visibility fixity oplist1
            ;

fixity      : INFIX NAT
            | INFIXR NAT
            | INFIXL NAT
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

topdecl     : visibility puredecl                             { printDecl("value",$2); }
            | visibility aliasdecl                            { printDecl("alias",$2); }
            | visibility typedecl                             { printDecl("type",$2); }
            | ABSTRACT typedecl                               { printDecl("type",$2); }
            | visibility externdecl                           { printDecl("extern",$2); }
            ;


/* ---------------------------------------------------------
-- External declarations
----------------------------------------------------------*/

externdecl  : inlineattr EXTERN funid externtype externbody    { $$ = $3; }
            | ID_INCLUDE EXTERN externincbody                  { $$ = "<extern include>"; }
            ;

inlineattr  : ID_INLINE
            | ID_NOINLINE
            | /* empty */
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
            ;


externincbody: '=' externinc
            | '{' semis externincs1 '}'
            ;

externincs1 : externincs1 externinc semis1
            | externinc semis1
            ;

externinc   : externtarget externfile STRING
            ;

externtarget: ID_CS
            | ID_JS
            | ID_C
            | /* empty */
            ;

externfile  : ID_FILE
            | /* empty */
            ;

externinline: ID_INLINE
            | /* empty */
            ;


/* ---------------------------------------------------------
-- Type declarations
----------------------------------------------------------*/
aliasdecl   : ALIAS typeid typeparams kannot '=' type     { $$ = $2; }
            ;

typedecl    : typesort typeid typeparams kannot typebody          { $$ = $2; }
            | typemod STRUCT typeid typeparams kannot conparams   { $$ = $3; }
            | effectsort varid typeparams kannot opdecls          { $$ = $2; }
            | effectsort typeparams kannot operation              { $$ = "<operation>"; }
            | namedsort varid typeparams kannot opdecls           { $$ = $2; }
            | namedsort typeparams kannot operation               { $$ = "<operation>"; }
            | namedsort varid typeparams kannot IN type opdecls   { $$ = $2; }  /* error on SCOPED (?) */
            ;

typesort    : typemod TYPE
            | ID_OPEN TYPE
            | ID_EXTEND TYPE
            | ID_CO TYPE
            | ID_REC TYPE
            ;

typemod     : ID_VALUE
            | ID_REFERENCE
            | /* empty */
            ;

namedsort   : NAMED effectsort
            | NAMED ID_SCOPED effectsort
            ;

effectsort  : EFFECT
            | ID_REC EFFECT
            | ID_LINEAR ID_REC EFFECT
            ;



typebody    : '{' semis constructors '}'
            | /* empty */
            ;

typeid      : '(' commas ')'      { $$ = "(,)"; }       /* tuples */
            | '[' ']'             { $$ = "[]"; }        /* lists */
            | '<' '>'             { $$ = "<>"; }        /* total effect */
            | '<' '|' '>'         { $$ = "<|>"; }       /* effect extension */
            | varid               { $$ = $1; }
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

constructor : visibility con conid typeparams conparams
            | visibility con STRING typeparams conparams
            ;

con         : CON
            | /* empty */
            ;

conparams   : '(' conpars1 ')'          /* deprecated */
            | '{' semis sconpars '}'
            | /* empty */
            ;

sconpars    : sconpars conpar semis1
            | /* empty */
            ;

conpars1    : conpars1 ',' conpar
            | conpar
            ;

conpar      : paramid ':' paramtype
            | paramid ':' paramtype '=' expr
            /*
            | ':' paramtype
            | ':' paramtype '=' expr
            */
            ;


/* ---------------------------------------------------------
-- Effect declarations
----------------------------------------------------------*/


opdecls     : '{' semis operations '}'
            ;

operations  : operations operation semis1
            | /* empty */
            ;

operation   : visibility VAL identifier typeparams ':' tatomic
            | visibility FUN identifier typeparams '(' parameters ')' ':' tatomic
            | visibility EXCEPT identifier typeparams '(' parameters ')' ':' tatomic
            | visibility CONTROL identifier typeparams '(' parameters ')' ':' tatomic
            ;


/* ---------------------------------------------------------
-- Pure (top-level) Declarations
----------------------------------------------------------*/
puredecl    : inlineattr VAL binder '=' blockexpr      { $$ = $3; }
            | inlineattr FUN funid funparam bodyexpr   { $$ = $3; }
            ;

fundecl     : funid funparam bodyexpr         { $$ = $1; }
            ;

binder      : identifier                    { $$ = $1; }
            | identifier ':' type           { $$ = $1; }
            ;

funid       : identifier         { $$ = $1; }
            | '[' commas ']'     { $$ = "[]"; }
            | STRING             { $$ = $1; }
            ;

funparam      : typeparams '(' parameters ')' annotres qualifier
            ;


parameters  : parameters1
            | /* empty */
            ;

parameters1 : parameters1 ',' parameter
            | parameter
            ;

parameter   : paramid
            | paramid ':' paramtype
            | paramid ':' paramtype '=' expr
            | paramid '=' expr
            ;

paramid     : identifier
            | WILDCARD
            ;

paramtype   : type
            | '?' type
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
bodyexpr    : RARROW blockexpr
            | block
            ;

blockexpr   : expr              /* a block is not interpreted as an anonymous function but as grouping */
            ;

expr        : withexpr
            | funexpr
            | returnexpr
            | basicexpr
            ;

basicexpr   : ifexpr
            | fnexpr
            | matchexpr
            | handlerexpr
            | opexpr
            ;


/* keyword expressions */

matchexpr   : MATCH atom '{' semis matchrules '}'
            ;

funexpr     : FUN funparam block
            | block                    /* zero-argument function */
            ;

fnexpr      : FN funparam block          /* always anonymous function */
            ;

returnexpr  : RETURN expr
            ;

ifexpr      : IF atom then elifs ELSE expr  %prec THEN
            | IF atom then elifs            %prec THEN
            ;

then        : THEN expr
            | expr           /* then keyword is optional */
            ;

elifs       : elifs ELIF atom then
            | %empty /* empty */
            ;

/* operator expression */

opexpr      : opexpr qoperator prefixexpr
            | prefixexpr
            ;

prefixexpr  : '!' prefixexpr
            | '~' prefixexpr
            | appexpr
            ;

/*
fappexpr    : fappexpr funexpr
            | appexpr
            ;
*/

appexpr     : appexpr '(' arguments ')'             /* application */
            | appexpr '[' arguments ']'             /* index expression */
            | appexpr '.' atom                      /* dot application */
            | appexpr funexpr                       /* trailing function application */
            | appexpr fnexpr                        /* trailing function application */
            | atom
            ;


/* atomic expressions */

atom        : qidentifier
            | qconstructor
            | literal
            | mask
            | '(' aexprs ')'             /* unit, parenthesized (possibly annotated) expression, tuple expression */
            | '[' cexprs ']'             /* list expression (elements may be terminated with comma instead of separated) */
            ;

literal     : NAT | FLOAT | CHAR | STRING
            ;

mask        : MASK behind '<' tbasic '>'
            ;

behind      : ID_BEHIND
            | /* empty */
            ;

/* arguments: separated by comma */


arguments   : arguments1
            | /* empty */
            ;

arguments1  : arguments1 ',' argument
            | argument
            ;

argument    : expr
            | identifier '=' expr                  /* named arguments */
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

qvarid      : QID
            ;

varid       : ID
            | ID_C            { $$ = "c"; }
            | ID_CS           { $$ = "cs"; }
            | ID_JS           { $$ = "js"; }
            | ID_FILE         { $$ = "file"; }
            | ID_INLINE       { $$ = "inline"; }
            | ID_NOINLINE     { $$ = "noinline"; }
            | ID_INCLUDE      { $$ = "include"; }
            | ID_OPEN         { $$ = "open"; }
            | ID_EXTEND       { $$ = "extend"; }
            | ID_LINEAR       { $$ = "linear"; }
            | ID_BEHIND       { $$ = "behind"; }
            | ID_VALUE        { $$ = "value"; }
            | ID_REFERENCE    { $$ = "reference"; }
            | ID_SCOPED       { $$ = "scoped"; }
            | ID_INITIALLY    { $$ = "initially"; }
            | ID_FINALLY      { $$ = "finally"; }
            | ID_REC          { $$ = "rec"; }
            | ID_CO           { $$ = "co"; }
            /* | ID_NAMED        { $$ = "named"; } */
            ;

qconstructor: conid
            | qconid
            ;

qconid      : QCONID { $$ = $1; }
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
            | patterns1 bodyexpr
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

apattern    : pattern annot                      /* annotated pattern */
            ;

pattern     : identifier
            | conid
            | conid '(' patargs ')'
            | '(' apatterns ')'                  /* unit, parenthesized, and tuple pattern */
            | '[' apatterns ']'                  /* list pattern */
            | apattern AS identifier             /* named pattern */
            | literal
            | WILDCARD
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
handlerexpr : HANDLER override witheff opclauses
            | HANDLE override witheff '(' expr ')' opclauses
            | NAMED HANDLER witheff opclauses
            | NAMED HANDLE witheff '(' expr ')' opclauses
            ;

override    : OVERRIDE
            | /* empty */
            ;

witheff     : '<' anntype '>'
            | /* empty */
            ;

withstat    : WITH basicexpr
            | WITH binder '=' basicexpr
            | WITH override witheff opclauses          /* shorthand for handler */
            | WITH binder '=' NAMED witheff opclauses  /* shorthand for named handler */
            ;

withexpr    : withstat IN blockexpr
            /* | withstat */
            ;

opclauses   : opclause
            | '{' semis opclauses1 semis1 '}'
            | '{' semis '}'
            ;

opclauses1  : opclauses1 semis1 opclausex
            | opclausex
            ;

opclausex   : ID_FINALLY bodyexpr
            | ID_INITIALLY bodyexpr
            | opclause
            ;

opclause    : VAL qidentifier '=' expr
            | VAL qidentifier ':' type '=' expr
            | FUN qidentifier opargs bodyexpr
            | EXCEPT qidentifier opargs bodyexpr
            | CONTROL qidentifier opargs bodyexpr
            | RCONTROL qidentifier opargs bodyexpr
            | RETURN '(' oparg ')' bodyexpr
            | RETURN paramid bodyexpr               /* deprecated */
            ;

opargs      : '(' opargs0 ')'
            | /* empty */
            ;

opargs0     : opargs1
            | /* empty */
            ;

opargs1     : opargs1 ',' oparg
            | oparg
            ;

oparg       : paramid
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
            | WILDCARD                       /* wildcard type variable */
            | '(' commas1 ')'                /* tuple constructor */
            | '[' ']'                        /* list constructor */
            | '(' RARROW ')'                 /* function constructor */
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

void printDecl( const char* sort, const char* name )
{
  printf( "parsed %s declaration: %s\n", sort, name );
}
