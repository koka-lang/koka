/* Copyright 2012 Microsoft Corporation, Daan Leijen
   This is free software; you can redistribute it and/or modify it under the
   terms of the Apache License, Version 2.0.
*/

%pure_parser

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

%token APP   /* '(' for applications */
%token IDX   /* '[' for indexing */

%token IF THEN ELSE ELIF
%token MATCH 
%token RARROW 

%token FUN FUNX VAL VAR
%token TYPE COTYPE RECTYPE STRUCT
%token ALIAS CON
%token FORALL EXISTS SOME
%token WITH 

%token IMPORT AS MODULE 
%token PUBLIC PRIVATE ABSTRACT
%token EXTERN
%token INFIX INFIXL INFIXR 

%token LEX_WHITE LEX_COMMENT 
%token SEMI
%token LE ASSIGN DCOLON EXTEND
%token RETURN 

%token HANDLER HANDLE EFFECT

%token YIELD REC TRY IFACE INST

%token ID_INLINE ID_INCLUDE 
%token ID_CS ID_JS ID_FILE 
%token ID_LINEAR ID_OPEN

%type <Id>  varid conid qvarid qconid op  
%type <Id>  identifier qidentifier qoperator qconstructor
%type <Id>  funid typeid modulepath binder 
%type <Id>  valdecl fundecl aliasdecl typedecl externdecl puredecl 

/* these are non-reserved words but have special meaning after
   an extern or type declaration. If seen, we should 'shift' instead of reduce.
   The following declaration make Bison prefer a shift in those situations */
%nonassoc "inline" "include" "open" "linear"
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
                
semis1      : semis
            ;

semis       : semis semi
            | /* empty */   
            ;

semi        : ';' 
            | SEMI
            ;

lparen      : APP | '(';


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

externdecl  : EXTERN funid externtype externbody               { $$ = $2; }                    
            | EXTERN ID_INLINE funid externtype externbody     { $$ = $3; }                    %prec "inline"
            | EXTERN ID_INCLUDE externincbody                  { $$ = "<extern include>"; }    %prec "include"
            ;

externtype  : ':' typescheme 
            | lparen parameters ')' annotres 
            ;

externbody  : '{' semis externstats1 '}'
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

typedecl    : typesort typeid typeparams kannot typebody              { $$ = $2; }
            | typesort ID_OPEN varid typeparams kannot typebody       { $$ = $3; } %prec "open"
            | STRUCT typeid typeparams kannot  conparams              { $$ = $2; }
            | EFFECT typeid typeparams kannot opdecls                 { $$ = $2; } 
            | EFFECT ID_LINEAR varid typeparams kannot opdecls        { $$ = $3; } %prec "linear"
            ;

typesort    : TYPE | COTYPE | RECTYPE
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


constructors: constructors1 semis
            | /* empty */
            ;

constructors1: constructors1 semis1 constructor 
            | constructor
            ;

constructor : visibility con conid typeparams conparams
            ;

con         : CON
            | /* empty */ 
            ; 

conparams   : lparen conpars1 ')'
            | lparen ')'
            | /* empty */
            ;

conpars1    : conpars1 ',' conpar
            | conpar
            ;

conpar      : paramid ':' paramtype
            | paramid ':' paramtype '=' expr            
            | ':' paramtype
            | ':' paramtype '=' expr
            ;

/* ---------------------------------------------------------
-- Effect declarations
----------------------------------------------------------*/


opdecls     : '{' semis operations '}'            

operations  : operations operation semis
            | /* empty */
            ;

operation   : visibility FUN identifier typeparams lparen parameters ')' ':' tatomic
            ;

/* ---------------------------------------------------------
-- Pure Declarations
----------------------------------------------------------*/   
puredecl    : VAL valdecl                   { $$ = $2; }
            | FUN fundecl                   { $$ = $2; }
            ;

valdecl     : binder '=' blockexpr          { $$ = $1; }
            ;

binder      : identifier                    { $$ = $1; }
            | identifier ':' type           { $$ = $1; }
            ;

funid       : identifier         { $$ = $1; }
            | '[' commas ']'     { $$ = "[]"; }
            | STRING             { $$ = $1; }
            ;


fundecl     : some funid fundef bodyexpr    { $$ = $2; }
            ;

fundef      : typeparams lparen parameters ')' annotres qualifier
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
            | nofunexpr
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

expr        : ifexpr
            | noifexpr
            ;

noifexpr    : returnexpr
            | matchexpr
            | handleexpr
            | funexpr
            | opexpr
            ;

nofunexpr   : ifexpr
            | returnexpr
            | matchexpr
            | handleexpr
            | opexpr

/* keyword expressions */

matchexpr   : MATCH atom '{' semis matchrules '}'
            ; 

handleexpr  : HANDLER handlereff handlerpars '{' semis handlerrules1 semis '}'
            | HANDLE handlereff lparen arguments1 ')' handlerpars '{' semis handlerrules1 semis '}'
            ;        

funexpr     : FUNX fundef block
            | block                    /* zero-argument function */
            ;

returnexpr  : RETURN opexpr          
            ;

ifexpr      : IF atom then elifs else
            ;

then        : THEN noifexpr 
            | noifexpr                 /* then keyword is optional */
            ;

else        : ELSE noifexpr
            | /* empty */
            ;

elifs       : elifs ELIF atom then 
            | /* empty */
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

appexpr     : appexpr APP arguments ')'       /* application */
            | appexpr IDX arguments ']'       /* index expression */
            | appexpr '.' atom                /* dot application */
            | appexpr funexpr                 /* trailing function application */  
            | atom
            ; 


/* atomic expressions */

atom        : qidentifier
            | qconstructor
            | literal
            | '(' aexprs ')'             /* unit, parenthesized (possibly annotated) expression, tuple expression */
            | '[' cexprs ']'             /* list expression (elements may be terminated with comma instead of separated) */
            ;

literal     : NAT | FLOAT | CHAR | STRING
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

aexprs      : aexprs1                              /* separated by comma */
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
            | ID_CS           { $$ = "cs"; }
            | ID_JS           { $$ = "js"; }      
            | ID_FILE         { $$ = "file"; }
            | ID_INLINE       { $$ = "inline"; }
            | ID_INCLUDE      { $$ = "include"; }
            | ID_OPEN         { $$ = "open"; }     
            | ID_LINEAR       { $$ = "linear"; }   
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

matchrules  : matchrules1 semis
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
            | conid APP patargs ')'
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

handlereff  : '<' anntype '>'
            | /* empty */
            ; 

handlerpars : lparen parameters ')'
            | /* empty */
            ;

handlerrules1: handlerrules1 semis1 handlerrule
            | handlerrule
            ;

handlerrule : qidentifier opargs bodyexpr
            | RETURN lparen oparg ')' bodyexpr
            | RETURN paramid bodyexpr
            ;
                        
opargs      : lparen opargs0 ')'
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


some        : SOME typeparams1
            | /* empty */
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
