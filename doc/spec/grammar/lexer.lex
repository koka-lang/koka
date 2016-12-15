/* Copyright 2012-2016 Microsoft Corporation, Daan Leijen
   This is free software; you can redistribute it and/or modify it under the
   terms of the Apache License, Version 2.0.
*/
/* Requires at least Flex 2.5.37; you can get a version for windows from 
   https://sourceforge.net/projects/winflexbison
*/
%option 8bit noyywrap bison-bridge bison-locations reentrant 

/* Exlusive Lexer states */
%x comment
%x linecomment
%x string
%x litstring

%{
#define CHECK_BALANCED  // check balanced parenthesis

#define INDENT_LAYOUT   // use full layout rule based on nested indentation
#undef LINE_LAYOUT    // use simple layout based on line ending token

/* Standard types and includes */
typedef int bool;
#define true (1==1)
#define false (!true)

#include "stdlib.h"
#include "string.h"
#include "stdarg.h"
#include "assert.h"
#include "parser.tab.h"

/* The extra scanner state */
#define YY_EXTRA_TYPE struct _ExtraState*

/*  Errors */
void  yyerror( YYLTYPE* loc, yyscan_t scanner, char* s, ... );
void  illegal( char* s, yyscan_t scanner );
void  illegalchar( char c, char* s, yyscan_t scanner );

/* Comments */
void  commentNestingInc(yyscan_t scanner); 
int   commentNestingDec(yyscan_t scanner);   

/*  Allocation of identifiers and string literals */
char* identifier( const char* s, yyscan_t scanner, bool wellformedCheck );
char* stringDup( const char* s, yyscan_t scanner );
void  stringStart( yyscan_t scanner );
void  stringAdd( unsigned int c, yyscan_t scanner);
char* stringEnd( yyscan_t scanner );
unsigned int utfDecode( const char* buf, int len, yyscan_t scanner );

/* Character escape codes */
char escapeToChar( char esc, yyscan_t scanner )
{
  switch(esc) {
    case 'n' : return '\n';
    case 'r' : return '\r';
    case 't' : return '\t';
    case '\\': return '\\';
    case '"' : return '"';
    case '\'': return '\'';
    default  : illegalchar(esc,"escape code",scanner);
               return esc;
  }
}

%}

  /* Character classes */

Symbols         {Symbol}+|[/]
Symbol          [\$\%\&\*\+\@!\\\^\~=\.\-\:\?\|\<\>]
AngleBar        [\<\>\|]
Angle           [\<\>]

ConId           {Upper}{IdChar}*{Final}*
Id              {Lower}{IdChar}*{Final}*
IdChar          {Letter}|{Digit}|[_\-]

HexEsc          x{Hex}{Hex}|u{Hex}{Hex}{Hex}{Hex}|U{Hex}{Hex}{Hex}{Hex}{Hex}{Hex}  
CharEsc         [nrt\\\"\']                         
/* for editor highlighting " */

LineChar        {GraphicLine}|{Utf8}
BlockChar       {GraphicBlock}|{Utf8}

Letter          {Lower}|{Upper}
Upper           [A-Z]
Lower           [a-z]
Digit           [0-9]
Hex             [0-9a-fA-F]
Space           [ \t]
Newline         [\r]?[\n]
Final           [\'\?]              
/* for editor highlighting ' */

GraphicChar     [ \x21-\x26\x28-\[\]-\x7E]
GraphicStr      [ \x21\x23-\[\]-\x7E]
GraphicRaw      [\t \n\r\x21\x23-\x7E]
GraphicLine     [\t \x21-\x7E]
GraphicBlock    [\t \x21-\)\+-\.0-\x7E]

 /* Valid UTF-8 sequences. Based on http://www.w3.org/2005/03/23-lex-U 
    Added \xC0\x80 as a valid sequence to represent 0 (also called 'modified' utf-8)
 */
UC              [\x80-\xBF]
U0              [\xC0][\x80]
U2              [\xC2-\xDF]{UC} 
U3              [\xE0][\xA0-\xBF]{UC}|[\xE1-\xEC]{UC}{UC}|[\xED][\x80-\x9F]{UC}|[\xEE-\xEF]{UC}{UC} 
U4              [\xF0][\x90-\xBF]{UC}{UC}|[\xF1-\xF3]{UC}{UC}{UC}|[\xF4][\x80-\x8F]{UC}{UC} 
Utf8            {U0}|{U2}|{U3}|{U4}


%%

  /* -------- INITIAL ------------- */

  /* keywords */
infix                     { return INFIX; }
infixl                    { return INFIXL; }
infixr                    { return INFIXR; }

type                      { return TYPE; }
cotype                    { return COTYPE; }
rectype                   { return RECTYPE; }
alias                     { return ALIAS; }
struct                    { return STRUCT; }

forall                    { return FORALL; }
exists                    { return EXISTS; }
some                      { return SOME; }

with                      { return WITH; }

abstract                  { return ABSTRACT; }
extern                    { return EXTERN; }
external                  { return EXTERN; }

function[\(\<]            { yyless(7); return FUNX; }
fun[\(\<]                 { yyless(3); return FUNX; }

function                  { return FUN; }
fun                       { return FUN; }

val                       { return VAL; }
var                       { return VAR; }
con                       { return CON; }

if                        { return IF;}
then                      { return THEN; }
else                      { return ELSE;}
elif                      { return ELIF;}

match                     { return MATCH;}
return                    { return RETURN;}

module                    { return MODULE;}
import                    { return IMPORT;}
public                    { return PUBLIC;}
private                   { return PRIVATE;}
as                        { return AS;}

inline                    { return ID_INLINE;  }
include                   { return ID_INCLUDE; }

open                      { return ID_OPEN; }
linear                    { return ID_LINEAR;  }

handler                   { return HANDLER; }
handle                    { return HANDLE; }
effect                    { return EFFECT; } 

  /* unused reserved identifiers */
yield                     { return YIELD;}
rec                       { return REC; }
try                       { return TRY; }
interface                 { return IFACE; }
instance                  { return INST; }

  /* reserved operators */
:                         { return ':';    }
=                         { return '=';    }
\.                        { return '.';    }
\-\>                      { return RARROW; }

  /* special operators and identifiers (not reserved but have special meaning in certain contexts) */
:=                        { return ASSIGN; }
::                        { return DCOLON; }
\|                        { return '|';    }
\<                        { return '<';    }
\>                        { return '>';    }
!                         { return '!';    }
\~                        { return '~';    }

file                      { return ID_FILE;    }
cs                        { return ID_CS;      }
js                        { return ID_JS;      }

  /* Special symbols (cannot be an operator) */
\)                        { return ')'; }
\(                        { return '('; }
\{                        { return '{'; }
\}                        { return '}'; }
\[                        { return '['; }
\]                        { return ']'; }
;                         { return ';'; }
,                         { return ','; }
`                         { return '`'; }

  /* Comments */
\/\/                      { BEGIN(linecomment); yymore(); }
\/\*                      { BEGIN(comment); commentNestingInc(yyscanner); yyless(2); yymore(); } 

  /* Type operators: these are all illegal operators and should be parsed as single characters
     For example, in types, we can have sequences like "<<exn>|<div|e>>" where "<<", ">|<", and ">>"
     should not be parsed as operator tokens. */
\|\|                      { yylval->Id = identifier(yytext,yyscanner,false); return OP; }
{AngleBar}{AngleBar}+     { yyless(1); return yytext[0]; }
  /*     
  \<{AngleBar}+            { yyless(1); return '<'; }
  \>{AngleBar}+            { yyless(1); return '>'; }
  \|{Angle}{Symbol}*       { yyless(1); return '|'; }
  \-\>\<{Symbol}*          { yyless(2); return RARROW; }
  \:\?{Symbol}*            { yyless(1); return ':'; }
  */

  /* Non escaped string literal start */
@\"                      { BEGIN(litstring);          /* " for editor highlighting */
                           stringStart(yyscanner); 
                           yymore();
                         }

  /* Identifiers and operators */
({Id}\/)+{ConId}          { yylval->Id = identifier(yytext,yyscanner,true); return QCONID; }
({Id}\/)+{Id}             { yylval->Id = identifier(yytext,yyscanner,true); return QID; }
({Id}\/)+\({Symbols}\)    { yylval->Id = identifier(yytext,yyscanner,true); return QIDOP; }

{ConId}                   { yylval->Id = identifier(yytext,yyscanner,true); return CONID; }
{Id}                      { yylval->Id = identifier(yytext,yyscanner,true); return ID; }
\({Symbols}\)             { yylval->Id = identifier(yytext,yyscanner,false); return IDOP; }
{Symbols}                 { yylval->Id = identifier(yytext,yyscanner,false); return OP; }
_{IdChar}*                { yylval->Id = identifier(yytext,yyscanner,true); return WILDCARD; }

  /* Numbers */
0[xX]{Hex}+               { yylval->Nat = strtol(yytext+2,NULL,16); return NAT; }
{Digit}+                  { yylval->Nat = strtol(yytext,NULL,10); return NAT; }

{Digit}+\.{Digit}+                    { yylval->Float = strtod(yytext,NULL); return FLOAT; }          
{Digit}+\.{Digit}+[eE]{Digit}+        { yylval->Float = strtod(yytext,NULL); return FLOAT; }
{Digit}+\.{Digit}+[eE][\-\+]{Digit}+  { yylval->Float = strtod(yytext,NULL); return FLOAT; }

  /* Character literals */
\'{GraphicChar}\'         { yylval->Char = yytext[1]; return CHAR; }   
\'\\{HexEsc}\'            { yylval->Char = strtol(yytext+3,NULL,16); return CHAR; }
\'\\{CharEsc}\'           { yylval->Char = escapeToChar(yytext[2],yyscanner); return CHAR; }
\'{Utf8}\'                { yylval->Char = utfDecode(yytext+1,yyleng-2,yyscanner); return CHAR; }
\'.\'                     { illegalchar(yytext[1],"character literal",yyscanner);
                            yylval->Char = ' '; 
                            return CHAR;
                          }
\'.                       { illegal("illegal character literal",yyscanner);  // '
                            yylval->Char = ' '; 
                            return CHAR;
                          }

  /* String literal start  */
\"                        { BEGIN(string);  // "
                            stringStart(yyscanner); 
                            yymore();
                          }

  /* White space  */
{Space}+                  { return LEX_WHITE; }
{Newline}                 { return LEX_WHITE; }
.                         { illegalchar(yytext[yyleng-1],NULL,yyscanner);
                            return LEX_WHITE;
                          }

  /* --------- Non escaped string literals --------- */
<litstring>\"\"           { stringAdd('"',yyscanner); yymore(); }
<litstring>\"             { BEGIN(INITIAL);  // ' 
                            yylval->String = stringEnd(yyscanner); 
                            return STRING; 
                          }
<litstring>{GraphicRaw}+  { char* p = yytext + YY_MORE_ADJ; 
                            while (*p) {
                              stringAdd( *p++, yyscanner);
                            }
                            yymore();
                          }
<litstring>{Utf8}         { stringAdd(utfDecode(yytext+YY_MORE_ADJ,yyleng-YY_MORE_ADJ,yyscanner), yyscanner); yymore(); }
<litstring>.              { illegalchar(yytext[yyleng-1],"raw string", yyscanner);
                            yymore();
                          }


  /* --------- String literals --------- */
<string>\"                { BEGIN(INITIAL); // "
                            yylval->String = stringEnd(yyscanner); 
                            return STRING; 
                          }
<string>{GraphicStr}+     { char* p = yytext + YY_MORE_ADJ; 
                            while (*p) {
                              stringAdd( *p++, yyscanner);
                            }
                            yymore();
                          }
<string>\\{HexEsc}        { stringAdd(strtol(yytext+2+YY_MORE_ADJ,NULL,16),yyscanner); yymore(); }
<string>\\{CharEsc}       { stringAdd(escapeToChar(yytext[1+YY_MORE_ADJ],yyscanner),yyscanner); yymore(); }
<string>{Utf8}            { stringAdd(utfDecode(yytext+YY_MORE_ADJ,yyleng-YY_MORE_ADJ,yyscanner),yyscanner); yymore(); }

<string>{Newline}         { BEGIN(INITIAL); 
                            illegal( "illegal newline ends string", yyscanner );
                            yylval->String = stringEnd(yyscanner); 
                            return STRING; 
                          }
<string>.                 { illegalchar(yytext[yyleng-1],"string", yyscanner);
                            yymore();
                          }


  /* ---------- Comments ------------ " */
<comment>{BlockChar}+     { yymore(); }
<comment>\/\*             { commentNestingInc(yyscanner); yymore(); }
<comment>\*\/             { if (commentNestingDec(yyscanner) == 0) {
                              BEGIN(INITIAL);
                              return LEX_COMMENT;
                            }
                            else yymore();
                          }
<comment>\*               { yymore(); }
<comment>\/               { yymore(); }
<comment>{Newline}        { return LEX_COMMENT; }
<comment>.                { illegalchar(yytext[yyleng-1], "comment", yyscanner);
                            yymore();
                          }

<linecomment>{LineChar}+  { yymore(); }
<linecomment>{Newline}    { BEGIN(INITIAL); return LEX_COMMENT; }
<linecomment>.            { illegalchar( yytext[yyleng-1], "line comment", yyscanner );
                            yymore();
                          }

%%

/* Enable the use of regular Flex macros (like yyextra) inside user defined functions */
#define EnableMacros(s)  yyget_extra(s); struct yyguts_t* yyg = (struct yyguts_t*)(s); 


/* Keep a list of allocated memory 
  in order to free all allocated identifiers and string literals afterwards*/
typedef struct _allocList* allocList;

void alistAdd ( allocList* list, void* p );
void alistFree( allocList* list );

// show  character or string
char* showChar( unsigned int c, yyscan_t scanner );
char* showString( const char* s, yyscan_t scanner );

/*---------------------------------------------------------
   The extra state
   This is used to maintain:
   - nesting level of comments 
   - the precise position
   - the previous token 
   - the layout stack for semicolon insertion
   - the saved token when a semicolon was inserted
   - a buffer for string literals
   - an allocation list to free allocated identifiers and string literals.
   - the number of errors
---------------------------------------------------------*/
#define errorMax  25
#define layoutMax 255   /* Limit maximal layout stack to 255 for simplicity */
#define braceMax  255   /* maximal nesting depth of parenthesis */
#define Token     int

typedef struct _ExtraState {
  /* nested comments */
  int         commentNesting;

  /* precise position */
  int         column;
  int         line;

  /* layout stack */
  bool        noLayout;     // apply the layout rule and insert semicolons? */
#ifdef INDENT_LAYOUT
  int         layoutTop;
  int         layout[layoutMax];
  
  /* location of the last seen comment -- used to prevent comments in indentation */
  YYLTYPE     commentLoc;              
#endif

#ifdef CHECK_BALANCED
  /* balanced braces */
  int         braceTop;
  Token       braces[braceMax];
  YYLTYPE     bracesLoc[braceMax];
#endif

  /* the previous non-white token and its location */
  Token       previous;
  YYLTYPE     previousLoc;

  /* the saved token and location: used to insert semicolons */
  Token       savedToken;
  YYLTYPE     savedLoc;

  /* temporary string buffer for string literals */
  int         stringMax;
  int         stringLen;
  char*       stringBuf;

  /* list of storage for yylval allocations */
  allocList   allocated;

  /* number of calls to yyerror */
  int         errorCount;

  /* be verbose */
  int         verbose;

  /* tab size used for error reporting */
  int         tab;

} ExtraState;

/* Forward declarations on the state */
YYLTYPE updateLoc( yyscan_t scanner );                      /* update the location after yylex returns */
void printToken( int token, int state, yyscan_t scanner );  /* print a token for debugging purposes */

/*----------------------------------------------------
   For semi-colon insertion, we look at the tokens that
   end statements, and ones that continue a statement
----------------------------------------------------*/
static int find( Token tokens[], Token token )
{
  int i = 0;
  while (tokens[i] != 0) {
    if (tokens[i] == token) return i;
    i++;
  }
  return -1;
}

static bool contains( Token tokens[], Token token ) {
  return (find(tokens,token) >= 0);
}

static Token appTokens[] = { ')', ']', ID, CONID, IDOP, QID, QCONID, QIDOP, 0 };

static bool isAppToken( Token token ) {
  return contains(appTokens, token );
}


#ifdef INDENT_LAYOUT
  static Token continuationTokens[] = { THEN, ELSE, ELIF, ')', ']', '{', 0 };

  static bool continuationToken( Token token ) {
    return contains(continuationTokens, token );
  }
#endif

#ifdef LINE_LAYOUT
  static Token endingTokens[]    = { ID, CONID, IDOP, QIDOP, QID, QCONID, NAT, FLOAT, STRING, CHAR, ')', ']', '}', '>', 0 };
  static Token continueTokens[]  = { THEN, ELSE, ELIF, '=', '{', '}', ')', ']', '>', 0 };

  bool endingToken( Token token  ) {
    return contains(endingTokens,token);
  }

  bool continueToken( Token token ) {
    return contains(continueTokens,token);
  }
#endif

#ifdef CHECK_BALANCED
  static Token closeTokens[] = { ')', '}', ']', ')', ']', 0 };
  static Token openTokens[]  = { '(', '{', '[', APP, IDX, 0 };

  Token isCloseBrace( Token token ) {
    int i = find(closeTokens,token);
    return (i >= 0 ? openTokens[i] : -1);
  }

  Token isOpenBrace( Token token ) {
    int i = find(openTokens,token);
    return (i >= 0 ? closeTokens[i] : -1);
  }
#endif

/*----------------------------------------------------
   Main lexical analysis routine 'mylex'
----------------------------------------------------*/

Token mylex( YYSTYPE* lval, YYLTYPE* loc, yyscan_t scanner)
{
  EnableMacros(scanner);
  Token       token;
  int         startState = YYSTATE;

  // do we have a saved token?
  if (yyextra->savedToken >= 0) {
    token  = yyextra->savedToken;
    *loc   = yyextra->savedLoc;
    yyextra->savedToken = -1;
  }

  // if not, scan ahead
  else {
    token = yylex( lval, loc, scanner ); 
    *loc = updateLoc( scanner );
    
    // this is to avoid needing semicolons
    if (token=='(' && isAppToken(yyextra->previous)) token = APP;
    if (token=='[' && isAppToken(yyextra->previous)) token = IDX;

    // skip whitespace
    while (token == LEX_WHITE || token == LEX_COMMENT) {
#ifdef INDENT_LAYOUT
      // save last comment location (to check later if it was not part of indentation)
      if (token == LEX_COMMENT) {
        yyextra->commentLoc = *loc;      
      }
#endif
      // scan again
      token = yylex( lval, loc, scanner ); 
      *loc = updateLoc(scanner);
    }

#ifdef CHECK_BALANCED
    // check balanced braces
    Token closeBrace = isOpenBrace(token);
    //fprintf(stderr,"scan: %d, %d, (%d,%d)\n", token, closeBrace, loc->first_line, loc->first_column);
    if (closeBrace>=0) {
      if (yyextra->braceTop >= (braceMax-1)) {
        yyerror(loc,scanner, "maximal nesting level of braces reached");        
      }
      else {
        // push the close brace
        yyextra->braceTop++;
        yyextra->braces[yyextra->braceTop] = closeBrace;
        yyextra->bracesLoc[yyextra->braceTop] = *loc;
      }
    }
    else if (isCloseBrace(token) >= 0) {
      // check if the close brace matches the context
      if (yyextra->braceTop < 0) {
        yyerror(loc, scanner, "unbalanced braces: '%c' is not opened", token);
      }
      else if (yyextra->braces[yyextra->braceTop] != token) {
        YYLTYPE openLoc = yyextra->bracesLoc[yyextra->braceTop];
        // try to pop to nearest open brace; otherwise don't pop at all
        int top = yyextra->braceTop-1;
        while( top >= 0 && yyextra->braces[top] != token) top--;
        if (top >= 0) {
          // there is a matching open brace on the stack
          yyerror(&openLoc, scanner, "unbalanced braces: '%c' is not closed", isCloseBrace(yyextra->braces[yyextra->braceTop]) );
          yyextra->braceTop = top-1; // pop to matching one          
        }
        else {
          // no matching brace
          yyerror(loc, scanner, "unbalanced braces: '%c' is not opened", token ); //, yyextra->braces[yyextra->braceTop],openLoc.first_line,openLoc.first_column);
        }       
      }
      else {
        // pop
        yyextra->braceTop--;
      }
    }
#endif

    // Do layout ?
    if (!yyextra->noLayout) 
    {
#ifdef INDENT_LAYOUT
      // set a new layout context? 
      if (yyextra->previous == '{') {
        if (token != '}' && loc->first_column <= yyextra->layout[yyextra->layoutTop]) {
          yyerror(loc,scanner,"illegal layout start; the line must be indented at least as much as its enclosing layout context (column %d)", yyextra->layout[yyextra->layoutTop-1]);
        }
        if (yyextra->verbose) {
          fprintf(stderr," layout start: %d\n", loc->first_column);
        }

        if (yyextra->layoutTop == layoutMax) {
          yyerror(loc,scanner,"maximal layout nesting level reached!");
        }
        else {
          yyextra->layoutTop++;
          yyextra->layout[yyextra->layoutTop] = loc->first_column; 
        }
      }

      // pop from the layout stack?
      if (token == '}') {
        if (yyextra->layoutTop <= 1) {
          yyerror(loc,scanner,"unexpected closing brace");
        }
        else {
          if (yyextra->verbose) {
            fprintf( stderr, " layout end %d\n", yyextra->layout[yyextra->layoutTop] );
          }
          yyextra->layoutTop--;
        }
      }

      bool newline = (yyextra->previousLoc.last_line < loc->first_line);     
      int layoutColumn = yyextra->layout[yyextra->layoutTop];

      if (newline) {
        // check comment in indentation
        if (yyextra->commentLoc.last_line == loc->first_line) {
          yyerror(&yyextra->commentLoc,scanner,"comments are not allowed in indentation; rewrite by putting the comment on its own line or at the end of the line");
        }
        // check layout
        if (loc->first_column < layoutColumn) {
          yyerror(loc,scanner,"illegal layout: the line must be indented at least as much as its enclosing layout context (column %d)", layoutColumn);
        }
      }
       
      // insert a semi colon?      
      if ((newline && loc->first_column == layoutColumn && !continuationToken(token))
          || token == '}' || token == 0) 
      {
        // save the currently scanned token
        yyextra->savedToken = token;
        yyextra->savedLoc   = *loc;

        // and replace it by a semicolon
        *loc = yyextra->previousLoc;
        loc->first_line = loc->last_line;
        loc->first_column = loc->last_column;
        loc->last_column++;
        token = SEMI;    
      }
#endif
#ifdef LINE_LAYOUT // simple semicolon insertion     
      bool newline = (yyextra->previousLoc.last_line < loc->first_line);     
      if ((newline && endingToken(yyextra->previous) && !continueToken(token)) || 
          ((token == '}' || token == 0) && yyextra->previous != SEMI) )  // always insert before a '}' and eof
      {
        // save the currently scanned token
        yyextra->savedToken = token;
        yyextra->savedLoc   = *loc;

        // and replace it by a semicolon
        *loc = yyextra->previousLoc;
        loc->first_line = loc->last_line;
        loc->first_column = loc->last_column;
        loc->last_column++;
        token = SEMI;    
      }
#endif
    } // do layout?
  }

  // save token for the next run to previous
  yyextra->previous = token;
  yyextra->previousLoc = *loc;

  // debug output  
  if (yyextra->verbose) {
    printToken(token,startState,scanner);
  }
  // return our token
  return token;
}


/*----------------------------------------------------
   Initialize the extra state
----------------------------------------------------*/
void initLoc( YYLTYPE* loc, int x )
{
  loc->first_line = x;
  loc->first_column = x;
  loc->last_line = x;
  loc->last_column = x;
}

void initScanState( ExtraState* st )
{
  st->tab = 8;
  st->commentNesting = 0;

  st->noLayout = false;
#ifdef INDENT_LAYOUT
  st->layoutTop = 0;
  st->layout[0] = 0;  
  initLoc(&st->commentLoc, 0);
#endif  

#ifdef CHECK_BALANCED
  st->braceTop = -1;
#endif

  st->column = 1;
  st->line = 1;
  
  st->previous = '{';   // so the layout context starts at the first token
  initLoc(&st->previousLoc, 1);
  
  st->savedToken = -1;
  initLoc(&st->savedLoc, 1);

  st->stringMax = 0;
  st->stringLen = 0;
  st->stringBuf = NULL;

  st->allocated = NULL;

  st->errorCount = 0;
  st->verbose    = 0;
}

void doneScanState( ExtraState* st )
{    
  /* free temporary string literal buffer */
  if (st->stringBuf != NULL) {
    free(st->stringBuf);
    st->stringMax = 0;
    st->stringLen = 0;
  }

  /* free all memory allocated during scanning */
  alistFree(&st->allocated);
  st->allocated = NULL;
}

/*----------------------------------------------------
   Maintain the location
----------------------------------------------------*/
YYLTYPE updateLoc( yyscan_t scanner ) 
{
  EnableMacros(scanner);
  YYLTYPE loc;
  int line   = loc.first_line = loc.last_line = yyextra->line;
  int column = loc.first_column = loc.last_column = yyextra->column;
  
  int i;
  for(i = 0; i < yyleng; i++) {
    loc.last_line = line;
    loc.last_column = column;

    if (yytext[i] == '\n') {
      line++;
      column=1;
    }
    else if (yytext[i] == '\t') {
      int tab = yyextra->tab;
      column = (((column+tab-1)/tab)*tab)+1;  
      loc.last_column = column-1; // adjust in case of tabs
    }
    else {
      column++;
    }
  }
  yyextra->line = line;
  yyextra->column = column;
  return loc;
}

YYLTYPE currentLoc( const yyscan_t scanner )
{
  EnableMacros(scanner);
  /* save */
  int line = yyextra->line;
  int column = yyextra->column;
  /* update */
  YYLTYPE loc = updateLoc(scanner);
  /* restore */
  yyextra->line = line;
  yyextra->column = column;
  return loc;
}

/*----------------------------------------------------
   Comment nesting
----------------------------------------------------*/
void commentNestingInc(yyscan_t scanner) 
{
  EnableMacros(scanner);
  yyextra->commentNesting++;
}

int commentNestingDec(yyscan_t scanner)
{
  EnableMacros(scanner);
  yyextra->commentNesting--;
  return yyextra->commentNesting;
}

/*----------------------------------------------------
   string allocation
----------------------------------------------------*/
char* stringDup( const char* s, yyscan_t scanner )
{
  EnableMacros(scanner);
  char* t = strdup(s);
  if (t==NULL) {
    yyerror(yylloc,scanner,"out of memory while scanning an identifier");
    exit(1);
  }
  alistAdd( &yyextra->allocated, t );
  return t;
}

/*----------------------------------------------------
   identifier allocation
----------------------------------------------------*/

bool isLetter(char c) {
  return ((c>='a' && c <= 'z') || (c>='A' && c<='Z') || c=='\0' || c==' ');
}

bool wellformed( const char* s ) {
  char prev = '\0';
  char next = '\0';
  const char* c;
  for(c = s; *c != 0; c++) {
    next = *(c+1);
    if (*c=='-' && (!isLetter(prev) || !isLetter(next))) return false;
    if (*c=='(') return true; // qualified operator, or operator name
    prev = *c;
  }
  return true;
}

char* identifier( const char* s, yyscan_t scanner, bool wellformedCheck )
{
  EnableMacros(scanner);
  if (wellformedCheck && !wellformed(s)) yyerror(yylloc,scanner,"malformed identifier: a dash must be preceded and followed by a letter");
  return stringDup(s,scanner);
}


/*----------------------------------------------------
   String literals
----------------------------------------------------*/
void stringStart( yyscan_t scanner )
{
  EnableMacros(scanner);
  yyextra->stringLen = 0;
}

void stringAdd( unsigned int c, yyscan_t scanner) 
{
  EnableMacros(scanner);
  /* reallocate if necessary (always 5 more to accomodate any UTF-8 encoding + \0 char) */
  int len = yyextra->stringLen;

  if (len >= yyextra->stringMax) {
    int newsize = (yyextra->stringMax==0 ? 128 : yyextra->stringMax*2);
    char* buf = (char*)malloc(newsize+5);
    if (buf==NULL) {
      yyerror(yylloc,scanner,"out of memory while scanning a string");
      exit(1);
    }
    if (yyextra->stringBuf != NULL) {
      strcpy(buf,yyextra->stringBuf);
      free(yyextra->stringBuf);
    }
    yyextra->stringBuf = buf;
    yyextra->stringMax = newsize;
  }
  /* add the new character to the buffer */
  /* encode to (modified) UTF-8 */
  if (c == 0) {
    yyextra->stringBuf[len++] = 0xC0;
    yyextra->stringBuf[len++] = 0x80;
  }
  else if (c <= 0x7F) {
    yyextra->stringBuf[len++] = c;
  }
  else if (c <= 0x7FF) {
    yyextra->stringBuf[len++] = (0xC0 | (c >> 6));
    yyextra->stringBuf[len++] = (0x80 | (c & 0x3F));   
  }
  else if (c <= 0xFFFF) {
    yyextra->stringBuf[len++] = 0xE0 | (c >> 12);
    yyextra->stringBuf[len++] = 0x80 | ((c >> 6) & 0x3F);   
    yyextra->stringBuf[len++] = 0x80 | (c & 0x3F);   
  }
  else if (c <= 0x10FFFF) {
    yyextra->stringBuf[len++] = 0xF0 | (c >> 18);
    yyextra->stringBuf[len++] = 0x80 | ((c >> 12) & 0x3F);   
    yyextra->stringBuf[len++] = 0x80 | ((c >> 6) & 0x3F);   
    yyextra->stringBuf[len++] = 0x80 | (c & 0x3F);
  }
  else {
    yyerror(yylloc,scanner,"illegal unicode character (0x%X)", c );
  }
  /* always add a 0 at the end */
  yyextra->stringBuf[len] = 0;
  yyextra->stringLen = len;
}

char* stringEnd( yyscan_t scanner ) 
{
  EnableMacros(scanner);

  char* buf = (char*)malloc((yyextra->stringLen+1));
  if (buf==NULL) {
    yyerror(yylloc,scanner, "out of memory while scanning a string");
    exit(1);
  }
  alistAdd( &yyextra->allocated, buf);
  if (yyextra->stringLen > 0) {
    strcpy(buf,yyextra->stringBuf);
  }
  else {
    buf[0] = 0;
  }
  return buf;
}

/* Decode a UTF8 encoded character. 
   "len" should be 1 or larger, and gets set to the actual number of bytes read (<= len)
   For an invalid UTF8 sequence, return the replacement character and set len to 0. */
unsigned int utfDecode1( const char* buf, int* len )
{
  unsigned int c = (unsigned char)(buf[0]);
  if (c <= 0x7F && *len>=1) {
    *len = 1;
    return c;
  }
  else if (c >= 0xC2 && c <= 0xDF && *len>=2) {
    unsigned int c1 = (unsigned char)(buf[1]);
    *len = 2;
    return (((c&0x1F)<<6) | (c1&0x3F));
  }
  else if (c >= 0xE0 && c <= 0xEF && *len>=3) {
    unsigned int c1 = (unsigned char)(buf[1]);
    unsigned int c2 = (unsigned char)(buf[2]);
    *len = 3;
    return (((c&0x0F)<<12) | ((c1&0x3F)<<6) | (c2&0x3F));
  }
  else if (c >= 0xF0 && c <= 0xF4 && *len>=4) {
    unsigned int c1 = (unsigned char)(buf[1]);
    unsigned int c2 = (unsigned char)(buf[2]);
    unsigned int c3 = (unsigned char)(buf[3]);
    *len = 4;
    return (((c&0x07)<<18) | ((c1&0x3F)<<12) | ((c2&0x3F)<<6) | (c3&0x3F));
  }
  else {
    *len = 0;
    return 0xFFFD; /* replacement character */
  }
}

/* Decode a UTF8 encoded character */
unsigned int utfDecode( const char* buf, int len, yyscan_t scanner )
{
  int scanned = len;
  unsigned int c = utfDecode1( buf, &scanned );
  if (scanned != len || len == 0) {
    YYLTYPE loc = currentLoc(scanner);
    yyerror( &loc, scanner, "illegal UTF-8 character sequence encountered: %s", buf );    
  }
  return c;
}
 

/*----------------------------------------------------
   Errors
----------------------------------------------------*/
void illegal(char* s, yyscan_t scanner )
{
  YYLTYPE loc = currentLoc(scanner);
  yyerror(&loc,scanner, s );
}

void illegalchar( char c, char* s, yyscan_t scanner )
{
  YYLTYPE loc = currentLoc(scanner);
  const char* schar = showChar(c,scanner);
  if (s == NULL && c == '\t') {
    s = "(replace tabs with spaces)";
  }
  if (s == NULL || strlen(s) == 0) {
    yyerror(&loc,scanner, "illegal character '%s'", schar); 
  }
  else {
    yyerror(&loc,scanner, "illegal character '%s' %s", schar, s );
  }
}

void yyerror( YYLTYPE* loc, yyscan_t scanner, char* s, ... )
{
  EnableMacros(scanner);
  va_list ap;
  va_start(ap, s);

  // print location
  if (loc->first_line >= 1) {
    fprintf(stderr,"(%d,%2d)-(%d,%2d): ", loc->first_line, loc->first_column, 
                                          loc->last_line, loc->last_column);
  }

  // print message
  vfprintf(stderr, s, ap);
  fprintf(stderr, "\n");

  // check error count
  yyextra->errorCount++;
  if (yyextra->errorCount >= errorMax) {
    fprintf(stderr, "maximum number of errors reached.\n" );
    exit(1);
  }
}

/*----------------------------------------------------
   Main
----------------------------------------------------*/
int yyparse( yyscan_t scanner );

static bool isPrefix( const char* pre, const char* s ) {
  if (pre==NULL) return true;
  if (s==NULL) return (pre[0] == 0);
  while (pre[0] != 0) {
    if (pre[0] != s[0]) return false;
    pre++;
    s++;
  }
  return true;
}

int main( int argc, char** argv )
{
  /* initialize */
  yyscan_t scanner;
  yylex_init( &scanner ); 
  EnableMacros(scanner);

  ExtraState st;
  initScanState( &st );
  yyset_extra( &st, scanner );

  /* read argument and parse */
  int arg = 1;
  while (arg < argc) {
    if (strcmp( argv[arg], "--nosemi") == 0) {
      st.noLayout = true;
    }
    else if (strcmp( argv[arg], "--verbose") == 0 || strcmp(argv[arg], "-v") == 0) {
      st.verbose++;
    }
    else if (isPrefix( "--tab=", argv[arg])) {
      st.tab = atoi(argv[arg]+6);
    }
    else if (strcmp( argv[arg], "--help") == 0) {
      yyin=NULL;
      break;
    }
    else if (argv[arg][0] == '-') {
      fprintf(stderr,"unrecognized option: %s\n", argv[arg] );
      exit(1);
    }
    else if (yyin != NULL) {
      fprintf(stderr,"too many file parameters: %s\n", argv[arg]);
      exit(1);
    }
    else {
      yyin = fopen(argv[arg], "r"); 
      if (!yyin) {
       fprintf(stderr,"couldn't open file: %s\n", argv[arg]);
       exit(1);
      }
      else {
        // skip UTF-8 BOM ?
        bool skippedBOM = (fgetc(yyin)==0xEF && fgetc(yyin)==0xBB && fgetc(yyin)==0xBF);
        if (!skippedBOM) {
          fseek(yyin,0,SEEK_SET); // rewind
        }
        else if (st.verbose) {
          fprintf(stderr,"skipped BOM\n");
        }
      }
    }
    arg++;
  }

  if (yyin==NULL) {
    printf("usage: koka-parser [--nosemi|--verbose|-v] <file>\n");
  }
  else {
    yyparse(scanner);
    
    /* destroy */
    int errorCount = st.errorCount;
    yylex_destroy(scanner);
    doneScanState(&st);

    /* final message */
    if (errorCount == 0) {
      printf("Success!\n");
      return 0;
    }
    else {
      printf("Failure (%i errors encountered)\n", errorCount);
      return 1;
    }
  }
}




/*----------------------------------------------------
   Nicely print a token to stderr
----------------------------------------------------*/
char* showChar( unsigned int c, yyscan_t scanner )
{
  char buf[11];  /* 11 = format of \U%06X + zero byte */
  if (c >= ' ' && c <= '~' && c != '\\' && c != '\'' && c != '\"') {
    sprintf(buf,"%c",c);
  }
  else if (c <= 0xFF) {
    if (c=='\t') sprintf(buf,"\\t");
    else if (c=='\n') sprintf(buf,"\\n");
    else if (c=='\r') sprintf(buf,"\\r");
    else if (c=='\'') sprintf(buf,"\\'");
    else if (c=='\"') sprintf(buf,"\"");
    else sprintf(buf,"\\x%02X",c);
  }
  else if (c <= 0xFFFF) {
    sprintf(buf,"\\u%04X",c);
  }
  else if (c <= 0xFFFFFF) {
    sprintf(buf,"\\U%06X",c);
  }
  else {
    sprintf(buf,"\\X%08X",c);
  }
  return stringDup(buf,scanner);
}

char* showString( const char* s, yyscan_t scanner ) 
{
  if (s==NULL) return ""; 

  const int max = 60;
  char buf[max + 10 + 3 + 1];  // max + maximal character width + " .." 0
  int dest = 0;
  int src = 0;
  int slen = strlen(s);
  buf[dest++] = '"';
  while (dest < max && s[src] != 0) {
    int len = slen - src;
    unsigned int c = utfDecode1(s + src,&len);
    if (len==0) src++;
           else src += len;
    const char* schar = showChar(c,scanner);
    strcpy(buf+dest,schar);
    dest += strlen(schar);
  }
  if (s[src] == 0) {
    buf[dest++] = '"';
  }
  else {
    buf[dest++] = ' ';
    buf[dest++] = '.';
    buf[dest++] = '.';
  }
  buf[dest] = 0;
  return stringDup(buf,scanner);
}

void printToken( int token, int state, yyscan_t scanner )
{ 
  EnableMacros(scanner);

  fprintf(stderr,"(%2d,%2d)-(%2d,%2d) <%d>: ", yylloc->first_line, yylloc->first_column, yylloc->last_line, yylloc->last_column, state ); 
  switch(token) {
    case ID:        fprintf(stderr,"ID    = '%s'", yylval->Id); break;
    case CONID:     fprintf(stderr,"CONID = '%s'", yylval->Id); break;
    case OP:        fprintf(stderr,"OP    = '%s'", yylval->Id); break;
    case QID:       fprintf(stderr,"QID   = '%s'", yylval->Id); break;
    case QCONID:    fprintf(stderr,"QCONID= '%s'", yylval->Id); break;
    // case QOP:       fprintf(stderr,"QOP   = '%s'", yylval->Id); break;
    case NAT:       fprintf(stderr,"NAT   = '%lu'", yylval->Nat); break;
    case FLOAT:     fprintf(stderr,"FLOAT = '%g'", yylval->Float); break;
    case CHAR:      fprintf(stderr,"CHAR  = '%s'", showChar(yylval->Char,scanner)); break;
    case SEMI:      fprintf(stderr,";     = (inserted)"); break;
    case STRING:    fprintf(stderr,"STRING(%u) = %s", strlen(yylval->String), showString(yylval->String,scanner)); break;
    default: {
      if (token >= ' ' && token <= '~') 
        fprintf(stderr,"%c", token);
      else if (token < ' ') 
        fprintf(stderr,"0x%x", token );
      else 
        fprintf(stderr,"%s", yytext);
    }
  }
  fprintf(stderr,"\n");
}



/*---------------------------------------------------------
   The allocation list 
   Used to free memory allocted of identifiers and
   string literals.
---------------------------------------------------------*/
struct _allocList {
  struct _allocList* next;
  void* mem;
};

void alistAdd( allocList* list, void* p )
{
  if (p == NULL) return;

  allocList head = (allocList)malloc(sizeof(struct _allocList));
  if (head == NULL) return;

  head->mem  = p;
  head->next = *list;
  *list      = head;
}

void alistFree( allocList* list )
{
  allocList head = *list;

  while (head != NULL) {
    allocList next = head->next;
    if (head->mem != NULL) {
      free(head->mem);
    }
    free(head);
    head = next;
  }
}
