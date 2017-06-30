%{
/*
 DOCUMENTATION INFORMATION                                         module: FUN
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system    : Hewlett-Packard HP9000/S735
 file      : lex.l
 unit-title: Lexical Rules
 ref.      : 
 author(s) : Copyright (c) 1995-1998 G.L.J.M. Janssen
 date      :  7-DEC-1998
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

/* List of all special symbols, and their use:
	!	NOT_SYM
	=	EQ_PF
	!=	NE_PF
	$	DOLLAR
	%	PF_MODULO
	^	PF_POW
	&&	PF_AND
	*	PF_TIMES
	(	L_PAREN
	)	R_PAREN
	+	PF_PLUS
	-	PF_MINUS
	`	BACKQUOTE
	{	L_BRACE
	}	R_BRACE
	[	L_BRACKET
	]	R_BRACKET
	:	COLON
	;	SEMICOLON
	"	STRING delimiter
	'	CHAR delimiter
	<	PF_LT
	<-	L_ARROW
	->	PF_IMPLIES
	<=	PF_LE
	<<	PF_SHIFTL
	>	PF_GT
	>=	PF_GE
	>>	PF_SHIFTR
	,	COMMA
	.	PERIOD
	..	DOTDOT
	/	PF_DIVIDE
	|	BAR
	||	PF_OR
	++	PF_APPEND

	Following symbols are not defined (future use?):
	@	
	#	PF_LEN
	_
	\	list difference
	//	
	.=
	:=
	==
	&
	\/
	--	
	~
	?
	::	type
*/

#undef YY_DECL
#define YY_DECL int yylex (YYSTYPE *yylvalp)
#define yylval	(*yylvalp)

/*
****************************************
***** Declarations and definitions *****
****************************************
*/

#include  <stdio.h>
#include  <stdlib.h>

#include "alloc.h"
#include "list.h"
#include "hash.h"
#include "fun.h"
#include "fun.tab.h"

static void comment (void);
static void lexerr (char *mess);
static void read_string (void);
static void read_char (void);
static char *ill_char_mess (int ch);
extern const char *Canonise (const char *str); /* see cell.h */
%}

/* space or tab */
BLANK		[ \t\v\f]
/* 1 or more spaces or tabs */
BLANKS 		({BLANK}+)
/* 1 or more spaces, tabs or newlines */
WS		([ \t\n\v\f]+)
/* a newline */
EOL		("\n")
/* Binary digits with don't care: */
BINX        	(0|1|x|X)
/* Binary digits: */
BIN        	(0|1)
/* Octal digits: */
OCT        	[0-7]
/* Decimal digits: */
DIG        	[0-9]
/* Hexadecimal digits: */
HEX        	[0-9a-fA-F]
/* Binary digit sequence with don't cares: */
BINXS        	({BINX}+)
/* Binary digit sequence: */
BINS        	({BIN}+)
/* Octal digit sequence: */
OCTS        	({OCT}+)
/* Decimal digit sequence: */
DIGS        	({DIG}+)
/* No-leading-zeroes decimal digit sequence: */
NLZDIGS        	([1-9]{DIG}*)
/* Optionally signed decimal digit sequence: */
INT		(-?{DIGS})
/* Hexadecimal digit sequence: */
HEXS        	({HEX}+)
/* Sign */
SIGN		[+-]
/* Exponent part: */
EXPP		([eE]{SIGN}?{DIGS})
/* Floating suffix character: */
FLTSUFFIX	[fFlL]
FLT1		{INT}{EXPP}
FLT2		{INT}"."{DIGS}{EXPP}?
FLT		({FLT1}|{FLT2}){FLTSUFFIX}?
/* a lower-case letter: */
LC_LET		[_a-z@]
/* an upper-case letter: */
UC_LET		[A-Z]
/* not the first char in identifier: */
LETDIG		[_A-Za-z0-9$'*]
/* an identifier */
id		({LC_LET}{LETDIG}*)
ID		({UC_LET}{LETDIG}*)

/*
*****************
***** Start *****
*****************

Only 1 state is used at the moment.
*/

%START NORMAL

%%

%{
/* 1 and 2 letter special symbols (case-sensitive): */
%}

<NORMAL>L		return L_SYM;

%{
/* Reserved words: */
%}

<NORMAL>"let" 		return LET_SYM;
<NORMAL>"letrec"	return LETREC_SYM;
<NORMAL>"where"		return WHERE_SYM;
<NORMAL>"in" 		return IN_SYM;
<NORMAL>"end"		return END_SYM;
<NORMAL>"data"		return DATA_SYM;
<NORMAL>"type"		return TYPE_SYM;

%{
/* Top-level commands: For now, treat as reserved words: */
%}
<NORMAL>"print"		return PRINT_SYM;
<NORMAL>"load"		return LOAD_SYM;
<NORMAL>"save"		return SAVE_SYM;
<NORMAL>"undef"		return UNDEF_SYM;

%{
/* Built-in Constants; For now, treat as reserved words: */
%}

<NORMAL>"True"		return TRUE_SYM;
<NORMAL>"False"		return FALSE_SYM;
<NORMAL>"Nil"		return NIL_SYM;
<NORMAL>"Stddefs"	return DEFS_SYM;
<NORMAL>"Builtins"	return BUILTINS_SYM;
<NORMAL>"Argv"		return ARGV_SYM;

<NORMAL>^"#"!.*					{
/* Only meant for scripts; not to be used as regular comment. */
  BEGIN NORMAL;					}

<NORMAL>"/*"					{
/* * start regular comment * */
	comment ();
	BEGIN NORMAL;				}

<NORMAL>{id}					{
/* identifier */
	yylval.str = Canonise (yytext);
	return VARID;				}

<NORMAL>{ID}					{
/* Identifier */
	yylval.str = Canonise (yytext);
	return CONID;				}

<NORMAL>"'"					{
/* character constant */
        read_char ();
	yylval.cval = *yytext;
	return CHAR;				}

<NORMAL>"\""					{
/* start string */
        read_string ();
/*fprintf(stderr, "yytext:\"%s\", yyleng:%d\n", yytext, yyleng);*/
	yylval.str = COPY_STRING_1 (yytext, yyleng);
	return STRING;				}

<NORMAL>{INT}					{
/* optionally signed decimal number */
	yylval.ival = atol2 (yytext);
  	return INTEGER;				}

<NORMAL>{FLT}					{
	yylval.fval = atof (yytext);
  	return FLOAT;				}

%{
/* Operator symbols: */
%}
<NORMAL>"^"	{ yylval.ival = PF_POW;		return OPERATOR; }
<NORMAL>"*"	{ yylval.ival = PF_TIMES;	return OPERATOR; }
<NORMAL>"/"	{ yylval.ival = PF_DIVIDE;	return OPERATOR; }
<NORMAL>"%"	{ yylval.ival = PF_MODULO;	return OPERATOR; }
<NORMAL>"+"	{ yylval.ival = PF_PLUS;	return OPERATOR; }
<NORMAL>"-"	{ yylval.ival = PF_MINUS;	return OPERATOR; }
<NORMAL>"++"	{ yylval.ival = PF_APPEND;	return OPERATOR; }
<NORMAL>"=="	{ yylval.ival = PF_EQ;		return OPERATOR; }
<NORMAL>"!="	{ yylval.ival = PF_NE;		return OPERATOR; }
<NORMAL>"<"	{ yylval.ival = PF_LT;		return OPERATOR; }
<NORMAL>"<="	{ yylval.ival = PF_LE;		return OPERATOR; }
<NORMAL>">="	{ yylval.ival = PF_GE;		return OPERATOR; }
<NORMAL>">"	{ yylval.ival = PF_GT;		return OPERATOR; }
<NORMAL>"!"	{ yylval.ival = PF_NOT;		return OPERATOR; }
<NORMAL>"&&"	{ yylval.ival = PF_AND;		return OPERATOR; }
<NORMAL>"||"	{ yylval.ival = PF_OR;		return OPERATOR; }
<NORMAL>"->"	{ yylval.ival = PF_IMPLIES;	return R_ARROW; }
<NORMAL>"<<"	{ yylval.ival = PF_SHIFTL;	return OPERATOR; }
<NORMAL>">>"	{ yylval.ival = PF_SHIFTR;	return OPERATOR; }

%{
/* Punctuation: */
%}
<NORMAL>"::"	return COCO;
<NORMAL>".."	return DOTDOT;
<NORMAL>"<-"	return L_ARROW;
<NORMAL>[:;,.|`$=] 	return yytext[0];

%{
/* Brackets: */
%}
<NORMAL>[(){}\[\]]	return yytext[0];

<NORMAL>{WS}					{
/* unimportant white space or new_line */	}

<NORMAL>[^ \t\n\v\f/`'";.,(){}[\]$0-9_a-zA-Z][^ \t\n\v\f`'";.,(){}[\]0-9_a-zA-Z]*	{
/* A hack to allow user to define his own symbols.
   Should be used with extreme care.
*/
	yylval.str = Canonise (yytext);
	if (fun_debug)
  	  fprintf (stderr, "[lex] Special id: %s\n", yytext);
	return SPECIAL_ID;			}

<NORMAL>.					{
/* UNREACHABLE */
	lexerr (ill_char_mess (yytext[0]));
	BEGIN NORMAL;				}

%%

/*
********************************
***** End of lexical rules *****
********************************
*/

static char *nice_char_desc (int c, char *buf)
{
  register char *p;

  p = buf;

  if (c >= 0200) {		/* real Meta chars: M- */
    *p++ = 'M';
    *p++ = '-';
    c -= 0200;
  }

  if (c < 040) {
    if (c == 033) {
      *p++ = 'E';
      *p++ = 'S';
      *p++ = 'C';
    }
    else
    if (c == 011) {
      *p++ = 'T';
      *p++ = 'A';
      *p++ = 'B';
    }
    else
    if (c == 012) {
      *p++ = 'L';
      *p++ = 'F';
      *p++ = 'D';
    }
    else
    if (c == 015) {
      *p++ = 'R';
      *p++ = 'E';
      *p++ = 'T';
    }
    else {			/* other control chars: ^ */
      *p++ = '^';
      *p++ = c + 64;		/* 'A' - 1 */
    }
  }
  else
  if (c == 0177) {		/* DEL char: C-? */
    *p++ = 'D';
    *p++ = 'E';
    *p++ = 'L';
  }
  else
  if (c == ' ') {
    *p++ = 'S';
    *p++ = 'P';
    *p++ = 'C';
  }
  else
    *p++ = c;

  *p = '\0';

  return buf;  
}

static char *ill_char_mess (int ch)
{
  static char buf[80];
  char tem[6];

  strcpy  (buf, "illegal character: '");
  strcat  (buf, nice_char_desc (ch, tem));
  sprintf (buf + strlen (buf), "' (0x%x)", ch);
  strcat  (buf, ", skipped");
  return buf;
}

static void lexerr (char *mess)
{
  fprintf (stderr, "Lexical error in line %d: %s\n", yylineno, mess);
}

static void comment (void)
{
  int c = input ();

  do {
    while (c >= 0 && c != '*') c = input ();

    if (c == '*' && (c = input()) == '/')
      break;

    if (c < 0) {
      lexerr ("Unexpected end-of-file in comment");
      break;
    }
  } while (1);
}

static int read_escape (void)
{
  int c = input ();    /* read the char following the escape character \ */

  switch (c) {
  case 'a':			/*  */
    return '\a';
  case 'b':			/* backspace */
    return '\b';
  case 'f':			/* form feed */
    return '\f';
  case 'n':			/* real newline */
    return '\n';
  case 'r':			/* return */
    return '\r';
  case 't':			/* tab */
    return '\t';
  case 'v':			/*  */
    return '\v';
  case '\n':			/* escaped newline */
    return -1;

  case '0':			/* octal notation */
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
    {
      /* First octal digit already read. */
      int i = c - '0';        /* most significant digit */
      int count = 1;          /* already read 1 digit, at most 3 digits */

      /* Read at most another 2 digits: */
      while (((c = input ()) >= '0') && (c <= '7') && ((++count) <= 3))	{
	i *= 8;
	i += c - '0';
      }
      /* always read 1 char ahead, so push it back: */
      unput (c);
      return i;
    }

  default:
    return c;
  }
}

/* Leaves result char without the bracketing ' in yytext, yyleng. */
static void read_char (void)
{
  int c;
  char *p;

  /* Already seen opening ' of char, *yytext == '\'', yyleng == 1 */
  p = yytext;
  yyleng--;

  if ((c = input ()) >= 0) {
    if (c == '\'') {
      lexerr ("premature end of char constant");
      return;
    }
    if (c == '\\')
      c = read_escape ();

    /* c is -1 if \ newline has just been seen. */
    if (c == -1) {
      /* lex compatibility: */
/*      ++yylineno;*/
      lexerr ("end-of-line in char constant");
      return;
    }
    yyleng++;
    *p++ = c;
    if ((c = input ()) != '\'') {
      lexerr ("end of char constant expected");
      unput (c);
      return;
    }
    /* Here: have read the closing ' */
    /* Note: did not put closing ' in yytext. */
    /* Must insert trailing \0 though: */
    *p = '\0';
  }
  else {
    lexerr ("Unexpected end-of-file in char constant");
/*    unput (c);*/
  }
}

/* Leaves result string without the bracketing " in yytext, yyleng. */
static void read_string (void)
{
  int c;
  char *p;

  /* Already seen opening " of string, *yytext == '"', yyleng == 1 */
  p = yytext;
  yyleng--;

  while ((c = input ()) >= 0 && c != '\"') {
    if (c == '\\')
      c = read_escape ();

    /* c is -1 if \ newline has just been seen. */
    if (c >= 0) {
      yyleng++;
      *p++ = c;
    }
    else {
      /* lex compatibility: */
/*      ++yylineno;*/
    }
  }

  if (c < 0) {
    lexerr ("Unexpected end-of-file in string");
/*    unput (c);*/
  }
  else
    /* Here: have read the closing " . */
    /* Note: did not put closing " in yytext. */
    /* Must insert trailing \0 though: */
    *p = '\0';
}

int parse_file (const char *name, FILE *fp)
{
  /* Hack needed when switching back from top-level buffer.
     It is not allowed to switch to the NULL buffer.
  */
  static YY_BUFFER_STATE top = NULL;

  extern char *filename;
  const char *saved_filename = filename;
  FILE           *saved_yyin = yyin;
  int      i, saved_yylineno = yylineno;
  YY_BUFFER_STATE    pyy_buf = YY_CURRENT_BUFFER;
  YY_BUFFER_STATE        tmp;

  yylineno = 1;
  filename = name;
  yyin = fp;

  yy_switch_to_buffer (tmp = yy_create_buffer (yyin, YY_BUF_SIZE));

  BEGIN NORMAL;

  do {
    i = yyparse ();
  } while (i && isatty (fileno (fp)) && !feof (fp));

  yy_delete_buffer (tmp);

  if (!top)
    top = yy_create_buffer (NULL, 1);

  yy_switch_to_buffer (pyy_buf ? pyy_buf : top);

  yylineno = saved_yylineno;
  filename = saved_filename;
  yyin     = saved_yyin;
  return !i;
}

#if 0
int parse_file (const char *name, FILE *fp)
{
  extern char *filename;
  const char *saved_filename = filename;
  FILE           *saved_yyin = yyin;
  int      i, saved_yylineno = yylineno;

  yylineno = 1;
  filename = name;
  yyin = fp;

  i = yyparse ();

  yylineno = saved_yylineno;
  filename = saved_filename;
  yyin     = saved_yyin;
  return !i;
}
#endif
