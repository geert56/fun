%{
/*
 DOCUMENTATION INFORMATION                                         module: FUN
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system    : Hewlett-Packard HP9000/S735
 file      : y.y
 unit-title: YACC GRAMMAR RULES FOR FUN INPUT
 ref.      : 
 author(s) : Copyright (c) 1995-1999 G.L.J.M. Janssen
 date      :  4-FEB-1999
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/
%}

/* These are bison specific. */
%pure_parser
%expect 70

/* The Reserved Words: */
%token L_SYM
%token LET_SYM
%token LETREC_SYM
%token IN_SYM
%token WHERE_SYM
%token END_SYM
%token DATA_SYM
%token TYPE_SYM

%token PRINT_SYM
%token LOAD_SYM
%token SAVE_SYM
%token UNDEF_SYM

/* Identifier and Constants: */
%token <str>  VARID
%token <str>  CONID
%token <str>  SPECIAL_ID
%token <str>  STRING
%token <ival> INTEGER
%token <fval> FLOAT
%token <cval> CHAR
%token NIL_SYM
%token FALSE_SYM
%token TRUE_SYM
%token DEFS_SYM
%token BUILTINS_SYM
%token ARGV_SYM

/* Operators: */
%token <ival> OPERATOR

/* Predefined Functions: */

/* Punctuation: */
%token COCO
%token DOTDOT
%token L_ARROW
%token <ival> R_ARROW

%{
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "alloc.h"
#include "list.h"
#include "fun.h"

extern char yytext[];
extern int yyleng;
extern int yylineno;
extern FILE *yyin;

extern int warnings;
extern char *filename;

extern const char *Canonise (const char *str); /* see cell.h */

void error (char *format, ...);

/* Add variable `v' to front of `list' if not already present. */
static LIST
distinct_var (const char *v, LIST list)
{
  if (in_list ((void *) v, list, 0 /* test == */)) {
    error ("Multiple occurrence of variable `%s'; skipped", v);
    return list;
  }
  return push_cont ((void *) v, list);
}

%}

%union
{
  IntT ival;
  double fval;
  char cval;
  const char *str;
  LIST list;
  L_EXPR exp;
}

%type <str>	Var FunName
%type <list>	DistinctVars L_ExprList QualifierList Qualifier
%type <list>	DistinctVars2 Defs ArgList WhereClause FunNameList
%type <exp>	L_Expr L_Expr0 L_Expr1 L_Expr2 L_Expr3
%type <exp>	Abstraction Composition Application Atom
%type <exp>	List Rhs Def

%start L_File

%%
L_File : /* EMPTY */
       | L_Input
       | L_Input ';'
       ;

L_Input : L_Expr_or_Statement
        | L_Input ';' L_Expr_or_Statement
        ;

L_Expr_or_Statement : Rhs
                      { L_compile_reduce_print (stdout, $1); }
                    | GlobalDef
                    | Statement
                    | DataTypeDcl
                    | TypeSpec
                    | TypeSynonym
                    ;

Rhs : L_Expr WhereClause
      { $$ = L_mk_letrecs ($2, $1); }
    ;

GlobalDef : LET_SYM Defs
            { L_mk_defs ($2); }
          | LETREC_SYM Defs
            { L_mk_defrecs ($2); }
/*
          | Defs
            { L_mk_defrecs ($1); }
*/
          ;

Defs : Def
       { $$ = push_cont ($1, NULL_LIST); }
     | Defs ',' Def
       { $$ = push_cont ($3, $1); }
     ;

Def : FunName ArgList '=' Rhs
      { $$ = mk_vE_cell ($1, L_mk_abs ($2, $4)); }
    ;

ArgList : /* EMPTY */
          { $$ = NULL_LIST; }
        | DistinctVars
        | '(' DistinctVars ')'
          { $$ = $2; }
        ;

WhereClause : /* EMPTY */
              { $$ = NULL_LIST; }
            | WHERE_SYM Defs END_SYM
              { $$ = $2; }
            ;

Statement : PRINT_SYM '(' STRING ')'
            {
	      fputs ($3, stdout);
	      fflush (stdout);
	      MA_FREE_STRING ($3);
	    }
          | LOAD_SYM STRING
            {
	      L_load ($2, 0);
	      MA_FREE_STRING ($2);
	    }
          | SAVE_SYM STRING
            {
	      L_save ($2);
	      MA_FREE_STRING ($2);
	    }
          | UNDEF_SYM FunNameList
            { L_undefs ($2); }
          ;

/* `Operator' bindings from weak to strong (associativity):
   Composition (right)
   Infix operation (left)
   Abstraction (right)
   Application (left)
   Atom
*/
L_Expr  : L_Expr0
        | Composition
        ;

/* Composition is right-associative: f . g . h = f . (g . h). */
Composition : L_Expr0 '.' L_Expr
              { $$ = L_mk_comp ($1, $3); }
            ;

L_Expr0 : L_Expr1
        /* Infix operators: */
        /* To allow them to be used in a curried way (i.e, without a second
	   operand) causes loads of shift-reduce conflicts.
	*/
        | L_Expr0 '`' FunName '`'
          { $$ = L_mk_app (mk_name_cell ($3), $1); }
        | L_Expr0 '`' FunName '`' L_Expr1
          { $$ = L_mk_app (L_mk_app (mk_name_cell ($3), $1), $5); }

        | L_Expr0 '$' FunName
          { $$ = L_mk_app (mk_name_cell ($3), $1); }
        | L_Expr0 '$' FunName L_Expr1
          { $$ = L_mk_app (L_mk_app (mk_name_cell ($3), $1), $4); }
        ;

L_Expr1 : L_Expr2
        | Abstraction
        ;

/* Scope extends as far to the right as possible. */
Abstraction : L_SYM DistinctVars '.' L_Expr1
              { $$ = L_mk_abs ($2, $4); }
            ;

L_Expr2 : L_Expr3
        | Application
        ;

/* Application is left-associative: f g h = (f g) h. */
Application : L_Expr2 L_Expr3
              { $$ = L_mk_app ($1, $2); }
            | L_Expr2 Abstraction
              { $$ = L_mk_app ($1, $2); }
            ;

L_Expr3 :
        /* Let[rec] definitions: */
          LET_SYM Defs IN_SYM L_Expr
          { $$ = L_mk_lets ($2, $4); }
        | LETREC_SYM Defs IN_SYM L_Expr
          { $$ = L_mk_letrecs ($2, $4); }

        | Atom
        ;

Atom :
     /* Variables: */
       FunName
       { $$ = mk_name_cell ($1); }

     /* Constants: */
     | INTEGER
       { $$ = mk_num_cell ($1); }
     | FLOAT
       { $$ = mk_float_cell ($1); }
     | CHAR
       { $$ = mk_char_cell ($1); }
     | STRING
       { $$ = mk_str ($1, L_Nil); MA_FREE_STRING ($1); }
     | NIL_SYM
       { $$ = L_Nil; }
     | FALSE_SYM
       { $$ = L_False; }
     | TRUE_SYM
       { $$ = L_True; }
     | DEFS_SYM
       { $$ = L_Stddefs; }
     | BUILTINS_SYM
       { $$ = L_Builtins; }
     | ARGV_SYM
       { $$ = L_Argv; }

     /* Parenthesised expression: */
     | '(' L_Expr ')'
       { $$ = $2; }

     /* List notation: */
     | '[' List ']'
       { $$ = $2; }

     /* Tuple notation (not yet supported): */
     | '(' ')'
       { $$ = L_Nil; }
     | '(' L_Expr ',' L_ExprList ')'
       { $$ = L_mk_cons ($2, L_mk_list ($4, L_Nil)); }
     ;

List :
     /* Enumeration: */
       /* EMPTY */
       { $$ = L_Nil; }
     | ':'
       { $$ = L_Cons; }
     | L_Expr
       { $$ = L_mk_cons ($1, L_Nil); }
     | L_Expr ':'
       { $$ = L_mk_app (L_Cons, $1); }
     | L_Expr ':' L_Expr
       { $$ = L_mk_cons ($1, $3); }
     | L_Expr ',' L_ExprList
       { $$ = L_mk_cons ($1, L_mk_list ($3, L_Nil)); }
     | L_Expr ',' L_ExprList ':' L_Expr
       { $$ = L_mk_cons ($1, L_mk_list ($3, $5)); }

     /* Arithmetic sequence: */
     | L_Expr DOTDOT
       { $$ = L_mk_dotdot (NULL, $1, NULL); }
     | L_Expr DOTDOT L_Expr
       { $$ = L_mk_dotdot (NULL, $1, $3); }
     | L_Expr ',' L_Expr DOTDOT
       { $$ = L_mk_dotdot ($1, $3, NULL); }
     | L_Expr ',' L_Expr DOTDOT L_Expr
       { $$ = L_mk_dotdot ($1, $3, $5); }

     /* List comprehension: */
     | L_Expr '|'
       { $$ = L_mk_cons ($1, L_Nil); }
     | L_Expr '|' QualifierList
       { $$ = L_mk_list_compr ($1, $3); }
     ;

L_ExprList : L_Expr
             { $$ = push_cont ($1, NULL_LIST); }
           | L_ExprList ',' L_Expr
             { $$ = push_cont ($3, $1); }
           ;

QualifierList : Qualifier
              | QualifierList ';' Qualifier
                { $$ = concat_lists ($1, $3); }
              ;

Qualifier : DistinctVars2 L_ARROW L_Expr
            { $$ = L_mk_vEs ($1, $3); }
          | L_Expr
            { $$ = push_cont ($1, NULL_LIST); }
          ;

DistinctVars : Var
               { $$ = push_cont ((void *) $1, NULL_LIST); }
             | DistinctVars opt_comma Var
               { $$ = distinct_var ($3, $1); }
             ;

opt_comma : /* EMPTY */ | ','
          ;

DistinctVars2 : Var
                { $$ = push_cont ((void *) $1, NULL_LIST); }
              | DistinctVars2 ',' Var
                { $$ = distinct_var ($3, $1); }
              ;

/* Functions names can be identifiers or operator symbols.
*/
FunName : VARID
        | CONID
        | OPERATOR
          { $$ = PF_STR ($1); }
        | R_ARROW
          { $$ = PF_STR ($1); }
        | COCO
          { $$ = PF_STR (PF_TYPE); }
        | SPECIAL_ID
        ;

FunNameList : FunName
              { $$ = append_cont ((void *) $1, NULL_LIST); }
            | FunNameList ',' FunName
              { $$ = append_cont ((void *) $3, $1); }
            ;

/* Still want variables to be able to start with capital letter! */
Var : VARID
    | CONID
    ;

/* No semantics implemented! */
DataTypeDcl : DATA_SYM TypeLhs '=' Constructors
            ;

TypeSpec : TYPE_SYM FunNameList COCO Type
           { free_list ($2, 0); }
/*
         | FunNameList COCO Type
           { free_list ($1, 0); }
*/
         ;

TypeSynonym : TYPE_SYM TypeLhs '=' Type
            ;

TypeLhs : Construct_Id
        | Construct_Id DistinctVars
        ;

Constructors : Constructor
             | Constructors '|' Constructor
             ;

Constructor : Construct_Id
            | Construct_Id TypeList1
            ;

Type : Ctype
       /* Function type; -> is right-associative. */
     | Ctype R_ARROW Type
     ;

Ctype : Ctype Atype
      | Atype
      ;

/* Identifier naming a constructor function:
   (includes the primitive types Bool, Char, Int, String)
*/
Construct_Id : CONID
               {}
             ;

Atype :
        /* Type variable: */
        Var
        {}
        /* Data type/synonym: */
/*
      | Construct_Id
        {}
*/
        /* Unit type: */
      | '(' ')'
        /* Parenthesised type: */
      | '(' Type ')'
        /* Tuple type: */
      | '(' Type ',' TypeList2 ')'
        /* List type: */
      | '[' Type ']'
      | '[' ']'
      ;

TypeList1 : Type
          | TypeList1 Type
          ;

TypeList2 : Type
          | TypeList2 ',' Type
          ;

%%

void
error (char *format, ...)
{
  va_list ap;

  va_start (ap, format);

  fprintf (stderr, "[%s:%d, near `%s'] Error: ",
	   filename, yylineno, yytext);
  vfprintf (stderr, format, ap);
  fprintf (stderr, ".\n");

  va_end (ap);
}


int
yyerror (char *msg)
{
  error ("%s", msg);
}

void
warning (char *format, ...)
{
  if (warnings) {
    va_list ap;

    va_start (ap, format);

    fprintf (stderr, "[%s:%d, near `%s'] Warning: ",
	     filename, yylineno, yytext);
    vfprintf (stderr, format, ap);
    fprintf (stderr, ".\n");

    va_end (ap);
  }
}
