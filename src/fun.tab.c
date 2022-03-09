
/* A Bison parser, made by GNU Bison 2.4.1.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C
   
      Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.4.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Copy the first part of user declarations.  */

/* Line 189 of yacc.c  */
#line 1 "fun.y"

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

/* Line 189 of yacc.c  */
#line 60 "fun.y"

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



/* Line 189 of yacc.c  */
#line 123 "fun.tab.c"

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     L_SYM = 258,
     LET_SYM = 259,
     LETREC_SYM = 260,
     IN_SYM = 261,
     WHERE_SYM = 262,
     END_SYM = 263,
     DATA_SYM = 264,
     TYPE_SYM = 265,
     PRINT_SYM = 266,
     LOAD_SYM = 267,
     SAVE_SYM = 268,
     UNDEF_SYM = 269,
     VARID = 270,
     CONID = 271,
     SPECIAL_ID = 272,
     STRING = 273,
     INTEGER = 274,
     FLOAT = 275,
     CHAR = 276,
     NIL_SYM = 277,
     FALSE_SYM = 278,
     TRUE_SYM = 279,
     DEFS_SYM = 280,
     BUILTINS_SYM = 281,
     ARGV_SYM = 282,
     OPERATOR = 283,
     COCO = 284,
     DOTDOT = 285,
     L_ARROW = 286,
     R_ARROW = 287
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 214 of yacc.c  */
#line 95 "fun.y"

  IntT ival;
  double fval;
  char cval;
  const char *str;
  LIST list;
  L_EXPR exp;



/* Line 214 of yacc.c  */
#line 202 "fun.tab.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 264 of yacc.c  */
#line 214 "fun.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  73
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   273

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  45
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  42
/* YYNRULES -- Number of rules.  */
#define YYNRULES  119
/* YYNRULES -- Number of states.  */
#define YYNSTATES  182

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   287

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,    40,     2,     2,     2,
      36,    37,     2,     2,    34,     2,    38,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    43,    33,
       2,    35,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    41,     2,    42,     2,     2,    39,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,    44,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     4,     6,     9,    11,    15,    17,    19,
      21,    23,    25,    27,    30,    33,    36,    38,    42,    47,
      48,    50,    54,    55,    59,    64,    67,    70,    73,    75,
      77,    81,    83,    88,    94,    98,   103,   105,   107,   112,
     114,   116,   119,   122,   127,   132,   134,   136,   138,   140,
     142,   144,   146,   148,   150,   152,   154,   156,   160,   164,
     167,   173,   174,   176,   178,   181,   185,   189,   195,   198,
     202,   207,   213,   216,   220,   222,   226,   228,   232,   236,
     238,   240,   244,   245,   247,   249,   253,   255,   257,   259,
     261,   263,   265,   267,   271,   273,   275,   280,   285,   290,
     292,   295,   297,   301,   303,   306,   308,   312,   315,   317,
     319,   321,   324,   328,   334,   338,   341,   343,   346,   348
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      46,     0,    -1,    -1,    47,    -1,    47,    33,    -1,    48,
      -1,    47,    33,    48,    -1,    49,    -1,    50,    -1,    55,
      -1,    75,    -1,    76,    -1,    77,    -1,    56,    54,    -1,
       4,    51,    -1,     5,    51,    -1,    52,    -1,    51,    34,
      52,    -1,    72,    53,    35,    49,    -1,    -1,    69,    -1,
      36,    69,    37,    -1,    -1,     7,    51,     8,    -1,    11,
      36,    18,    37,    -1,    12,    18,    -1,    13,    18,    -1,
      14,    73,    -1,    58,    -1,    57,    -1,    58,    38,    56,
      -1,    59,    -1,    58,    39,    72,    39,    -1,    58,    39,
      72,    39,    59,    -1,    58,    40,    72,    -1,    58,    40,
      72,    59,    -1,    61,    -1,    60,    -1,     3,    69,    38,
      59,    -1,    63,    -1,    62,    -1,    61,    63,    -1,    61,
      60,    -1,     4,    51,     6,    56,    -1,     5,    51,     6,
      56,    -1,    64,    -1,    72,    -1,    19,    -1,    20,    -1,
      21,    -1,    18,    -1,    22,    -1,    23,    -1,    24,    -1,
      25,    -1,    26,    -1,    27,    -1,    36,    56,    37,    -1,
      41,    65,    42,    -1,    36,    37,    -1,    36,    56,    34,
      66,    37,    -1,    -1,    43,    -1,    56,    -1,    56,    43,
      -1,    56,    43,    56,    -1,    56,    34,    66,    -1,    56,
      34,    66,    43,    56,    -1,    56,    30,    -1,    56,    30,
      56,    -1,    56,    34,    56,    30,    -1,    56,    34,    56,
      30,    56,    -1,    56,    44,    -1,    56,    44,    67,    -1,
      56,    -1,    66,    34,    56,    -1,    68,    -1,    67,    33,
      68,    -1,    71,    31,    56,    -1,    56,    -1,    74,    -1,
      69,    70,    74,    -1,    -1,    34,    -1,    74,    -1,    71,
      34,    74,    -1,    15,    -1,    16,    -1,    28,    -1,    32,
      -1,    29,    -1,    17,    -1,    72,    -1,    73,    34,    72,
      -1,    15,    -1,    16,    -1,     9,    78,    35,    79,    -1,
      10,    73,    29,    81,    -1,    10,    78,    35,    81,    -1,
      83,    -1,    83,    69,    -1,    80,    -1,    79,    44,    80,
      -1,    83,    -1,    83,    85,    -1,    82,    -1,    82,    32,
      81,    -1,    82,    84,    -1,    84,    -1,    16,    -1,    74,
      -1,    36,    37,    -1,    36,    81,    37,    -1,    36,    81,
      34,    86,    37,    -1,    41,    81,    42,    -1,    41,    42,
      -1,    81,    -1,    85,    81,    -1,    81,    -1,    86,    34,
      81,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   114,   114,   115,   116,   119,   120,   123,   125,   126,
     127,   128,   129,   132,   136,   138,   146,   148,   152,   157,
     158,   159,   164,   165,   169,   175,   180,   185,   196,   197,
     201,   205,   210,   212,   215,   217,   221,   222,   226,   230,
     231,   235,   237,   243,   245,   248,   253,   257,   259,   261,
     263,   265,   267,   269,   271,   273,   275,   279,   283,   287,
     289,   296,   297,   299,   301,   303,   305,   307,   311,   313,
     315,   317,   321,   323,   327,   329,   333,   334,   338,   340,
     344,   346,   350,   350,   353,   355,   361,   362,   363,   365,
     367,   369,   372,   374,   379,   380,   384,   387,   395,   398,
     399,   402,   403,   406,   407,   410,   412,   415,   416,   422,
     428,   436,   438,   440,   442,   443,   446,   447,   450,   451
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "L_SYM", "LET_SYM", "LETREC_SYM",
  "IN_SYM", "WHERE_SYM", "END_SYM", "DATA_SYM", "TYPE_SYM", "PRINT_SYM",
  "LOAD_SYM", "SAVE_SYM", "UNDEF_SYM", "VARID", "CONID", "SPECIAL_ID",
  "STRING", "INTEGER", "FLOAT", "CHAR", "NIL_SYM", "FALSE_SYM", "TRUE_SYM",
  "DEFS_SYM", "BUILTINS_SYM", "ARGV_SYM", "OPERATOR", "COCO", "DOTDOT",
  "L_ARROW", "R_ARROW", "';'", "','", "'='", "'('", "')'", "'.'", "'`'",
  "'$'", "'['", "']'", "':'", "'|'", "$accept", "L_File", "L_Input",
  "L_Expr_or_Statement", "Rhs", "GlobalDef", "Defs", "Def", "ArgList",
  "WhereClause", "Statement", "L_Expr", "Composition", "L_Expr0",
  "L_Expr1", "Abstraction", "L_Expr2", "Application", "L_Expr3", "Atom",
  "List", "L_ExprList", "QualifierList", "Qualifier", "DistinctVars",
  "opt_comma", "DistinctVars2", "FunName", "FunNameList", "Var",
  "DataTypeDcl", "TypeSpec", "TypeSynonym", "TypeLhs", "Constructors",
  "Constructor", "Type", "Ctype", "Construct_Id", "Atype", "TypeList1",
  "TypeList2", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,    59,    44,    61,    40,    41,    46,    96,
      36,    91,    93,    58,   124
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    45,    46,    46,    46,    47,    47,    48,    48,    48,
      48,    48,    48,    49,    50,    50,    51,    51,    52,    53,
      53,    53,    54,    54,    55,    55,    55,    55,    56,    56,
      57,    58,    58,    58,    58,    58,    59,    59,    60,    61,
      61,    62,    62,    63,    63,    63,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    65,    65,    65,    65,    65,    65,    65,    65,    65,
      65,    65,    65,    65,    66,    66,    67,    67,    68,    68,
      69,    69,    70,    70,    71,    71,    72,    72,    72,    72,
      72,    72,    73,    73,    74,    74,    75,    76,    77,    78,
      78,    79,    79,    80,    80,    81,    81,    82,    82,    83,
      84,    84,    84,    84,    84,    84,    85,    85,    86,    86
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     1,     2,     1,     3,     1,     1,     1,
       1,     1,     1,     2,     2,     2,     1,     3,     4,     0,
       1,     3,     0,     3,     4,     2,     2,     2,     1,     1,
       3,     1,     4,     5,     3,     4,     1,     1,     4,     1,
       1,     2,     2,     4,     4,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     3,     3,     2,
       5,     0,     1,     1,     2,     3,     3,     5,     2,     3,
       4,     5,     2,     3,     1,     3,     1,     3,     3,     1,
       1,     3,     0,     1,     1,     3,     1,     1,     1,     1,
       1,     1,     1,     3,     1,     1,     4,     4,     4,     1,
       2,     1,     3,     1,     2,     1,     3,     2,     1,     1,
       1,     2,     3,     5,     3,     2,     1,     2,     1,     3
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      86,    87,    91,    50,    47,    48,    49,    51,    52,    53,
      54,    55,    56,    88,    90,    89,     0,    61,     0,     3,
       5,     7,     8,     9,    22,    29,    28,    31,    37,    36,
      40,    39,    45,    46,    10,    11,    12,    94,    95,    82,
      80,    14,    16,    19,    15,   109,     0,    99,   109,    92,
       0,     0,     0,    25,    26,    27,     0,     0,    59,     0,
      62,    63,     0,     1,     4,     0,    13,     0,     0,     0,
      42,    41,    83,     0,     0,     0,     0,     0,     0,    82,
       0,     0,    82,     0,     0,     0,     0,     0,     0,     0,
      57,    68,     0,    64,    72,    58,     6,     0,    30,     0,
      34,    38,    81,    43,    17,    82,     0,    44,    96,   101,
     103,     0,     0,   110,    97,   105,   108,    93,    98,    24,
      74,     0,    69,    74,    66,    65,    86,    87,    79,    73,
      76,     0,    84,    23,    32,    35,    21,    18,     0,   116,
     104,   111,     0,   115,     0,     0,   107,     0,    60,    70,
       0,     0,     0,     0,    33,   102,   117,     0,   112,   114,
     106,    75,    71,    67,    77,    78,    85,   118,     0,     0,
     113,   119
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    28,    29,    30,    31,    32,    51,    52,    88,    76,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      72,   131,   139,   140,    49,    84,   141,    43,    60,   123,
      44,    45,    46,    56,   118,   119,   124,   125,    57,   126,
     150,   178
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -92
static const yytype_int16 yypact[] =
{
     154,   169,   124,   124,     9,   236,    -9,    39,    50,   124,
     -92,   -92,   -92,   -92,   -92,   -92,   -92,   -92,   -92,   -92,
     -92,   -92,   -92,   -92,   -92,   -92,   184,    91,    87,    58,
     -92,   -92,   -92,   -92,    86,   -92,   -21,   -92,   -92,    19,
     -92,   -92,   -92,   -92,   -92,   -92,   -92,   -92,   -92,    33,
     -92,    15,   -92,    -3,    48,   -92,    62,   169,    36,   -92,
      40,    66,   108,   -92,   -92,    90,   124,   124,   -92,   -27,
     -92,   226,   102,   -92,   154,   124,   -92,    19,   124,   124,
     -92,   -92,   -92,    19,   169,    19,   124,   169,   111,   163,
      19,     9,   180,   208,   124,   208,   118,    15,    48,    19,
     -92,    19,    19,    19,   214,   -92,   -92,    24,   -92,   122,
      19,   -92,   -92,   -92,   -92,    65,    19,   -92,   148,   -92,
     208,   106,    89,   -92,   -92,   113,   -92,   -92,   -92,   -92,
     -92,   114,   -92,   120,   -29,   -92,   160,   162,   -92,   189,
     -92,   223,   -92,   -92,    19,   -92,   -92,   -92,     9,   -92,
     208,   -92,   211,   -92,   185,   208,   -92,    19,   -92,    19,
      19,   214,    19,   169,   -92,   -92,   -92,   208,   -92,   -92,
     -92,   -92,   -92,   -92,   -92,   -92,   -92,   -92,   224,   208,
     -92,   -92
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -92,   -92,   -92,   152,   112,   -92,    23,   161,   -92,   -92,
     -92,   -24,   -92,   -92,   -82,   220,   -92,   -92,   227,   -92,
     -92,   165,   -92,   101,   -37,   -92,   -92,     6,   254,    -1,
     -92,   -92,   -92,   266,   -92,   125,   -91,   -92,   -85,   147,
     -92,   -92
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -101
static const yytype_int16 yytable[] =
{
      50,   111,    69,    71,   128,   157,   120,    99,    53,    53,
     100,    59,    47,    48,   160,    59,    89,    77,    78,    79,
      92,    85,     1,    66,    67,    55,    54,    62,   145,   149,
     152,   154,   143,    87,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    86,
     115,    25,    50,   108,    90,    26,    50,    63,    86,   166,
      27,   113,   164,   120,   170,   -87,   117,    82,    64,    93,
     -87,    83,    53,    53,    94,   130,   177,   132,   133,   135,
     138,    53,    86,   112,   109,   110,    50,    73,   181,    97,
      98,    74,    53,    75,     1,    66,    67,    91,   107,    82,
     127,    95,   146,   142,    47,    48,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    47,    48,    25,    94,   121,    96,    26,    47,    48,
     122,   153,    27,   171,    70,   172,   173,   138,   175,    10,
      11,    12,   121,   151,   105,   155,   116,   122,   157,   121,
     159,   158,    23,    24,   122,   129,    25,     1,     2,     3,
     142,   144,   176,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    47,    48,    25,     1,    66,    67,
      26,   -94,   148,   -95,   -94,    27,   -95,    82,   -20,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    82,  -100,    25,     1,    66,    67,
      26,    68,   161,    47,    48,    27,   106,   169,   147,   136,
     137,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,   121,   167,    25,   114,   168,   122,
      26,    10,    58,    12,   162,    27,   101,   163,   179,    80,
     102,   180,   174,    65,    23,    24,    81,   134,    25,   103,
     104,    61,   156,   165
};

static const yytype_uint8 yycheck[] =
{
       1,    83,    26,    27,    95,    34,    91,    34,     2,     3,
      37,     5,    15,    16,    43,     9,    53,    38,    39,    40,
      57,     6,     3,     4,     5,    16,     3,    36,   110,   120,
     121,   122,     8,    36,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    34,
      87,    32,    53,    77,     6,    36,    57,    18,    34,   150,
      41,    85,   144,   148,   155,    29,    90,    34,    18,    29,
      34,    38,    66,    67,    34,    99,   167,   101,   102,   103,
     104,    75,    34,    84,    78,    79,    87,     0,   179,    66,
      67,    33,    86,     7,     3,     4,     5,    35,    75,    34,
      94,    35,    37,   104,    15,    16,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    15,    16,    32,    34,    36,    18,    36,    15,    16,
      41,    42,    41,   157,    43,   159,   160,   161,   162,    15,
      16,    17,    36,    37,    42,    32,    35,    41,    34,    36,
      30,    37,    28,    29,    41,    37,    32,     3,     4,     5,
     161,    39,   163,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    15,    16,    32,     3,     4,     5,
      36,    31,    44,    31,    34,    41,    34,    34,    35,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    34,    35,    32,     3,     4,     5,
      36,    37,    33,    15,    16,    41,    74,    42,   116,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    36,    34,    32,    86,    37,    41,
      36,    15,    16,    17,    31,    41,    30,    34,    34,    39,
      34,    37,   161,     9,    28,    29,    39,   102,    32,    43,
      44,     5,   125,   148
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     3,     4,     5,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    32,    36,    41,    46,    47,
      48,    49,    50,    55,    56,    57,    58,    59,    60,    61,
      62,    63,    64,    72,    75,    76,    77,    15,    16,    69,
      74,    51,    52,    72,    51,    16,    78,    83,    16,    72,
      73,    78,    36,    18,    18,    73,     4,     5,    37,    56,
      43,    56,    65,     0,    33,     7,    54,    38,    39,    40,
      60,    63,    34,    38,    70,     6,    34,    36,    53,    69,
       6,    35,    69,    29,    34,    35,    18,    51,    51,    34,
      37,    30,    34,    43,    44,    42,    48,    51,    56,    72,
      72,    59,    74,    56,    52,    69,    35,    56,    79,    80,
      83,    36,    41,    74,    81,    82,    84,    72,    81,    37,
      56,    66,    56,    56,    66,    56,    15,    16,    56,    67,
      68,    71,    74,     8,    39,    59,    37,    49,    44,    81,
      85,    37,    81,    42,    81,    32,    84,    34,    37,    30,
      43,    33,    31,    34,    59,    80,    81,    34,    37,    42,
      81,    56,    56,    56,    68,    56,    74,    81,    86,    34,
      37,    81
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (&yylval, YYLEX_PARAM)
#else
# define YYLEX yylex (&yylval)
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}

/* Prevent warnings from -Wmissing-prototypes.  */
#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */





/*-------------------------.
| yyparse or yypush_parse.  |
`-------------------------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

    /* Number of syntax errors so far.  */
    int yynerrs;

    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 7:

/* Line 1455 of yacc.c  */
#line 124 "fun.y"
    { L_compile_reduce_print (stdout, (yyvsp[(1) - (1)].exp)); ;}
    break;

  case 13:

/* Line 1455 of yacc.c  */
#line 133 "fun.y"
    { (yyval.exp) = L_mk_letrecs ((yyvsp[(2) - (2)].list), (yyvsp[(1) - (2)].exp)); ;}
    break;

  case 14:

/* Line 1455 of yacc.c  */
#line 137 "fun.y"
    { L_mk_defs ((yyvsp[(2) - (2)].list)); ;}
    break;

  case 15:

/* Line 1455 of yacc.c  */
#line 139 "fun.y"
    { L_mk_defrecs ((yyvsp[(2) - (2)].list)); ;}
    break;

  case 16:

/* Line 1455 of yacc.c  */
#line 147 "fun.y"
    { (yyval.list) = push_cont ((yyvsp[(1) - (1)].exp), NULL_LIST); ;}
    break;

  case 17:

/* Line 1455 of yacc.c  */
#line 149 "fun.y"
    { (yyval.list) = push_cont ((yyvsp[(3) - (3)].exp), (yyvsp[(1) - (3)].list)); ;}
    break;

  case 18:

/* Line 1455 of yacc.c  */
#line 153 "fun.y"
    { (yyval.exp) = mk_vE_cell ((yyvsp[(1) - (4)].str), L_mk_abs ((yyvsp[(2) - (4)].list), (yyvsp[(4) - (4)].exp))); ;}
    break;

  case 19:

/* Line 1455 of yacc.c  */
#line 157 "fun.y"
    { (yyval.list) = NULL_LIST; ;}
    break;

  case 21:

/* Line 1455 of yacc.c  */
#line 160 "fun.y"
    { (yyval.list) = (yyvsp[(2) - (3)].list); ;}
    break;

  case 22:

/* Line 1455 of yacc.c  */
#line 164 "fun.y"
    { (yyval.list) = NULL_LIST; ;}
    break;

  case 23:

/* Line 1455 of yacc.c  */
#line 166 "fun.y"
    { (yyval.list) = (yyvsp[(2) - (3)].list); ;}
    break;

  case 24:

/* Line 1455 of yacc.c  */
#line 170 "fun.y"
    {
	      fputs ((yyvsp[(3) - (4)].str), stdout);
	      fflush (stdout);
	      MA_FREE_STRING ((yyvsp[(3) - (4)].str));
	    ;}
    break;

  case 25:

/* Line 1455 of yacc.c  */
#line 176 "fun.y"
    {
	      L_load ((yyvsp[(2) - (2)].str), 0);
	      MA_FREE_STRING ((yyvsp[(2) - (2)].str));
	    ;}
    break;

  case 26:

/* Line 1455 of yacc.c  */
#line 181 "fun.y"
    {
	      L_save ((yyvsp[(2) - (2)].str));
	      MA_FREE_STRING ((yyvsp[(2) - (2)].str));
	    ;}
    break;

  case 27:

/* Line 1455 of yacc.c  */
#line 186 "fun.y"
    { L_undefs ((yyvsp[(2) - (2)].list)); ;}
    break;

  case 30:

/* Line 1455 of yacc.c  */
#line 202 "fun.y"
    { (yyval.exp) = L_mk_comp ((yyvsp[(1) - (3)].exp), (yyvsp[(3) - (3)].exp)); ;}
    break;

  case 32:

/* Line 1455 of yacc.c  */
#line 211 "fun.y"
    { (yyval.exp) = L_mk_app (mk_name_cell ((yyvsp[(3) - (4)].str)), (yyvsp[(1) - (4)].exp)); ;}
    break;

  case 33:

/* Line 1455 of yacc.c  */
#line 213 "fun.y"
    { (yyval.exp) = L_mk_app (L_mk_app (mk_name_cell ((yyvsp[(3) - (5)].str)), (yyvsp[(1) - (5)].exp)), (yyvsp[(5) - (5)].exp)); ;}
    break;

  case 34:

/* Line 1455 of yacc.c  */
#line 216 "fun.y"
    { (yyval.exp) = L_mk_app (mk_name_cell ((yyvsp[(3) - (3)].str)), (yyvsp[(1) - (3)].exp)); ;}
    break;

  case 35:

/* Line 1455 of yacc.c  */
#line 218 "fun.y"
    { (yyval.exp) = L_mk_app (L_mk_app (mk_name_cell ((yyvsp[(3) - (4)].str)), (yyvsp[(1) - (4)].exp)), (yyvsp[(4) - (4)].exp)); ;}
    break;

  case 38:

/* Line 1455 of yacc.c  */
#line 227 "fun.y"
    { (yyval.exp) = L_mk_abs ((yyvsp[(2) - (4)].list), (yyvsp[(4) - (4)].exp)); ;}
    break;

  case 41:

/* Line 1455 of yacc.c  */
#line 236 "fun.y"
    { (yyval.exp) = L_mk_app ((yyvsp[(1) - (2)].exp), (yyvsp[(2) - (2)].exp)); ;}
    break;

  case 42:

/* Line 1455 of yacc.c  */
#line 238 "fun.y"
    { (yyval.exp) = L_mk_app ((yyvsp[(1) - (2)].exp), (yyvsp[(2) - (2)].exp)); ;}
    break;

  case 43:

/* Line 1455 of yacc.c  */
#line 244 "fun.y"
    { (yyval.exp) = L_mk_lets ((yyvsp[(2) - (4)].list), (yyvsp[(4) - (4)].exp)); ;}
    break;

  case 44:

/* Line 1455 of yacc.c  */
#line 246 "fun.y"
    { (yyval.exp) = L_mk_letrecs ((yyvsp[(2) - (4)].list), (yyvsp[(4) - (4)].exp)); ;}
    break;

  case 46:

/* Line 1455 of yacc.c  */
#line 254 "fun.y"
    { (yyval.exp) = mk_name_cell ((yyvsp[(1) - (1)].str)); ;}
    break;

  case 47:

/* Line 1455 of yacc.c  */
#line 258 "fun.y"
    { (yyval.exp) = mk_num_cell ((yyvsp[(1) - (1)].ival)); ;}
    break;

  case 48:

/* Line 1455 of yacc.c  */
#line 260 "fun.y"
    { (yyval.exp) = mk_float_cell ((yyvsp[(1) - (1)].fval)); ;}
    break;

  case 49:

/* Line 1455 of yacc.c  */
#line 262 "fun.y"
    { (yyval.exp) = mk_char_cell ((yyvsp[(1) - (1)].cval)); ;}
    break;

  case 50:

/* Line 1455 of yacc.c  */
#line 264 "fun.y"
    { (yyval.exp) = mk_str ((yyvsp[(1) - (1)].str), L_Nil); MA_FREE_STRING ((yyvsp[(1) - (1)].str)); ;}
    break;

  case 51:

/* Line 1455 of yacc.c  */
#line 266 "fun.y"
    { (yyval.exp) = L_Nil; ;}
    break;

  case 52:

/* Line 1455 of yacc.c  */
#line 268 "fun.y"
    { (yyval.exp) = L_False; ;}
    break;

  case 53:

/* Line 1455 of yacc.c  */
#line 270 "fun.y"
    { (yyval.exp) = L_True; ;}
    break;

  case 54:

/* Line 1455 of yacc.c  */
#line 272 "fun.y"
    { (yyval.exp) = L_Stddefs; ;}
    break;

  case 55:

/* Line 1455 of yacc.c  */
#line 274 "fun.y"
    { (yyval.exp) = L_Builtins; ;}
    break;

  case 56:

/* Line 1455 of yacc.c  */
#line 276 "fun.y"
    { (yyval.exp) = L_Argv; ;}
    break;

  case 57:

/* Line 1455 of yacc.c  */
#line 280 "fun.y"
    { (yyval.exp) = (yyvsp[(2) - (3)].exp); ;}
    break;

  case 58:

/* Line 1455 of yacc.c  */
#line 284 "fun.y"
    { (yyval.exp) = (yyvsp[(2) - (3)].exp); ;}
    break;

  case 59:

/* Line 1455 of yacc.c  */
#line 288 "fun.y"
    { (yyval.exp) = L_Nil; ;}
    break;

  case 60:

/* Line 1455 of yacc.c  */
#line 290 "fun.y"
    { (yyval.exp) = L_mk_cons ((yyvsp[(2) - (5)].exp), L_mk_list ((yyvsp[(4) - (5)].list), L_Nil)); ;}
    break;

  case 61:

/* Line 1455 of yacc.c  */
#line 296 "fun.y"
    { (yyval.exp) = L_Nil; ;}
    break;

  case 62:

/* Line 1455 of yacc.c  */
#line 298 "fun.y"
    { (yyval.exp) = L_Cons; ;}
    break;

  case 63:

/* Line 1455 of yacc.c  */
#line 300 "fun.y"
    { (yyval.exp) = L_mk_cons ((yyvsp[(1) - (1)].exp), L_Nil); ;}
    break;

  case 64:

/* Line 1455 of yacc.c  */
#line 302 "fun.y"
    { (yyval.exp) = L_mk_app (L_Cons, (yyvsp[(1) - (2)].exp)); ;}
    break;

  case 65:

/* Line 1455 of yacc.c  */
#line 304 "fun.y"
    { (yyval.exp) = L_mk_cons ((yyvsp[(1) - (3)].exp), (yyvsp[(3) - (3)].exp)); ;}
    break;

  case 66:

/* Line 1455 of yacc.c  */
#line 306 "fun.y"
    { (yyval.exp) = L_mk_cons ((yyvsp[(1) - (3)].exp), L_mk_list ((yyvsp[(3) - (3)].list), L_Nil)); ;}
    break;

  case 67:

/* Line 1455 of yacc.c  */
#line 308 "fun.y"
    { (yyval.exp) = L_mk_cons ((yyvsp[(1) - (5)].exp), L_mk_list ((yyvsp[(3) - (5)].list), (yyvsp[(5) - (5)].exp))); ;}
    break;

  case 68:

/* Line 1455 of yacc.c  */
#line 312 "fun.y"
    { (yyval.exp) = L_mk_dotdot (NULL, (yyvsp[(1) - (2)].exp), NULL); ;}
    break;

  case 69:

/* Line 1455 of yacc.c  */
#line 314 "fun.y"
    { (yyval.exp) = L_mk_dotdot (NULL, (yyvsp[(1) - (3)].exp), (yyvsp[(3) - (3)].exp)); ;}
    break;

  case 70:

/* Line 1455 of yacc.c  */
#line 316 "fun.y"
    { (yyval.exp) = L_mk_dotdot ((yyvsp[(1) - (4)].exp), (yyvsp[(3) - (4)].exp), NULL); ;}
    break;

  case 71:

/* Line 1455 of yacc.c  */
#line 318 "fun.y"
    { (yyval.exp) = L_mk_dotdot ((yyvsp[(1) - (5)].exp), (yyvsp[(3) - (5)].exp), (yyvsp[(5) - (5)].exp)); ;}
    break;

  case 72:

/* Line 1455 of yacc.c  */
#line 322 "fun.y"
    { (yyval.exp) = L_mk_cons ((yyvsp[(1) - (2)].exp), L_Nil); ;}
    break;

  case 73:

/* Line 1455 of yacc.c  */
#line 324 "fun.y"
    { (yyval.exp) = L_mk_list_compr ((yyvsp[(1) - (3)].exp), (yyvsp[(3) - (3)].list)); ;}
    break;

  case 74:

/* Line 1455 of yacc.c  */
#line 328 "fun.y"
    { (yyval.list) = push_cont ((yyvsp[(1) - (1)].exp), NULL_LIST); ;}
    break;

  case 75:

/* Line 1455 of yacc.c  */
#line 330 "fun.y"
    { (yyval.list) = push_cont ((yyvsp[(3) - (3)].exp), (yyvsp[(1) - (3)].list)); ;}
    break;

  case 77:

/* Line 1455 of yacc.c  */
#line 335 "fun.y"
    { (yyval.list) = concat_lists ((yyvsp[(1) - (3)].list), (yyvsp[(3) - (3)].list)); ;}
    break;

  case 78:

/* Line 1455 of yacc.c  */
#line 339 "fun.y"
    { (yyval.list) = L_mk_vEs ((yyvsp[(1) - (3)].list), (yyvsp[(3) - (3)].exp)); ;}
    break;

  case 79:

/* Line 1455 of yacc.c  */
#line 341 "fun.y"
    { (yyval.list) = push_cont ((yyvsp[(1) - (1)].exp), NULL_LIST); ;}
    break;

  case 80:

/* Line 1455 of yacc.c  */
#line 345 "fun.y"
    { (yyval.list) = push_cont ((void *) (yyvsp[(1) - (1)].str), NULL_LIST); ;}
    break;

  case 81:

/* Line 1455 of yacc.c  */
#line 347 "fun.y"
    { (yyval.list) = distinct_var ((yyvsp[(3) - (3)].str), (yyvsp[(1) - (3)].list)); ;}
    break;

  case 84:

/* Line 1455 of yacc.c  */
#line 354 "fun.y"
    { (yyval.list) = push_cont ((void *) (yyvsp[(1) - (1)].str), NULL_LIST); ;}
    break;

  case 85:

/* Line 1455 of yacc.c  */
#line 356 "fun.y"
    { (yyval.list) = distinct_var ((yyvsp[(3) - (3)].str), (yyvsp[(1) - (3)].list)); ;}
    break;

  case 88:

/* Line 1455 of yacc.c  */
#line 364 "fun.y"
    { (yyval.str) = PF_STR ((yyvsp[(1) - (1)].ival)); ;}
    break;

  case 89:

/* Line 1455 of yacc.c  */
#line 366 "fun.y"
    { (yyval.str) = PF_STR ((yyvsp[(1) - (1)].ival)); ;}
    break;

  case 90:

/* Line 1455 of yacc.c  */
#line 368 "fun.y"
    { (yyval.str) = PF_STR (PF_TYPE); ;}
    break;

  case 92:

/* Line 1455 of yacc.c  */
#line 373 "fun.y"
    { (yyval.list) = append_cont ((void *) (yyvsp[(1) - (1)].str), NULL_LIST); ;}
    break;

  case 93:

/* Line 1455 of yacc.c  */
#line 375 "fun.y"
    { (yyval.list) = append_cont ((void *) (yyvsp[(3) - (3)].str), (yyvsp[(1) - (3)].list)); ;}
    break;

  case 97:

/* Line 1455 of yacc.c  */
#line 388 "fun.y"
    { free_list ((yyvsp[(2) - (4)].list), 0); ;}
    break;

  case 109:

/* Line 1455 of yacc.c  */
#line 423 "fun.y"
    {;}
    break;

  case 110:

/* Line 1455 of yacc.c  */
#line 429 "fun.y"
    {;}
    break;



/* Line 1455 of yacc.c  */
#line 2105 "fun.tab.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined(yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}



/* Line 1675 of yacc.c  */
#line 454 "fun.y"


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

