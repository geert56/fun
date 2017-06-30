/*
 DOCUMENTATION INFORMATION				          module: MAIN
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 system    : Hewlett-Packard HP9000/S735
 file	   : main.c
 unit-title: 
 ref.	   : 
 author(s) : Copyright (c) 1995-1997 G.L.J.M. Janssen
 date	   :  8-DEC-1997
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

#include "fun.h"
#include "compile.h"

extern int parse_file (const char *name, FILE *fp);

/* Program parameter flags: */
Bool warnings = 1;

char *filename = "stdin";

int
main(int argc, char *argv[])
{
  extern char *optarg;
  extern int opterr;
  extern int optind;
  int option;
  char *opt_str = "dhlOpvVw";
  char usage_str[80];
  FILE *fp = stdin;
  int passc = argc;

  sprintf(usage_str, "usage: %%s [ -%s ] [ f1 f2 ... fn - ]\n", opt_str);

  /* Process arguments: */
  while ((option = getopt(argc, argv, opt_str)) != EOF)
    switch (option) {
    case 'd':
      fun_debug = 1;
      break;

    case 'e':
      fun_echo = 1;
      break;

    case 'h':
fprintf(stderr, usage_str, argv[0]);
fprintf(stderr,
"-d    : prints debug info to stderr; implies -v ON.\n"
"-h    : prints just this text to stderr and stops.\n"
"-l    : don't load the usual startup files.\n"
"-O    : switches off compiler optimisation.\n"
"-p    : followed by filename; that and rest of args goes to FUN's argv.\n"
"-v    : verbose level 0, prints action summary to stderr.\n"
"-V    : verbose level 1, prints more to stderr.\n"
"-w    : inhibits warning messages.\n");
      return 0;

    case 'l':
      fun_virginal = 1;
      break;

    case 'O':
      compile_optimise = 0;
      break;

    case 'p':
      /* Next arg must be filename to execute, rest of args will be
	 passed to FUN's argv constant.
      */
      passc = optind;
      if(fun_debug)    fun_verbose1 = 1;
      if(fun_verbose1) fun_verbose0 = 1;
      fun_init(argc - optind, argv + optind);
      goto break_loop;

    case 'v':
      fun_verbose0 = 1;
      break;

    case 'V':
      fun_verbose1 = 1;
      break;

    case 'w':
      warnings = 0;
      break;

    case '?':
    default:
      fprintf(stderr, "Error: unknown option. Stop.\n");
      fprintf(stderr, usage_str, argv[0]);
      return 1;
    }

  if (fun_debug)    fun_verbose1 = 1;
  if (fun_verbose1) fun_verbose0 = 1;
  fun_init(argc, argv);

 break_loop:

  if (optind == argc)
    goto doit;

  do {
    filename = argv[optind];
    if (!strcmp(filename, "-")) {
      /* Read from stdin. */
      fp = stdin;
      filename = "stdin";
    }
    else {
      if (!(fp = fopen(filename, "r"))) {
	fprintf(stderr, "Cannot read file: %s\n", filename);
	continue;
      }
    }

  doit:
    if (fun_verbose0) fprintf(stderr, "Processing file %s...\n", filename);

    parse_file(filename, fp);
    fclose(fp);

  } while (++optind < passc);

  fun_quit();

  return 0;
}
