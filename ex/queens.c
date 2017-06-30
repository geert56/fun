/* C version of queens problem.
   Mimicks queens.fun as much as possible; is much faster though!
   Copyright (c) 1996 - G. Janssen
*/

#include <stdio.h>
#include <stdlib.h>

#include "alloc.h"
#include "list.h"

static void
printi(FILE *fp, void *i)
{
  fprintf(stderr, "%d", (int) i);
}

static void
printl(LIST l)
{
  print_list(stderr, "[ ", l, printi, ", ", " ]\n");
}

static LIST
my_push_cont(int i, LIST b)
{
  LIST p = push_cont((void *) i, NULL_LIST);

  LIST_NEXT(LIST_LAST(p)) = LIST_FIRST(b);
  LIST_LAST(p) = LIST_LAST(b);
  p->size += LIST_SIZE(b);
  return p;
}

static int
ith(int i, LIST b)
{
  LIST_ELEM_PTR p = LIST_FIRST(b);

  while (i--) p = LIST_NEXT(p);
  return (int) ELEM_CONTENTS(p);
}

/* checks q b i = || (= q (ith (- i 1) b))
                             (= (abs (- q (ith (- i 1) b))) i),
*/
static int
checks(int q, LIST b, int i)
{
  int r = ith(i-1, b);

  return q == r || abs(q - r) == i;
}

/* safe q b = and [ not(checks q b i) | i <- [1..len b] ] */
static int
safe(int q, LIST b)
{
  int len = LIST_SIZE(b);
  int i;

  for (i = 1; i <= len; i++)
    if (checks(q, b, i))
      return 0;
  return 1;
}

/* queens1 M n =
    if (= n 1)
      [ [x] | x <- [1..M] ]
     [ [q:b] | q <- [1..M]; b <- queens1 M (- n 1); safe q b ]
*/
static LIST
queens1(int M, int n)
{
  LIST L = NULL_LIST;

  if (n == 1) {
    int x;

    for (x = 1; x <= M; x++)
      L = append_cont(push_cont((void *) x, NULL_LIST), L);
    return L;
  }

#if 0
  {
    int q;

    for (q = 1; q <= M; q++) {
      LIST Qn_1 = queens1(M, n - 1);

      while (Qn_1) {
	LIST b = pop_cont(&Qn_1);

	if (safe(q, b))
	  L = append_cont(push_cont((void *) q, b), L);
	else
	  free_list(b, 0);
      }
    }
  }
#endif

  {
    /* This is crucial for fast performance. */
    LIST Qn_1 = queens1(M, n - 1);
    int q;

    for (q = 1; q <= M; q++)
      FOR_EACH_LIST_ELEM(Qn_1, elem) {
	LIST b = ELEM_CONTENTS(elem);

	if (safe(q, b))
	  L = append_cont(my_push_cont(q, b), L);
      } END_FOR_EACH_LIST_ELEM;
    free_list(Qn_1, 0);
  }
  return L;
}

/* n >= 1
  queens n = queens1 n n
*/
static LIST
queens(int n)
{
  return queens1(n, n);
}

int
main(int argc, char *argv[])
{
  int n = argc > 1 ? atoi(argv[1]) : 8;
  LIST solutions = queens(n);

  fprintf(stderr, "%d\n", LIST_SIZE(solutions));
  return 0;
}
