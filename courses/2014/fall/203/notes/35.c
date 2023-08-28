// .h stands for "header"
#include <stdio.h>
#include <math.h>

/* linking

 cc -c 35h.c
~203/notes
% cc -c 35.c
~203/notes
% cc -o 35 35.o 35h.o
~203/notes
% ./35
0. ./35
3.141593
~203/notes

extern int f(int x);
*/

typedef struct {
    int x;
    int y;
} posn_t;

// A List is either Empty or Cons
typedef struct {
  char areYouEmpty;
} list_t;
typedef struct {
  list_t l;
} empty_t;
typedef struct {
  list_t l;
  int first; // = abcd
  list_t *rest;
} cons_t;

void print_list(list_t *l) {
  if ( l->areYouEmpty ) {
    printf("[]\n");
  } else {
    cons_t *c = (cons_t*)l;
    printf("%d :: ", c->first);
    print_list(c->rest);
  }
}

// Java, main = String[] -> void
int main(int argc, char **argv) {
  for (int i = 0; i < argc; i++) {
    printf("%d. %s\n", i, argv[i]);
  }
  printf("%f\n", M_PI);

  long xl = 0;
  int x = 0;
  short xs = 1;
  char xc = 0;
  printf("long %d int %d short %d char %d\n",
         sizeof(xl), sizeof(x), sizeof(xs), sizeof(xc));
  
  char j = 'j';
  float pie = 3.14;

  posn_t posn;
  posn.x = 5;
  posn.y = 7;
  printf("%d %d\n", posn.x, posn.y);

  int *xp;
  xp = &x;
  printf("xp %d points to %d which is %d\n", xp, *xp, x);

  int xa[5];
  xa[0] = x;
  printf("xa %d points to %d which is %d\n", xa, xa[0], x);
  printf("xa %d points to %d which is %d\n", xa, xa[17], x);
  printf("xa %d points to %d which is %d\n", xa, *(int*)((long)xa + 17 * 4), x);

  empty_t mt;
  mt.l.areYouEmpty = 1;
  print_list(&mt);
  cons_t l1;
  l1.l.areYouEmpty = 0;
  l1.first = 5;
  l1.rest = &mt;
  print_list(&l1);
  cons_t l2;
  l2.l.areYouEmpty = 0;
  l2.first = 6;
  l2.rest = &l1;
  print_list(&l2);

  union { int i; long l; float f; } u;
  printf("%d\n", sizeof(u));
  printf("%d\n", u.i);
  printf("%ld\n", u.l);
  printf("%f\n", u.f);
  u.f = 3.14;
  printf("%d\n", u.i);
  printf("%ld\n", u.l);
  printf("%f\n", u.f);
  
  return 0;
}

// DONE Running and compiling
// DONE Atomic values and types
// DONE Functions
// DONE Linking
// DONE if, while, for, switch
// DONE enum, union, struct
// DONE typedef struct Posn
// DONE manual dispatch
// DONE lists
// DONE pointers vs inclusion

// classes

// interfaces

// templates

// memory management
