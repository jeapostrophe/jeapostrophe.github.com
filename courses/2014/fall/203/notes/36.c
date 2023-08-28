// .h stands for "header"
#include <stdio.h>
#include <stdlib.h>

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
  int first;
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

int main(int argc, char **argv) {
  posn_t posn;
  posn.x = 5;
  posn.y = 7;
  printf("%d %d\n", posn.x, posn.y);

  empty_t mt;
  mt.l.areYouEmpty = 1;
  print_list((list_t*)&mt);
  cons_t l1;
  l1.l.areYouEmpty = 0;
  l1.first = 5;
  l1.rest = (list_t*)&mt;
  print_list((list_t*)&l1);
  cons_t l2;
  l2.l.areYouEmpty = 0;
  l2.first = 6;
  l2.rest = (list_t*)&l1;
  print_list((list_t*)&l2);

  list_t *l3p = (list_t*)&l2;
  for ( int i = 0; i < 10; i++ ) {
    cons_t *lNp = (cons_t*)malloc(sizeof(cons_t));
    lNp->l.areYouEmpty = 0;
    lNp->first = i;
    lNp->rest = l3p;
    l3p = (list_t *)lNp;
  }
  print_list(l3p);

  return 0;
}

// classes
// interfaces
// templates
// memory management
