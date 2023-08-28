// .h stands for "header"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

// C garbage collector:
// #include <gc.h>
// -lgc

typedef struct {
  int x;
  int y;
} posn_t;

class Posn {
public:
  int x;
  int y;
};

// A List is either Empty or Cons
typedef struct {
  char areYouEmpty;
  void(*printFun)(void *l);
} list_t;
void list_printFun(void *l) {
  ((list_t *)l)->printFun(l);
  // Take l and view it as a number
  // Add sizeof(char) to l
  // Take that number and read it as a number, PF
  // JUMP to PF (meaning set the ProgramCounter to that)
}

typedef struct {
  list_t l;
} empty_t;
void empty_printFun(void *l) {
  printf("[]\n");
}

typedef struct {
  list_t l;
  int first;
  list_t *rest;
} cons_t;
void cons_printFun(void *l) {
  cons_t *c = (cons_t *)l;
  printf("%d :: ", c->first);
  list_printFun(c->rest);
}

void print_list(list_t *l) {
  if ( l->areYouEmpty ) {
    printf("[]\n");
  } else {
    cons_t *c = (cons_t*)l;
    printf("%d :: ", c->first);
    print_list(c->rest);
  }
}

class List {
public:
  virtual void print() = 0;
};

class Empty : public List {
public:
  void print() {
    printf("[]\n");
  }
};

class Cons : public List {
public:
  int first;
  List *rest;
  void print() {
    printf("%d :: ", first);
    rest->print();
  }
};

int main(int argc, char **argv) {
  posn_t posn;
  posn.x = 5;
  posn.y = 7;
  printf("%d %d\n", posn.x, posn.y);

  Posn posn_pp;
  posn_pp.x = 5;
  posn_pp.y = 7;
  printf("%d %d\n", posn_pp.x, posn_pp.y);


  empty_t mt;  
  mt.l.areYouEmpty = 1;
  mt.l.printFun = &empty_printFun;
  print_list((list_t*)&mt);
  list_printFun(&mt);
  cons_t l1;
  l1.l.areYouEmpty = 0;
  l1.l.printFun = &cons_printFun;
  l1.first = 5;
  l1.rest = (list_t*)&mt;
  print_list((list_t*)&l1);
  list_printFun(&l1);
  cons_t l2;
  l2.l.areYouEmpty = 0;  
  l2.l.printFun = &cons_printFun; //(void (*)(void *))&cos; // &cons_printFun;
  l2.first = 6;
  l2.rest = (list_t*)&l1;
  print_list((list_t*)&l2);
  list_printFun(&l2);

  list_t *l3p = (list_t*)&l2;
  for ( int i = 0; i < 10; i++ ) {
    cons_t *lNp = (cons_t*)calloc(1, sizeof(cons_t));
    lNp->l.areYouEmpty = 0;
    lNp->l.printFun = &cons_printFun;
    lNp->first = i;
    lNp->rest = l3p;
    l3p = (list_t *)lNp;
  }
  print_list(l3p);
  list_printFun(l3p);

  Empty mt_pp;
  mt_pp.print();
  Cons l1_pp;
  l1_pp.first = 5;
  l1_pp.rest = &mt_pp;
  l1_pp.print();
  Cons l2_pp;
  l2_pp.first = 6;
  l2_pp.rest = &l1_pp;
  l2_pp.print();

  List *l3p_pp = &l2_pp;
  for ( int i = 0; i < 10; i++ ) {
    Cons *lNp = new Cons();
    lNp->first = i;
    lNp->rest = l3p_pp;
    l3p_pp = lNp;
  }
  l3p_pp->print();

  for ( int i = 0; i < 100; i++ ) {
    // malloc <-> free
    // new <-> delete
    Cons *lNp = new Cons();
    delete(lNp);
  }

  // F : List(E) -> Number
  // F owns the List
  // F's caller owns the Es inside

  // G : List(A) -> List(B)
  // G's callers owns the input and output lists
  // G owns the As
  // G's caller owns the Bs

  return 0;
}

// classes
// interfaces
// templates
// memory management
