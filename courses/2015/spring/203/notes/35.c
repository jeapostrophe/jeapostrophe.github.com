#include <stdio.h>

// A "forward declaration"
int factorial(int x);

typedef struct {
  int x;
  int y;
} posn;

typedef struct {
  int first;
  void *rest;
} cons;

int count(cons *l) {
  if ( l == NULL ) {
    return 0;
  } else {
    return 1 + count(l->rest);
  }
}

int sum(cons *l) {
  if ( l == NULL ) {
    return 0;
  } else {
    return l->first + sum(l->rest);
  }
}

// Don't forget about cmdline args
int main(int argc, char **argv) {
  for (int i = 0; i < argc; i++) {
    printf("Got one: %s\n", argv[i]);
  }
  
  printf("Hello\n");

  if ( 5 < 20 ) {
    printf("Yep\n");
  } else {
    printf("Nope\n");
  }

  printf("Fac of 5 is %d\n", factorial(5) );

  printf("Afterwards\n");

  float fred = 10.4; // -2^31 and +2^31
  char alex = 5; // -2^7 and +2^7
  short wilma = 889;
  long john = 900*900*900;
  float pie = 3.12;
  double tau = 6.24;

  printf("John is %d long\n", sizeof(john));

  // STACK allocated memory
  int facts[20];
  for (int i = 0; i < 20; i++) {
    facts[i] = factorial(i);
    printf("%d. fac(%d) = %d\n",
           i, i, factorial(i));
  }
  printf("-2 = %d\n", facts[-2]);
  printf("22 = %d\n", facts[22]); 

  // HEAP allocated memory
  int *faxes = malloc(20 * sizeof(int));
  printf("-2 = %d\n", faxes[-2]);
  faxes[15] = 50;
  printf("15 = %d\n", faxes[15]);

  // try 20 million
  printf("outside? %d\n", faxes[20000]);

  printf("fred before %f\n", fred);
  facts[-2] = 13241;
  printf("fred after %f\n", fred);

  // A float f is
  // (s = Sign [-1, +1, 1 bit], m = Mantissa [18 bits], e = Exponent [13 bits])
  // f = s * m * 2^e

  posn p;
  p.x = 5; p.y = 6;
  printf("p.x = %d\n", p.x);

  cons *l = NULL;
  for (int i = 0; i < 40; i++) {
    cons *new_l = malloc(sizeof(cons));
    new_l->first = i;
    new_l->rest = l;
    l = new_l;
  }
  printf("l is %d long\n", count(l));
  printf("l's sum is %d\n", sum(l));
  
  return 0;
}

int factorial(int x) {
  if ( x == 0 ) {
    return 1;
  } else {
    return x * factorial(x - 1);
  }
}

// Includes
// Main
// Compiling
// Functions
// if, while, for, switch
// Structs
// Manual dispatch
// Lists
// Boxed vs not boxed
// Sizeof
// Linking with other files

// classes
// interfaces
// templates
// memory
