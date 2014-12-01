#include <stdio.h>

int main(int argc, char **argv) {
  printf("Hello\n");
  printf("Arg count: %d\n", argc);
  printf("Args\n");
  for (int i = 0; i < argc; i++) {
    printf("\t%d. %s\n", i, argv[i]);
  }
  return 0;
}

// Running and compiling

// Atomic values and types

// Functions

// Linking

// if, while, for, switch

// enum, union, struct
// typedef struct Posn

// manual dispatch

// lists
// pointers vs inclusion

// classes

// interfaces

// templates

// memory management
