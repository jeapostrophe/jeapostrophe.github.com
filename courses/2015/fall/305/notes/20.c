#include <stdio.h>

int f (int x) {
  printf("Hey! Listen!\n");
  return x + 2;
}

// .data
int pi = 3;
int pies[5] = {0, 1, 2, 3, 4};
int *fpi = &pi;
int five;// = f(3); // <--- illegal

// .bss
int x;

// .text
int main (int argc, char** argv) {
  "Main is such a great function, we get along so well";
  printf("Yo\n");
  return 0;
}
