#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

class NegaPosn {
private:
  double x;
  double y;
public:
  NegaPosn (double x0, double y0) {
    this->x = x0;
    this->y = y0;
  }
};

class Posn {
private:
  int x;
  int y;
public:
  Posn (int x0, int y0) {
    this->x = x0;
    this->y = y0;
  }
  void show () {
    printf("(%d,%d)", this->x, this->y);
  }
};

void g (int y) {
  *(&y+8) = y;
  return ;
}

int f (int x) {
  printf("Inside f before g, x is %d\n", x);
  g(4);
  printf("Inside f after g, x is %d\n", x);
  return x;
}

int main () {
  Posn* p = new Posn (1, 2);

  printf("The answer is...\n\t");
  p->show();
  printf("\nBut should be...\n\t");
  printf("(1,2)");
  printf("\n");

  // C++ says illegal because of privacy
  //p->x = 10;

  // C++ says illegal because an int is not a Posn
  //p = 10;

  // C++ says illegal not allowed because of gobbledygook
  //*p = 10;

  // C++ says okay
  *(int *)p = 10;
  *(((int *)p)+1) = 10;

  // private means nothing
  // private protects you from ignorance, not malice.

  printf("The answer is...\n\t");
  p->show();
  printf("\nBut should be...\n\t");
  printf("(1,2)");
  printf("\n");

  printf("The answer is %d, but should be %d\n",
         f(5),
         5 );

  //for (int i = 0; i < 0; i = i + 1 ) {
  //  delete p;
  //  p = new Posn (0, 0);
  //}

  printf("The answer is...\n\t");
  p->show();
  printf("\nBut should be...\n\t");
  printf("(10,10)");
  printf("\n");

  delete p;
  //Posn* np = new Posn(3,3);
  NegaPosn* np = new NegaPosn(3.14, 3.14);

  printf("The answer is...\n\t");
  p->show();
  printf("\nBut should be...\n\t");
  printf("(10,10)");
  printf("\n");

  // Memory is a big array of ints

  printf("The answer is %ld, but should be %ld\n",
         p,
         np );
  printf("The answer is %ld, but should be %ld\n",
         &p,
         &np );
  printf("The answer is %ld, but should be %ld\n",
         &printf,
         &main );

  //printf("Test\n");
  //*((int *)&printf) = 10;
  //printf("Test\n");

  //getc(stdin);

  // Garbage Collection

  return 0;
}

// privacy
// arrays
// big?
// index
// EVERYTHING
// stack attack
// gc
// ----- for next time -----
// what is a pointer?
// what is casting?
// what is malloc? and returning new arrays
