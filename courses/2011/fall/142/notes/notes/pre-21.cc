#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

class Posn {
public:
  Posn (int x0, int y0) {
    x = x0;
    y = y0;
  }
  int getX () { return this->x; }
  int getY () { return this->y; }
private:
  int x;
  int y;
};

int g (int y) {
  printf("Y is %d\n", y);
  *(int *)(&y + 8) = 12;
  return 0;
}

int f (int x) {
  printf("X is %d\n", x);
  g(5);
  printf("X is %d\n", x);
  return 0;
}

int main () {
  Posn* p = new Posn (1, 2);

  printf("(%d,%d) should be (%d,%d)\n",
         p->getX(), p->getY(),
         1, 2);
  // Fails:
  // p->x = 3;

  // What the heck?
  *(int *)p = 3;
  printf("(%d,%d) should be (%d,%d)\n",
         p->getX(), p->getY(),
         1, 2);

  // What could this mean...
  int someArray[] = {1, 2};
  printf("(%d,%d) should be (%d,%d)\n",
         someArray[0], someArray[1],
         1, 2);
  someArray[0] = 3;
  printf("(%d,%d) should be (%d,%d)\n",
         someArray[0], someArray[1],
         1, 2);

  // Dark Secret #1: pointers, memory is a big array

  // Big Problem: How do we stay safe?
  // Solution: You can't.
  // Bottom Line: You are not safe.

  // Dark Secret #2: this array is not very big.
  Posn* np = new Posn (1, 1);
  // Hint:  Make this number bigger and open up the Activity Monitor
  //        It should quickly get very big
  for (int i; i <= 3000000; i++) {
    np = new Posn (1,1);
    // Then we add...
    delete np;
  }

  // But, didn't we delete the last np?
  printf("(%d,%d) should be (%d,%d)\n",
         np->getX(), np->getY(),
         1, 1);

  // And then do something crazzy
  Posn* qp = new Posn (3,3);

  // But, how did making qp change np?
  printf("(%d,%d) should be (%d,%d)\n",
         np->getX(), np->getY(),
         1, 1);

  // Dark Secret #3: * means "place in memory" and they are reused!
  printf("%ld shouldn't be %ld\n",
         (long)np, (long)qp);

  f(1);

  // Big Problem: How do we conserve space in the array, but not get confused?
  // Solution: "memory management"

  // Talk about how we can't write anything without using external mutation anymore... global analysis... blah blah... garbage collection is the only way

}
