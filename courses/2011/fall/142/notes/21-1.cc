// C
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

// C++
#include <iostream>
#include <string>
using namespace std;

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

  int papply( int fun (int,int) ) {
    return fun(this->x, this->y);
  }

  friend ostream& operator<<(ostream& cout, Posn* obj) {
    cout << "(" << obj->x << "," << obj->y << ")";
    return cout;
  }
};


int f (int x, int y) {
  return x + y;
}
int g (int x, int y) {
  return x * y;
}

int main () {
  printf("The answer is %d, but should be %d\n",
         5,
         5 );

  printf("The answer is %d, but should be %d\n",
         f(5, 6),
         11 );
  printf("The answer is %d, but should be %d\n",
         g(5, 6),
         30 );

  Posn* p = new Posn(5, 6);
  p->show();
  printf("\n");

  int x = 5;
  // .... printf/43897452897 ... show ... p ... x
  // array[4] is the fifth thing in the array
  // 4 is called an "index"
  // memory is the giant array
  // POINTER
  // * means "pointer" in C
  // & means "what's the pointer to this"

  printf("The answer is %d, but should be %d\n",
         x,
         &x );
  printf("The answer is %d, but should be %d\n",
         p,
         &p );

  Posn** pointer_to_p = &p;
  printf("The answer is %d, but should be %d\n",
         pointer_to_p,
         &pointer_to_p );

  Posn*** pointer_pointer_to_p = &pointer_to_p;
  printf("The answer is %d, but should be %d\n",
         pointer_pointer_to_p,
         &pointer_pointer_to_p );

  printf("The answer is %d, but should be %d\n",
         x,
         *(&x) );

  printf("The answer is %d, but should be %d\n",
         // CASTING
         //*x, // doesn't work
         //*(int*)x, "works"
         x,
         *(&x) );

  printf("The answer is %d, but should be %d\n",
         p,
         *(int*)p );

  int array[3] = {0, 1, 2};

  printf("The answer is %d, but should be %d\n",
         array[1],
         *(array + 1) );

  int * totally_new_array = (int *)malloc(4*sizeof(int));
  printf("The answer is %d, but should be %d\n",
         totally_new_array,
         totally_new_array[0]);
  totally_new_array[0] = 1;
  totally_new_array[1] = 3;
  totally_new_array[2] = 5;
  totally_new_array[3] = 7;
  printf("The answer is %d, but should be %d\n",
         totally_new_array,
         totally_new_array[0]);
  
  // Stack and Heap
  // Stack means arguments to functions and local variables in functions
  // Heap means things that came from "new" or "malloc"

  printf("The answer is %d, but should be %d\n",
         array,
         totally_new_array);
  printf("The answer is %d, but should be %d\n",
         &p,
         p);

  printf("The answer is %d, but should be %d\n",
         NULL,
         0);

  // Memory terminology above here

  cout << "Hey eyes, watch out!\n"; 
  cout << "Hey eyes" << "Here's more stuff\n";
  cout << "This next thing will be an integer, but look ma no percents" << x << "\n";
  cout << "This next thing will be a posn, but look ma no show " << p << "\n";

  //cin >> x;

  printf("The answer is %d, but should be %d\n",
         f(5, 6),
         p->papply(f) );
  printf("The answer is %d, but should be %d\n",
         g(5, 6),
         p->papply(g) );


  return 0;
}

// pointer (*, &, **, NULL)
// cast
// malloc
// returning new array
// stack
// heap

// cout
//   friend ostream& operator<<(ostream& cout, Posn* obj)
// cin
// function pointer

/// NEXT TIME 

// do-while
// ++
// break, continue
// goto
// switch

// template
//  STL, containers, etc
