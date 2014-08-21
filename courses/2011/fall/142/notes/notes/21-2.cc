// C
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

// C++
#include <iostream>
#include <string>
using namespace std;

template <class T>
class Posn {
private:
  T x;
  T y;
public:
  Posn (T x0, T y0) {
    this->x = x0;
    this->y = y0;
  }

  friend ostream& operator<<(ostream& cout, Posn* obj) {
    cout << "(" << obj->x << "," << obj->y << ")";
    return cout;
  }
};

int main () {
  cout << "The answer is " << 5 << ", but should be " << 5 << "\n";

  int n = 0;
  while ( n != 0 ) {
    printf("This happened\n");
  }

  do {
    printf("This happened\n");
  } while ( n != 0 );

  // code
  // while ( cond ) { code }

  //n = n + 4;
  n += 4;
  printf("n is %d\n", n);

  n ++;
  printf("n is %d\n", n);

  n --;
  printf("n is %d\n", n);

  while ( n > 0 ) {
    if ( n == 10 ) {
      break;
    }
    n ++;
  }
  printf("Something\n");

  while ( n > 0 ) {
    if ( n == 10 ) {
      printf("n is %d\n", n);
      //continue; // this makes it run forever
      break;
    }
    n ++;
  }
  printf("Something else\n");

 line1:  printf("Line 1\n"); goto line5;
 line2:  printf("Line 2\n"); goto line4;
 line3:  printf("Line 3\n"); goto line2;
 line4:  printf("Line 4\n"); goto end;
 line5:  printf("Line 5\n"); goto line3;
 end:

  printf("Something else else\n");

  n = 0;
  while ( n < 4 ) {
    printf("N is %d\n", n);
    n++;
  }
  printf("Something else else else\n");

  n = 0;
 backagain:  if ( n < 4 ) {
    printf("N is %d\n", n);
    n++;
    goto backagain;
  }
  printf("Something else else else else\n");

  // Dijkstra: GOTO, consider harmful

  n = 4;
  if ( n == 0 ) {
    printf("N is 0\n");
  } else {
    if ( n == 1 ) {
      printf("N is 1\n");
    } else {
      if ( n == 2 ) {
        printf("N is 2\n");
      } else {
        if ( n == 3 ) {
          printf("N is 3\n");
        } else {
          printf("N is 4\n");
        }
      }
    }
  }

  printf("In betweeny\n");

  n = 4;
  if ( n == 0 ) 
    printf("N is 0\n");
  else if ( n == 1 ) 
    printf("N is 1\n");
  else if ( n == 2 ) 
    printf("N is 2\n");
  else if ( n == 3 ) 
    printf("N is 3\n");
  else 
    printf("N is 4\n");  

  printf("In betweeny 2\n");

  n = 3;
  if ( n == 0 ) 
    printf("N is 0\n");
  else if ( n == 1 ) 
    printf("N is 1\n");
  else if ( n == 2 ) 
    printf("N is 2\n");
  else if ( n == 3 ) 
    printf("N is 3\n");
  else 
    printf("N is 4\n");  
    printf("N is really really 4\n");

  n = 3;
  if ( n == 0 ) {
    printf("N is 0\n");
  } else if ( n == 1 ) {
    printf("N is 1\n");
  } else if ( n == 2 ) {
    printf("N is 2\n");
  } else if ( n == 3 ) {
    printf("N is 3\n");
  } else {
    printf("N is 4\n");  
    printf("N is really really 4\n");
  }

  printf("Switch is next\n");

  n = 3;
  switch ( n ) {
  case 0:
    printf("N is 0\n"); break;
  case 1:
    printf("N is 1\n"); break;
  case 2:
    printf("N is 2\n"); break;
  case 3:
    printf("N is 3\n"); break;
  default:
    printf("N is 4\n");  
    printf("N is really really 4\n");
  }

  Posn<int>* p = new Posn<int>(5, 6);
  cout << "The posn is " << p << "\n";

  Posn<double>* dp = new Posn<double>(5.15, 6.24);
  cout << "The double posn is " << dp << "\n";

  // Functions abstract over values
  // Templates abstract over types

  // STL -- Standard Template Library

  return 0;
}

// do-while
// ++
// break, continue
// goto
// switch

// template
//  STL, containers, etc

// learn C++ better
// learn a new language

// show off other language
