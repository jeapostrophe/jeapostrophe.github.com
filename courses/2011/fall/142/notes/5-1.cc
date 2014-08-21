#include <stdio.h>
#include <math.h>
#include <string.h>

// booleanToString : boolean -> string
const char* booleanToString ( bool it ) {
 if ( it ) { return "true"; } else { return "false"; }
}

// A Point is a 
// new_Point (x, y, z)
// where 
// - x is a double
// - y is a double
// - z is a double
typedef struct Point;

typedef struct Point {
  double x;
  double y;
  double z;
};

// Contract: new_Point : double double double -> Point*
Point* new_Point(double x0, double y0, double z0) {
  Point* p = new Point();
  p->x = x0;
  p->y = y0;
  p->z = z0;
  return p;
}

// Contract: translate : Point* number number -> Point*
// Purpose: move the x, y coordinate by dx, dy
Point* translate(Point* p, double dx, double dy) {
  // Template: p->x, p->y, p->z, dx, dy

  // Example 1:
  // p->x = 3
  // p->y = 3
  // dx = 1
  // dy = -1
  // ans = new_Point(4,2, z)
  // return new_Point(4, 2, z);

  // Example 2:
  // p->x = -1
  // p->y = 5
  // dx = 3
  // dy = 5
  // ans = new_Point(2, 10, z)
  // return new_Point(2, 10, z);

  // Generalize 1 & 2
  return new_Point(p->x + dx, p->y + dy, p->z);
}

// Contract: distanceToOrigin: Point* -> double
// Purpose: calculate the distance from the point to the origin
double distanceToOrigin(Point* p) {
  // Template: p->x, p->y, p->z
  // Example 1:
  // p = new_Point(3, 4, 5)
  // ans = sqrt(3 * 3 + 4 * 4 + 5 * 5)
  // return 7.071;

  // Example 2:
  // p = new_Point(5, 9, 2)
  // ans = sqrt(5 * 5 + 9 * 9 + 2 * 2)
  // return 10.488;
  
  // Generalize 1 & 2
  return sqrt(p->x * p->x + p->y * p->y + p->z * p->z);
}

// main : -> number
int main () {
 printf ( "The answer is %f, but should be %f\n",
          1.0/2.0,
          0.5 ) ;
 printf ( "C++ says %s\n",
          booleanToString(strcmp("Jay", "Libby") == 0)) ;
 printf ( "The answer is (%f, %f), but should be (%f, %f)\n",
          translate(new_Point(3, 3, 0), 1, -1)->x,
          translate(new_Point(3, 3, 0), 1, -1)->y,
          new_Point(4,2,0)->x, new_Point(4,2, 0)->y) ;

 Point* p = translate(new_Point(-1, 5, 0), 3, 5);

 printf ( "The answer is (%f, %f), but should be (%f, %f)\n",
          p->x,
          p->y,
          new_Point(2,10,0)->x, new_Point(2,10,0)->y) ;

 printf ( "The answer is %f, but should be %f\n",
          distanceToOrigin(new_Point(3,4,5)),
          7.071) ;
 printf ( "The answer is %f, but should be %f\n",
          distanceToOrigin(new_Point(5,9,2)),
          10.488) ;
 

 return 0;
}
