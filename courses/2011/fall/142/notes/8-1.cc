#include <stdio.h>
#include <math.h>
#include <string.h>

// booleanToString : boolean -> string
const char* booleanToString ( bool it ) {
  if ( it ) { return "true"; } else { return "false"; }
}

// A Point is a 
// new Point (x, y)
// where 
// - x is a double
// - y is a double
class Point {
public:
  double x;
  double y;

  // Contract: new Point : double double -> Point*
  Point(double x0, double y0) {
    this->x = x0;
    this->y = y0;
  }

  // Contract: translate : Point* number number -> Point*
  // Purpose: move the x, y coordinate by dx, dy
  Point* translate(double dx, double dy) {
    // Template: this, this->x, this->y, dx, dy
  
    // Example 1:
    // this->x = 3
    // this->y = 3
    // dx = 1
    // dy = -1
    // ans = new Point(4,2)
    // return new Point(4, 2);
  
    // Example 2:
    // this->x = -1
    // this->y = 5
    // dx = 3
    // dy = 5
    // ans = new Point(2, 10)
    // return new Point(2, 10);
  
    // Generalize 1 & 2
    return new Point(this->x + dx, this->y + dy);
  }

  // Contract: distanceToOrigin: Point* -> double
  // Purpose: calculate the distance from the point to the origin
  double distanceToOrigin() {
    // Template: this, this->x, this->y
    // Example 1:
    // this = new Point(3, 4)
    // ans = sqrt(3 * 3 + 4 * 4)
  
    // Example 2:
    // this = new Point(5, 9, 2)
    // ans = sqrt(5 * 5 + 9 * 9)
  
    // Generalize 1 & 2
    return sqrt(this->x * this->x + this->y * this->y);
  }
  
  // distanceBetweenPoints : Point Point -> double
  double distanceBetweenPoints ( Point* t ) {
    return sqrt( pow((this->x - t->x), 2) + pow((this->y - t->y), 2) );
  }

  // show : Point -> int
  int show ( ) {
    // Template: this, this->x, this->y
    return printf("(%f,%f)", this->x, this->y);
  }
};

// A Shape is a...
//  Circle
//  or, Square
class Shape {
public:
  // area : Shape -> double
  // Purpose: return the area of the given shape
  // virtual is Cplusplusanese for "wish"
  // "= 0" is Cplusplusanese for "unfulfilled"
  virtual double area ( ) = 0 ;
};
// C++ calls that thing a "pure virtual class"

// A MixedData is a...
//  Variant One
//  or, Variant Two
//  or, Variant Three

// C++ says Circle is a sub-type, or sub-class, or child, of Shape
//          or that Circle "inherits" from Shape
//          or Shape is the parent of Circle

// A Circle is a..
//  new Circle ( center, radius )
// where
//  center is a Point
//  radius is a double
class Circle : public Shape {
public:
  Point* center;
  double radius;

  Circle( Point* center0, double radius0 ) {
    this->center = center0;
    this->radius = radius0;
  }

  // area : Circle -> double
  // Purpose: return the area of the given shape
  double area ( ) {
    // Template: this, this->center, this->radius

    return M_PI * this->radius * this->radius ;
  }

};

// A Square is a...
//  new Square( top_left, side_len )
// where
//  top_left is a Point
//  side_len is a double
class Square : public Shape {
public:
  Point* top_left;
  double side_len;

  Square( Point* top_left0, double side_len0 ) {
    this->top_left = top_left0;
    this->side_len = side_len0;
  }

  // area : Square -> double
  // Purpose : area-ize that Square, bro
  double area () {
    // Template: this, this->top_left, this->side_len

    return this->side_len * this->side_len;
  }
};

// Compound data is when you have an X AND a Y
// Mixed data is when you have an X OR a Y

// area : Shape -> double
// Purpose: to return the area of the given shape
double area ( Shape* s ) {
  // Template: s

  return s->area();
}

// doubleArea : Shape -> double
// Purpose: to return the area of the given shape
double doubleArea ( Shape* s ) {
  // Template: s

  return 2.0 * s->area();
}

// main : -> number
int main () {
  printf ( "The answer is %f, but should be %f\n",
           1.0/2.0,
           0.5 ) ;
  printf ( "C++ says %s\n",
           booleanToString(strcmp("Jay", "Libby") == 0)) ;
    
  printf ( "The answer is (%f, %f), but should be (%f, %f)\n",
           ((new Point(3, 3))->translate(1, -1))->x,
           (new Point(3, 3))->translate( 1, -1)->y,
           (new Point(4,2))->x, (new Point(4,2))->y) ;
  
  Point* p = (new Point(-1, 5))->translate(3, 5);
  
  printf ( "The answer is (%f, %f), but should be (%f, %f)\n",
           p->x,
           p->y,
           (new Point(2,10))->x, (new Point(2,10))->y) ;
  
  printf ( "The answer is %f, but should be %f\n",
           (new Point(3,4))->distanceToOrigin(),
           5.0) ;
  printf ( "The answer is %f, but should be %f\n",
           (new Point(5,9))->distanceToOrigin(),
           10.29) ;
  
  printf("The point is........");
  p->show();
  printf("\n");

  printf("The point is ");
  p->translate(0,10)->show();
  printf(" but should be ");
  (new Point( 2, 20 ))->show();
  printf("\n");

  Circle* gopher = new Circle( new Point( 0, 0 ), 25 );
  Circle* hawk = new Circle( new Point( 0, 40 ), 40 );

  Square* grandpa = new Square( new Point( 77, 99 ), 12 );
  Square* spongebob = new Square( new Point( -300, -5 ), 0.75 );

  printf ( "The answer is %f, but should be %f\n",
           area(gopher),
           M_PI * 25 * 25 ) ;
  printf ( "The answer is %f, but should be %f\n",
           area(grandpa),
           12.0 * 12.0) ;

  printf ( "The answer is %f, but should be %f\n",
           gopher->area(),
           M_PI * 25 * 25 ) ;
  printf ( "The answer is %f, but should be %f\n",
           grandpa->area(),
           12.0 * 12.0) ;


  Shape* gopherTheShape = new Circle( new Point( 0, 0 ), 25 );
  Shape* grandpaTheShape = new Square( new Point( 77, 99 ), 12 );

  printf ( "The answer is %f, but should be %f\n",
           gopherTheShape->area(),
           M_PI * 25 * 25 ) ;
  printf ( "The answer is %f, but should be %f\n",
           grandpaTheShape->area(),
           12.0 * 12.0) ;

  printf ( "The answer is %f\n",
           gopher->radius );
  printf ( "The answer is %f\n",
           grandpa->side_len );
  //printf ( "The answer is %f\n",
  //         gopherTheShape->radius );

  return 0;
}

// XXX circle
// XXX square
// XXX area
