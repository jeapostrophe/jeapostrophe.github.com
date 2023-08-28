#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// MIXED DATA DEFINITION:
// A Shape is either
//   - a Circle
//   - a Square
class Shape {
public:
  // area : Shape -> double
  // Purpose: compute the area of the shape
  virtual double area ( ) = 0 ;

  // translate : Shape double double -> Shape [same kind of Shape]
  // Purpose: translate the shape
  virtual Shape* translate ( double dx, double dy ) = 0 ;
};

class Posn {
public:
  double x;
  double y;
    
  Posn ( double x0, double y0 ) {
	x = x0;
	y = y0;
  }

  // translate : Posn double double -> Posn
  Posn* translate ( double dx, double dy ) {
    return new Posn ( this->x + dx, this->y + dy );
  }
};

// A Square is a
//  new Square ( nwCorner, length )
// where
//  nwCorner is a Posn
//  length is a double
class Square : public Shape {
public:
  Posn* nwCorner;
  double length;

  // Write the "constructor"
  Square ( Posn* nwCorner0, double len0 ) {
	nwCorner = nwCorner0;
	length = len0;
  }

  // area : Square -> double
  // Purpose: to compute the area of the square
  // Function Examples:
  //  area ( new Square ( new Posn (0, 0), 25 ) ) = 25*25
  // Method Examples:
  //  implicit . methodName ( other, arg, u, ments ) = ans
  //  new Square ( new Posn (0, 0), 25 ) . area ( ) = 25*25
  //  new Square ( new Posn (0, 0), 1 ) . area ( ) = 1*1
  //  new Square ( new Posn (0, 0), 10 ) . area ( ) = 10*10
  double area ( ) {
	// someSquare->nwCorner someSquare->length
	// this->nwCorner this->length
	return this->length * this->length ;
	// return pow( this->length, 2 );
  }

  // translate : Square double double -> Square
  // Purpose: translate the square by dx, dy
  // Example:
  //  new Square ( new Posn (0,0), 25 ) -> translate ( -25, 0 ) = 
  //    new Square ( new Posn (-25,0), 25 )
  Square* translate ( double dx, double dy ) {
	// this.nwCorner this.length dx dy
	return new Square ( this->nwCorner->translate( dx, dy ), this->length ) ;
  }
};

// COMPOUND DATA DEFINITION
// A Circle is...
//  new Circle ( center, radius )
// where
//  center is a Posn
//  radius is a double
class Circle : public Shape {
public:
  Posn* center;
  double radius;

  Circle ( Posn* center0, double radius0 ) {
	center = center0;
	radius = radius0;
  }

  // area : Circle -> double
  // Purpose: compute the area of the circle
  // Examples:
  //  new Circle ( new Posn ( 0, 0 ), 1 ) -> area () = M_PI
  //  new Circle ( new Posn ( 0, 0 ), 10 ) -> area () = M_PI * 10 * 10
  double area ( ) {
	// this.center this.radius
	return M_PI * this->radius * this->radius ;
  }

  // translate : Circle double double -> Circle
  // Purpose: translate the circle by dx, dy
  // Example:
  //  new Circle ( new Posn (0,0), 25 ) . translate ( -25, 0 ) = 
  //    new Circle ( new Posn (-25,0), 25 )
  Circle* translate ( double dx, double dy ) {
	// this->center this->radius dx dy
	return new Circle ( this->center->translate( dx, dy ), this->radius ) ;
  }

};

// area : (Square OR Circle) -> double
// Purpose: to compute the area of the shape given
// Examples:
//  area ( new Circle ( new Posn ( 0, 0 ), 1 ) ) = M_PI
//  area ( new Square ( new Posn (0, 0), 10 ) ) = 10*10
double area ( Shape* fig ) { // remember that this says Square not Circle||Square
  return fig->area ();
  //return 0.0;
}

// translateTwice : Shape double double -> Shape
// Purpose: compute the new shape after translate, twice
Shape* translateTwice ( Shape* theShape, double dx, double dy ) {
  return (theShape->translate(dx,dy))->translate(dx,dy);
}

int main ( ) {
  printf( "The answer is %d, but should be %d\n",
          5*2,
          10 );
  printf( "The answer is %f,%f, but should be %f,%f\n",
          ((new Posn (0, 0))->translate( 10, 10 ))->x,
          ((new Posn (0, 0))->translate( 10, 10 ))->y,
          10.0,
          10.0 );

  Square* harold = new Square ( new Posn (0, 0), 25 );
  printf( "The answer is %f, but should be %f\n",
          harold->length,
          25.0 );

  Shape* larold = new Square ( new Posn (0, 0), 25 );
  // Errors:
  /* printf( "The answer is %f, but should be %f\n",
     larold->length,
     25.0 );
  */
  printf( "The answer is %f, but should be %f\n",
          larold->area(),
          25.0 * 25.0);

  printf( "The answer is %f, but should be %f\n",
          (new Square ( new Posn (0, 0), 25 )) -> area ( ),
          25.0*25.0 );
  printf( "The answer is %f, but should be %f\n",
          (new Square ( new Posn (0, 0), 1 )) -> area ( ),
          1.0 );
  printf( "The answer is %f, but should be %f\n",
          (new Square ( new Posn (0, 0), 10 )) -> area ( ),
          100.0 );

  printf( "The answer is %f, but should be %f\n",
          (new Circle ( new Posn (0, 0), 1 )) -> area ( ),
          M_PI );
  printf( "The answer is %f, but should be %f\n",
          (new Circle ( new Posn (0, 0), 10 )) -> area ( ),
          M_PI * 10.0 * 10.0 );

  printf( "The answer is %f, but should be %f\n",
          area ( new Square ( new Posn (0, 0), 10 ) ),
          100.0 );
  printf( "The answer is %f, but should be %f\n",
          area ( new Circle ( new Posn (0, 0), 10 ) ),
          M_PI * 10.0 * 10.0 );

  Square* before = new Square ( new Posn (0,0), 25 );
  Square* after = before->translate ( -25, 0 );
  printf( "The answer is %f,%f, but should be %f,%f\n",
          after->nwCorner->x, after->nwCorner->y,
          -25.0, 0.0 );

  Square* before2 = new Square ( new Posn (10,0), 25 );
  Square* after2 = before2->translate ( -25, 0 );
  printf( "The answer is %f,%f, but should be %f,%f\n",
          after2->nwCorner->x, after2->nwCorner->y,
          -15.0, 0.0 );

  Circle* before3 = new Circle ( new Posn (10,0), 25 );
  Circle* after3 = before3->translate ( -25, 0 );
  printf( "The answer is %f,%f, but should be %f,%f\n",
          after3->center->x, after3->center->y,
          -15.0, 0.0 );

  Shape* before4 = new Circle ( new Posn (10,0), 25 );
  Shape* after4 = before4->translate ( -25, 0 );
  printf( "The answer is %f, but should be %f\n",
          after4->area(),
          M_PI * 25.0 * 25.0 );
  /* We can only do Shape-ish things.
     printf( "The answer is %f,%f, but should be %f,%f\n",
     after4->center->x, after4->center->y,
     -15.0, 0.0 );
  */
	
}
