class Posn {
    public double x;
    public double y;
    
    public Posn ( double x0, double y0 ) {
	x = x0;
	y = y0;
    }

    // translate : Posn double double -> Posn
    public Posn translate ( double dx, double dy ) {
	return new Posn ( this.x + dx, this.y + dy );
    }
}

// A Square is a
//  new Square ( nwCorner, length )
// where
//  nwCorner is a Posn
//  length is a double
class Square implements Shape {
    public Posn nwCorner;
    public double length;

    // Write the "constructor"
    public Square ( Posn nwCorner0, double len0 ) {
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
    public double area ( ) {
	// someSquare.nwCorner someSquare.length
	// this.nwCorner this.length
	return this.length * this.length ;
	// return Math.pow( this.length, 2 );
    }

    // translate : Square double double -> Square
    // Purpose: translate the square by dx, dy
    // Example:
    //  new Square ( new Posn (0,0), 25 ) . translate ( -25, 0 ) = 
    //    new Square ( new Posn (-25,0), 25 )
    public Square translate ( double dx, double dy ) {
	// this.nwCorner this.length dx dy
	return new Square ( this.nwCorner.translate( dx, dy ), this.length ) ;
    }
}

// COMPOUND DATA DEFINITION
// A Circle is...
//  new Circle ( center, radius )
// where
//  center is a Posn
//  radius is a double
class Circle implements Shape {
    public Posn center;
    public double radius;

    public Circle ( Posn center0, double radius0 ) {
	center = center0;
	radius = radius0;
    }

    // area : Circle -> double
    // Purpose: compute the area of the circle
    // Examples:
    //  new Circle ( new Posn ( 0, 0 ), 1 ) . area () = Math.PI
    //  new Circle ( new Posn ( 0, 0 ), 10 ) . area () = Math.PI * 10 * 10
    public double area ( ) {
	// this.center this.radius
	return Math.PI * this.radius * this.radius ;
    }

    // translate : Circle double double -> Circle
    // Purpose: translate the circle by dx, dy
    // Example:
    //  new Circle ( new Posn (0,0), 25 ) . translate ( -25, 0 ) = 
    //    new Circle ( new Posn (-25,0), 25 )
    public Circle translate ( double dx, double dy ) {
	// this.center this.radius dx dy
	return new Circle ( this.center.translate( dx, dy ), this.radius ) ;
    }

}

// MIXED DATA DEFINITION:
// A Shape is either
//   - a Circle
//   - a Square    
interface Shape {
    // area : Shape -> double
    // Purpose: compute the area of the shape
    public double area ( ) ;

    // translate : Shape double double -> Shape [same kind of Shape]
    // Purpose: translate the shape
    public Shape translate ( double dx, double dy ) ;
}

class scratch {
    // area : (Square OR Circle) -> double
    // Purpose: to compute the area of the shape given
    // Examples:
    //  area ( new Circle ( new Posn ( 0, 0 ), 1 ) ) = Math.PI
    //  area ( new Square ( new Posn (0, 0), 10 ) ) = 10*10
    static double area ( Shape fig ) { // remember that this says Square not Circle||Square
	return fig.area ();
	//return 0.0;
    }

    // translateTwice : Shape double double -> Shape
    // Purpose: compute the new shape after translate, twice
    static Shape translateTwice ( Shape theShape, double dx, double dy ) {
	return (theShape.translate(dx,dy)).translate(dx,dy);
    }

    public static void main ( String[] args ) {
	System.out.format( "The answer is %d, but should be %d%n",
			   5*2,
			   10 );
	System.out.format( "The answer is %f,%f, but should be %f,%f%n",
			   new Posn (0, 0).translate( 10, 10 ).x,
			   new Posn (0, 0).translate( 10, 10 ).y,
			   10.0,
			   10.0 );

	Square harold = new Square ( new Posn (0, 0), 25 );
	System.out.format( "The answer is %f, but should be %f%n",
			   harold.length,
			   25.0 );

	Shape larold = new Square ( new Posn (0, 0), 25 );
	// Errors:
	/* System.out.format( "The answer is %f, but should be %f%n",
			   larold.length,
			   25.0 );
	*/
	System.out.format( "The answer is %f, but should be %f%n",
			   larold.area(),
			   25.0 * 25.0);

	System.out.format( "The answer is %f, but should be %f%n",
			   new Square ( new Posn (0, 0), 25 ) . area ( ),
			   25.0*25.0 );
	System.out.format( "The answer is %f, but should be %f%n",
			   new Square ( new Posn (0, 0), 1 ) . area ( ),
			   1.0 );
	System.out.format( "The answer is %f, but should be %f%n",
			   new Square ( new Posn (0, 0), 10 ) . area ( ),
			   100.0 );

	System.out.format( "The answer is %f, but should be %f%n",
			   new Circle ( new Posn (0, 0), 1 ) . area ( ),
			   Math.PI );
	System.out.format( "The answer is %f, but should be %f%n",
			   new Circle ( new Posn (0, 0), 10 ) . area ( ),
			   Math.PI * 10.0 * 10.0 );


	System.out.format( "The answer is %f, but should be %f%n",
			   area ( new Square ( new Posn (0, 0), 10 ) ),
			   100.0 );
	System.out.format( "The answer is %f, but should be %f%n",
			   area ( new Circle ( new Posn (0, 0), 10 ) ),
			   Math.PI * 10.0 * 10.0 );

	Square before = new Square ( new Posn (0,0), 25 );
	Square after = before.translate ( -25, 0 );
	System.out.format( "The answer is %f,%f, but should be %f,%f%n",
			   after.nwCorner.x, after.nwCorner.y,
			   -25.0, 0.0 );

	Square before2 = new Square ( new Posn (10,0), 25 );
	Square after2 = before2.translate ( -25, 0 );
	System.out.format( "The answer is %f,%f, but should be %f,%f%n",
			   after2.nwCorner.x, after2.nwCorner.y,
			   -15.0, 0.0 );

	Circle before3 = new Circle ( new Posn (10,0), 25 );
	Circle after3 = before3.translate ( -25, 0 );
	System.out.format( "The answer is %f,%f, but should be %f,%f%n",
			   after3.center.x, after3.center.y,
			   -15.0, 0.0 );


	Shape before4 = new Circle ( new Posn (10,0), 25 );
	Shape after4 = before4.translate ( -25, 0 );
	System.out.format( "The answer is %f, but should be %f%n",
			   after4.area(),
			   Math.PI * 25.0 * 25.0 );
	/* We can only do Shape-ish things.
	System.out.format( "The answer is %f,%f, but should be %f,%f%n",
			   after4.center.x, after4.center.y,
			   -15.0, 0.0 );
	*/
	
    }
}