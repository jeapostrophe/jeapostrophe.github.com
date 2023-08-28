// A Posn is a 
//   new Posn ( x, y )
// where
//  - x is a double
//  - y is a double
class Posn {
    public double x;
    public double y;

    public Posn ( double x0, double y0 ) {
	x = x0;
	y = y0;
    }
}

interface Shape {
    public double perimeter ();
}

class Square implements Shape {
    public Posn nw;
    public double len;

    public Square ( Posn nw0, double len0 ) {
	nw = nw0;
	len = len0;
    }

    public double perimeter () {
	return 4 * this.len;
    }
}

class Circle implements Shape {
    public Posn center;
    public double radius;

    public Circle ( Posn center0, double radius0 ) {
	center = center0;
	radius = radius0;
    }

    public double perimeter () {
	return Math.PI * 2 * this.radius;
    }
}

class exp {
    static double perimeter ( Shape s0 ) {
	return s0.perimeter();
    }

    public static void main (String[] args) {	
	Posn p0 = new Posn ( 2.54, 7.1 ) ;
	Square s0 = new Square ( p0, 90 ) ;
	Circle c0 = new Circle ( p0, 90 ) ;

	System.out.format("The answer is: (%f,%f)%n", p0.x, p0.y);
	System.out.format("The answer is: %f%n", perimeter(s0));
	System.out.format("The answer is: %f%n", perimeter(c0));
    }
}