// A Person is a
//   new Person ( firstName, lastName, cuteness, eyeColor, height, weight, isAlive )
// where
//  - firstName is a String
//  - lastName is a String
//  - cuteness is a double [0,100]
//  - eyeColor is a String
//  - height is a double (inchers)
//  - weight is a double (pounders)
//  - isAlive is a boolean
class Person {
    public String firstName;
    public String lastName;
    public double cuteness;
    public String eyeColor;
    public double height;
    public double weight;
    public boolean isAlive;

    public Person ( String firstName0, String lastName0, double cuteness0, String eyeColor0,
		    double height0, double weight0, boolean isAlive0 ) {
	firstName = firstName0;
	lastName = lastName0;
	cuteness = cuteness0;
	eyeColor = eyeColor0;
	height = height0;
	weight = weight0;
	isAlive = isAlive0;
    }
}

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

class scratch {

    // moveTheGuy : Posn double double -> Posn
    // Computes the new position of the guy after moving dx in the x direction and dy in the y dir.
    // Examples:
    //  moveTheGuy( new Posn( 2, 2 ), 1, -1 ) = new Posn (3, 1)
    static Posn moveTheGuy ( Posn p, double dx, double dy ) {
	// return .... ;
	// Take stock
	// return ... p ... dx ... dy ; // <- We know this from Posn p
	// return new Posn ( ... p.x ... p.y ... dx ... dy ); // <- We know this from Posn moveTheGuy
	// return new Posn ( ... p.x ... p.y ... dx ... dy, ... p.x ... p.y ... dx ... dy );	
	return new Posn ( p.x + dx, p.y + dy );	
    }

    // distanceToOrigin : Posn -> double
    // Compute the distance of the posn from the origin (0,0)
    // Examples:
    //  distanceToOrigin ( new Posn ( 0, 3 ) ) = 3
    //  distanceToOrigin ( new Posn ( 2, 0 ) ) = 2
    //  distanceToOrigin ( new Posn ( 3, 4 ) ) = 5
    static double distanceToOrigin ( Posn p ) {
	// return ... p.x ... p.y ... ;
	return Math.sqrt(Math.pow(p.x,2) + Math.pow(p.y,2)) ;
    }

    // willMarry : Person -> boolean
    // Computes whether the person should be married (to you!)
    // Examples:
    //  willMarry(jay) = true;
    //  willMarry(tom) = false;
    static boolean willMarry ( Person candidate ) {
	// Take stock!
	/*
	return ... candidate.firstName ... candidate.lastName ...
	    candidate.cuteness ... candidate.weight ... candidate.height ...
	    candidate.eyeColor ... candidate.isAlive ...;
	*/
	return (candidate.cuteness >= 80.35) ;
    }

    // Java doesn't let us return two things.
    // moveTheGuy : double double double double -> double double
    // Computes the new position of the guy after moving dx in the x direction and dy in the y dir.
    // Examples:
    //  moveTheGuy( 2, 2, 1, -1 ) = 3 1
    /*
    static double double moveTheGuy ( double x, double y, double dx, double dy ) {
	// Return twice?
	return x + dx;
	return y + dy;
	
	//return (x + dx) (y + dy) ;
	// return ... x ... y ... dx ... dy ;
    }
    */

    // Atomic data means contains one thing: numbers, booleans, strings

    // Compound data means contains a few things: a number AND a number, a string AND a number

    public static void main (String[] args) {	
	Posn p0 = new Posn ( 2.54, 7.1 ) ;
	Posn p1 = new Posn ( 2.0, Math.sqrt(2) ) ;
	Posn pMario = new Posn ( 2.0, 2.0 ) ;

	System.out.format("The answer is: %s%n", "Amaaaaaazing");
	// System.out.format("The answer is: %Posn%n", p0);
	System.out.format("The answer is: (%f,%f)%n", p0.x, p0.y);
	System.out.format("The answer is: (%f,%f)%n", p1.x, p1.y);
	System.out.format("The answer is: (%f,%f)%n", pMario.x, pMario.y);
	
	System.out.format("The answer is: (%f,%f) but should be (%f,%f)%n", 
			  moveTheGuy( new Posn( 2, 2 ), 1, -1 ).x,
			  moveTheGuy( new Posn( 2, 2 ), 1, -1 ).y,
			  new Posn (3, 1).x,
			  new Posn (3, 1).y);

	System.out.format("The answer is: (%f,%f) but should be (%f,%f)%n", 
			  moveTheGuy( pMario, 1, -1 ).x,
			  moveTheGuy( pMario, 1, -1 ).y,
			  new Posn (3, 1).x,
			  new Posn (3, 1).y);

	Posn pMarioKillinAGoomba = new Posn (3, 1);
	System.out.format("The answer is: (%f,%f) but should be (%f,%f)%n", 
			  moveTheGuy( pMario, 1, -1 ).x,
			  moveTheGuy( pMario, 1, -1 ).y,
			  pMarioKillinAGoomba.x,
			  pMarioKillinAGoomba.y);

	Posn pMarioAfterMovin = moveTheGuy( pMario, 1, -1 );
	System.out.format("The answer is: (%f,%f) but should be (%f,%f)%n", 
			  pMarioAfterMovin.x,
			  pMarioAfterMovin.y,
			  pMarioKillinAGoomba.x,
			  pMarioKillinAGoomba.y);

	System.out.format("The answer is: (%f,%f)%n", pMario.x, pMario.y);


	System.out.format("The answer is: %f but should be %f%n", 
			  distanceToOrigin ( new Posn ( 0, 3 ) ),
			  3.0);
	System.out.format("The answer is: %f but should be %f%n", 
			  distanceToOrigin ( new Posn ( 2, 0 ) ),
			  2.0);
	System.out.format("The answer is: %f but should be %f%n", 
			  distanceToOrigin ( new Posn ( 3, 4 ) ),
			  5.0);

	Person jay = new Person ( "Jay", "McCarthy", 100.00, "Bleu", 71.0, 140.0, true );
	Person tom = new Person ( "Tom", "Thumb", 75.0, "Brown", 3.0, 1.0, false );
	// Uncomment and see what these doooze!
	// System.out.format("The answer is: %f%n", jay.cutness);
	// System.out.format("The answer is: %f%n", moveTheGuy(jay, 4.0, 5.0));
	System.out.format("The answer is: %f%n", jay.cuteness);
	System.out.format("The answer is: %f%n", tom.cuteness);

	System.out.format("The answer is: %b but should be %b%n",
			  willMarry(jay), true);
	System.out.format("The answer is: %b but should be %b%n",
			  willMarry(tom), false);

	/*
	System.out.format("The answer is %d%n",
			  if ( 4 > 3 ) {
			      return 4;
			  } else { 
			      return 5;
			  });
	*/
	System.out.format("The answer is %d%n",
			  (4 > 3) ? 4 : 5);
	System.out.format("The answer is %d%n",
			  ((4 > 3) ? 4 : 5)+90);
	// sign(x) == 0

    }
}