// A Ball is a
//   new Ball ( pos, xvelocity )
// where
//   - pos is a Posn
//   - xvelocity is double [measured in Planck length / Planck time]
class Ball {
    public Posn pos;
    public double xvelocity;

    public Ball ( Posn pos0, double xvelocity0 ) {
	pos = pos0;
	xvelocity = xvelocity0;
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

// A PhoneNumber is a...
//   new PhoneNumber ( areaCode, prefix, line )
// where
//   - areaCode is an int
//   - prefix is an int
//   - line is an int
class PhoneNumber {
    public int areaCode;
    public int prefix;
    public int line;

    public PhoneNumber ( int areaCode0, int prefix0, int line0 ) {
	areaCode = areaCode0;
	prefix = prefix0;
	line = line0;
    }
}

// An AddressBookEntry is a...
//   new AddressBookEntry ( home, office, cell )
// where
//   - home is a PhoneNumber
//   - office is a PhoneNumber
//   - cell is a PhoneNumber
class AddressBookEntry {
    /*
    public int homeAreaCode;
    public int homePrefix;
    public int homeLine;
    */
    public PhoneNumber home;
    public PhoneNumber office;
    public PhoneNumber cell;

    public AddressBookEntry ( PhoneNumber home0, PhoneNumber office0, PhoneNumber cell0 ) {
	home = home0;
	office = office0;
	cell = cell0;
    }
}

/*
Write a data definition for three digit numbers, where there are three integers.
*/

// A TDN is a...
//   new TDN ( first, second, third )
// where
//   - first is a integer
//   - second is a integer
//   - third is an integer
class TDN {
    public int first;
    public int second;
    public int third;

    public TDN ( int first0, int second0, int third0 ) {
	first = first0;
	second = second0;
	third = third0;
    }
}

class scratch {
    // allInAreaHuh : AddressBookEntry int -> boolean
    // Purpose: determine if every phone number is in the same specific area
    // Example:
    //   allInAreaHuh ( new AddressBookEntry ( new PhoneNumber ( 978, 555, 1723 ),
    //                                         new PhoneNumber ( 978, 555, 9211 ),
    //                                         new PhoneNumber ( 978, 565, 9211 ) ),
    //                  978 )
    //   = true
    static boolean allInAreaHuh ( AddressBookEntry ae, int someArea ) {
	/*
	return ... ae.home ... ae.office ... ae.cell ... someArea ;
	return ... inAreaHuh( ... ae.home ...) ... inAreaHuh( ... ae.office ...) ... inAreaHuh( ... ae.cell ...) ... someArea ;
	return ... inAreaHuh( ae.home, someArea ) ... inAreaHuh( ae.office, someArea ) ... inAreaHuh( ae.cell, someArea ) ... someArea ;
	*/
	return inAreaHuh( ae.home, someArea ) 
	    && inAreaHuh( ae.office, someArea ) 
	    && inAreaHuh( ae.cell, someArea );	
    }

    // inAreaHuh : PhoneNumber int -> boolean
    // Examples:
    // inAreaHuh ( new PhoneNumber ( 978, 555, 1723 ), 978 ) = true
    // inAreaHuh ( new PhoneNumber ( 978, 555, 1723 ), 801 ) = false
    static boolean inAreaHuh ( PhoneNumber pn, int someArea ) {
	// return ... pn.areaCode ... pn.prefix ... pn.line ... someArea ;
	return pn.areaCode == someArea ;
    }


    // translate : Posn double double -> Posn
    // Computes the new position of the guy after moving dx in the x direction and dy in the y dir.
    // Examples:
    //  translate( new Posn( 2, 2 ), 1, -1 ) = new Posn (3, 1)
    static Posn translate ( Posn p, double dx, double dy ) {
	// return .... ;
	// Take stock
	// return ... p ... dx ... dy ; // <- We know this from Posn p
	// return new Posn ( ... p.x ... p.y ... dx ... dy ); // <- We know this from Posn translate
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

    /*
      Write a function called reveal that takes three arguments: a 'chosen' three digit number, a 'current' three digit number, and 'guess' integer. The function produces a new 'current' number where the digits are the same as before, unless the 'guess' is the same as one of the digits, in which case it is "revealed". For example,

      reveal( new TDN(1, 2, 3), new TDN(0, 0, 0), 1) = new TDN( 1, 0, 0)
      reveal( new TDN(1, 2, 3), new TDN(1, 0, 0), 3) = new TDN( 1, 0, 3)
      reveal( new TDN(1, 2, 3), new TDN(1, 0, 3), 6) = new TDN( 1, 0, 3)
      reveal( new TDN(1, 2, 3), new TDN(1, 0, 3), 2) = new TDN( 1, 2, 3)
      reveal( new TDN(1, 2, 2), new TDN(1, 0, 3), 2) = new TDN( 1, 2, 2)
    */
    // Contract: reveal : TDN TDN int -> TDN
    // Purpose: see above
    static TDN reveal ( TDN chosen, TDN current, int guess ) {
	return revealThird( chosen,
			    revealSecond ( chosen,
					   revealFirst ( chosen, current, guess ),
					   guess ),
			    guess );
    }

    // Contract: revealFirst : TDN TDN int -> TDN
    // Purpose: like reveal, but only reveals the first digit
    // Examples:
    // revealFirst( new TDN(1, 2, 3), new TDN(0, 0, 0), 1) = new TDN( 1, 0, 0)
    // revealFirst( new TDN(1, 2, 3), new TDN(1, 0, 0), 3) = new TDN( 1, 0, 0)
    static TDN revealFirst ( TDN chosen, TDN current, int guess ) {
	// return ... ;
	// return ... chosen.first ... chosen.second ... chosen.third ... current.first ... current.second ... current.third .. guess ;
	/*
	return new TDN ( ... chosen.first ... chosen.second ... chosen.third ... current.first ... current.second ... current.third .. guess,
			 ... chosen.first ... chosen.second ... chosen.third ... current.first ... current.second ... current.third .. guess,
			 ... chosen.first ... chosen.second ... chosen.third ... current.first ... current.second ... current.third .. guess ) ;
	*/
	if ( chosen.first == guess ) {
	    return new TDN ( chosen.first,
			     current.second,
			     current.third) ;
	} else {
	    return current;
	    /*
	      return new TDN ( current.first,
	                       current.second,
			       current.third) ;
	    */
	}
    }

    static TDN revealSecond ( TDN chosen, TDN current, int guess ) {
	if ( chosen.second == guess ) {
	    return new TDN ( current.first,
			     chosen.second,
			     current.third) ;
	} else {
	    return current;
	}
    }

    static TDN revealThird ( TDN chosen, TDN current, int guess ) {
	if ( chosen.third == guess ) {
	    return new TDN ( current.first,
			     current.second,
			     chosen.third) ;
	} else {
	    return current;
	}
    }

    // physics : Ball -> Ball
    // Purpose: Computes the new ball after one step of time (Planck time units)
    // Example:
    //  physics ( new Ball ( new Posn ( 0, 0 ), 2.3 ) ) =
    //            new Ball ( new Posn ( 2.3, 0 ), 2.3 )
    static Ball physics ( Ball aBall ) {
	// return ... physicsForPosns( ... aBall.pos ... ) ... aBall.xvelocity ... ;
	/*
	return new Ball ( ... physicsForPosns( ... aBall.pos ... ) ... aBall.xvelocity ...,
			  ... physicsForPosns( ... aBall.pos ... ) ... aBall.xvelocity ... );
	*/
	return new Ball ( physicsForPosns( aBall.pos, aBall.xvelocity ),
			  aBall.xvelocity );

    }
    // physicsForPosns : Posn double -> Posn
    // Purpose: Computes a new Posn after an x movment of some units
    // Examples:
    //  physicsForPosns ( new Posn ( 0, 0 ), 2.3 ) = new Posn ( 2.3 , 0 )
    //  physicsForPosns ( new Posn ( 1, 0 ), 2.3 ) = new Posn ( 3.3 , 0 )
    static Posn physicsForPosns ( Posn p, double xmovement ) {
	/*
	  return ... p.x ... p.y ... xmovement ;
	  return new Posn ( ... p.x ... p.y ... xmovement,
	 		    ... p.x ... p.y ... xmovement );
	*/
	return new Posn ( p.x + xmovement,
			  p.y );
    }

    public static void main (String[] args) {	
	Posn pMario = new Posn ( 2.0, 2.0 ) ;

	System.out.format("The answer is: (%f,%f)%n", pMario.x, pMario.y);

	Posn pMarioKillinAGoomba = new Posn (3, 1);
	Posn pMarioAfterMovin = translate( pMario, 1, -1 );
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

	double fixedCost = 180;
	TDN harry = new TDN(1, 2, 3);
	TDN hermione = new TDN(0, 0, 0);
	TDN snape = new TDN( 1, 0, 0);

	System.out.format("Harry is %d%d%d%n",
			  harry.first,
			  harry.second,
			  harry.third);

	TDN ex1after = revealFirst( new TDN(1, 2, 3), new TDN(0, 0, 0), 1);
	System.out.format("Ex1 after is %d%d%d but should be 100%n",
			  ex1after.first, ex1after.second, ex1after.third);
	TDN ex2after = revealFirst( new TDN(1, 2, 3), new TDN(1, 0, 0), 3);
	System.out.format("Ex2 after is %d%d%d but should be 100%n",
			  ex2after.first, ex2after.second, ex2after.third);


	TDN ex3after = reveal( new TDN(1, 1, 1), new TDN(0, 0, 0), 1);
	System.out.format("Ex3 after is %d%d%d but should be 111%n",
			  ex3after.first, ex3after.second, ex3after.third);

	Ball steve = new Ball ( new Posn ( 0, 0 ), 2.3 );
	System.out.format("Steve is at (%f,%f) and moving in the x-direction at %fu/t%n",
			  (steve.pos).x, (steve.pos).y, steve.xvelocity);
	System.out.format("Steve is at (%f,%f) and moving in the x-direction at %fu/t%n",
			  steve.pos.x, steve.pos.y, steve.xvelocity);
	Posn stevesposn = steve.pos;
	System.out.format("Steve is at (%f,%f) and moving in the x-direction at %fu/t%n",
			  stevesposn.x, stevesposn.y, steve.xvelocity);

	Ball steveAfterPhysics = physics(steve);
	System.out.format("Steve after physics is at (%f,%f) and moving in the x-direction at %fu/t%n",
			  (steveAfterPhysics.pos).x, (steveAfterPhysics.pos).y, steveAfterPhysics.xvelocity);
	System.out.format("But he should be at (2.3,0) and moving at 2.3u/t%n");

	System.out.format("The answer is %b, but should be %b%n",
			  allInAreaHuh ( new AddressBookEntry ( new PhoneNumber ( 978, 555, 1723 ),
								new PhoneNumber ( 978, 555, 9211 ),
								new PhoneNumber ( 978, 565, 9211 ) ),
					 978 ),
			  true );
	System.out.format("The answer is %b, but should be %b%n",
			  allInAreaHuh ( new AddressBookEntry ( new PhoneNumber ( 978, 555, 1723 ),
								new PhoneNumber ( 978, 555, 9211 ),
								new PhoneNumber ( 978, 565, 9211 ) ),
					 801 ),
			  false );
	System.out.format("The answer is %b, but should be %b%n",
			  allInAreaHuh ( new AddressBookEntry ( new PhoneNumber ( 978, 555, 1723 ),
								new PhoneNumber ( 647, 555, 9211 ),
								new PhoneNumber ( 978, 565, 9211 ) ),
					 978 ),
			  false );

    }
}