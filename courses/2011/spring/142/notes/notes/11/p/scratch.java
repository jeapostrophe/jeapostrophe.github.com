// A ListOfNumbers is either
//  - EmptyListOfNumbers
//  - OneMoreNumber
interface ListOfNumbers {
    // wages : ListOfNumbers -> ListOfNumbers
    public ListOfNumbers wages ( );
    // eliminateOverworkers : ListOfNumbers -> ListOfNumbers
    public ListOfNumbers eliminateOverworkers () ;
    // capOverworkers : ListOfNumbers double -> ListOfNumbers
    public ListOfNumbers capOverworkers ( double cap ) ;
    // update : ListOfNumbers int double -> ListOfNumbers
    public ListOfNumbers update ( int n, double newOne );
}

// A EmptyListOfNumbers is a..
//  new EmptyListOfNumbers ( )
// where
class EmptyListOfNumbers implements ListOfNumbers {
    
    public EmptyListOfNumbers ( ) {
    }

    // wages : EmptyListOfNumbers -> ListOfNumbers
    public ListOfNumbers wages ( ) {
	// ... ...
	return new EmptyListOfNumbers() ;
    }

    // toString : EmptyListOfNumbers -> String
    public String toString () {
	return "!";
    }

    // eliminateOverworkers : EmptyListOfNumbers -> ListOfNumbers
    public ListOfNumbers eliminateOverworkers () {
	// ... ...
	return new EmptyListOfNumbers() ;
    }

    // capOverworkers : EmptyListOfNumbers double -> ListOfNumbers
    public ListOfNumbers capOverworkers ( double cap ) {
	return new EmptyListOfNumbers() ;
    }

    // update : EmptyListOfNumbers int double -> ListOfNumbers
    public ListOfNumbers update ( int n, double newOne ) {
	return new EmptyListOfNumbers() ;
    }
}

// A OneMoreNumber is a...
//  new OneMoreNumber ( first, rest )
// where
//  first is a double
//  rest is a ListOfNumbers
class OneMoreNumber implements ListOfNumbers {
    public double first;
    public ListOfNumbers rest;

    public OneMoreNumber ( double first0, ListOfNumbers rest0 ) {
	first = first0;
	rest = rest0;
    }

    // wages : OneMoreNumber -> ListOfNumbers
    public ListOfNumbers wages ( ) {
	// this.first ... this.rest
	// this.first ... (this.rest).wages()

	/*
	  Input: new OneMoreNumber( 10.0, new EmptyListOfNumbers() )
	  Output: new OneMoreNumber( 10.0*12.0, new EmptyListOfNumbers() )

	  Input: new OneMoreNumber( this.first, new EmptyListOfNumbers() )
	  Output: new OneMoreNumber( this.first*12.0, new EmptyListOfNumbers() )

	  Input: new OneMoreNumber( this.first, this.rest )
	  Output: new OneMoreNumber( this.first*12.0, new EmptyListOfNumbers() )

	  Input: new OneMoreNumber( this.first, this.rest )
	  Output: new OneMoreNumber( this.first*12.0, this.rest )
	*/
	// return new OneMoreNumber( this.first*12.0, this.rest ) ;

	/* 
	   Input: new OneMoreNumber( 15.0, new OneMoreNumber( 10.0, new EmptyListOfNumbers() ) )
	   Output: new OneMoreNumber( 15.0*12.0, new OneMoreNumber( 10.0*12.0, new EmptyListOfNumbers() ) )

	   Input: new OneMoreNumber( this.first, this.rest )
	   Output: new OneMoreNumber( this.first*12.0, this.rest.wages() )
	*/
	return new OneMoreNumber( this.first*12.0, this.rest.wages() );
    }

    // toString : OneMoreNumber -> String
    public String toString () {
	// this.first ... this.rest
	return String.format("%f:%s", this.first, this.rest );
	//return String.format("%f:%s", this.first, this.rest.toString() );
    }

    // eliminateOverworkers : OneMoreNumber -> ListOfNumbers
    public ListOfNumbers eliminateOverworkers () {
	// this.first ... this.rest
	// this.first ... (this.rest).eliminateOverworkers()

	if ( this.first > 100.0 ) {
	/*
	  Input: new OneMoreNumber( 400.0, new EmptyListOfNumbers() )
	  Output: new EmptyListOfNumbers()

	  Input: new OneMoreNumber( this.first, this.rest )
	  Output: this.rest
	*/
	    //return this.rest ;

	    /*
	      Input: new OneMoreNumber( 800.0, new OneMoreNumber( 40.0, new OneMoreNumber( 400.0, new EmptyListOfNumbers() ) ))
	      Output: new OneMoreNumber( 40.0, new EmptyListOfNumbers() )

	      Input: new OneMoreNumber( this.first, this.rest )
	      Output: this.rest.eliminateOverworkers()
	    */
	    return this.rest.eliminateOverworkers(); 
	} else {
	/*
	  Input: new OneMoreNumber( 40.0, new EmptyListOfNumbers() )
	  Output: new OneMoreNumber( 40.0, new EmptyListOfNumbers() )
	  
	  Input: new OneMoreNumber( this.first, this.rest )
	  Output: new OneMoreNumber( this.first, this.rest )
	*/
	    // return new OneMoreNumber( this.first, this.rest );

	    /*
	      Input: new OneMoreNumber( 40.0, new OneMoreNumber( 400.0, new EmptyListOfNumbers() ) ),
	      Output: new OneMoreNumber( 40.0, new EmptyListOfNumbers() )
	      
	      Input: new OneMoreNumber( this.first, this.rest ),
	      Output: new OneMoreNumber( this.first, this.rest.eliminateOverworkers() )
	    */
	    return new OneMoreNumber( this.first, this.rest.eliminateOverworkers() );
	}
    }

    // capOverworkers : OneMoreNumber double -> ListOfNumbers
    public ListOfNumbers capOverworkers ( double cap ) {
	// this.first ... this.rest.capOverworkers()

	return new OneMoreNumber( Math.min( cap, this.first ), this.rest.capOverworkers(cap) ) ;
    }

    public ListOfNumbers capOverworkersWayNumeroUno ( double cap ) {
	if ( this.first <= cap ) {
	/*
	  Input: new OneMoreNumber( 40.0, mt)
	  Output: new OneMoreNumber( 40.0, mt)

	  Input: new OneMoreNumber( this.first, this.rest)
	  Output: new OneMoreNumber( this.first, this.rest.capOverworkers() )
	*/
	    return new OneMoreNumber( this.first, this.rest.capOverworkers(cap) ) ;

	    //	  ----
	} else {

	    /*
	  Input: new OneMoreNumber( 400.0, mt)
	  Output: new OneMoreNumber( 100.0, mt) 

	  Input: new OneMoreNumber( this.first, this.rest)
	  Output: new OneMoreNumber( 100.0, this.rest.capOverworkers()) 
	*/
	    return new OneMoreNumber( cap, this.rest.capOverworkers(cap)) ;
	}
    }

    // update : OneNumber int double -> ListOfNumbers
    public ListOfNumbers update ( int n, double newOne ) {
	// this.first ... this.rest ... n ... newOne
	// this.first ... this.rest.update( ... , ... ) ... n ... newOne

	if ( n == 0 ) {
	/*
	  Input: new OneMoreNumber( 400.0, andLightning ).update(0, 756.0)
	  Output: new OneMoreNumber( 756.0, andLightning )

	  Input: new OneMoreNumber( this.first, this.rest ).update(n, newOne)
	  Output: new OneMoreNumber( newOne, this.rest )
	*/
	    return new OneMoreNumber( newOne, this.rest ) ;
	} else {
	// ---

	/*
	  Input: new OneMoreNumber( 72.0, new OneMoreNumber( 400.0, andLightning )).update(1, 756.0)
	  Output: new OneMoreNumber( 72.0, new OneMoreNumber( 756.0, andLightning ) ) 


	  Input: new OneMoreNumber( this.first, this.rest).update(n, newOne)
	  Output: new OneMoreNumber( this.first, this.rest.update(n-1,newOne) ) 
	*/
	    return new OneMoreNumber( this.first, this.rest.update(n-1,newOne) ) ;
	}
    }

}

class scratch {
    // wage : double -> double
    // Purpose: to compute the salary of a worker who worked some-many hours at $12/h
    static double wage ( double hours ) {
	return 12 * hours;
    }

    // devert : int int -> ListOfNumbers
    // Examples:
    //  devert(9) = new OneMoreNumber ( 9, mt )
    //  devert(95) = new OneMoreNumber( 5, new OneMoreNumber ( 9, mt ) )
    //  devert(95) = new OneMoreNumber( 5, devert(9) )
    //  devert(951) = new OneMoreNumber( 1, new OneMoreNumber( 5, new OneMoreNumber ( 9, mt ) ) )
    //  devert(951) = new OneMoreNumber( 1, devert(95) )
    static ListOfNumbers devert ( int theNumber, int base ) {
	if ( theNumber < base ) {
	    return new OneMoreNumber ( theNumber, new EmptyListOfNumbers () ) ;
	} else {
	    // x * b + y = z
	    // z / b = x
	    // z % b = y
	    return new OneMoreNumber( theNumber % base, devert( theNumber / base, base ) );
	}
    }

    public static void main ( String[] args ) {
	System.out.format("The answer is %f, but should be %f%n",
			  wage(10.0),
			  120.0);

	ListOfNumbers mt = new EmptyListOfNumbers();
	ListOfNumbers andKefka = new OneMoreNumber( 10.0, mt );
	ListOfNumbers andBlaz = new OneMoreNumber( 15.0, andKefka );
	ListOfNumbers andLightning = new OneMoreNumber( 40.0, andBlaz );

	System.out.format("The answer is %s, but should be %s%n",
			  andKefka.toString(),
			  "10.0000:!" );
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber( 10.0, mt ).toString(),
			  "10.0000:!" );
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber( 10.0, new EmptyListOfNumbers() ).toString(),
			  "10.0000:!" );
	System.out.format("The answer is %s, but should be %s%n",
			  String.format("%f:%s", 10.0, new EmptyListOfNumbers() ),
			  "10.0000:!" );
	System.out.format("The answer is %s, but should be %s%n",
			  String.format("10.000000:%s", new EmptyListOfNumbers() ),
			  "10.0000:!" );
	System.out.format("The answer is %s, but should be %s%n",
			  String.format("10.000000:%s", new EmptyListOfNumbers().toString() ),
			  "10.0000:!" );
	System.out.format("The answer is %s, but should be %s%n",
			  String.format("10.000000:%s", "!" ),
			  "10.0000:!" );
	System.out.format("The answer is %s, but should be %s%n",
			  String.format("10.000000:!"),
			  "10.0000:!" );
	System.out.format("The answer is %s, but should be %s%n",
			  "10.000000:!",
			  "10.0000:!" );

	System.out.format("The answer is %s, but should be %s%n",
			  mt.wages(),
			  new EmptyListOfNumbers() );

	System.out.format("The answer is %s, but should be %s%n",
			  mt.wages().toString(),
			  new EmptyListOfNumbers().toString() );


	System.out.format("The answer is %s, but should be %s%n",
			  andKefka.wages(),
			  new OneMoreNumber( 10.0*12.0,
					     new EmptyListOfNumbers() ) );
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber( 10.0, new EmptyListOfNumbers() ).wages(),
			  new OneMoreNumber( 10.0*12.0,
					     new EmptyListOfNumbers() ) );

	System.out.format("The answer is %s, but should be %s%n",
			  andBlaz.wages(),
			  new OneMoreNumber( 15.0*12.0,
					     new OneMoreNumber( 10.0*12.0,
								new EmptyListOfNumbers() ) ) );
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber( 15.0, new OneMoreNumber( 10.0, new EmptyListOfNumbers() ) ).wages(),
			  new OneMoreNumber( 15.0*12.0,
					     new OneMoreNumber( 10.0*12.0,
								new EmptyListOfNumbers() ) ) );

	System.out.format("The answer is %s, but should be %s%n",
			  andLightning.wages(),
			  new OneMoreNumber( 40.0*12.0,
					     new OneMoreNumber( 15.0*12.0,
								new OneMoreNumber( 10.0*12.0,
										   new EmptyListOfNumbers() ) ) ) );
       
	ListOfNumbers andSquall = new OneMoreNumber( 400.0, andLightning );
	ListOfNumbers andVivi = new OneMoreNumber( 60.0, andSquall );

	System.out.format("The answer is %s, but should be %s%n",
			  mt.eliminateOverworkers(),
			  new EmptyListOfNumbers() );

	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber( 400.0, new EmptyListOfNumbers() ).eliminateOverworkers(),
			  new EmptyListOfNumbers() );
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber( 40.0, new EmptyListOfNumbers() ).eliminateOverworkers(),
			  new OneMoreNumber( 40.0, new EmptyListOfNumbers() ) );
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber( 40.0, new OneMoreNumber( 400.0, new EmptyListOfNumbers() ) ).eliminateOverworkers(),
			  new OneMoreNumber( 40.0, new EmptyListOfNumbers() ) );
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber( 800.0, new OneMoreNumber( 40.0, new OneMoreNumber( 400.0, new EmptyListOfNumbers() ) )).eliminateOverworkers(),
			  new OneMoreNumber( 40.0, new EmptyListOfNumbers() ) );


	System.out.format("The answer is %s, but should be %s%n",
			  andSquall.eliminateOverworkers(),
			  andLightning );
	System.out.format("The answer is %s, but should be %s%n",
			  andVivi.eliminateOverworkers(),
			  new OneMoreNumber( 60.0, andLightning ));

	System.out.format("capOverworkers beloooooooow!%n");

	System.out.format("The answer is %s, but should be %s%n",
			  mt.capOverworkers(100),
			  mt );
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber( 40.0, mt).capOverworkers(100),
			  new OneMoreNumber( 40.0, mt) );
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber( 400.0, mt).capOverworkers(100),
			  new OneMoreNumber( 100.0, mt) );

	System.out.format("The answer is %s, but should be %s%n",
			  andSquall.capOverworkers(100),
			  new OneMoreNumber( 100.0, andLightning ) );
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber( 400.0, andLightning ).capOverworkers(100),
			  new OneMoreNumber( 100.0, andLightning ) );
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber( 400.0, andLightning ).capOverworkers(300),
			  new OneMoreNumber( 300.0, andLightning ) );
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber( 400.0, andLightning ).capOverworkers(500),
			  new OneMoreNumber( 400.0, andLightning ) );

	System.out.format("coolFunction beloooooooow!%n");

	System.out.format("The answer is %s, but should be %s%n",
			  mt.update(923, 756.0),
			  mt );
	System.out.format("The answer is %s, but should be %s%n",
			  mt.update(51, 756.0),
			  mt );
	System.out.format("The answer is %s, but should be %s%n",
			  mt.update(0, 756.0),
			  mt );
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber( 400.0, andLightning ).update(0, 756.0),
			  new OneMoreNumber( 756.0, andLightning ) );
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber( 72.0, new OneMoreNumber( 400.0, andLightning )).update(1, 756.0),
			  new OneMoreNumber( 72.0, new OneMoreNumber( 756.0, andLightning ) ) );
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber( 13.0, new OneMoreNumber( 72.0, new OneMoreNumber( 400.0, andLightning ))).update(2, 756.0),
			  new OneMoreNumber( 13.0, new OneMoreNumber( 72.0, new OneMoreNumber( 756.0, andLightning ) ) ) );


	System.out.format("The answer is %s%n",
			  andSquall );
	System.out.format("The answer is %s, but should be %s%n",
			  andSquall.update(0, 756.0),
			  new OneMoreNumber( 756.0, andLightning ) );

	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber( 400.0, andLightning).update(0, 756.0),
			  new OneMoreNumber( 756.0, andLightning ) );
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber( 756.0, andLightning),
			  new OneMoreNumber( 756.0, andLightning ) );

	System.out.format("The answer is %s%n",
			  andSquall );
	System.out.format("The answer is %s%n",
			  new OneMoreNumber( 400.0, andLightning) );

	// Bad picture:
	// 400 -> 40 -> 15 -> 10 -> !
	// 756 -> 40 -> 15 -> 10 -> !

	// Correct picture:
	// 400 \
	//      +-- 40 -> 15 -> 10 -> !
	// 756 /

	System.out.format("devert%n");

	System.out.format("The answer is %s, but should be %s%n",
			  devert(456, 10),
			  new OneMoreNumber( 6, new OneMoreNumber( 5, new OneMoreNumber ( 4, mt) ) ));		
	System.out.format("The answer is %s, but should be %s%n",
			  devert(951, 10),
			  new OneMoreNumber( 1, new OneMoreNumber( 5, new OneMoreNumber ( 9, mt ) ) ) ); 
	System.out.format("The answer is %s%n",
			  devert(951, 2) ); 

    }
}