// A ListOfString is...
// either a...
//  0 = new EmptyListOfString ()
//  1+ = new OneMoreString ( String newOne, ListOfString allTheOthers )

interface ListOfString {
    public int listLength () ;
    public int howManyLetters () ;
    public boolean hasPlanetLessThan4CharactersHuh () ;
}

class EmptyListOfString implements ListOfString {
    public EmptyListOfString ( ) {
    }

    public int listLength () {
	// ... ...
	return 0 ;
    }

    public int howManyLetters () {
	return 0;
    }

    public boolean hasPlanetLessThan4CharactersHuh () {
	return false;
    }
}

class OneMoreString implements ListOfString {
    public String newOne;
    public ListOfString allTheOthers;

    public OneMoreString ( String newOne0, ListOfString allTheOthers0 ) {
	newOne = newOne0;
	allTheOthers = allTheOthers0;
    }

    public int listLength () {
	// this.newOne this.allTheOthers
	// this.newOne (this.allTheOthers).listLength()
	return 1 + (this.allTheOthers).listLength() ;
    }

    public int howManyLetters () {
	// this.newOne this.allTheOthers
	// this.newOne (this.allTheOthers).howManyLetters()
	return (this.newOne).length() + (this.allTheOthers).howManyLetters() ;
    }

    public boolean hasPlanetLessThan4CharactersHuh () {
	// this.newOne this.allTheOthers
	// this.newOne (this.allTheOthers).hasPlanetLessThan4CharactersHuh ()
	/*
	if ( this.newOne.length() < 4
	     || (this.allTheOthers).hasPlanetLessThan4CharactersHuh () ) {
	    return true ;
	} else {
	    return false ;
	}
	*/
	/*
	return ( this.newOne.length() < 4
		 || (this.allTheOthers).hasPlanetLessThan4CharactersHuh () );
	*/
	if ( this.newOne.length() < 4 ) {
	    return true ;
	} else {
	    return (this.allTheOthers).hasPlanetLessThan4CharactersHuh () ;
	}

    }
}

class scratch {
    static double averageLength ( ListOfString someSolar ) {
	// someSolar
	return (1.0 * someSolar.howManyLetters ()) / someSolar.listLength ();
    }

    public static void main ( String[] args ) {
	ListOfString mt = new EmptyListOfString ();
	ListOfString andMercury = new OneMoreString ( "Mercury", mt );
	ListOfString andVenus = new OneMoreString ( "Venus", andMercury );
	ListOfString andEarth = new OneMoreString ( "Earth", andVenus );
	ListOfString andBizzaroEarth = new OneMoreString ( "Bizzaro Earth", andVenus );
	ListOfString andEarth2049 = new OneMoreString ( "Earth 2049", andVenus );
	ListOfString andMars = new OneMoreString ( "Mars", andEarth );
	ListOfString andJupiter = new OneMoreString ( "Jupiter", andMars );
	ListOfString andSaturn = new OneMoreString ( "Saturn", andJupiter );
	ListOfString andUranus = new OneMoreString ( "Uranus", andSaturn );
	ListOfString andNeptune = new OneMoreString ( "Neptune", andUranus );
	ListOfString andPluto = new OneMoreString ( "Pluto", andNeptune );
	ListOfString theRealSS = andPluto;

	System.out.format( "The answer is %d, but should be %d%n",
			   andVenus.listLength(),
			   2 );

	System.out.format( "The answer is %d, but should be %d%n",
			   new OneMoreString ( "Venus", new OneMoreString ( "Mercury", new EmptyListOfString () ) ).listLength(),
			   2 );

	System.out.format( "The answer is %d, but should be %d%n",
			   new OneMoreString ( "Venus", andMercury ).listLength(),
			   2 );
	System.out.format( "The answer is %d, but should be %d%n",
			   1 + andMercury.listLength(),
			   2 );
	System.out.format( "The answer is %d, but should be %d%n",
			   1 + new OneMoreString ( "Mercury", mt ).listLength(),
			   2 );
	System.out.format( "The answer is %d, but should be %d%n",
			   1 + 1 + mt.listLength(),
			   2 );
	System.out.format( "The answer is %d, but should be %d%n",
			   1 + 1 + new EmptyListOfString ().listLength(),
			   2 );
	System.out.format( "The answer is %d, but should be %d%n",
			   1 + 1 + 0,
			   2 );
	System.out.format( "The answer is %d, but should be %d%n",
			   1 + 1,
			   2 );
	System.out.format( "The answer is %d, but should be %d%n",
			   2,
			   2 );

	System.out.format("Look up!%n");

	System.out.format( "The answer is %d, but should be %d%n",
			   5*2,
			   10 );
	System.out.format( "The answer is %d, but should be %d%n",
			   mt.listLength(),
			   0 );
	System.out.format( "The answer is %d, but should be %d%n",
			   andMercury.listLength(),
			   1 );
	System.out.format( "The answer is %d, but should be %d%n",
			   andVenus.listLength(),
			   2 );

	System.out.format( "The answer is %d, but should be %d%n",
			   mt.howManyLetters(),
			   0 );
	System.out.format( "The answer is %d, but should be %d%n",
			   andMercury.howManyLetters(),
			   7 + 0 );
	System.out.format( "The answer is %d, but should be %d%n",
			   andVenus.howManyLetters(),
			   5 + 7 + 0 );

	System.out.format( "The answer is %d, but should be %d%n",
			   andEarth.listLength(),
			   3 );
	System.out.format( "The answer is %d, but should be %d%n",
			   andBizzaroEarth.listLength(),
			   3 );
	System.out.format( "The answer is %d, but should be %d%n",
			   andEarth2049.listLength(),
			   3 );


	System.out.format( "The answer is %d, but should be %d%n",
			   theRealSS.listLength(),
			   9 );
	System.out.format( "The answer is %d, but should be %d%n",
			   theRealSS.howManyLetters(),
			   52 );

	System.out.format( "The answer is %f, but should be %f%n",
			   averageLength ( andVenus ),
			   12.0 / 2.0 );
	System.out.format( "The answer is %f, but should be %f%n",
			   averageLength ( theRealSS ),
			   52.0 / 9.0 );
	System.out.format( "The answer is %f, but should be %f%n",
			   averageLength ( mt ),
			   0.0 / 0.0 );

	System.out.format( "The answer is %b, but should be %b%n",
			   theRealSS.hasPlanetLessThan4CharactersHuh(),
			   false );
	System.out.format( "The answer is %b, but should be %b%n",
			   new OneMoreString ( "Zed", theRealSS ).hasPlanetLessThan4CharactersHuh(),
			   true );
	System.out.format( "The answer is %b, but should be %b%n",
			   new OneMoreString ( "Zedz", theRealSS ).hasPlanetLessThan4CharactersHuh(),
			   false );
	System.out.format( "The answer is %b, but should be %b%n",
			   new OneMoreString ( "Zedz", new OneMoreString( "Zed", theRealSS ) ).hasPlanetLessThan4CharactersHuh(),
			   true );

	
	System.out.format("Look down!%n");

	System.out.format( "The answer is %b, but should be %b%n",
			   new OneMoreString ( "Zed", theRealSS ).hasPlanetLessThan4CharactersHuh(),
			   true );
	System.out.format( "The answer is %b, but should be %b%n",
			   ( "Zed".length() < 4
			     || theRealSS.hasPlanetLessThan4CharactersHuh () ),
			   true );
	System.out.format( "The answer is %b, but should be %b%n",
			   ( 3 < 4
			     || theRealSS.hasPlanetLessThan4CharactersHuh () ),
			   true );
	System.out.format( "The answer is %b, but should be %b%n",
			   ( true
			     || theRealSS.hasPlanetLessThan4CharactersHuh () ),
			   true );
	System.out.format( "The answer is %b, but should be %b%n",
			   // || is "short circuiting"
			   ( true ),
			   true );

	/*
	ListOfString weirdSS = new OneMoreString( "weird", weirdSS );
	ListOfString zedSS = new OneMoreString( "Zed", weirdSS );
	
	System.out.format( "The answer is %b, but should be %b%n",
			   zedSS.hasPlanetLessThan4CharactersHuh (),
			   true );
	*/

    }
}