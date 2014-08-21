// A ListOfString is either...
//  EmptyListOfString
//  OneMoreString
interface ListOfString {
    // containsPlutoHuh : ListOfString -> boolean
    // Purpose: to compute if Pluto is in the list of strings
    public boolean containsPlutoHuh ( ) ;
    // nthPlanet : ListOfString int String -> String
    // Purpose: to return the nth planet in the list OR mtString 
    public String nthPlanet ( int n, String mtString ) ;
    // wholeSolarSystemTrain : ListOfString -> String
    public String wholeSolarSystemTrain () ;

    public String longestString ();

    public boolean containsLessThanNCharactersHuh ( int n ) ;
}

// An Error is a
//  new Error ( message )
// where
//  message is a String

// An EmptyListOfString is a
//  new EmptyListOfString ( )
// where
class EmptyListOfString implements ListOfString {
    
    public EmptyListOfString ( ) {
    }

    public String longestString () {
	return "";
    }

    // containsPlutoHuh : EmptyListOfString -> boolean
    public boolean containsPlutoHuh ( ) {
	// ...  ...
	return false ;
    }

    // nthPlanet : EmptyListOfString int String -> String
    public String nthPlanet ( int n, String mtString ) {
	// ... ...
	// return NaS ;
	// throw (new Error ( "There is no such planet" ));
	// return ""; <- Not the same
	return mtString;
    }

    // wholeSolarSystemTrain : EmptyListOfString -> String
    public String wholeSolarSystemTrain ( ) {
	// ... ...
	return "!" ;
    }

    public boolean containsLessThanNCharactersHuh ( int n ) {
	return false ;
    }
}

//            "Pluto"    "Mars"    first    , rest ,
// <====^== * o-----o * o-----o * o-----o * []
//               0         1         2          3      3 3/4

// A OneMoreString is a
//  new OneMoreString ( first, rest )
// where
//  first is a String
//  rest is a ListOfString
class OneMoreString implements ListOfString {
    public String first;

    // public EmptyListOfString rest;
    // leads to:
    //            first
    // <===^=== * o---o * []

    // public OneMoreString rest;
    // leads to:
    // <===^=== * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * ....
    
    public ListOfString rest;

    public OneMoreString ( String first0, ListOfString rest0 ) {
	first = first0;
	rest = rest0;
    }

    // containsPlutoHuh : OneMoreString -> boolean
    public boolean containsPlutoHuh ( ) {
	// ... this.first ... this.rest ...
	// ... this.first ... (this.rest).containsPlutoHuh() ...
	return 
	    (this.first).equals("Pluto")
	    || (this.rest).containsPlutoHuh() ;
    }    

    // nthPlanet : OneMoreString int String -> String
    public String nthPlanet ( int n, String mtString ) {
	// ... this.first ... this.rest ...
	// ... this.first ... (this.rest).nthPlanet( ... n ... ) ... n ....
	if ( n == 0 ) {
	    return this.first ;
	} else {
	    return (this.rest).nthPlanet( n - 1, mtString ) ;
	}
    }

    // wholeSolarSystemTrain : OneMoreString -> String
    public String wholeSolarSystemTrain ( ) {
	// ... this.first ... this.rest ...
	// ... this.first ... (this.rest).wholeSolarSystemTrain() ...

	// 

	return 
	    String.format("o- %s -o * %s",
			  this.first,			  
			  (this.rest).wholeSolarSystemTrain() )
	    ;
    }

    public boolean containsLessThanNCharactersHuh ( int n ) {
	// this.first (this.rest).containsLessThanNCharactersHuh ( n )
	return (this.first).length() < n || (this.rest).containsLessThanNCharactersHuh ( n ) ;
    }

    public String longestString () {
	if ( this.first.length() >  (this.rest).longestString()) {
	    return this.first;
	} else {
	    return (this.rest).longestString();
	}
    }

}

// Glaucon & Socrates:
// "They are all true" = (for all x . P x)
// "They are not all true" = ! (for all x . P x)
// "There is at least one that is false" = exists x . ! P x
// "They are all false" = for all x . ! P x

class scratch {
    public static void main ( String[] args ) {
	ListOfString mt = new EmptyListOfString ();
	ListOfString empty = new EmptyListOfString ();
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
	ListOfString andPlanetX = new OneMoreString ( "Planet X", andPluto );
	ListOfString theRealSS = andPluto;

	/*
	System.out.format("The answer is %s, but should be %s%n",
			  andPluto.first,
			  "Pluto");
	*/
	System.out.format("The answer is %s, but should be %s%n",
			  andEarth.nthPlanet(0, "Sun"),
			  "Earth");
	System.out.format("The answer is %s, but should be %s%n",
			  andPluto.nthPlanet(0, "Sun"),
			  "Pluto");
	System.out.format("The answer is %s, but should be %s%n",
			  andPluto.nthPlanet(3, "Sun"),
			  "Saturn");

	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreString ( "Pluto", andNeptune ).nthPlanet(3, "Sun"),
			  "Saturn");
	System.out.format("The answer is %s, but should be %s%n",
			  andNeptune.nthPlanet( 2, "Sun" ),
			  "Saturn");
	System.out.format("The answer is %s, but should be %s%n",
			  andNeptune.nthPlanet( 3, "Sun" ),
			  "Jupiter");

	System.out.format("The answer is %s, but should be %s%n",
			  andNeptune.nthPlanet( 30, "Sun" ),
			  "Sun");

	System.out.format("The test case is correct? %b%n",
			  andNeptune.nthPlanet( 30, "Sun" ).equals("Sun") );

	if ( andNeptune.nthPlanet( 30, "Sun" ).equals("Sun") ) {
	} else {
	    System.out.format("The test case about nthPlanet on 30 and Sun is broken%n");
	}

	System.out.format("The answer is %d, but should be %d%n",
			  5+5,
			  10);

	System.out.format("The answer is %b, but should be %b%n",
			  mt.containsPlutoHuh(),
			  false);
	System.out.format("The answer is %b, but should be %b%n",
			  andPluto.containsPlutoHuh(),
			  true);
	System.out.format("The answer is %b, but should be %b%n",
			  andNeptune.containsPlutoHuh(),
			  false);
	System.out.format("The answer is %b, but should be %b%n",
			  andEarth.containsPlutoHuh(),
			  false);

	System.out.format("The answer is %b, but should be %b%n",
			  andPlanetX.containsPlutoHuh(),
			  true);
	System.out.format("The answer is %b, but should be %b%n",
			  (new OneMoreString ( "Planet X", andPluto )).containsPlutoHuh(),
			  true);
	System.out.format("The answer is %b, but should be %b%n",
			  (new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", andNeptune ) )).containsPlutoHuh(),
			  true);
	System.out.format("The answer is %b, but should be %b%n",
			  (new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", andUranus ) ) )).containsPlutoHuh(),
			  true);
	System.out.format("The answer is %b, but should be %b%n",
			  (new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus", andSaturn ) ) ) )).containsPlutoHuh(),
			  true);
	System.out.format("The answer is %b, but should be %b%n",
			  (new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", andJupiter ) ) ) ) )).containsPlutoHuh(),
			  true);
	System.out.format("The answer is %b, but should be %b%n",
			  (new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", andMars ) ) ) ) ) )).containsPlutoHuh(),
			  true);
	System.out.format("The answer is %b, but should be %b%n",
			  (new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", andEarth ) ) ) ) ) ) )).containsPlutoHuh(),
			  true);
	System.out.format("The answer is %b, but should be %b%n",
			  (new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", andVenus ) ) ) ) ) ) ) )).containsPlutoHuh(),
			  true);
	System.out.format("The answer is %b, but should be %b%n",
			  (new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", andMercury ) ) ) ) ) ) ) ) )).containsPlutoHuh(),
			  true);
	System.out.format("The answer is %b, but should be %b%n",
			  (new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", new OneMoreString ( "Mercury", mt ) ) ) ) ) ) ) ) ) )).containsPlutoHuh(),
			  true);
	System.out.format("The answer is %b, but should be %b%n",
			  (new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", new OneMoreString ( "Mercury", new EmptyListOfString () ) ) ) ) ) ) ) ) ) )).containsPlutoHuh(),
			  true);
	System.out.format("The answer is %b, but should be %b%n",
	((new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", new OneMoreString ( "Mercury", new EmptyListOfString () ) ) ) ) ) ) ) ) ) )).first).equals("Pluto")
	    || ((new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", new OneMoreString ( "Mercury", new EmptyListOfString () ) ) ) ) ) ) ) ) ) )).rest).containsPlutoHuh(),
			  true);

	System.out.format("The answer is %b, but should be %b%n",
			  "Planet X".equals("Pluto")
	    || ((new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", new OneMoreString ( "Mercury", new EmptyListOfString () ) ) ) ) ) ) ) ) ) )).rest).containsPlutoHuh(),
			  true);
	System.out.format("The answer is %b, but should be %b%n",
			  false
	    || ((new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", new OneMoreString ( "Mercury", new EmptyListOfString () ) ) ) ) ) ) ) ) ) )).rest).containsPlutoHuh(),
			  true);
	System.out.format("The answer is %b, but should be %b%n",
			  ((new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", new OneMoreString ( "Mercury", new EmptyListOfString () ) ) ) ) ) ) ) ) ) )).rest).containsPlutoHuh(),
			  true);
	System.out.format("The answer is %b, but should be %b%n",
			  new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", new OneMoreString ( "Mercury", new EmptyListOfString () ) ) ) ) ) ) ) ) ).containsPlutoHuh(),
			  true);

	// f(x) = x + 4
	// f(2) = 2 + 4 = 6

	// g(x) = f(x) + f(2+x)
	// g(2) = f(2) + f(2+2)
	//      = 2 + 4 + f(2+2)
	//      = 2 + 4 + f(4)
	//      = 2 + 4 + 4 + 4
	//      = 2 + 4 + 8
	//      = 2 + 12
	//      = 14	
	System.out.format("The answer is %b, but should be %b%n",
			  (new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", new OneMoreString ( "Mercury", new EmptyListOfString () ) ) ) ) ) ) ) ) ).first).equals("Pluto")
			  || (new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", new OneMoreString ( "Mercury", new EmptyListOfString () ) ) ) ) ) ) ) ) ).rest).containsPlutoHuh(),
			  true);
	System.out.format("The answer is %b, but should be %b%n",
			  "Pluto".equals("Pluto")
			  || (new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", new OneMoreString ( "Mercury", new EmptyListOfString () ) ) ) ) ) ) ) ) ).rest).containsPlutoHuh(),
			  true);
	System.out.format("The answer is %b, but should be %b%n",
			  true
			  || (new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", new OneMoreString ( "Mercury", new EmptyListOfString () ) ) ) ) ) ) ) ) ).rest).containsPlutoHuh(),
			  true);
	System.out.format("The answer is %b, but should be %b%n",
			  true,
			  true);

	/*
	System.out.format("The answer is %d,%d, but should be %d,%d%n",
			  pMario.x, pMario.y,
			  0, 2);
	*/
	/*
	  System.out.format("The answer is %s,%s,%ListOfString, but should be %ListOfString%n",
			  theRealSS.first, theRealSS.rest.first, theRealSS.rest.rest,
			  theRealSS);
	*/
	System.out.format("The answer is %s, but should be %s%n",
			  mt.wholeSolarSystemTrain(),
			  "");
 	System.out.format("The answer is %s, but should be %s%n",
			  theRealSS.wholeSolarSystemTrain(),
			  "o- Pluto -o * o- Neptune -o * o- Uranus -o * o- Saturn -o * o- Jupiter -o * o- Mars -o * o- Earth -o * o- Venus -o * o- Mercury -o * !");

	
	System.out.format("The answer is %b, but should be %b%n",
			  theRealSS.containsLessThanNCharactersHuh(4),
			  false);
	System.out.format("The answer is %b, but should be %b%n",
			  theRealSS.containsLessThanNCharactersHuh(5),
			  true);

	System.out.format("The answer is %s, but should be %s%n",
			  theRealSS.longestString(),
			  "");
 
    }
}