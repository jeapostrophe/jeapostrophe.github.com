#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

// A ListOfString is either...
//  EmptyListOfString
//  OneMoreString
class ListOfString {
public:
  // containsPlutoHuh : ListOfString -> bool
  // Purpose: to compute if Pluto is in the list of strings
  virtual bool containsPlutoHuh ( ) = 0;
  // nthPlanet : ListOfString int const char* -> const char*
  // Purpose: to return the nth planet in the list OR mtString 
  virtual const char* nthPlanet ( int n, const char* mtString ) = 0;
  // wholeSolarSystemTrain : ListOfString -> int
  virtual int wholeSolarSystemTrain () = 0;
  virtual const char* longestString () = 0;
  virtual bool containsLessThanNCharactersHuh ( int n ) = 0 ;
};

// An Error is a
//  new Error ( message )
// where
//  message is a const char*

// An EmptyListOfString is a
//  new EmptyListOfString ( )
// where
class EmptyListOfString : public ListOfString {
public:
    
  EmptyListOfString ( ) {
  }

  const char* longestString () {
	return "";
  }

  // containsPlutoHuh : EmptyListOfString -> bool
  bool containsPlutoHuh ( ) {
	// ...  ...
	return false ;
  }

  // nthPlanet : EmptyListOfString int const char* -> const char*
  const char* nthPlanet ( int n, const char* mtString ) {
    // ... ...
    // return NaS ;
    // throw (new Error ( "There is no such planet" ));
    // return ""; <- Not the same
    return mtString;
  }

  // wholeSolarSystemTrain : EmptyListOfString -> const char*
  int wholeSolarSystemTrain ( ) {
	// ... ...
    return printf("!") ;
  }

  bool containsLessThanNCharactersHuh ( int n ) {
	return false ;
  }
};

//            "Pluto"    "Mars"    first    , rest ,
// <====^== * o-----o * o-----o * o-----o * []
//               0         1         2          3      3 3/4

// A OneMoreString is a
//  new OneMoreString ( first, rest )
// where
//  first is a const char*
//  rest is a ListOfString
class OneMoreString : public ListOfString {
public:
  const char* first;

  // public EmptyListOfString rest;
  // leads to:
  //            first
  // <===^=== * o---o * []

  // public OneMoreString rest;
  // leads to:
  // <===^=== * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * o---o * ....
    
  ListOfString* rest;

  OneMoreString ( const char* first0, ListOfString* rest0 ) {
	first = first0;
	rest = rest0;
  }

  // containsPlutoHuh : OneMoreString -> bool
  bool containsPlutoHuh ( ) {
	// ... this->first ... this->rest ...
	// ... this->first ... (this->rest).containsPlutoHuh() ...
	return 
      strcmp(this->first,"Pluto") == 0
      || (this->rest)->containsPlutoHuh() ;
  }    

  // nthPlanet : OneMoreString int const char* -> const char*
  const char* nthPlanet ( int n, const char* mtString ) {
    // ... this->first ... this->rest ...
    // ... this->first ... (this->rest)->nthPlanet( ... n ... ) ... n ....
    if ( n == 0 ) {
      return this->first ;
    } else {
      return (this->rest)->nthPlanet( n - 1, mtString ) ;
    }
  }

  // wholeSolarSystemTrain : OneMoreString -> int
  int wholeSolarSystemTrain ( ) {
    // ... this->first ... this->rest ...
    // ... this->first ... (this->rest)->wholeSolarSystemTrain() ...

    // 

    printf("o- %s -o * ", this->first);
    return (this->rest)->wholeSolarSystemTrain();
  }

  bool containsLessThanNCharactersHuh ( int n ) {
	// this->first (this->rest)->containsLessThanNCharactersHuh ( n )
    return strlen((this->first)) < n || (this->rest)->containsLessThanNCharactersHuh ( n ) ;
  }

  const char* longestString () {
    if ( strlen(this->first) > strlen((this->rest)->longestString()) ) {
      return this->first;
	} else {
      return (this->rest)->longestString();
	}
  }
};

// Glaucon & Socrates:
// "They are all true" = (for all x . P x)
// "They are not all true" = ! (for all x . P x)
// "There is at least one that is false" = exists x . ! P x
// "They are all false" = for all x . ! P x

int main () {
  ListOfString* mt = new EmptyListOfString ();
  ListOfString* empty = new EmptyListOfString ();
  ListOfString* andMercury = new OneMoreString ( "Mercury", mt );
  ListOfString* andVenus = new OneMoreString ( "Venus", andMercury );
  ListOfString* andEarth = new OneMoreString ( "Earth", andVenus );
  ListOfString* andBizzaroEarth = new OneMoreString ( "Bizzaro Earth", andVenus );
  ListOfString* andEarth2049 = new OneMoreString ( "Earth 2049", andVenus );
  ListOfString* andMars = new OneMoreString ( "Mars", andEarth );
  ListOfString* andJupiter = new OneMoreString ( "Jupiter", andMars );
  ListOfString* andSaturn = new OneMoreString ( "Saturn", andJupiter );
  ListOfString* andUranus = new OneMoreString ( "Uranus", andSaturn );
  ListOfString* andNeptune = new OneMoreString ( "Neptune", andUranus );
  ListOfString* andPluto = new OneMoreString ( "Pluto", andNeptune );
  ListOfString* andPlanetX = new OneMoreString ( "Planet X", andPluto );
  ListOfString* theRealSS = andPluto;

  /*
	printf("The answer is %s, but should be %s\n",
    andPluto->first,
    "Pluto");
  */
  printf("The answer is %s, but should be %s\n",
         andEarth->nthPlanet(0, "Sun"),
         "Earth");
  printf("The answer is %s, but should be %s\n",
         andPluto->nthPlanet(0, "Sun"),
         "Pluto");
  printf("The answer is %s, but should be %s\n",
         andPluto->nthPlanet(3, "Sun"),
         "Saturn");

  printf("The answer is %s, but should be %s\n",
         (new OneMoreString ( "Pluto", andNeptune ))->nthPlanet(3, "Sun"),
         "Saturn");
  printf("The answer is %s, but should be %s\n",
         andNeptune->nthPlanet( 2, "Sun" ),
         "Saturn");
  printf("The answer is %s, but should be %s\n",
         andNeptune->nthPlanet( 3, "Sun" ),
         "Jupiter");

  printf("The answer is %s, but should be %s\n",
         andNeptune->nthPlanet( 30, "Sun" ),
         "Sun");

  printf("The test case is correct? %d\n",
         strcmp(andNeptune->nthPlanet( 30, "Sun" ), "Sun") == 0 );

  if ( strcmp(andNeptune->nthPlanet( 30, "Sun" ), "Sun") == 0 ) {
  } else {
    printf("The test case about nthPlanet on 30 and Sun is broken\n");
  }

  printf("The answer is %d, but should be %d\n",
         5+5,
         10);

  printf("The answer is %d, but should be %d\n",
         mt->containsPlutoHuh(),
         false);
  printf("The answer is %d, but should be %d\n",
         andPluto->containsPlutoHuh(),
         true);
  printf("The answer is %d, but should be %d\n",
         andNeptune->containsPlutoHuh(),
         false);
  printf("The answer is %d, but should be %d\n",
         andEarth->containsPlutoHuh(),
         false);

  printf("The answer is %d, but should be %d\n",
         andPlanetX->containsPlutoHuh(),
         true);
  printf("The answer is %d, but should be %d\n",
         (new OneMoreString ( "Planet X", andPluto ))->containsPlutoHuh(),
         true);
  printf("The answer is %d, but should be %d\n",
         (new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", andNeptune ) ))->containsPlutoHuh(),
         true);
  printf("The answer is %d, but should be %d\n",
         (new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", andUranus ) ) ))->containsPlutoHuh(),
         true);
  printf("The answer is %d, but should be %d\n",
         (new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus", andSaturn ) ) ) ))->containsPlutoHuh(),
         true);
  printf("The answer is %d, but should be %d\n",
         (new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", andJupiter ) ) ) ) ))->containsPlutoHuh(),
         true);
  printf("The answer is %d, but should be %d\n",
         (new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", andMars ) ) ) ) ) ))->containsPlutoHuh(),
         true);
  printf("The answer is %d, but should be %d\n",
         (new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", andEarth ) ) ) ) ) ) ))->containsPlutoHuh(),
         true);
  printf("The answer is %d, but should be %d\n",
         (new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", andVenus ) ) ) ) ) ) ) ))->containsPlutoHuh(),
         true);
  printf("The answer is %d, but should be %d\n",
         (new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", andMercury ) ) ) ) ) ) ) ) ))->containsPlutoHuh(),
         true);
  printf("The answer is %d, but should be %d\n",
         (new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", new OneMoreString ( "Mercury", mt ) ) ) ) ) ) ) ) ) ))->containsPlutoHuh(),
         true);
  printf("The answer is %d, but should be %d\n",
         (new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", new OneMoreString ( "Mercury", new EmptyListOfString () ) ) ) ) ) ) ) ) ) ))->containsPlutoHuh(),
         true);
  printf("The answer is %d, but should be %d\n",
         strcmp(((new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", new OneMoreString ( "Mercury", new EmptyListOfString () ) ) ) ) ) ) ) ) ) ))->first),("Pluto")) == 0
         || ((new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", new OneMoreString ( "Mercury", new EmptyListOfString () ) ) ) ) ) ) ) ) ) ))->rest)->containsPlutoHuh(),
         true);

  printf("The answer is %d, but should be %d\n",
         strcmp("Planet X", "Pluto") == 0
         || ((new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", new OneMoreString ( "Mercury", new EmptyListOfString () ) ) ) ) ) ) ) ) ) ))->rest)->containsPlutoHuh(),
         true);
  printf("The answer is %d, but should be %d\n",
         false
         || ((new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", new OneMoreString ( "Mercury", new EmptyListOfString () ) ) ) ) ) ) ) ) ) ))->rest)->containsPlutoHuh(),
         true);
  printf("The answer is %d, but should be %d\n",
         ((new OneMoreString ( "Planet X", new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", new OneMoreString ( "Mercury", new EmptyListOfString () ) ) ) ) ) ) ) ) ) ))->rest)->containsPlutoHuh(),
         true);
  printf("The answer is %d, but should be %d\n",
         (new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", new OneMoreString ( "Mercury", new EmptyListOfString () ) ) ) ) ) ) ) ) ))->containsPlutoHuh(),
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
  printf("The answer is %d, but should be %d\n",
         strcmp(((new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", new OneMoreString ( "Mercury", new EmptyListOfString () ) ) ) ) ) ) ) ) ))->first),("Pluto")) == 0
         || ((new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", new OneMoreString ( "Mercury", new EmptyListOfString () ) ) ) ) ) ) ) ) ))->rest)->containsPlutoHuh(),
         true);
  printf("The answer is %d, but should be %d\n",
         strcmp("Pluto",("Pluto"))==0
         || ((new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", new OneMoreString ( "Mercury", new EmptyListOfString () ) ) ) ) ) ) ) ) ))->rest)->containsPlutoHuh(),
         true);
  printf("The answer is %d, but should be %d\n",
         true
         || ((new OneMoreString ( "Pluto", new OneMoreString ( "Neptune", new OneMoreString ( "Uranus",  new OneMoreString ( "Saturn", new OneMoreString ( "Jupiter", new OneMoreString ( "Mars", new OneMoreString ( "Earth", new OneMoreString ( "Venus", new OneMoreString ( "Mercury", new EmptyListOfString () ) ) ) ) ) ) ) ) ))->rest)->containsPlutoHuh(),
         true);
  printf("The answer is %d, but should be %d\n",
         true,
         true);

  /*
	printf("The answer is %d,%d, but should be %d,%d\n",
    pMario.x, pMario.y,
    0, 2);
  */
  /*
    printf("The answer is %s,%s,%ListOfString*, but should be %ListOfString*\n",
    theRealSS->first, theRealSS->rest->first, theRealSS->rest->rest,
    theRealSS);
  */
  printf("The answer is ");
  mt->wholeSolarSystemTrain();
  printf(", but should be %s\n",
         "");
  printf("The answer is ");
  theRealSS->wholeSolarSystemTrain();
  printf(", but should be %s\n",
         "o- Pluto -o * o- Neptune -o * o- Uranus -o * o- Saturn -o * o- Jupiter -o * o- Mars -o * o- Earth -o * o- Venus -o * o- Mercury -o * !");
	
  printf("The answer is %d, but should be %d\n",
         theRealSS->containsLessThanNCharactersHuh(4),
         false);
  printf("The answer is %d, but should be %d\n",
         theRealSS->containsLessThanNCharactersHuh(5),
         true);

  printf("The answer is %s, but should be %s\n",
         theRealSS->longestString(),
         "Mercury");
 
}
