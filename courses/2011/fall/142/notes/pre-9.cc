#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

// A ListOfString is...
// either a...
//  0 = new EmptyListOfString ()
//  1+ = new OneMoreString ( const char* newOne, ListOfString allTheOthers )
class ListOfString {
public:
  virtual int listLength () = 0;
  virtual int howManyLetters () = 0;
  virtual bool hasPlanetLessThan4CharactersHuh () = 0;
};

class EmptyListOfString : public ListOfString {
public:

  EmptyListOfString ( ) {
  }

  int listLength () {
	// ... ...
	return 0 ;
  }

  int howManyLetters () {
	return 0;
  }

  bool hasPlanetLessThan4CharactersHuh () {
	return false;
  }
};

class OneMoreString : public ListOfString {
public:
  const char* newOne;
  ListOfString* allTheOthers;

  OneMoreString ( const char* newOne0, ListOfString* allTheOthers0 ) {
    newOne = newOne0;
    allTheOthers = allTheOthers0;
  }

  int listLength () {
    // this->newOne this->allTheOthers
    // this->newOne (this->allTheOthers)->listLength()
    return 1 + (this->allTheOthers)->listLength() ;
  }

  int howManyLetters () {
    // this->newOne this->allTheOthers
    // this->newOne (this->allTheOthers)->howManyLetters()
    return strlen(this->newOne) + (this->allTheOthers)->howManyLetters() ;
  }

  bool hasPlanetLessThan4CharactersHuh () {
    // this->newOne this->allTheOthers
    // this->newOne (this->allTheOthers)->hasPlanetLessThan4CharactersHuh ()
    /*
      if ( strlen(this->newOne) < 4
      || (this->allTheOthers)->hasPlanetLessThan4CharactersHuh () ) {
      return true ;
      } else {
      return false ;
      }
    */
    /*
      return ( strlen(this->newOne) < 4
      || (this->allTheOthers)->hasPlanetLessThan4CharactersHuh () );
    */
    if ( strlen(this->newOne) < 4 ) {
      return true ;
    } else {
      return (this->allTheOthers)->hasPlanetLessThan4CharactersHuh () ;
    }

  }
};

double averageLength ( ListOfString* someSolar ) {
  // someSolar
  return (1.0 * someSolar->howManyLetters ()) / someSolar->listLength ();
}

int main ( ) {
  ListOfString* mt =  ( new EmptyListOfString () );
  ListOfString* andMercury =  ( new OneMoreString ( "Mercury", mt ) );
  ListOfString* andVenus =  ( new OneMoreString ( "Venus", andMercury ) );
  ListOfString* andEarth =  ( new OneMoreString ( "Earth", andVenus ) );
  ListOfString* andBizzaroEarth =  ( new OneMoreString ( "Bizzaro Earth", andVenus ) );
  ListOfString* andEarth2049 =  ( new OneMoreString ( "Earth 2049", andVenus ) );
  ListOfString* andMars =  ( new OneMoreString ( "Mars", andEarth ) );
  ListOfString* andJupiter =  ( new OneMoreString ( "Jupiter", andMars ) );
  ListOfString* andSaturn =  ( new OneMoreString ( "Saturn", andJupiter ) );
  ListOfString* andUranus =  ( new OneMoreString ( "Uranus", andSaturn ) );
  ListOfString* andNeptune =  ( new OneMoreString ( "Neptune", andUranus ) );
  ListOfString* andPluto =  ( new OneMoreString ( "Pluto", andNeptune ) );
  ListOfString* theRealSS = andPluto;

  printf( "The answer is %d, but should be %d\n",
          andVenus->listLength(),
          2 );

  printf( "The answer is %d, but should be %d\n",
          (new OneMoreString ( "Venus",  ( new OneMoreString ( "Mercury",  ( new EmptyListOfString ()) )) ))->listLength(),
          2 );

  printf( "The answer is %d, but should be %d\n",
          (new OneMoreString ( "Venus", andMercury ))->listLength(),
          2 );
  printf( "The answer is %d, but should be %d\n",
          1 + andMercury->listLength(),
          2 );
  printf( "The answer is %d, but should be %d\n",
          1 + (new OneMoreString ( "Mercury", mt ))->listLength(),
          2 );
  printf( "The answer is %d, but should be %d\n",
          1 + 1 + mt->listLength(),
          2 );
  printf( "The answer is %d, but should be %d\n",
          1 + 1 + (new EmptyListOfString ())->listLength(),
          2 );
  printf( "The answer is %d, but should be %d\n",
          1 + 1 + 0,
          2 );
  printf( "The answer is %d, but should be %d\n",
          1 + 1,
          2 );
  printf( "The answer is %d, but should be %d\n",
          2,
          2 );

  printf("Look up!\n");

  printf( "The answer is %d, but should be %d\n",
          5*2,
          10 );
  printf( "The answer is %d, but should be %d\n",
          mt->listLength(),
          0 );
  printf( "The answer is %d, but should be %d\n",
          andMercury->listLength(),
          1 );
  printf( "The answer is %d, but should be %d\n",
          andVenus->listLength(),
          2 );

  printf( "The answer is %d, but should be %d\n",
          mt->howManyLetters(),
          0 );
  printf( "The answer is %d, but should be %d\n",
          andMercury->howManyLetters(),
          7 + 0 );
  printf( "The answer is %d, but should be %d\n",
          andVenus->howManyLetters(),
          5 + 7 + 0 );

  printf( "The answer is %d, but should be %d\n",
          andEarth->listLength(),
          3 );
  printf( "The answer is %d, but should be %d\n",
          andBizzaroEarth->listLength(),
          3 );
  printf( "The answer is %d, but should be %d\n",
          andEarth2049->listLength(),
          3 );


  printf( "The answer is %d, but should be %d\n",
          theRealSS->listLength(),
          9 );
  printf( "The answer is %d, but should be %d\n",
          theRealSS->howManyLetters(),
          52 );

  printf( "The answer is %f, but should be %f\n",
          averageLength ( andVenus ),
          12.0 / 2.0 );
  printf( "The answer is %f, but should be %f\n",
          averageLength ( theRealSS ),
          52.0 / 9.0 );
  printf( "The answer is %f, but should be %f\n",
          averageLength ( mt ),
          0.0 / 0.0 );

  printf( "The answer is %d, but should be %d\n",
          theRealSS->hasPlanetLessThan4CharactersHuh(),
          false );
  printf( "The answer is %d, but should be %d\n",
          (new OneMoreString ( "Zed", theRealSS ))->hasPlanetLessThan4CharactersHuh(),
          true );
  printf( "The answer is %d, but should be %d\n",
          (new OneMoreString ( "Zedz", theRealSS ))->hasPlanetLessThan4CharactersHuh(),
          false );
  printf( "The answer is %d, but should be %d\n",
          (new OneMoreString ( "Zedz",  ( new OneMoreString( "Zed", theRealSS ) ) ))->hasPlanetLessThan4CharactersHuh(),
          true );

	
  printf("Look down!\n");

  printf( "The answer is %d, but should be %d\n",
          (new OneMoreString ( "Zed", theRealSS ))->hasPlanetLessThan4CharactersHuh(),
          true );
  printf( "The answer is %d, but should be %d\n",
          ( strlen("Zed") < 4
            || theRealSS->hasPlanetLessThan4CharactersHuh () ),
          true );
  printf( "The answer is %d, but should be %d\n",
          ( 3 < 4
            || theRealSS->hasPlanetLessThan4CharactersHuh () ),
          true );
  printf( "The answer is %d, but should be %d\n",
          ( true
            || theRealSS->hasPlanetLessThan4CharactersHuh () ),
          true );
  printf( "The answer is %d, but should be %d\n",
          // || is "short circuiting"
          ( true ),
          true );

  /*
	ListOfString* weirdSS = new OneMoreString( "weird", weirdSS );
	ListOfString* zedSS = new OneMoreString( "Zed", weirdSS );
	
	printf( "The answer is %d, but should be %d\n",
    zedSS->hasPlanetLessThan4CharactersHuh (),
    true );
  */

}
