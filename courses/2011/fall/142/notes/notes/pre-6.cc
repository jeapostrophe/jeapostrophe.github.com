#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// A Posn is a 
//   new_Posn ( x, y )
// where
//  - x is a double
//  - y is a double
typedef struct Posn;

typedef struct Posn{
  double x;
  double y;
};

Posn* new_Posn ( double x0, double y0 ) {
  Posn* p = new Posn();
  p->x = x0;
  p->y = y0;
  return p;
}

// A Ball is a
//   new_Ball ( pos, xvelocity )
// where
//   - pos is a Posn
//   - xvelocity is double [measured in Planck length / Planck time]
typedef struct Ball;

typedef struct Ball {
  Posn* pos;
  double xvelocity;
};

Ball* new_Ball ( Posn* pos0, double xvelocity0 ) {
  Ball* b = new Ball();
  b->pos = pos0;
  b->xvelocity = xvelocity0;
  return b;
}

// A PhoneNumber is a...
//   new_PhoneNumber ( areaCode, prefix, line )
// where
//   - areaCode is an int
//   - prefix is an int
//   - line is an int
typedef struct PhoneNumber;

typedef struct PhoneNumber {
  int areaCode;
  int prefix;
  int line;
};

PhoneNumber* new_PhoneNumber ( int areaCode0, int prefix0, int line0 ) {
  PhoneNumber* p = new PhoneNumber();
  p->areaCode = areaCode0;
  p->prefix = prefix0;
  p->line = line0;
  return p;
}

// An AddressBookEntry is a...
//   new_AddressBookEntry ( home, office, cell )
// where
//   - home is a PhoneNumber
//   - office is a PhoneNumber
//   - cell is a PhoneNumber
typedef struct AddressBookEntry;

typedef struct AddressBookEntry {
  /*
    int homeAreaCode;
    int homePrefix;
    int homeLine;
  */
  PhoneNumber* home;
  PhoneNumber* office;
  PhoneNumber* cell;
};

AddressBookEntry* new_AddressBookEntry ( PhoneNumber* home0, PhoneNumber* office0, PhoneNumber* cell0 ) {
  AddressBookEntry* a = new AddressBookEntry();
  a->home = home0;
  a->office = office0;
  a->cell = cell0;
  return a;
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
typedef struct TDN;

typedef struct TDN {
  int first;
  int second;
  int third;
};

TDN* new_TDN ( int first0, int second0, int third0 ) {
  TDN* t = new TDN();
  t->first = first0;
  t->second = second0;
  t->third = third0;
  return t;
}

// inAreaHuh : PhoneNumber int -> boolean
// Examples:
// inAreaHuh ( new PhoneNumber ( 978, 555, 1723 ), 978 ) = true
// inAreaHuh ( new PhoneNumber ( 978, 555, 1723 ), 801 ) = false
bool inAreaHuh ( PhoneNumber* pn, int someArea ) {
  // return ... pn->areaCode ... pn->prefix ... pn->line ... someArea ;
  return pn->areaCode == someArea ;
}

// allInAreaHuh : AddressBookEntry int -> boolean
// Purpose: determine if every phone number is in the same specific area
// Example:
//   allInAreaHuh ( new_AddressBookEntry ( new_PhoneNumber ( 978, 555, 1723 ),
//                                         new_PhoneNumber ( 978, 555, 9211 ),
//                                         new_PhoneNumber ( 978, 565, 9211 ) ),
//                  978 )
//   = true
bool allInAreaHuh ( AddressBookEntry* ae, int someArea ) {
  /*
	return ... ae->home ... ae->office ... ae->cell ... someArea ;
	return ... inAreaHuh( ... ae->home ...) ... inAreaHuh( ... ae->office ...) ... inAreaHuh( ... ae->cell ...) ... someArea ;
	return ... inAreaHuh( ae->home, someArea ) ... inAreaHuh( ae->office, someArea ) ... inAreaHuh( ae->cell, someArea ) ... someArea ;
  */
  return inAreaHuh( ae->home, someArea ) 
    && inAreaHuh( ae->office, someArea ) 
    && inAreaHuh( ae->cell, someArea );	
}

// translate : Posn double double -> Posn
// Computes the new position of the guy after moving dx in the x direction and dy in the y dir.
// Examples:
//  translate( new Posn( 2, 2 ), 1, -1 ) = new Posn (3, 1)
Posn* translate ( Posn* p, double dx, double dy ) {
  // return .... ;
  // Take stock
  // return ... p ... dx ... dy ; // <- We know this from Posn p
  // return new_Posn ( ... p.x ... p.y ... dx ... dy ); // <- We know this from Posn translate
  // return new_Posn ( ... p.x ... p.y ... dx ... dy, ... p.x ... p.y ... dx ... dy );	
  return new_Posn ( p->x + dx, p->y + dy );	
}

// distanceToOrigin : Posn -> double
// Compute the distance of the posn from the origin (0,0)
// Examples:
//  distanceToOrigin ( new_Posn ( 0, 3 ) ) = 3
//  distanceToOrigin ( new_Posn ( 2, 0 ) ) = 2
//  distanceToOrigin ( new_Posn ( 3, 4 ) ) = 5
double distanceToOrigin ( Posn* p ) {
  // return ... p->x ... p->y ... ;
  return sqrt(pow(p->x,2) + pow(p->y,2)) ;
}

/*
  Write a function called reveal that takes three arguments: a 'chosen' three digit number, a 'current' three digit number, and 'guess' integer. The function produces a new 'current' number where the digits are the same as before, unless the 'guess' is the same as one of the digits, in which case it is "revealed". For example,

  reveal( new_TDN(1, 2, 3), new_TDN(0, 0, 0), 1) = new_TDN( 1, 0, 0)
  reveal( new_TDN(1, 2, 3), new_TDN(1, 0, 0), 3) = new_TDN( 1, 0, 3)
  reveal( new_TDN(1, 2, 3), new_TDN(1, 0, 3), 6) = new_TDN( 1, 0, 3)
  reveal( new_TDN(1, 2, 3), new_TDN(1, 0, 3), 2) = new_TDN( 1, 2, 3)
  reveal( new_TDN(1, 2, 2), new_TDN(1, 0, 3), 2) = new_TDN( 1, 2, 2)
*/
// Contract: revealFirst : TDN TDN int -> TDN
// Purpose: like reveal, but only reveals the first digit
// Examples:
// revealFirst( new_TDN(1, 2, 3), new_TDN(0, 0, 0), 1) = new_TDN( 1, 0, 0)
// revealFirst( new_TDN(1, 2, 3), new_TDN(1, 0, 0), 3) = new_TDN( 1, 0, 0)
TDN* revealFirst ( TDN* chosen, TDN* current, int guess ) {
  // return ... ;
  // return ... chosen->first ... chosen->second ... chosen->third ... current->first ... current->second ... current->third .. guess ;
  /*
	return new_TDN ( ... chosen->first ... chosen->second ... chosen->third ... current->first ... current->second ... current->third .. guess,
    ... chosen->first ... chosen->second ... chosen->third ... current->first ... current->second ... current->third .. guess,
    ... chosen->first ... chosen->second ... chosen->third ... current->first ... current->second ... current->third .. guess ) ;
  */
  if ( chosen->first == guess ) {
    return new_TDN ( chosen->first,
                     current->second,
                     current->third) ;
  } else {
    return current;
    /*
      return new_TDN ( current->first,
      current->second,
      current->third) ;
    */
  }
}

static TDN* revealSecond ( TDN* chosen, TDN* current, int guess ) {
  if ( chosen->second == guess ) {
    return new_TDN ( current->first,
                     chosen->second,
                     current->third) ;
  } else {
    return current;
  }
}

static TDN* revealThird ( TDN* chosen, TDN* current, int guess ) {
  if ( chosen->third == guess ) {
    return new_TDN ( current->first,
                     current->second,
                     chosen->third) ;
  } else {
    return current;
  }
}

// Contract: reveal : TDN TDN int -> TDN
// Purpose: see above
TDN* reveal ( TDN* chosen, TDN* current, int guess ) {
  return revealThird( chosen,
                      revealSecond ( chosen,
                                     revealFirst ( chosen, current, guess ),
                                     guess ),
                      guess );
}

// physicsForPosns : Posn double -> Posn
// Purpose: Computes a new_Posn after an x movment of some units
// Examples:
//  physicsForPosns ( new_Posn ( 0, 0 ), 2.3 ) = new_Posn ( 2.3 , 0 )
//  physicsForPosns ( new_Posn ( 1, 0 ), 2.3 ) = new_Posn ( 3.3 , 0 )
Posn* physicsForPosns ( Posn* p, double xmovement ) {
  /*
    return ... p->x ... p->y ... xmovement ;
    return new_Posn ( ... p->x ... p->y ... xmovement,
    ... p->x ... p->y ... xmovement );
  */
  return new_Posn ( p->x + xmovement,
                    p->y );
}

// physics : Ball -> Ball
// Purpose: Computes the new_ball after one step of time (Planck time units)
// Example:
//  physics ( new_Ball ( new_Posn ( 0, 0 ), 2.3 ) ) =
//            new_Ball ( new_Posn ( 2.3, 0 ), 2.3 )
Ball* physics ( Ball* aBall ) {
  // return ... physicsForPosns( ... aBall->pos ... ) ... aBall->xvelocity ... ;
  /*
	return new_Ball ( ... physicsForPosns( ... aBall->pos ... ) ... aBall->xvelocity ...,
    ... physicsForPosns( ... aBall->pos ... ) ... aBall->xvelocity ... );
  */
  return new_Ball ( physicsForPosns( aBall->pos, aBall->xvelocity ),
                    aBall->xvelocity );

}

int main () {	
  Posn* pMario = new_Posn ( 2.0, 2.0 ) ;

  printf("The answer is: (%f,%f)\n", pMario->x, pMario->y);

  Posn* pMarioKillinAGoomba = new_Posn (3, 1);
  Posn* pMarioAfterMovin = translate( pMario, 1, -1 );
  printf("The answer is: (%f,%f) but should be (%f,%f)\n", 
         pMarioAfterMovin->x,
         pMarioAfterMovin->y,
         pMarioKillinAGoomba->x,
         pMarioKillinAGoomba->y);

  printf("The answer is: (%f,%f)\n", pMario->x, pMario->y);


  printf("The answer is: %f but should be %f\n", 
         distanceToOrigin ( new_Posn ( 0, 3 ) ),
         3.0);
  printf("The answer is: %f but should be %f\n", 
         distanceToOrigin ( new_Posn ( 2, 0 ) ),
         2.0);
  printf("The answer is: %f but should be %f\n", 
         distanceToOrigin ( new_Posn ( 3, 4 ) ),
         5.0);

  double fixedCost = 180;
  TDN* harry = new_TDN(1, 2, 3);
  TDN* hermione = new_TDN(0, 0, 0);
  TDN* snape = new_TDN( 1, 0, 0);

  printf("Harry is %d%d%d\n",
         harry->first,
         harry->second,
         harry->third);

  TDN* ex1after = revealFirst( new_TDN(1, 2, 3), new_TDN(0, 0, 0), 1);
  printf("Ex1 after is %d%d%d but should be 100\n",
         ex1after->first, ex1after->second, ex1after->third);
  TDN* ex2after = revealFirst( new_TDN(1, 2, 3), new_TDN(1, 0, 0), 3);
  printf("Ex2 after is %d%d%d but should be 100\n",
         ex2after->first, ex2after->second, ex2after->third);


  TDN* ex3after = reveal( new_TDN(1, 1, 1), new_TDN(0, 0, 0), 1);
  printf("Ex3 after is %d%d%d but should be 111\n",
         ex3after->first, ex3after->second, ex3after->third);

  Ball* steve = new_Ball ( new_Posn ( 0, 0 ), 2.3 );
  printf("Steve is at (%f,%f) and moving in the x-direction at %fu/t\n",
         (steve->pos)->x, (steve->pos)->y, steve->xvelocity);
  printf("Steve is at (%f,%f) and moving in the x-direction at %fu/t\n",
         steve->pos->x, steve->pos->y, steve->xvelocity);
  Posn* stevesposn = steve->pos;
  printf("Steve is at (%f,%f) and moving in the x-direction at %fu/t\n",
         stevesposn->x, stevesposn->y, steve->xvelocity);

  Ball* steveAfterPhysics = physics(steve);
  printf("Steve after physics is at (%f,%f) and moving in the x-direction at %fu/t\n",
         (steveAfterPhysics->pos)->x, (steveAfterPhysics->pos)->y, steveAfterPhysics->xvelocity);
  printf("But he should be at (2.3,0) and moving at 2.3u/t\n");

  printf("The answer is %d, but should be %d\n",
         allInAreaHuh ( new_AddressBookEntry ( new_PhoneNumber ( 978, 555, 1723 ),
                                               new_PhoneNumber ( 978, 555, 9211 ),
                                               new_PhoneNumber ( 978, 565, 9211 ) ),
                        978 ),
         true );
  printf("The answer is %d, but should be %d\n",
         allInAreaHuh ( new_AddressBookEntry ( new_PhoneNumber ( 978, 555, 1723 ),
                                               new_PhoneNumber ( 978, 555, 9211 ),
                                               new_PhoneNumber ( 978, 565, 9211 ) ),
                        801 ),
         false );
  printf("The answer is %d, but should be %d\n",
         allInAreaHuh ( new_AddressBookEntry ( new_PhoneNumber ( 978, 555, 1723 ),
                                               new_PhoneNumber ( 647, 555, 9211 ),
                                               new_PhoneNumber ( 978, 565, 9211 ) ),
                        978 ),
         false );

  printf("Test %d\n", 84604);

}
