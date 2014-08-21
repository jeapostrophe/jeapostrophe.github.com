#include <stdio.h>
#include <stdlib.h>
#include <math.h>

// A Person is a
//   new_Person ( firstName, lastName, cuteness, eyeColor, height, weight, isAlive )
// where
//  - firstName is a const char*
//  - lastName is a const char*
//  - cuteness is a double [0,100]
//  - eyeColor is a const char*
//  - height is a double (inchers)
//  - weight is a double (pounders)
//  - isAlive is a bool
typedef struct Person;

typedef struct Person {
  const char* firstName;
  const char* lastName;
  double cuteness;
  const char* eyeColor;
  double height;
  double weight;
  bool isAlive;
};

Person* new_Person (const char* firstName0, const char* lastName0, double cuteness0, const char* eyeColor0, double height0, double weight0, bool isAlive0 ) {
  Person* p = new Person();
  p->firstName = firstName0;
  p->lastName = lastName0;
  p->cuteness = cuteness0;
  p->eyeColor = eyeColor0;
  p->height = height0;
  p->weight = weight0;
  p->isAlive = isAlive0;
  return p;
}

// A Posn is a 
//   new_Posn ( x, y )
// where
//  - x is a double
//  - y is a double
typedef struct Posn;

typedef struct Posn {
  double x;
  double y;
};

Posn* new_Posn ( double x0, double y0 ) {
  Posn* p = new Posn();
  p->x = x0;
  p->y = y0;
  return p;
}

// moveTheGuy : Posn double double -> Posn
// Computes the new position of the guy after moving dx in the x direction and dy in the y dir.
// Examples:
//  moveTheGuy( new_Posn( 2, 2 ), 1, -1 ) = new_Posn (3, 1)
Posn* moveTheGuy ( Posn* p, double dx, double dy ) {
  // return .... ;
  // Take stock
  // return ... p ... dx ... dy ; // <- We know this from Posn p
  // return new Posn ( ... p.x ... p.y ... dx ... dy ); // <- We know this from Posn moveTheGuy
  // return new Posn ( ... p.x ... p.y ... dx ... dy, ... p.x ... p.y ... dx ... dy );	
  return new_Posn ( p->x + dx, p->y + dy );	
}

// distanceToOrigin : Posn -> double
// Compute the distance of the posn from the origin (0,0)
// Examples:
//  distanceToOrigin ( new_Posn ( 0, 3 ) ) = 3
//  distanceToOrigin ( new_Posn ( 2, 0 ) ) = 2
//  distanceToOrigin ( new_Posn ( 3, 4 ) ) = 5
double distanceToOrigin ( Posn* p ) {
  // return ... p.x ... p.y ... ;
  return sqrt(pow(p->x,2) + pow(p->y,2)) ;
}

// willMarry : Person -> bool
// Computes whether the person should be married (to you!)
// Examples:
//  willMarry(jay) = true;
//  willMarry(tom) = false;
bool willMarry ( Person* candidate ) {
  // Take stock!
  /*
	return ... candidate->firstName ... candidate->lastName ...
    candidate->cuteness ... candidate->weight ... candidate->height ...
    candidate->eyeColor ... candidate->isAlive ...;
  */
  return (candidate->cuteness >= 80.35) ;
}

// C++ doesn't let us return two things.
// moveTheGuy : double double double double -> double double
// Computes the new position of the guy after moving dx in the x direction and dy in the y dir.
// Examples:
//  moveTheGuy( 2, 2, 1, -1 ) = 3 1
/*
  double double moveTheGuy ( double x, double y, double dx, double dy ) {
  // Return twice?
  return x + dx;
  return y + dy;
	
  //return (x + dx) (y + dy) ;
  // return ... x ... y ... dx ... dy ;
  }
*/

// Atomic data means contains one thing: numbers, bools, strings

// Compound data means contains a few things: a number AND a number, a string AND a number

int main () {	
  Posn* p0 = new_Posn ( 2.54, 7.1 ) ;
  Posn* p1 = new_Posn ( 2.0, sqrt(2) ) ;
  Posn* pMario = new_Posn ( 2.0, 2.0 ) ;

  printf("The answer is: %s\n", "Amaaaaaazing");
  // printf("The answer is: %Posn\n", p0);
  printf("The answer is: (%f,%f)\n", p0->x, p0->y);
  printf("The answer is: (%f,%f)\n", p1->x, p1->y);
  printf("The answer is: (%f,%f)\n", pMario->x, pMario->y);
	
  printf("The answer is: (%f,%f) but should be (%f,%f)\n", 
         moveTheGuy( new_Posn( 2, 2 ), 1, -1 )->x,
         moveTheGuy( new_Posn( 2, 2 ), 1, -1 )->y,
         new_Posn (3, 1)->x,
         new_Posn (3, 1)->y);

  printf("The answer is: (%f,%f) but should be (%f,%f)\n", 
         moveTheGuy( pMario, 1, -1 )->x,
         moveTheGuy( pMario, 1, -1 )->y,
         new_Posn (3, 1)->x,
         new_Posn (3, 1)->y);

  Posn* pMarioKillinAGoomba = new_Posn (3, 1);
  printf("The answer is: (%f,%f) but should be (%f,%f)\n", 
         moveTheGuy( pMario, 1, -1 )->x,
         moveTheGuy( pMario, 1, -1 )->y,
         pMarioKillinAGoomba->x,
         pMarioKillinAGoomba->y);

  Posn* pMarioAfterMovin = moveTheGuy( pMario, 1, -1 );
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

  Person* jay = new_Person ( "Jay", "McCarthy", 100.00, "Bleu", 71.0, 140.0, true );
  Person* tom = new_Person ( "Tom", "Thumb", 75.0, "Brown", 3.0, 1.0, false );
  // Uncomment and see what these doooze!
  // printf("The answer is: %f\n", jay->cutness);
  // printf("The answer is: %f\n", moveTheGuy(jay, 4.0, 5.0));
  printf("The answer is: %f\n", jay->cuteness);
  printf("The answer is: %f\n", tom->cuteness);

  printf("The answer is: %d but should be %d\n",
         willMarry(jay), true);
  printf("The answer is: %d but should be %d\n",
         willMarry(tom), false);

  /*
	printf("The answer is %d\n",
    if ( 4 > 3 ) {
    return 4;
    } else { 
    return 5;
    });
  */
  printf("The answer is %d\n",
         (4 > 3) ? 4 : 5);
  printf("The answer is %d\n",
         ((4 > 3) ? 4 : 5)+90);
  // sign(x) == 0

}

