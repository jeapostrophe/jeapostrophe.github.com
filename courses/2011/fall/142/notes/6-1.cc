#include <stdio.h>
#include <math.h>
#include <string.h>

// booleanToString : boolean -> string
const char* booleanToString ( bool it ) {
 if ( it ) { return "true"; } else { return "false"; }
}

// A Point is a 
// new_Point (x, y)
// where 
// - x is a double
// - y is a double
typedef struct Point;

typedef struct Point {
  double x;
  double y;
};

// Contract: new_Point : double double -> Point*
Point* new_Point(double x0, double y0) {
  Point* p = new Point();
  p->x = x0;
  p->y = y0;
  return p;
}

// Contract: translate : Point* number number -> Point*
// Purpose: move the x, y coordinate by dx, dy
Point* translate(Point* p, double dx, double dy) {
  // Template: p->x, p->y, dx, dy

  // Example 1:
  // p->x = 3
  // p->y = 3
  // dx = 1
  // dy = -1
  // ans = new_Point(4,2)
  // return new_Point(4, 2);

  // Example 2:
  // p->x = -1
  // p->y = 5
  // dx = 3
  // dy = 5
  // ans = new_Point(2, 10)
  // return new_Point(2, 10);

  // Generalize 1 & 2
  return new_Point(p->x + dx, p->y + dy);
}

// Contract: distanceToOrigin: Point* -> double
// Purpose: calculate the distance from the point to the origin
double distanceToOrigin(Point* p) {
  // Template: p->x, p->y
  // Example 1:
  // p = new_Point(3, 4)
  // ans = sqrt(3 * 3 + 4 * 4)

  // Example 2:
  // p = new_Point(5, 9, 2)
  // ans = sqrt(5 * 5 + 9 * 9)
  
  // Generalize 1 & 2
  return sqrt(p->x * p->x + p->y * p->y);
}

// A TDN is a...
//  new_TDN( fst, snd, thd )
// where
//  fst is an int
//  snd is an int
//  thd is an int

typedef struct TDN ;

typedef struct TDN {
  int fst;
  int snd;
  int thd;
};

TDN* new_TDN( int fst0, int snd0, int thd0 ) {
  TDN* t = new TDN();
  t->fst = fst0;
  t->snd = snd0;
  t->thd = thd0;
  return t;
}

// revealFirst : TDN TDN int -> TDN
// Purpose: challenge new students
TDN* revealFirst ( TDN* chosen , TDN* current, int guess ) {
  // Template: chosen, current, guess, chosen->fst, chosen->snd (int), chosen->thd, current->fst, current->snd, current->thd

  // Distinguish 1&2
  if ( guess == chosen->fst ) {
  // Example 1:
  // chosen = new_TDN( 3, 5, 4 );
  // current = new_TDN( 0, 0, 0 );
  // guess = 3
  //return new_TDN( 3, 0, 0 );
  
  // Generalize
  return new_TDN( guess, current->snd, current->thd );
  } else {
  // Example 2:
  // chosen = new_TDN( 3, 5, 4 );
  // current = new_TDN( 0, 0, 0 );
  // guess = 6
  return new_TDN( current->fst, current->snd, current->thd );
  }
}

// revealSecond : TDN TDN int -> TDN
// Purpose: challenge new students
TDN* revealSecond ( TDN* chosen , TDN* current, int guess ) {
  // Template: chosen, current, guess, chosen->fst, chosen->snd (int), chosen->thd, current->fst, current->snd, current->thd

  if ( guess == chosen->snd ) {
    return new_TDN( current->fst, guess, current->thd );
  } else {
    return new_TDN( current->fst, current->snd, current->thd );
  }
}

// revealThird : TDN TDN int -> TDN
// Purpose: challenge new students
TDN* revealThird ( TDN* chosen , TDN* current, int guess ) {
  // Template: chosen, current, guess, chosen->fst, chosen->snd (int), chosen->thd, current->fst, current->snd, current->thd

  if ( guess == chosen->thd ) {
    return new_TDN( current->fst, current->snd, guess );
  } else {
    return new_TDN( current->fst, current->snd, current->thd );
  }
}

TDN* reveal ( TDN* chosen , TDN* current, int guess ) {
  TDN* firstRevealed = revealFirst( chosen, current, guess );
  TDN* secondRevealed = revealSecond( chosen, firstRevealed, guess );
  TDN* thirdRevealed = revealThird( chosen, secondRevealed, guess );
  return thirdRevealed;
}

// A UFO is a...
//  new_UFO ( posn, xvel )
// where
//  posn is a Point
//  xvel is a double

typedef struct UFO;

typedef struct UFO {
  Point* posn ;
  double xvel ;
};

UFO* new_UFO ( Point* posn0, double xvel0 ) {
  UFO* u = new UFO();
  u->posn = posn0;
  u->xvel = xvel0;
  return u;
}

// tick : UFO -> UFO
// Purpose: to compute the new UFO after one Planck second
UFO* tick ( UFO* thisGuy ) {
  // Template: thisGuy, thisGuy->posn, thisGuy->xvel, 
  // Note on the side: thisGuy->posn is a Point, so we can call translate(thisGuy->posn, ...) and distanceToOrigin( thisGuy->posn ) 

  // Test 1
  // thisGuy = new_UFO( new_Point( 7000.0, 2000.0 ), 1000.0 )
  // thisGuy->posn = new_Point( 7000.0, 2000.0 )
  // thisGuy->xvel = 1000.0
  // f ( thisGuy->posn ) = new_Point( 8000.0, 2000.0 )
  // f ( new_Point( 7000.0, 2000.0 ) ) = new_Point( 8000.0, 2000.0 )
  // ans = new_UFO( new_Point( 8000.0, 2000.0 ), 1000.0 )
  return new_UFO( translate( thisGuy->posn, thisGuy->xvel, 0.0 ), thisGuy->xvel );
}

// main : -> number
int main () {
 printf ( "The answer is %f, but should be %f\n",
          1.0/2.0,
          0.5 ) ;
 printf ( "C++ says %s\n",
          booleanToString(strcmp("Jay", "Libby") == 0)) ;
 printf ( "The answer is (%f, %f), but should be (%f, %f)\n",
          translate(new_Point(3, 3), 1, -1)->x,
          translate(new_Point(3, 3), 1, -1)->y,
          new_Point(4,2)->x, new_Point(4,2)->y) ;

 Point* p = translate(new_Point(-1, 5), 3, 5);

 printf ( "The answer is (%f, %f), but should be (%f, %f)\n",
          p->x,
          p->y,
          new_Point(2,10)->x, new_Point(2,10)->y) ;

 printf ( "The answer is %f, but should be %f\n",
          distanceToOrigin(new_Point(3,4)),
          5.0) ;
 printf ( "The answer is %f, but should be %f\n",
          distanceToOrigin(new_Point(5,9)),
          10.29) ;

 // DONE reveal 
 TDN* badGuy = new_TDN( 6, 6, 6 );
 TDN* superJesus = new_TDN( 9, 9, 9 );
 TDN* easy = new_TDN( 1, 2, 3 );

 TDN* ans1 = revealFirst( new_TDN( 3, 5, 4 ),
                          new_TDN( 0, 0, 0 ),
                          3 );
 /*
   revealFirst( new_TDN( 3, 5, 4 ),
                new_TDN( 0, 0, 0 ),
                3 );
   >>   if ( guess == chosen->fst ) {
   >>   if ( 3 == new_TDN( 3, 5, 4 )->fst ) {
   >>   if ( 3 == 3 ) {
   >>   if ( true ) {
  return new_TDN( guess, current->snd, current->thd );
  return new_TDN( 3, new_TDN( 0, 0, 0 )->snd, new_TDN( 0, 0, 0 )->thd );
  return new_TDN( 3, 0, 0 );
 */
  
 printf ( "The answer is %d%d%d, but should be %d%d%d\n",
          ans1->fst, ans1->snd, ans1->thd,
          3, 0, 0 ) ;

 TDN* ans2 = reveal( new_TDN( 3, 5, 4 ),
                     new_TDN( 0, 0, 0 ),
                     6 );
 printf ( "The answer is %d%d%d, but should be %d%d%d\n",
          ans2->fst, ans2->snd, ans2->thd,
          0, 0, 0 ) ;

 TDN* ans3 = reveal( new_TDN( 6, 6, 6 ),
                     new_TDN( 0, 0, 0 ),
                     6 );
 printf ( "The answer is %d%d%d, but should be %d%d%d\n",
          ans3->fst, ans3->snd, ans3->thd,
          6,6,6 ) ;

 // TODO ball

 UFO* et = new_UFO( new_Point( 7000.0, 2000.0 ), 1000.0 );
 UFO* etGoneHome = new_UFO( new_Point( 8000.0, 2000.0 ), 1000.0 );
 
 UFO* ans4 = tick( et );
 printf ( "The answer is (%f,%f) (going %f Pm/Ps), but should be (%f,%f) (going %f Pm/Ps)\n",
          ans4->posn->x, ans4->posn->y, ans4->xvel,
          etGoneHome->posn->x, etGoneHome->posn->y, etGoneHome->xvel );

 // TODO tick
 // TODO ball_distance

 return 0;
}
