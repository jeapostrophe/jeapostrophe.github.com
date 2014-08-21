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
  // Two options
  double xvel ;
  //double yvel ;
  // or
  //Point* vel ;
  // because Points ARE two doubles
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

// UFOdistanceToProvo : UFO -> double
// Purpose: find distance to Provonia
double UFOdistanceToProvo ( UFO* hombre ) {
  // Template: hombre, hombre->posn, hombre->xvel

  return distanceToOrigin( hombre->posn );
}

// PhoneNumber : area prefix-line
// A PhoneNumber is a...
//  new_PhoneNumber( area, prefix-line )
// where
//  area is a three-digit number
//  prefix-line is a string
typedef struct PhoneNumber;

typedef struct PhoneNumber {
  int area;
  const char* prefix_line;
};

// Switch to another file with C-x C-f

PhoneNumber* new_PhoneNumber( int area0, const char* prefix_line0 ) {
  PhoneNumber* pn = new PhoneNumber();
  pn->area = area0;
  pn->prefix_line = prefix_line0;
  return pn;
}

// BookEntry : name cell office home
// A BookEntry is a ....
//  new_BookEntry( name, cell, office, home )
// where
//  name is a string
//  cell is a PhoneNumber
//  office is a PhoneNumber
//  home is a PhoneNumber
typedef struct BookEntry;

typedef struct BookEntry {
  const char* name;
  PhoneNumber* cell;
  PhoneNumber* office;
  PhoneNumber* home;
};

BookEntry* new_BookEntry( const char* name0, PhoneNumber* cell0, PhoneNumber* office0, PhoneNumber* home0 ) {
  BookEntry* be = new BookEntry();
  be -> cell = cell0;
  be -> office = office0;
  be -> home = home0;
  return be;
}

// areaSameAs : PN PN -> bool
// Purpose: obeys hygeine
bool areaSameAs ( PhoneNumber* fst, PhoneNumber* snd ) {
  // Template: fst, snd, fst->area, snd->area, fst->prefix_line, snd->prefix_line

  return fst->area == snd->area ;
}

// allInSameAreaHuh : BookEntry -> bool
bool allInSameAreaHuh ( BookEntry* be ) {
  // Template: be, be->name, be->cell, be->office, be->home

  // Test 1...
  // name = Celly
  // cell = 777-...-8211
  // office = 666-...-8211
  // home = 777-...-8212
  //return false ;

  // Test 1...
  // name = Bizzaro-Celly
  // cell = 777-...-8211
  // office = 777-...-8211
  // home = 777-...-8212
  //return true ;
  
  // Bad
  /*
  be->cell->area == be->office->area
    &&
  be->cell->area == be->home->area
  */

  // Better
  /*
    cellArea = phoneNumberArea( be->cell );
  areaSameAs( be->office, cellArea ) && areaSameAs( be->home, cellArea )
  */

  // Best
  return areaSameAs( be->office, be->cell ) && areaSameAs( be->home, be->cell );
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

 // DONE ball

 UFO* et = new_UFO( new_Point( 7000.0, 2000.0 ), 1000.0 );
 UFO* etGoneHome = new_UFO( new_Point( 8000.0, 2000.0 ), 1000.0 );
 
 UFO* ans4 = tick( et );
 printf ( "The answer is (%f,%f) (going %f Pm/Ps), but should be (%f,%f) (going %f Pm/Ps)\n",
          ans4->posn->x, ans4->posn->y, ans4->xvel,
          etGoneHome->posn->x, etGoneHome->posn->y, etGoneHome->xvel );
 printf ( "The answer is\n  (%f,%f) (going %f Pm/Ps)\n, but should be\n  (%f,%f) (going %f Pm/Ps)\n",
          ans4->posn->x, ans4->posn->y, ans4->xvel,
          etGoneHome->posn->x, etGoneHome->posn->y, etGoneHome->xvel );
 /*
    UFO* ans4 = tick( et );
    UFO* ans4 = tick( new_UFO( new_Point( 7000.0, 2000.0 ), 1000.0 ) );

    [ thisGuy = new_UFO( new_Point( 7000.0, 2000.0 ), 1000.0 ) ]
      return new_UFO( translate( thisGuy->posn, thisGuy->xvel, 0.0 ), thisGuy->xvel );

      return new_UFO( translate( new_UFO( new_Point( 7000.0, 2000.0 ), 1000.0 )->posn, thisGuy->xvel, 0.0 ), thisGuy->xvel );

      return new_UFO( translate( new_UFO( new_Point( 7000.0, 2000.0 ), 1000.0 )->posn, new_UFO( new_Point( 7000.0, 2000.0 ), 1000.0 )->xvel, 0.0 ), new_UFO( new_Point( 7000.0, 2000.0 ), 1000.0 )->xvel );

      return new_UFO( translate( new_Point( 7000.0, 2000.0 ), 1000.0, 0.0 ), 1000.0 );
  */

 // DONE tick substitution

 // DONE ball_distance

 printf("The answer is\n  %f\n, but should be\n  %f\n",
        distanceToOrigin(et->posn),
        7280.109 );
 printf("The answer is\n  %f\n, but should be\n  %f\n",
        UFOdistanceToProvo(et),
        7280.109 );
        
 // TODO Phone number (area, extension)
 // TODO Cell phone entry (name, cell, home, office)

 BookEntry* celineDione = new_BookEntry( "Celly", 
                                         new_PhoneNumber( 777, "453-8211" ),
                                         new_PhoneNumber( 666, "499-8211" ),
                                         new_PhoneNumber( 777, "453-8212" ) );
 
 printf("The answer is\n  %s\n, but should be\n  %s\n",
        booleanToString(allInSameAreaHuh(celineDione)),
        booleanToString(false) );

 BookEntry* bizzaroCelineDione = new_BookEntry( "Celly", 
                                         new_PhoneNumber( 777, "453-8211" ),
                                         new_PhoneNumber( 777, "499-8211" ),
                                         new_PhoneNumber( 777, "453-8212" ) );
 
 printf("The answer is\n  %s\n, but should be\n  %s\n",
        booleanToString(allInSameAreaHuh(bizzaroCelineDione)),
        booleanToString(true) );
                                         

 // TODO all in area?

 return 0;
}
