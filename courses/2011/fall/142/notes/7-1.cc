#include <stdio.h>
#include <math.h>
#include <string.h>

// booleanToString : boolean -> string
const char* booleanToString ( bool it ) {
  if ( it ) { return "true"; } else { return "false"; }
}

// A Point is a 
// CHANGE 1: remove the underscore, insert a space (and update the program)
// new Point (x, y)
// where 
// - x is a double
// - y is a double
// CHANGE 2: replace "typedef struct" with class and add facial hair
class Point {
  // CHANGE 3: replace "typedef struct Guy {" with "public:"
public:
  double x;
  double y;
  // CHANGE 4: remove this facial, and put after all the Point stuff

  // CHANGE 5: remove the underscore in this contract:
  // Contract: new Point : double double -> Point*
  // CHANGE 6: delete "Kind* new_"
Point(double x0, double y0) {
  // CHANGE 7, remove the "Kind* k = new Kind()"
  // CHANGE 8, rename k to "this"
  this->x = x0;
  this->y = y0;
  // CHANGE 8a, remove return
}

// Contract: translate : Point* number number -> Point*
// Purpose: move the x, y coordinate by dx, dy
// CHANGE 9: remove "Kind* k" from the parameter list
Point* translate(double dx, double dy) {
  // CHANGE 10: rename k everywhere to "this"
  // Template: this, this->x, this->y, dx, dy
  
  // Example 1:
  // this->x = 3
  // this->y = 3
  // dx = 1
  // dy = -1
  // ans = new Point(4,2)
  // return new Point(4, 2);
  
  // Example 2:
  // this->x = -1
  // this->y = 5
  // dx = 3
  // dy = 5
  // ans = new Point(2, 10)
  // return new Point(2, 10);
  
  // Generalize 1 & 2
  return new Point(this->x + dx, this->y + dy);
}

// Contract: distanceToOrigin: Point* -> double
// Purpose: calculate the distance from the point to the origin
double distanceToOrigin() {
  // Template: this, this->x, this->y
  // Example 1:
  // this = new Point(3, 4)
  // ans = sqrt(3 * 3 + 4 * 4)
  
  // Example 2:
  // this = new Point(5, 9, 2)
  // ans = sqrt(5 * 5 + 9 * 9)
  
  // Generalize 1 & 2
  return sqrt(this->x * this->x + this->y * this->y);
}
  
  // distanceBetweenPoints : Point Point -> double
  double distanceBetweenPoints ( Point* t ) {
    return sqrt( pow((this->x - t->x), 2) + pow((this->y - t->y), 2) );
  }
};

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
  // thisGuy = new_UFO( new Point( 7000.0, 2000.0 ), 1000.0 )
  // thisGuy->posn = new Point( 7000.0, 2000.0 )
  // thisGuy->xvel = 1000.0
  // f ( thisGuy->posn ) = new Point( 8000.0, 2000.0 )
  // f ( new Point( 7000.0, 2000.0 ) ) = new Point( 8000.0, 2000.0 )
  // ans = new_UFO( new Point( 8000.0, 2000.0 ),x 1000.0 )
  // BEFORE
  // return new_UFO( translate((thisGuy->posn), thisGuy->xvel, 0.0 ), thisGuy->xvel );
  // AFTER
  return new_UFO( (thisGuy->posn)->translate(thisGuy->xvel, 0.0 ), thisGuy->xvel );
}

// UFOdistanceToProvo : UFO -> double
// Purpose: find distance to Provonia
double UFOdistanceToProvo ( UFO* hombre ) {
  // Template: hombre, hombre->posn, hombre->xvel
  
  return (hombre->posn)->distanceToOrigin();
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

// Homework excerpt, start!
typedef struct Cat;

typedef struct Cat {
  Point* pos;
  int happyness;
};

Cat* new_Cat ( Point* pos0, int h0 ) {
  Cat* c = new Cat();
  c->pos = pos0;
  c->happyness = h0;
  return c;
}

typedef struct Zoo;

typedef struct Zoo {
  Cat* lefty;
  Cat* righty;
};

Zoo* new_Zoo ( Cat* l0, Cat* r0 ) {
  Zoo* z = new Zoo();
  z->lefty = l0;
  z->righty = r0;
  return z;
}

Cat* jump ( Cat* c ) {
  return new_Cat((c->pos)->translate(0, 10), c->happyness);
}

Zoo* jumpLeftUnsafe( Zoo* z ) {
  return new_Zoo(jump(z->lefty),z->righty);
}

// twoCatsAreTouchingHuh : Cat Cat -> bool
bool twoCatsAreTouchingHuh ( Cat* o, Cat* t ) {
  // Template: o, t, o->pos, t->pos, o->happyness, t->happyness
  
  return (o->pos)->distanceBetweenPoints( t->pos ) <= 10;
}

// catsAreTouchingHuh : Zoo -> bool
// Purpose: to compute if the cats are touching
bool catsAreTouchingHuh ( Zoo* z ) {
  // Template: z, z->lefty, z->righty
  
  return twoCatsAreTouchingHuh( z->lefty, z->righty );
}

Zoo* jumpLeft( Zoo* z ) {
  // Template: z, z->lefty, z->righty
  
  Zoo* afterwards = jumpLeftUnsafe(z);
  
  // After jumping:
  if ( ! catsAreTouchingHuh(afterwards) ) {
    // Example 1
    // the cats don't land each other after a jump
    return afterwards;
  } else {
    // Example 2
    // the cats do
    return z;
  }
}
// Homework excerpt, end!

// main : -> number
int main () {
  printf ( "The answer is %f, but should be %f\n",
          1.0/2.0,
          0.5 ) ;
  printf ( "C++ says %s\n",
          booleanToString(strcmp("Jay", "Libby") == 0)) ;
  
  // Function call looks like...
  //  f( arg1, arg2, ..., argN )
  // New special function calls look like...
  //  arg1->f( arg2, ..., argN )
  
  printf ( "The answer is (%f, %f), but should be (%f, %f)\n",
          ((new Point(3, 3))->translate(1, -1))->x,
          (new Point(3, 3))->translate( 1, -1)->y,
          (new Point(4,2))->x, (new Point(4,2))->y) ;
  
  Point* p = (new Point(-1, 5))->translate(3, 5);
  
  printf ( "The answer is (%f, %f), but should be (%f, %f)\n",
          p->x,
          p->y,
          (new Point(2,10))->x, (new Point(2,10))->y) ;
  
  printf ( "The answer is %f, but should be %f\n",
          (new Point(3,4))->distanceToOrigin(),
          5.0) ;
  printf ( "The answer is %f, but should be %f\n",
          (new Point(5,9))->distanceToOrigin(),
          10.29) ;
  
  UFO* et = new_UFO( new Point( 7000.0, 2000.0 ), 1000.0 );
  UFO* etGoneHome = new_UFO( new Point( 8000.0, 2000.0 ), 1000.0 );
  
  UFO* ans4 = tick( et );
  printf ( "The answer is (%f,%f) (going %f Pm/Ps), but should be (%f,%f) (going %f Pm/Ps)\n",
          ans4->posn->x, ans4->posn->y, ans4->xvel,
          etGoneHome->posn->x, etGoneHome->posn->y, etGoneHome->xvel );
  printf ( "The answer is\n  (%f,%f) (going %f Pm/Ps)\n, but should be\n  (%f,%f) (going %f Pm/Ps)\n",
          ans4->posn->x, ans4->posn->y, ans4->xvel,
          etGoneHome->posn->x, etGoneHome->posn->y, etGoneHome->xvel );
  /*
   UFO* ans4 = tick( et );
   UFO* ans4 = tick( new_UFO( new Point( 7000.0, 2000.0 ), 1000.0 ) );
   
   [ thisGuy = new_UFO( new Point( 7000.0, 2000.0 ), 1000.0 ) ]
   return new_UFO( translate( thisGuy->posn, thisGuy->xvel, 0.0 ), thisGuy->xvel );
   
   return new_UFO( translate( new_UFO( new Point( 7000.0, 2000.0 ), 1000.0 )->posn, thisGuy->xvel, 0.0 ), thisGuy->xvel );
   
   return new_UFO( translate( new_UFO( new Point( 7000.0, 2000.0 ), 1000.0 )->posn, new_UFO( new Point( 7000.0, 2000.0 ), 1000.0 )->xvel, 0.0 ), new_UFO( new Point( 7000.0, 2000.0 ), 1000.0 )->xvel );
   
   return new_UFO( translate( new Point( 7000.0, 2000.0 ), 1000.0, 0.0 ), 1000.0 );
   */
  
  printf("The answer is\n  %f\n, but should be\n  %f\n",
         (et->posn)->distanceToOrigin(),
         7280.109 );
  printf("The answer is\n  %f\n, but should be\n  %f\n",
         UFOdistanceToProvo(et),
         7280.109 );
  
  BookEntry* celineDione = 
  new_BookEntry( "Celly", 
                new_PhoneNumber( 777, "453-8211" ),
                new_PhoneNumber( 666, "499-8211" ),
                new_PhoneNumber( 777, "453-8212" ) );
  
  printf("The answer is\n  %s\n, but should be\n  %s\n",
         booleanToString(allInSameAreaHuh(celineDione)),
         booleanToString(false) );
  
  BookEntry* bizzaroCelineDione = 
  new_BookEntry( "Celly", 
                new_PhoneNumber( 777, "453-8211" ),
                new_PhoneNumber( 777, "499-8211" ),
                new_PhoneNumber( 777, "453-8212" ) );
  
  printf("The answer is\n  %s\n, but should be\n  %s\n",
         booleanToString(allInSameAreaHuh(bizzaroCelineDione)),
         booleanToString(true) );

  // DONE (compound data =) CLASSes & (functions =) METHODs
  // TODO ->equals
  // TODO ->show
  
  return 0;
}
