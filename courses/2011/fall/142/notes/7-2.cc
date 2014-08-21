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

  // show : Point -> int
  int show ( ) {
    // Template: this, this->x, this->y
    return printf("(%f,%f)", this->x, this->y);
  }
};

// A UFO is a...
//  new UFO ( posn, xvel )
// where
//  posn is a Point
//  xvel is a double

class UFO {
public:
  Point* posn ;
  // Two options
  double xvel ;
  //double yvel ;
  // or
  //Point* vel ;
  // because Points ARE two doubles

UFO ( Point* posn0, double xvel0 ) {
  this->posn = posn0;
  this->xvel = xvel0;
}

  // Emacs-fu : M-x replace-string <enter> string to replace <enter> string to replace with <enter>
  // To be asked: M-% will ask instead of do them all

// tick : UFO -> UFO
// Purpose: to compute the new UFO after one Planck second
UFO* tick ( ) {
  // Template: this, this->posn, this->xvel, 
  // Note on the side: this->posn is a Point, so we can call translate(this->posn, ...) and distanceToOrigin( this->posn ) 
  
  // Test 1
  // this = new_UFO( new Point( 7000.0, 2000.0 ), 1000.0 )
  // this->posn = new Point( 7000.0, 2000.0 )
  // this->xvel = 1000.0
  // f ( this->posn ) = new Point( 8000.0, 2000.0 )
  // f ( new Point( 7000.0, 2000.0 ) ) = new Point( 8000.0, 2000.0 )
  // ans = new_UFO( new Point( 8000.0, 2000.0 ),x 1000.0 )
  // BEFORE
  // return new_UFO( translate((this->posn), this->xvel, 0.0 ), this->xvel );
  // AFTER
  return (new UFO( (this->posn)->translate(this->xvel, 0.0 ), this->xvel ));
}

// distanceToOrigin : UFO -> double
// Purpose: find distance to Provonia
double distanceToOrigin ( ) {
  // Template: this, this->posn, this->xvel
  
  return (this->posn)->distanceToOrigin();
}

  // show : UFO -> int
  int show () {
    // Template: this, this->posn, this->xvel
    printf("Hovering at ");
    this->posn->show();
    printf(", going %f pm/ps",
           this->xvel );
    return 0;
  }
};

// PhoneNumber : area prefix-line
// A PhoneNumber is a...
//  new PhoneNumber( area, prefix-line )
// where
//  area is a three-digit number
//  prefix-line is a string
class PhoneNumber {

public:
  int area;
  const char* prefix_line;

// Switch to another file with C-x C-f

PhoneNumber( int area0, const char* prefix_line0 ) {
  this->area = area0;
  this->prefix_line = prefix_line0;
}

// areaSameAs : PN PN -> bool
// Purpose: obeys hygeine
bool areaSameAs ( PhoneNumber* snd ) {
  // Template: this, snd, this->area, snd->area, this->prefix_line, snd->prefix_line
  
  return this->area == snd->area ;
}

};

// BookEntry : name cell office home
// A BookEntry is a ....
//  new BookEntry( name, cell, office, home )
// where
//  name is a string
//  cell is a PhoneNumber
//  office is a PhoneNumber
//  home is a PhoneNumber
class BookEntry {

public:
  const char* name;
  PhoneNumber* cell;
  PhoneNumber* office;
  PhoneNumber* home;

  BookEntry( const char* name0, PhoneNumber* cell0, PhoneNumber* office0, PhoneNumber* home0 ) {
    this -> name = name0;
    this -> cell = cell0;
    this -> office = office0;
    this -> home = home0;
  }

  // allInSameAreaHuh : BookEntry -> bool
  bool allInSameAreaHuh ( ) {
    // Template: this, this->name, this->cell, this->office, this->home
  
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
  
    return (this->office)->areaSameAs( this->cell ) && (this->home)->areaSameAs( this->cell );
  }
};

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
  
  UFO* et = (new UFO( new Point( 7000.0, 2000.0 ), 1000.0 ));
  UFO* etGoneHome = (new UFO( new Point( 8000.0, 2000.0 ), 1000.0 ));
  
  // f(x) _______ x->f()
  //      o     o

  // launchTheRocketsAtCanadaIfThisNumberIsEqualTo42( translate(p,0,10)->y )
  // launchTheRocketsAtCanadaIfThisNumberIsEqualTo42( p->translate(0,10)->y )
  UFO* ans4 = et->tick(  );
  printf ( "The answer is (%f,%f) (going %f Pm/Ps), but should be (%f,%f) (going %f Pm/Ps)\n",
          ans4->posn->x, ans4->posn->y, ans4->xvel,
          etGoneHome->posn->x, etGoneHome->posn->y, etGoneHome->xvel );
  printf ( "The answer is\n  (%f,%f) (going %f Pm/Ps)\n, but should be\n  (%f,%f) (going %f Pm/Ps)\n",
          ans4->posn->x, ans4->posn->y, ans4->xvel,
          etGoneHome->posn->x, etGoneHome->posn->y, etGoneHome->xvel );
  /*
   UFO* ans4 = tick( et );
   UFO* ans4 = tick( new_UFO( new Point( 7000.0, 2000.0 ), 1000.0 ) );
   
   [ this = new_UFO( new Point( 7000.0, 2000.0 ), 1000.0 ) ]
   return new_UFO( translate( this->posn, this->xvel, 0.0 ), this->xvel );
   
   return new_UFO( translate( new_UFO( new Point( 7000.0, 2000.0 ), 1000.0 )->posn, this->xvel, 0.0 ), this->xvel );
   
   return new_UFO( translate( new_UFO( new Point( 7000.0, 2000.0 ), 1000.0 )->posn, new_UFO( new Point( 7000.0, 2000.0 ), 1000.0 )->xvel, 0.0 ), new_UFO( new Point( 7000.0, 2000.0 ), 1000.0 )->xvel );
   
   return new_UFO( translate( new Point( 7000.0, 2000.0 ), 1000.0, 0.0 ), 1000.0 );
   */
  
  printf("The answer is\n  %f\n, but should be\n  %f\n",
         (et->posn)->distanceToOrigin(),
         7280.109 );
  printf("The answer is\n  %f\n, but should be\n  %f\n",
         et->distanceToOrigin(),
         7280.109 );
  /*
  ||||||||
  || \  ||
  ||o--<|| "Help, I'm trapped in Emacs, how do I quit again?"
  || /  ||
  ||||||||
  */
  BookEntry* celineDione = 
    (new BookEntry( "Celly", 
                    (new PhoneNumber( 777, "453-8211" )),
                    (new PhoneNumber( 666, "499-8211" )),
                    (new PhoneNumber( 777, "453-8212" )) ));
  
  printf("The answer is\n  %s\n, but should be\n  %s\n",
         booleanToString(celineDione->allInSameAreaHuh()),
         booleanToString(false) );
  
  BookEntry* bizzaroCelineDione = 
    (new BookEntry( "Celly", 
                    (new PhoneNumber( 777, "453-8211" )),
                    (new PhoneNumber( 777, "499-8211" )),
                    (new PhoneNumber( 777, "453-8212" )) ));
  
  printf("The answer is\n  %s\n, but should be\n  %s\n",
         booleanToString(bizzaroCelineDione->allInSameAreaHuh()),
         booleanToString(true) );

  // DONE (compound data =) CLASSes & (functions =) METHODs
  // DONE ->show
  // DONE ->equals

  // == can compare numbers
  // strcmp can compare strings
  // ->equals
  // if ( p1->equals(p2) ) { launchthenukes } else { sendflowers }

  // DONE convert more

  printf("The point is........");
  et->posn->show();
  printf("\n");

  printf("The point is ");
  et->posn->translate(0,10)->show();
  printf(" but should be ");
  (new Point( 7000, 2010 ))->show();
  printf("\n");

  printf("The UFO is........");
  et->show();
  printf("\n");

  // TODO review substitution
  // Rule 1: Replace names with their values
  int x = 42;
  printf("The answer is %d\n",
         x + 5 );
  printf("The answer is %d\n",
         42 + 5 );

  int y = et->posn->y;
  printf("The answer is %d\n",
         y + 5 );
  printf("The answer is %d\n",
         et->posn->y + 5 );
  printf("The answer is %d\n",
         (new UFO( new Point( 7000.0, 2000.0 ), 1000.0 ))->posn->y + 5 );

  // Rule 2: Replace function calls with their bodies (and remember the arguments)
  // f(x) = x + 7
  // f(3) 
  //  = x + 7 [hey, x is 3]
  //  = 3 + 7
  
  return 0;
}
