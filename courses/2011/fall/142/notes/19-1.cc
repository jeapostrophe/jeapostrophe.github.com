#include <stdio.h>
#include <math.h>
#include <string.h>

// max : int int -> int
// Purpose: returns the bigger of the two integers
int max (int x , int y ) {
  if ( x > y ) {
    return x;
  } else {
    return y;
  }
}

// booleanToString : boolean -> string
// Purpose: convert a boolean into a string for printing
const char* booleanToString ( bool it ) {
  if ( it ) { return "true"; } else { return "false"; }
}

// streq : string string -> boolean
// Purpose: compares to strings for equality, use this rather than == to compare strings
bool streq ( const char* l, const char* r ) {
  return strcmp(l,r) == 0;
}

// changeHelper : string -> string
// Purpose: to compute the new color given the past color
const char* changeHelper ( const char* pastColor ) {
  // Template: pastColor

  if (streq(pastColor, "yellow")) {
    // Example
    // pastColor = yellow
    return "red";
  } else {
    if (streq(pastColor, "red")) {
      // Example
      // pastColor = red
      return "green";
    } else {
      // Example
      // pastColor = green
      return "yellow";
    }
  }
}

// The stop light is...
const char* theLightAtUniversity = "red";

// Global variable
//  or deadly sin #4

// change : nothing -> nothing
// Purpose: updates theLightAtUniversity to the new color
void change ( ) {
  // Template: theLightAtUniversity

  // Example
  // before:
  // theLightAtUniversity = red
  // arguments:
  // after:
  // theLightAtUniversity = green

  // Will not work
  //someLight = changeHelper(someLight);

  theLightAtUniversity = changeHelper(theLightAtUniversity);

  return ;
}

const char* theLightAtCanyon = "red";

// Get thee hence, Satan!
// I will not copy code.

// changeDifferentOne : nothing -> nothing
// Purpose: updates theLightAtUniversity to the new color
void changeDifferentOne ( ) {
  // Template: theLightAtUniversity

  // Example
  // before:
  // theLightAtUniversity = red
  // arguments:
  // after:
  // theLightAtUniversity = green

  // Will not work
  //someLight = changeHelper(someLight);

  theLightAtCanyon = changeHelper(theLightAtCanyon);

  return ;
}

// A StopLight is a...
//  new StopLight ( )
// where
// and there is...
//  color is a string
class StopLight {
private:
  const char* color;

public:
  StopLight ( ) {
    this->color = "red";
  }

  const char* getColor () { return this->color; }
  void setColor( const char* newColor ) {
    if ( streq(newColor, "red") || streq(newColor, "green") || streq(newColor, "yellow")) {
      this->color = newColor;
      return ;
    } else {
      return ;
    }
  }

  // change : StopLight -> void
  // Purpose: to update the color of this stoplight according to the stop light law
  void change () {
    // Template: this, this->color

    // Example
    // PRE
    //  this->color = red
    // ARGUMENTS
    // ANSWER
    // POST
    //  this->color = green
    this->color = changeHelper(this->color);

    return ;
  }
};

// main : -> number
int main () {
  printf ( "The answer is %f, but should be %f\n",
           1.0/2.0,
           0.5 ) ;
  printf ( "C++ says %s\n",
           booleanToString(strcmp("Jay", "Libby") == 0)) ;
  printf ( "C++ says %s\n",
           booleanToString(streq("Jay", "Libby"))) ;


  printf ( "The answer is %s, but should be %s\n",
           changeHelper("green"),
           "yellow" ) ;
  printf ( "The answer is %s, but should be %s\n",
           changeHelper("red"),
           "green" ) ;
  printf ( "The answer is %s, but should be %s\n",
           changeHelper("yellow"),
           "red" ) ;


  // A light at canyon rd and center st
  // A light at university ave and center st

  // Pre condition
  theLightAtUniversity = "red";
  // Function call
  change();
  // Post condition
  printf ( "The answer is %s, but should be %s\n",
           theLightAtUniversity,
           "green" ) ;

  // Pre condition
  theLightAtUniversity = "green";
  // Function call
  change();
  // Post condition
  printf ( "The answer is %s, but should be %s\n",
           theLightAtUniversity,
           "yellow" ) ;
  
  // Pre condition
  theLightAtUniversity = "yellow";
  // Function call
  change();
  // Post condition
  printf ( "The answer is %s, but should be %s\n",
           theLightAtUniversity,
           "red" ) ;

  // Canyon rd

    // Pre condition
  theLightAtCanyon = "red";
  // Function call
  change();
  // Post condition
  printf ( "The answer is %s, but should be %s\n",
           theLightAtCanyon,
           "green" ) ;

  // Pre condition
  theLightAtCanyon = "green";
  // Function call
  change();
  // Post condition
  printf ( "The answer is %s, but should be %s\n",
           theLightAtCanyon,
           "yellow" ) ;
  
  // Pre condition
  theLightAtCanyon = "yellow";
  // Function call
  change();
  // Post condition
  printf ( "The answer is %s, but should be %s\n",
           theLightAtCanyon,
           "red" ) ;

  // Use stop light class
  StopLight* atUniversity = new StopLight();
  StopLight* atCanyon = new StopLight();

  // Pre condition
  atUniversity->setColor( "red");
  // Function call
  atUniversity->change();
  // Post condition
  printf ( "The answer is %s, but should be %s\n",
           atUniversity->getColor(),
           "green" ) ;

  // Pre condition
  atUniversity->setColor("green");
  // Function call
  atUniversity->change();
  // Post condition
  printf ( "The answer is %s, but should be %s\n",
           atUniversity->getColor(),
           "yellow" ) ;
  
  // Pre condition
  atUniversity->setColor("yellow");
  // Function call
  atUniversity->change();
  // Post condition
  printf ( "The answer is %s, but should be %s\n",
           atUniversity->getColor(),
           "red" ) ;

  // Pre condition
  atCanyon->setColor("yellow");
  // Function call
  atCanyon->change();
  // Post condition
  printf ( "The answer is %s, but should be %s\n",
           atCanyon->getColor(),
           "red" ) ;

  printf("Fused\n");

  // Pre condition
  atUniversity->setColor("yellow");
  atCanyon->setColor( "red");

  // Function call
  atUniversity->change();
  atCanyon->change();

  // Post condition
  printf ( "The answer is %s, but should be %s\n",
           atUniversity->getColor(),
           "red" ) ;
  printf ( "The answer is %s, but should be %s\n",
           atCanyon->getColor(),
           "green" ) ;

  // Classes represents abstractions
  // Abstractions have invariants
  // Mutation can break invariants

  // Pre condition
  atCanyon->setColor("yellow");
  atCanyon->setColor("purple");
  printf ( "The answer is %s, but should be %s\n",
           atCanyon->getColor(),
           "yellow" ) ;
  // Function call
  atCanyon->change();
  // Post condition
  printf ( "The answer is %s, but should be %s\n",
           atCanyon->getColor(),
           "red" ) ;


  return 0;
}

// Stop light: red -> green -> yellow -> back
// Test cases
// Two lights
// Class
// Invariants and privacy
// cycle
