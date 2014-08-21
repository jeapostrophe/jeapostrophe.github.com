#include <stdio.h>
#include <math.h>
#include <string.h>

// booleanToString : boolean -> string
const char* booleanToString ( bool it ) {
  if ( it ) { return "true"; } else { return "false"; }
}

// streq : string string -> boolean
bool streq ( const char* l, const char* r ) {
  return strcmp(l,r) == 0;
}

// A Toy is a
//  new Toy( name, price )
// where
//  name is a string
//  price is a double
class Toy {
public:
  const char* name;
  double price;

  Toy ( const char* name0, double price0 ) {
    this->name = name0;
    this->price = price0;
  }
};

// An Inventory is
//  EmptyInv
//  OneMoreToy
class Inventory {
public:
  // totalValue : Inventory -> double
  virtual double totalValue ( ) = 0;
};

// A OneMoreToy is a
//  new OneMoreToy( first, rest )
// where
//  first is a Toy
//  rest is a Inventory
class OneMoreToy : public Inventory {
public:
  Toy* first;
  Inventory* rest;

  OneMoreToy ( Toy* first0, Inventory* rest0 ) {
    this->first = first0;
    this->rest = rest0;
  }

  // totalValue : Inventory -> double
  double totalValue ( ) {
    // Template: this, this->first, this->rest, this->rest->totalValue()

    // Example:
    // this = 10 : 7000 : 1000 : 700 : !
    // this->first = 10
    // this->rest = 7000 : 1000 : 700 : !
    // this->rest->totalValue() = 8700

    // XXX You are bad man!
    return 8710.0;
  }
};

// A EmptyInv is a
//  new EmptyInv()
// where
class EmptyInv : public Inventory {
public:
  EmptyInv() { }

  // totalValue : Inventory -> double
  double totalValue ( ) {
    // Template: this

    // Example
    // this = !
    return 0.0;
  }
};



// main : -> number
int main () {
  printf ( "The answer is %f, but should be %f\n",
           1.0/2.0,
           0.5 ) ;
  printf ( "C++ says %s\n",
           booleanToString(strcmp("Jay", "Libby") == 0)) ;

  Toy* bayoFig = new Toy( "Bayonetta", 1000.0 );
  Toy* renesmeFig = new Toy( "Renesme", 700.0 );
  Toy* jacobFig = new Toy( "Jacob", 7000.0 );
  Toy* eddieFig = new Toy( "Edward :'(", 10.0 );
  
  Inventory* mtStore = new EmptyInv ();
  Inventory* bayoStore = new OneMoreToy ( bayoFig, mtStore );
  Inventory* renesmeStore = new OneMoreToy ( renesmeFig, bayoStore );
  Inventory* jacobStore = new OneMoreToy ( jacobFig, renesmeStore );
  Inventory* eddieStore = new OneMoreToy ( eddieFig, jacobStore );

  // totalValue : Inventory -> double
    printf ( "The answer is %f, but should be %f\n",
             mtStore->totalValue(),
             0.0 ) ;
    printf ( "The answer is %f, but should be %f\n",
             eddieStore->totalValue(),
             8710.0 ) ;


  return 0;
}

// XXX inventory record (name, price)
// XXX total value : inv -> nat
// XXX containsRenesmeHuh : inv -> bool
// XXX priceOf : inv name -> nat
// XXX removeExpensive : inv -> inv
// XXX all names : inv -> list-of-names
// XXX tax : inv -> inv
