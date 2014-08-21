#include <stdio.h>
#include <math.h>
#include <string.h>

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

// A Toy is a
//  new Toy( name, gender, price )
// where
//  name is a string
//  gender is a string
//  price is a double
class Toy {
public:
  const char* name;
  const char* gender;
  double price;

  Toy ( const char* name0, const char* gender0, double price0 ) {
    this->name = name0;
    this->gender = gender0;
    this->price = price0;
  }

  int show () {
    return printf("new Toy(\"%s\", \"%s\", %f)", this->name, this->gender, this->price);
  }

  // NOTE: This was written with an earlier version and is agnostic to additional fields
  // returnThePriceOfThisWithFailureValue : Toy name price -> price
  // Purpose: return the price of the toy named 'target' or the failure value
  double returnThePriceOfThisWithFailureValue ( const char* target, double failure ) {
    // Template: this, this->name, this->price, target, failure

    if (! streq(this->name, target)) {
    // Example:
    // this = eddieFig
    // this->name = "Eddie :'("
    // this->price = 10.00
    // target = "Renesme"
    // failure = 700.00
    //return 700.00;
    return failure;
    } else {
    // Example:
    // this = bellaFig
    // this->name = "Bella"
    // this->price = 10000.00
    // target = "Bella"
    // failure = 99999.99
    //return 10000.00;
    return this->price;
    }
  }
};

// An Inventory is
//  EmptyInv
//  OneMoreToy
class Inventory {
public:
  virtual int show () = 0; 
  // whatsItsPrice : inv name -> double
  // Purpose: returns the price of the item in the store, if it is available, otherwise tells a joke
  virtual double whatsItsPrice ( const char* name ) = 0;
  // justGirls : inv -> inv
  // Purpose: return a list that only has lady action figures in it
  virtual Inventory* justGirls () = 0;
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

  int show () {
    printf("new OneMoreToy( ");
    this->first->show();
    printf(", ");
    this->rest->show();
    return printf(" )");
  }

  // whatsItsPrice : OneMoreToy name -> double
  // Purpose: returns the price of the item in the store, if it is available, otherwise tells a joke
  double whatsItsPrice ( const char* name ) {
    // Template: this, this->first, this->rest, name, this->rest->whatsItPrice( ... )

    // Example:
    // this = eddieFig : jacobFig : renesmeFig : bayoFig : !
    // this->first = eddieFig
    // this->rest = jacobFig : renesmeFig : bayoFig : !
    // name = "Renesme"
    // this->rest->whatsItsPrice(name) = 700.00
    //return 700.00;
    //return this->rest->whatsItsPrice(name);

    // Example
    // this = bellaFig : eddieFig : jacobFig : renesmeFig : bayoFig : !
    // this->first = bellaFig = new Toy( "Bella", 10000.00 )
    // this->rest = eddieFig : jacobFig : renesmeFig : bayoFig : !
    // name = "Bella"
    // this->rest->whatsItsPrice(name) = 99999.99
    // return 10000.00
    //return SAF(this->first);

    // Generalize, but we need name to distinguish inside of SDAF
    return this->first->returnThePriceOfThisWithFailureValue(name, this->rest->whatsItsPrice(name));
    // return cmb(this->first, arg0, ..., argN, this->rest->f(arg0, ..., argN))
    // Folding function
  }

  // justGirls : OneMoreToy -> inv
  // Purpose: return a list that only has lady action figures in it
  Inventory* justGirls () {
    // Template: this, this->first, this->rest, this->rest->justGirls()

    // Example
    // XXX Jay you are a bad bad man
    return this;
  }
};

// A EmptyInv is a
//  new EmptyInv()
// where
class EmptyInv : public Inventory {
public:
  EmptyInv() { }

  int show () {
    return printf("new EmptyInv()");
  }

  // whatsItsPrice : EmptyInv name -> double
  // Purpose: returns the price of the item in the store, if it is available, otherwise tells a joke
  double whatsItsPrice ( const char* name ) {
    // Template: this, name

    // Example:
    // this = !
    // name = Bella
    return 99999.99;
  }

  // justGirls : EmptyInv -> inv
  // Purpose: return a list that only has lady action figures in it
  Inventory* justGirls () {
    // Template: this

    // Example:
    // this = !
    return (new EmptyInv());
  }

};


// main : -> number
int main () {
  printf ( "The answer is %f, but should be %f\n",
           1.0/2.0,
           0.5 ) ;
  printf ( "C++ says %s\n",
           booleanToString(strcmp("Jay", "Libby") == 0)) ;

  // Nerd dot com action figure store
  Toy* bayoFig = new Toy( "Bayonetta", "Girl", 1000.0 );
  Toy* renesmeFig = new Toy( "Renesme", "Girl", 700.0 );
  Toy* jacobFig = new Toy( "Jacob", "Werewolf", 7000.0 );
  Toy* eddieFig = new Toy( "Edward :'(", "Guy", 10.0 );
  Toy* bellaFig = new Toy( "Bella", "Girl", 10000.0 );
  
  Inventory* mtStore = new EmptyInv ();
  Inventory* bayoStore = new OneMoreToy ( bayoFig, mtStore );
  Inventory* renesmeStore = new OneMoreToy ( renesmeFig, bayoStore );
  Inventory* jacobStore = new OneMoreToy ( jacobFig, renesmeStore );
  Inventory* eddieStore = new OneMoreToy ( eddieFig, jacobStore );
  Inventory* bellaStore = new OneMoreToy ( bellaFig, eddieStore );

    eddieStore->show();
    printf("\n");

    printf ( "The answer is %f, but should be %f\n",
             eddieFig->returnThePriceOfThisWithFailureValue("Renesme", 700.00 ),
             700.00 ) ;
    printf ( "The answer is %f, but should be %f\n",
             bellaFig->returnThePriceOfThisWithFailureValue("Bella", 99999.99),
             10000.00 ) ;


    // whatsItsPrice : inv name -> double
    printf ( "The answer is %f, but should be %f\n",
             mtStore->whatsItsPrice("Renesme"),
             99999.99 ) ;
    printf ( "The answer is %f, but should be %f\n",
             eddieStore->whatsItsPrice("Renesme"),
             700.0 ) ;
    printf ( "The answer is %f, but should be %f\n",
             bellaStore->whatsItsPrice("Bella"),
             10000.0 );
    printf ( "The answer is %f, but should be %f\n",
             bellaStore->whatsItsPrice("Jay Action Figure"),
             99999.99 );

    // justGirls : inv -> inv
    // Purpose: return a list that only has lady action figures in it
    printf("The answer is\n  ");
    bellaStore->justGirls()->show();
    printf("\nbut it should be\n  ");
    (new OneMoreToy ( bellaFig, new OneMoreToy ( renesmeFig, new OneMoreToy ( bayoFig, mtStore ) ) ))->show();
    printf("\n");

  return 0;
}

// whatsItsPrice : inv name -> double
// XXX justGirls : inv -> inv
// XXX reverse : inv -> inv
// XXX discount : inv name -> inv
