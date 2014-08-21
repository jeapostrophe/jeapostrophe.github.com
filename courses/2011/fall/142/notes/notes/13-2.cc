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
//  new Toy( name, sex, price )
// where
//  name is a string
//  sex is a string
//  price is a double
class Toy {
public:
  const char* name;
  const char* sex;
  double price;

  Toy ( const char* name0, const char* sex0, double price0 ) {
    this->name = name0;
    this->sex = sex0;
    this->price = price0;
  }

  int show () {
    return printf("new Toy(\"%s\", \"%s\", %f)", this->name, this->sex, this->price);
  }

  // areYouALadyHuh : Toy -> boolean
  // Purpose: returns true if the Toy is a girl
  bool areYouALadyHuh ( )  {
    // Template: this, this->name, this->sex, this->price

    return streq(this->sex, "Girl");
  }

  // isThisYou : Toy string -> bool
  bool isThisYou ( const char* given ) {
    // Template: this, this->name, this->sex, this->price, given
    return streq(this->name, given);
  }

  // Overloading is dangerous because typos become real programs

  // cheep : Toy -> Toy
  Toy* cheep () {
    // Template: this, this->name, this->sex, this->price
    return new Toy (this->name, this->sex, this->price*0.5);
  }
  // cheep : Toy string -> Toy
  Toy* cheep ( const char* given ) {
    // Template: this, this->name, this->sex, this->price, given
    if ( streq(this->name, given)) {
      return new Toy (this->name, this->sex, this->price*0.5);
    } else {
      return this;
    }    
  }

};

// An Inventory is
//  EmptyInv
//  OneMoreToy
class Inventory {
public:
  virtual int show () = 0; 
  // justGirls : inv -> inv
  // Purpose: return a list that only has lady action figures in it
  virtual Inventory* justGirls () = 0;
  // reverse : Inventory -> Inventory
  // Purpose: to return an inventory at the second coming (where the first is last and the last is first)
  virtual Inventory* reverse () = 0;
  virtual Inventory* addAtEnd ( Toy* t ) = 0;
  // discount : Inventory name -> Inventory
  // Purpose: to return an inventory, where the given toy is half off
  virtual Inventory* discount ( const char* given ) = 0;
  // deescount : Inventory name -> Inventory
  // Purpose: to return an inventory, where the given toy is half off
  virtual Inventory* deescount ( const char* given ) = 0;
};

// Sometimies peoples call OneMoreToy a "cons"
// These people also call addAtEnd a "snoc"

class OneMoreToy : public Inventory {
public:
  Toy* first;
  Inventory* rest;

  OneMoreToy ( Toy* first0, Inventory* rest0 ) ;

  int show () ;

  // justGirls : OneMoreToy -> inv
  // Purpose: return a list that only has lady action figures in it
  Inventory* justGirls () ;

  // "n squered function"
  // n^2 where n represents the number of things in the list
  // reverse does n^2 work

  // reverse : Inventory -> Inventory
  // Purpose: to return an inventory at the second coming (where the first is last and the last is first)
  Inventory* reverse () ;

  Inventory* addAtEnd ( Toy* t ) ;

  // discount : Inventory name -> Inventory
  // Purpose: to return an inventory, where the given toy is half off
  Inventory* discount ( const char* given ) ;

  // deescount : Inventory name -> Inventory
  // Purpose: to return an inventory, where the given toy is half off
  Inventory* deescount ( const char* given ) ;
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

  // justGirls : EmptyInv -> inv
  // Purpose: return a list that only has lady action figures in it
  Inventory* justGirls () {
    // Template: this

    // Example:
    // this = !
    return (new EmptyInv());
  }

  // reverse : Inventory -> Inventory
  // Purpose: to return an inventory at the second coming (where the first is last and the last is first)
  Inventory* reverse () {
    // Template: this

    // Example
    // this = !
    return (new EmptyInv());
  }

  Inventory* addAtEnd ( Toy* t ) {
    return (new OneMoreToy( t, new EmptyInv( ) ));
  }

  // discount : Inventory name -> Inventory
  // Purpose: to return an inventory, where the given toy is half off
  Inventory* discount ( const char* given ) {
    // Template: this, given

    // Example
    // this = !
    // given = Bayonetta
    return (new EmptyInv ());
  }

  // deescount : Inventory name -> Inventory
  // Purpose: to return an inventory, where the given toy is half off
  Inventory* deescount ( const char* given ) {
    // Template: this, given

    // Example
    // this = !
    // given = Bayonetta
    return (new EmptyInv ());
  }
};

// A OneMoreToy is a
//  new OneMoreToy( first, rest )
// where
//  first is a Toy
//  rest is a Inventory
OneMoreToy::OneMoreToy ( Toy* first0, Inventory* rest0 ) {
    this->first = first0;
    this->rest = rest0;
  }

  int OneMoreToy::show () {
    printf("new OneMoreToy( ");
    this->first->show();
    printf(", ");
    this->rest->show();
    return printf(" )");
  }

  // justGirls : OneMoreToy -> inv
  // Purpose: return a list that only has lady action figures in it
  Inventory* OneMoreToy::justGirls () {
    // Template: this, this->first, this->rest, this->rest->justGirls()

    // Filtering function structure
    if ( this->first->areYouALadyHuh() ) {
      return new OneMoreToy( this->first, this->rest->justGirls() );
    } else {
      return this->rest->justGirls();
    }
  }

  // "n squered function"
  // n^2 where n represents the number of things in the list
  // reverse does n^2 work

  // reverse : Inventory -> Inventory
  // Purpose: to return an inventory at the second coming (where the first is last and the last is first)
  Inventory* OneMoreToy::reverse () {
    // Template: this, this->first, this->rest, this->rest->reverse()

    // Example
    // this = bella, eddie, jacob, renesme, bayo
    // this->first = bella
    // this->rest = eddie, jacob, renesme, bayo
    // this->rest->reverse() = bayo, renesme, jacob, eddie
    // return bayo, renesme, jacob, eddie, bella
    // return bayo, renesme, jacob, eddie, this->first
    // return this->rest->reverse(), this->first
    return (this->rest->reverse())->addAtEnd( this->first );
  }

  Inventory* OneMoreToy::addAtEnd ( Toy* t ) {
    return new OneMoreToy( this->first, this->rest->addAtEnd(t) );
  }

  // discount : Inventory name -> Inventory
  // Purpose: to return an inventory, where the given toy is half off
  Inventory* OneMoreToy::discount ( const char* given ) {
    // Template: this, this->first, this->rest, this->rest->discount(...), given

    if (this->first->isThisYou(given)){
    // Example
    // this = bayo, rene, jacob, eddie, bella, !
    // this->first = bayo
    // this->rest = rene, jacob, eddie, bella, !
    // given = Bayonetta
    // this->rest->discount(given) = rene, jacob, eddie, bella, !
    //return cheepbayo, rene, jacob, eddie, bella, !;
    return new OneMoreToy(this->first->cheep(), this->rest->discount(given));
    } else {
    // Example
    // this = rene, jacob, eddie, bella, !
    // this->first = bayo
    // this->rest = jacob, eddie, bella, !
    // given = Bayonetta
    // this->rest->discount(given) = jacob, eddie, bella, !
    // return rene, jacob, eddie, bella, !
    return new OneMoreToy(this->first, this->rest->discount(given));
    }
  }

  // deescount : Inventory name -> Inventory
  // Purpose: to return an inventory, where the given toy is half off
  Inventory* OneMoreToy::deescount ( const char* given ) {
    // Template: this, this->first, this->rest, this->rest->deescount(...), given

    return new OneMoreToy(this->first->cheep(given), this->rest->deescount(given));
  }

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

  // justGirls : inv -> inv
  // Purpose: return a list that only has lady action figures in it
  printf("The answer is\n  ");
  bellaStore->justGirls()->show();
  printf("\nbut it should be\n  ");
  (new OneMoreToy ( bellaFig, new OneMoreToy ( renesmeFig, new OneMoreToy ( bayoFig, mtStore ) ) ))->show();
  printf("\n");

  // areYouALadyHuh : Toy -> boolean
  // Purpose: returns true if the Toy is a girl
  // XXX You should do examples

  // reverse : Inventory -> Inventory
  // Purpose: to return an inventory at the second coming (where the first is last and the last is first)
  printf("The answer is\n  ");
  bellaStore->reverse()->show();
  printf("\nbut it should be\n  ");
  (new OneMoreToy ( bayoFig, new OneMoreToy ( renesmeFig, new OneMoreToy ( jacobFig, new OneMoreToy ( eddieFig, new OneMoreToy ( bellaFig, mtStore ) ) ) ) ))->show();
  printf("\n");

  // discount : Inventory name -> Inventory
  // Purpose: to return an inventory, where the given toy is half off
  printf("The answer is\n  ");
  bellaStore->reverse()->discount("Bayonetta")->show();
  printf("\nbut it should be\n  ");
  (new OneMoreToy ( new Toy( "Bayonetta", "Girl", 500.0 ), new OneMoreToy ( renesmeFig, new OneMoreToy ( jacobFig, new OneMoreToy ( eddieFig, new OneMoreToy ( bellaFig, mtStore ) ) ) ) ))->show();
  printf("\n");
  
  printf("The answer is\n  ");
  bellaStore->reverse()->deescount("Bayonetta")->show();
  printf("\nbut it should be\n  ");
  (new OneMoreToy ( new Toy( "Bayonetta", "Girl", 500.0 ), new OneMoreToy ( renesmeFig, new OneMoreToy ( jacobFig, new OneMoreToy ( eddieFig, new OneMoreToy ( bellaFig, mtStore ) ) ) ) ))->show();
  printf("\n");

  return 0;
}

// justGirls : inv -> inv
// reverse : inv -> inv
// XXX discount : inv name -> inv
