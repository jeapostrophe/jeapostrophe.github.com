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

  int show () {
    return printf("new Toy(\"%s\", %f)", this->name, this->price);
  }

  // addYourPriceToThisThingKudasai : Toy num -> num
  double addYourPriceToThisThingKudasai ( double priceSoFar ) {
    // Template: this, this->name, this->price

    // Example
    // this = (Eddie, 10)
    // this->name = Eddie
    // this->price = 10
    // priceSoFar = 8700
    // return 8710
    // return 8700 + 10
    // return priceSoFar + 10
    // return priceSoFar + this->price
    return priceSoFar + this->price;
  }

  // isRenesmeInsideYouHuh : Toy -> bool
  // Purpose: returns true if the toy is a figure, that contains Renesme (i.e. Renee or Bella)
  bool isRenesmeInsideYouHuh ( ) {
    // Template : this, this->name, this->price

    // Example:
    // this = (Eddie, 10)
    //return false;

    // Example:
    // this = (Bella, 10k)
    //return true;

    // Generalize
    //return streq(this->name, "Bella");

    // Example
    // this = (Renee, 250)
    //return streq(this->name, "Renee");

    // Generalize
    return streq(this->name, "Bella") || streq(this->name, "Renee");
  }

};

// An Inventory is
//  EmptyInv
//  OneMoreToy
class Inventory {
public:
  // totalValue : Inventory -> double
  virtual double totalValue ( ) = 0;
  virtual int show () = 0; 
  // containsRenesmeHuh : inv -> bool
  // Purpose: it returns true, if the list contains a figure, that contains Renesme (i.e. Renee or Bella)
  virtual bool containsRenesmeHuh () = 0;
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

  // containsRenesmeHuh : inv -> bool
  // Purpose: it returns true, if the list contains a figure, that contains Renesme (i.e. Renee or Bella)
  bool containsRenesmeHuh () {
    // Template: this, this->first, this->rest, this->rest->containsRenesmeHuh()

    // Example:
    // this = (Bella, 10k) : (Eddie, 10) : (Jacob, 7000) : (Rene,1000) : (Bayo,700) : !
    // this->first =  (Bella, 10k)
    // this->rest = (Eddie, 10) : (Jacob, 7000) : (Rene,1000) : (Bayo,700) : !
    // this->rest->containsRenesmeHuh() = false
    //return true;
    //return cmb(this->first, this->rest->containsRenesmeHuh());

    return this->first->isRenesmeInsideYouHuh() || this->rest->containsRenesmeHuh();
  }

  // totalValue : Inventory -> double
  double totalValue ( ) {
    // Template: this, this->first, this->rest, this->rest->totalValue()

    // Example:
    // this = (Eddie, 10) : (Jacob, 7000) : (Rene,1000) : (Bayo,700) : !
    // this->first = new Toy ("Eddie", 10)
    // this->rest = (Jacob, 7000) : (Rene,1000) : (Bayo,700) : !
    // this->rest->totalValue() = 8700
    // return 8710.0;
    // return 8700 + 10;
    // return this->rest->totalValue() + 10;
    // return SAF(this->rest->totalValue(), this->first);
    // return SAFp(this->first, this->rest->totalValue());
    // return this->first->SAFp(this->rest->totalValue());
    // return this->first->addYourPriceToThisThingKudasai(this->rest->totalValue());

    // Example
    // this = (Jacob, 7000) : (Rene,1000) : (Bayo,700) : !
    // this->first = (Jacob, 7000)
    // this->rest = (Rene,1000) : (Bayo,700) : !
    // this->rest->totalValue() = 1700
    // return 8700;
    // return 1700 + 7000;
    // return this->rest->totalValue() + 7000;
    // return SAF(this->rest->totalValue(), this->first);
    // return SAF(this->rest->totalValue(), this->first);
    // return SAFp(this->first, this->rest->totalValue());
    // return this->first->SAFp(this->rest->totalValue());
    return this->first->addYourPriceToThisThingKudasai(this->rest->totalValue());
  }
};

// Kinds of list functions

// Fold (homomorphism)
// a : b : c : !
// cmb(a, cmb(b, cmb(c, empty)))

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

  // containsRenesmeHuh : inv -> bool
  // Purpose: it returns true, if the list contains a figure, that contains Renesme (i.e. Renee or Bella)
  bool containsRenesmeHuh () {
    // Template: this

    // Example:
    // this = :(
    return false;
  }

  int show () {
    return printf("new EmptyInv()");
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
  Toy* bayoFig = new Toy( "Bayonetta", 1000.0 );
  Toy* renesmeFig = new Toy( "Renesme", 700.0 );
  Toy* jacobFig = new Toy( "Jacob", 7000.0 );
  Toy* eddieFig = new Toy( "Edward :'(", 10.0 );
  Toy* bellaFig = new Toy( "Bella", 10000.0 );
  
  Inventory* mtStore = new EmptyInv ();
  Inventory* bayoStore = new OneMoreToy ( bayoFig, mtStore );
  Inventory* renesmeStore = new OneMoreToy ( renesmeFig, bayoStore );
  Inventory* jacobStore = new OneMoreToy ( jacobFig, renesmeStore );
  Inventory* eddieStore = new OneMoreToy ( eddieFig, jacobStore );
  Inventory* bellaStore = new OneMoreToy ( bellaFig, eddieStore );

    eddieStore->show();
    printf("\n");

  // totalValue : Inventory -> double
    printf ( "The answer is %f, but should be %f\n",
             mtStore->totalValue(),
             0.0 ) ;
    printf ( "The answer is %f, but should be %f\n",
             jacobStore->totalValue(),
             8700.0 ) ;
    printf ( "The answer is %f, but should be %f\n",
             eddieStore->totalValue(),
             8710.0 ) ;

    // addYourPriceToThisThingKudasai : Toy num -> num
    printf ( "The answer is %f, but should be %f\n",
             bayoFig->addYourPriceToThisThingKudasai(99.99),
             1099.99 ) ;
    printf ( "The answer is %f, but should be %f\n",
             jacobFig->addYourPriceToThisThingKudasai(5000),
             12000.00 ) ;
    printf ( "The answer is %f, but should be %f\n",
             jacobFig->addYourPriceToThisThingKudasai(1700),
             8700.00 ) ;
    printf ( "The answer is %f, but should be %f\n",
             eddieFig->addYourPriceToThisThingKudasai(8700),
             8710.00 ) ;

    printf ( "The answer is %s, but should be %s\n",
             booleanToString(eddieStore->containsRenesmeHuh()),
             booleanToString(false) ) ;
    printf ( "The answer is %s, but should be %s\n",
             booleanToString(bellaStore->containsRenesmeHuh()),
             booleanToString(true) ) ;

  return 0;
}

//  inventory record (name, price)
//  total value : inv -> nat
//  containsRenesmeHuh : inv -> bool

// XXX priceOf : inv name -> nat
// Purpose: return the price of the figure named "name"
// priceOf(eddieStore, "Jacob") = 7000

// ask: this->first->areYouNamed(name)
// this->first->thenTellMeYourPrice()
// this->first->price

// this->first->ifYouAreNamedSuchAndSuchThenReturnYourPriceOtherwiseReturnThis( name, this->rest->priceOf(name) )

// XXX removeExpensive : inv -> inv
// Purpose: removes expensive items from the store
// this->first < 10
// this->first->price < 10
// this->first->areYouSoExpensiveThatMyNephewCantAffordYouBTWHisAllowanceIs10DollarsHuh()

// XXX all names : inv -> list-of-names
// Purpose: returns a list of the names of the toys

// XXX tax : inv -> inv
// Purpose: returns a list where the man has taxed each item by 10%

// toy -> toy
// this->first->taxYourself()
