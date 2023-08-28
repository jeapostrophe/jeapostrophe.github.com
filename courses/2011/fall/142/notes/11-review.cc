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

// A ListOfNums is either
//  OneMoreNum
//  EmptyNL
class ListOfNums {
public:
  // show : ListOfNums -> int
  virtual int show () = 0;
  // insert : listofnums[sorted] num -> listofnums[sorted]
  virtual ListOfNums* insert ( int newGuy ) = 0;
  // sort : listofnums -> listofnums[sorted]
  virtual ListOfNums* sort ( ) = 0;
};

// A OneMoreNum is a
//  new OneMoreNum ( first, rest )
// where
//  first is a int
//  rest is a ListOfNums
class OneMoreNum : public ListOfNums {
public:
  int first;
  ListOfNums* rest;

  OneMoreNum ( int first0, ListOfNums* rest0 ) {
    this-> first = first0;
    this-> rest = rest0;
  }

  // show : OneMoreNum -> int
  int show ( ) {
    // Template: this, this->first, this->rest, this->rest->show()
    printf ("new OneMoreNum ( %d, ", this->first);
    this->rest->show();
    printf (" )");
    return 0;
  }

  // insert : OneMoreNum num -> listofnums[sorted]
  ListOfNums* insert ( int newGuy ) {
    // Template: this, this->first, this->rest, this->rest->insert( ... ), newGuy

    if ( newGuy > this->first ) {
    // Example
    // this = 12 : !
    // this->first = 12
    // this->rest = !
    // this->rest->insert( ??? ) = ??? : !
    // newGuy = 42
    // return 12 : 42 : !
    //return (new OneMoreNum( 12, new OneMoreNum( 42, new EmptyNL())));
      //return (new OneMoreNum( this->first, new OneMoreNum( newGuy, this->rest)));

    // Example
    // this = 12 : 13 : !
    // this->first = 12
    // this->rest = 13 : !
    // newGuy = 15
    // this->rest->insert( 11 ) = 11 : 13 : !
    // this->rest->insert( 99 ) = 13 : 99 : !
    // this->rest->insert( 15 ) = 13 : 15 : !
    // this->rest->insert( newGuy ) = 13 : 15 : !
    // return 12 : 13 : 15 : !
    // existing return is 12 : 15 : 13 : !
    return (new OneMoreNum( this->first, this->rest->insert( newGuy ) ));

    } else {
    // Example
    // this = 12 : !
    // this->first = 12
    // this->rest = !
    // this->rest->insert( ??? ) = ??? : !
    // newGuy = 3
    // return 3 : 12 : !
    //return (new OneMoreNum( 3, new OneMoreNum( 12, new EmptyNL())));
    return (new OneMoreNum( newGuy, new OneMoreNum( this->first, this->rest)));
    }
  }

  // sort : OneMoreNum -> listofnums[sorted]
  ListOfNums* sort ( ) {
    // Template: this, this->first, this->rest, this->rest->sort()

    // Example:
    // this = 3 : 90 : 12 : !
    // this->first = 3
    // this->rest = 90 : 12 : !
    // this->rest->sort() = 12 : 90 : !
    // return 3 : 12 : 90 : !
    //return new OneMoreNum( this->first, this->rest->sort() );

    // Example
    // this = 90 : 12 : !
    // this->first = 90
    // this->rest = 12 : !
    // this->rest->sort() = 12 : !
    // return 12 : 90 : !
    return this->rest->sort()->insert( this->first ) ;
  }
};

// An EmptyNL is a
//  new EmptyNL ()
// where
class EmptyNL : public ListOfNums {
public:

  EmptyNL ( ) {
  }

  // show : EmptyNL -> int
  int show ( ) {
    // Template: this
    printf ( "new EmptyNL ()" );
    return 0;
  }

  // insert : EmptyNL num -> listofnums[sorted]
  ListOfNums* insert ( int newGuy ) {
    // Template: this, newGuy

    // Example
    // this = !
    // newGuy = 42
    // return 42 : !
    //return (new OneMoreNum(42, new EmptyNL()));
    return (new OneMoreNum(newGuy, new EmptyNL()));
  }

  // sort : EmptyNL -> listofnums[sorted]
  ListOfNums* sort ( ) {
    // Template : this

    // Example
    // this = !
    // return !
    return (new EmptyNL());
  }

};

// main : -> number
int main () {
  printf ( "The answer is %f, but should be %f\n",
           1.0/2.0,
           0.5 ) ;
  printf ( "C++ says %s\n",
           booleanToString(strcmp("Jay", "Libby") == 0)) ;

  ListOfNums* mtStore = new EmptyNL () ;
  ListOfNums* dennaStore = new OneMoreNum ( 12, mtStore ) ;
  ListOfNums* danStore = new OneMoreNum ( 90, dennaStore ) ;
  ListOfNums* jayStore = new OneMoreNum ( 3, danStore ) ;

  printf("The answer is \n  ");
  jayStore->show();
  printf("\nbut should be \n  ");
  jayStore->show();
  printf("\n");

  // insert

  printf("The answer is \n  ");
  mtStore->insert(42)->show();
  printf("\nbut should be \n  ");
  (new OneMoreNum(42, (new EmptyNL())))->show();
  printf("\n");

  printf("The answer is \n  ");
  dennaStore->insert(42)->show();
  printf("\nbut should be \n  ");
  (new OneMoreNum(12, (new OneMoreNum(42, (new EmptyNL())))))->show();
  printf("\n");

  printf("The answer is \n  ");
  dennaStore->insert(3)->show();
  printf("\nbut should be \n  ");
  (new OneMoreNum(3, (new OneMoreNum(12, (new EmptyNL())))))->show();
  printf("\n");

  printf("The answer is \n  ");
  (new OneMoreNum(12, (new OneMoreNum(42, (new EmptyNL())))))->insert(15)->show();
  printf("\nbut should be \n  ");
  (new OneMoreNum(12, (new OneMoreNum(15, (new OneMoreNum(42, (new EmptyNL())))))))->show();
  printf("\n");

  printf("The answer is \n  ");
  (new OneMoreNum(12, (new OneMoreNum(13, (new EmptyNL())))))->insert(15)->show();
  printf("\nbut should be \n  ");
  (new OneMoreNum(12, (new OneMoreNum(13, (new OneMoreNum(15, (new EmptyNL())))))))->show();
  printf("\n");

  // sort

  printf("The answer is \n  ");
  // !
  mtStore->sort()->show();
  printf("\nbut should be \n  ");
  // !
  mtStore->show();
  printf("\n");

  printf("The answer is \n  ");
  // 90 : 12 : !
  danStore->sort()->show();
  printf("\nbut should be \n  ");
  // 12 : 90 : !
  (new OneMoreNum(12, (new OneMoreNum(90, (new EmptyNL())))))->show();
  printf("\n");

  printf("The answer is \n  ");
  // 3 : 90 : 12 : !
  jayStore->sort()->show();
  printf("\nbut should be \n  ");
  // 3 : 12 : 90 : !
  (new OneMoreNum(3, (new OneMoreNum(12, (new OneMoreNum(90, (new EmptyNL())))))))->show();
  printf("\n");

  return 0;
}
