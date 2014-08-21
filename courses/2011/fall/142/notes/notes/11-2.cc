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

// A TrainThatCarriesPeople is either
//  a Car
//  or, a Kaboom
class TrainThatCarriesPeople {
public:
  // show : TrainThatCarriesPeople -> int
  virtual int show () = 0;
};

// A Car is
//  new Car ( Person, TheRestOfTheTrain )
// where
//  Person is a string
//  TheRestOfTheTrain is a TrainThatCarriesPeople
class Car : public TrainThatCarriesPeople {
public:
  const char* Person;
  TrainThatCarriesPeople* TheRestOfTheTrain;

  // string TrainThatCarriesPeople -> Car
  Car ( const char* Person0, TrainThatCarriesPeople* TheRestOfTheTrain0 ) {
    this->Person = Person0;
    this->TheRestOfTheTrain = TheRestOfTheTrain0;
  }

  // show : TrainThatCarriesPeople -> int
  int show () {
    printf("(new Car (\"%s\", ",
           this->Person);
    this->TheRestOfTheTrain->show();
    printf("))");
    
    //printf("%s", this->Person);
    //printf(" o--o ");
    return 0;
  }
};

// A Kaboom is
//  new Kaboom ()
// where
class Kaboom : public TrainThatCarriesPeople {
public:

  // -> Kaboom
  Kaboom ( ) {
  }

  // show : TrainThatCarriesPeople -> int
  int show () {
    //printf(" the train is over :'( ");
    printf("(new Kaboom ())");
    return 0;
  }
};

// A ListOfNums is either
//  OneMoreNum
//  EmptyNL
class ListOfNums {
public:
  // show : ListOfNums -> int
  virtual int show () = 0;
  // wages : ListOfNums -> ListOfNums
  // Purpose: find the wages of each employee if they make 12 an hour
  virtual ListOfNums* wages ( ) = 0;

  // A mapping function:
  // a : b : c : !
  // f(a) : f(b) : f(c) : ! 
  // for wages, f(x) = 12x
  // cmb(x,y) = new List(x, y)

  // evens : ListOfNums -> TrainThatCarriesPeople
  // Purpose: find the praise phrase for each worker
  virtual TrainThatCarriesPeople* evens ( ) = 0;

  // a mapping function

  // eliminateSlackers : ListOfNums -> ListOfNums
  // return a list with no slackers
  virtual ListOfNums* eliminateSlackers () = 0;

  // a filter function
  // a : b : c : !
  // returns a new list
  // b : c : ! s.t. everything in the list f(x) == true
  // f(x) = x > 10

  // payGoodGuys : ListOfNums -> ListOfNums
  virtual ListOfNums* payGoodGuys ( ) = 0;

  // filtering and mapping
  // a : b : c : !
  // returns
  // f(b) : f(c) : !
  // everything where p(x) == true

  // addAtEnd : ListOfNums Num -> ListOfNums
  // Purpose: return a new list with this number at the end (and everything in the list is still there)
  virtual ListOfNums* addAtEnd( int newGuy ) = 0;

  // An insertion

  // a : b : c : !
  // a : b : c : ??? : !

  // addBetween : ListOfNums Num -> ListOfNums
  // Purpose: return a new list with this number in between all the elements of the list and at the end
  virtual ListOfNums* addBetween( int comma ) = 0;

  // An append-map function
  // a : b : c : !
  // f(x) = x : comma : !

  // f(x) @ f(b) @ f(c)

  // (a_0:...:a_n:!) @ (b_0:...:b_n:!) = (a_0:...:a_n:b_0:...:b_n:!)
  // @ is called append
  
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

  // wages : OneMoreNum -> ListOfNums
  // Purpose: find the wages of each employee if they make 12 an hour
  ListOfNums* wages ( ) {
    // Template: this, this->first, this->rest, this->rest->wages()

    // Example
    // this = 3 : 90 : 12 : !
    // this->first = 3
    // this->rest = 90 : 12 : !
    // this->rest->wages() = 1080 : 144 : !
    // return 36 : 1080 : 144 : !
    return (new OneMoreNum ( this->first * 12, this->rest->wages() ));
  }

  // evens : OneMoreNum -> TrainThatCarriesPeople
  // Purpose: find the praise phrase for each worker
  TrainThatCarriesPeople* evens ( ) {
    // Template: this, this->first, this->rest, this->rest->evens()

    if ( (this->first % 2) == 1 ) {

    // Example 1
    // this = 3 : 90 : 12 : !
    // this->first = 3
    // this->rest = 90 : 12 : !
    // this->rest->evens() = "Get Bonus!" <:> "Get Bonus!" <:> <!>
    // return No Soup <:> Bonus <:> Bonus <:> <!>
    return (new Car( "No Soup For You!", this->rest->evens() ));
    } else {
    // Example 2
    // this = 90 : 12 : !
    // this->first = 90
    // this->rest = 12 : !
    // this->rest->evens() = "GB" <:> <!>
    // return GB <:> GB <:> <!>
    return (new Car( "Get Bonus!", this->rest->evens() ));
    }
  }

  // eliminateSlackers : OneMoreNum -> ListOfNums
  // return a list with no slackers
  ListOfNums* eliminateSlackers () {
    // Template: this, this->first, this->rest, this->rest->eliminateSlackers()

    if ( this->first < 10 ) {
    // Example
    // this = 3 : 90 : 12 : !
    // this->first = 3
    // this->rest : 90 : 12 : !
      //return this->rest;

    // Example
    // this = 4: 3 : 90 : 12 : !
    // this->first = 4 
    // this->rest = 3 : 90 : 12 : !
      return this->rest->eliminateSlackers();
    } else {
    // Example
    // this = 1000: 3 : 90 : 12 : !
    // this->first = 1000
    // this->rest = 3 : 90 : 12 : !
    // this->rest->eliminateSlackers() = 90 : 12 : !
    return new OneMoreNum( this->first, this->rest->eliminateSlackers() );
    }
  }

  // addAtEnd : ListOfNums Num -> ListOfNums
  // Purpose: return a new list with this number at the end (and everything in the list is still there)
  ListOfNums* addAtEnd( int newGuy ) {
    // Template: this, this->first, this->rest, this->rest->addAtEnd( ... ), newGuy

    // Example
    // this = 3 : 90 : 12 : !
    // newGuy = 1000
    // this->first = 3
    // this->rest = 90 : 12 : !
    // this->rest->addAtEnd( ??? ) = 90 : 12 : ??? : !
    // return 3 : 90 : 12 : 1000 : ! ;
    // return this->first : 90 : 12 : 1000 : ! ;
    // return this->first : 90 : 12 : newGuy : ! ;
    // return (new OneMoreNum( this->first, (new OneMoreNum( 90, (new OneMoreNum( 12, (new OneMoreNum(newGuy, (new EmptyNL ())))))))));
    // return this->first : this->rest->addAtEnd( newGuy )
    return (new OneMoreNum( this->first, this->rest->addAtEnd( newGuy ) ));
    
  }


  // f : ListOfNums arg1 ... argn -> something
  // something f ( kind1 arg1, ... ..., kindn argn ) {
  //  // Template: this, this->first, this->rest, this->rest->f( ... ), arg1, ... argn
  //  return ... ;
  // }

  // addBetween : ListOfNums Num -> ListOfNums
  // Purpose: return a new list with this number in between all the elements of the list and at the end
  ListOfNums* addBetween( int comma ) {
    // Template : this, this->first, this->rest, this->rest->addBetween( ... ), comma

    // Example:
    // this = 3 : 90 : 12 : 1000 : !
    // comma = 0
    // this->first = 3
    // this->rest = 90 : 12 : 1000 : !
    // this->rest->addBetween(???) = 90 : ??? : 12 : ??? : 1000 : ??? : !
    // return 3 : 0 : 90 : 0 : 12 : 0 : 1000 : 0 : !
    // return this->first : 0 : 90 : 0 : 12 : 0 : 1000 : 0 : !
    // return this->first : comma : 90 : 0 : 12 : 0 : 1000 : 0 : !
    // return this->first : comma : this->rest->addBetween(0)
    return (new OneMoreNum(this->first, (new OneMoreNum(comma, this->rest->addBetween(comma)))));
  }

  // payGoodGuys : ListOfNums -> ListOfNums
  ListOfNums* payGoodGuys ( ) {
    // Template: this, this->first, this->rest, this->rest->payGoodGuys()

    // A filtering function
    // if ( p(this->first) ) {
    //   return (new OneMoreNum( this->first, this->rest->payGoodGuys()));
    // } else {
    //   return this->rest->payGoodGuys();
    // }

    // A mapping function
    // return (new OneMoreNum( f(this->first), this->rest->payGoodGuys()));

    // Every filtering-mapping function
    // if ( p(this->first) ) {
    //   return (new OneMoreNum( f(this->first), this->rest->payGoodGuys()));
    // } else {
    //   return this->rest->payGoodGuys();
    // }

    if ( (this->first) > 10 ) {
      return (new OneMoreNum( (this->first)*12, this->rest->payGoodGuys()));
    } else {
      return this->rest->payGoodGuys();
    }

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

  // wages : EmptyNL -> ListOfNums
  // Purpose: find the wages of each employee if they make 12 an hour
  ListOfNums* wages ( ) {
    // Template: this

    // Example:
    // this = !
    return (new EmptyNL ());
  }

  // evens : EmptyNL -> TrainThatCarriesPeople
  // Purpose: find the praise phrase for each worker
  TrainThatCarriesPeople* evens ( ) {
    // Template: this

    // Example:
    // this = !
    return (new Kaboom ());
  }

  // eliminateSlackers : EmptyNL -> ListOfNums
  // return a list with no slackers
  ListOfNums* eliminateSlackers () {
    // Template: this

    // Example:
    // this = !
    return (new EmptyNL ());
  }

  // addAtEnd : ListOfNums Num -> ListOfNums
  // Purpose: return a new list with this number at the end (and everything in the list is still there)
  ListOfNums* addAtEnd( int newGuy ) {
    // Template: this, newGuy, this->eliminateSlackers()

    // Example:
    // this = !
    // newGuy = 50
    // return 50 : !
    return (new OneMoreNum(newGuy, (new EmptyNL())));
    //return (new OneMoreNum(newGuy, this));
  }

  // addBetween : ListOfNums Num -> ListOfNums
  // Purpose: return a new list with this number in between all the elements of the list and at the end
  ListOfNums* addBetween( int comma ) {
    // Template: this, comma
    
    // Example:
    // this = !
    // comma = 0
    // return !
    return (new EmptyNL());
  }

  // payGoodGuys : ListOfNums -> ListOfNums
  ListOfNums* payGoodGuys ( ) {
    // Template: this

    // A filtering function
    // return !

    // A mapping function
    // return !

    // Every filtering-mapping function
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

  TrainThatCarriesPeople* lonelyTrain = new Kaboom ();
  TrainThatCarriesPeople* balladTrain = new Car ( "Celine", lonelyTrain );
  TrainThatCarriesPeople* trainWreck = new Car ( "Luke", balladTrain );
  TrainThatCarriesPeople* startsWithJay = new Car ( "Imaonit", trainWreck );
  TrainThatCarriesPeople* thisOne = new Car ( "Tommy", startsWithJay );

  printf("thisOne is ");
  thisOne->show();
  printf("\n");

  ListOfNums* mt = new EmptyNL () ;
  ListOfNums* lst = new OneMoreNum ( 5, mt ) ;
  ListOfNums* twozy = new OneMoreNum ( 2, lst ) ;
  ListOfNums* threezy = new OneMoreNum ( 3, lst ) ;
  ListOfNums* fivezy = new OneMoreNum ( 5, twozy ) ;

  twozy->show();
  printf("\n");

  threezy->show();
  printf("\n");

  fivezy->show();
  printf("\n");

  ListOfNums* mtStore = new EmptyNL () ;
  ListOfNums* dennaStore = new OneMoreNum ( 12, mtStore ) ;
  ListOfNums* danStore = new OneMoreNum ( 90, dennaStore ) ;
  ListOfNums* jayStore = new OneMoreNum ( 3, danStore ) ;

  // wage = 12 * hour

  ListOfNums* mtWages = new EmptyNL () ;
  ListOfNums* dennaWages = new OneMoreNum ( 144, mtWages ) ;
  ListOfNums* danWages = new OneMoreNum ( 1080, dennaWages ) ;
  ListOfNums* jayWages = new OneMoreNum ( 36, danWages ) ;

  //printf("The answer is %listofnums, but should be %d\n",
  //       lst->fun(),
  //       45.0);

  printf("The answer is \n  ");
  (jayStore->wages())->show();
  printf("\n, but should be\n  ");
  jayWages->show();
  printf("\n");

  TrainThatCarriesPeople* mtTrain = new Kaboom () ;
  TrainThatCarriesPeople* dennaTrain = new Car ( "Get Bonus!", mtTrain ) ;
  TrainThatCarriesPeople* danTrain = new Car ( "Get Bonus!", dennaTrain ) ;
  TrainThatCarriesPeople* jayTrain = new Car ( "No Soup For You!", danTrain ) ;

  printf("The answer is \n  ");
  (jayStore->evens())->show();
  printf("\n, but should be\n  ");
  jayTrain->show();
  printf("\n");

  printf("The answer is \n  ");
  (danStore->evens())->show();
  printf("\n, but should be\n  ");
  danTrain->show();
  printf("\n");

  printf("The answer is \n  ");
  (jayStore->eliminateSlackers())->show();
  printf("\n, but should be\n  ");
  danStore->show();
  printf("\n");
  
  ListOfNums* blakeStore = new OneMoreNum ( 1000, jayStore ) ;

  printf("The answer is \n  ");
  (blakeStore->eliminateSlackers())->show();
  printf("\n, but should be\n  ");
  (new OneMoreNum( 1000, danStore))->show();
  printf("\n");

  ListOfNums* negaMarkStore = new OneMoreNum ( -20, blakeStore ) ;

  printf("The answer is \n  ");
  (negaMarkStore->eliminateSlackers())->show();
  printf("\n, but should be\n  ");
  (new OneMoreNum( 1000, danStore))->show();
  printf("\n");

  printf("The answer is \n  ");
  ((new EmptyNL())->addAtEnd(50))->show();
  // = 3 : 90 : 12 : 1000 : !
  printf("\n, but should be\n  ");
  (new OneMoreNum( 50, (new EmptyNL())))->show();
  printf("\n");

  printf("The answer is \n  ");
  // jay = 3 : 90 : 12 : !
  (jayStore->addAtEnd(1000))->show();
  // = 3 : 90 : 12 : 1000 : !
  printf("\n, but should be\n  ");
  (new OneMoreNum(3, (new OneMoreNum( 90, (new OneMoreNum( 12, (new OneMoreNum( 1000, (new EmptyNL())))))))))->show();
  printf("\n");

  printf("The answer is \n  ");
  (new OneMoreNum(3, (new OneMoreNum( 90, (new OneMoreNum( 12, (new OneMoreNum( 1000, (new EmptyNL())))))))))->addBetween(0)->show();
  printf("\n, but should be\n  ");
  (new OneMoreNum(3, (new OneMoreNum( 0, (new OneMoreNum( 90, (new OneMoreNum( 0, (new OneMoreNum( 12, (new OneMoreNum( 0, (new OneMoreNum( 1000, (new OneMoreNum( 0, (new EmptyNL()) )) ))))))))))))))->show();
  printf("\n");

  printf("The answer is \n  ");
  ((jayStore->eliminateSlackers())->wages())->show();
  printf("\n, but should be\n  ");
  danWages->show();
  printf("\n");

  printf("The answer is \n  ");
  (jayStore->payGoodGuys())->show();
  printf("\n, but should be\n  ");
  danWages->show();
  printf("\n");



  return 0;
}

// XXX addAtEnd
// XXX addBetween
// XXX cap
// XXX capAt
// XXX wackyWage
// XXX update
