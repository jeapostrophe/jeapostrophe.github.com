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
  // evens : ListOfNums -> TrainThatCarriesPeople
  // Purpose: find the praise phrase for each worker
  virtual TrainThatCarriesPeople* evens ( ) = 0;
  // eliminateSlackers : ListOfNums -> ListOfNums
  // return a list with no slackers
  virtual ListOfNums* eliminateSlackers () = 0;
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

  return 0;
}

// XXX wage
// XXX hoursToWages
// XXX eliminateOverworkers [100 hours]
// XXX cap
// XXX capAt
// XXX wackyWage
// XXX update
// XXX evens
