#include <stdio.h>
#include <math.h>
#include <string.h>

// booleanToString : boolean -> string
const char* booleanToString ( bool it ) {
  if ( it ) { return "true"; } else { return "false"; }
}

// A Train that Carries X
// aka
// A List of X

// A Train
//  Car ... Kaboooouse

// A TrainThatCarriesPeople is either
//  a Car
//  or, a Kaboom
class TrainThatCarriesPeople {
public:
  // howManyPeople : TrainThatCarriesPeople -> int
  virtual int howManyPeople ( ) = 0;
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
    printf("%s", this->Person);
    printf(" o--o ");
    this->TheRestOfTheTrain->show();
    return 0;
  }

  // howManyPeople : Car -> int
  int howManyPeople ( ) {
    // Template: this, this->Person, this->TheRestOfTheTrain

    // Example 1:
    // this = thisOne = Tommy:Jay:Luke:Celine:!
    // this->Person = Tommy
    // this->TheRestOfTheTrain = Jay:Luke:Celine:!
    // this->TheRestOfTheTrain->howManyPeople() = 3
    // return 4;
    // return 1 + 3;
    // return 1 + this->TheRestOfTheTrain->howManyPeople();

    // Example 2:
    // this = trainWreck = Luke:Celine:!
    // this->Person = Luke
    // this->TheRest... = Celine:!
    // this->TheRestOfTheTrain->howManyPeople() = 1
    // return 2;
    // return 1 + 1;
    // return 1 + this->TheRestOfTheTrain->howManyPeople();

    // Generalize 1 & 2:    
    return 1 + this->TheRestOfTheTrain->howManyPeople();

    // Example 3
    // this = balladTrain = Celine:!
    // this->Person = Celine
    // this->TheRestOfTheTrain = !
    // this->TheRestOfTheTrain->howManyPeople() = 0
    //return 1;
    //return 1 + 0;
    //return 1 + this->TheRestOfTheTrain->howManyPeople()

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
    printf(" the train is over :'( ");
    return 0;
  }


  // howManyPeople : Kaboom -> int
  int howManyPeople ( ) {
    // Template: this

    // Example 1
    // this = lonelyTrain = !
    return 0;
  }

};

// howManyPeople : TrainThatCarriesPeople -> int
int howManyPeople ( TrainThatCarriesPeople* t ) {
  return t->howManyPeople();
}

// main : -> number
int main () {
  printf ( "The answer is %f, but should be %f\n",
           1.0/2.0,
           0.5 ) ;
  printf ( "C++ says %s\n",
           booleanToString(strcmp("Jay", "Libby") == 0)) ;

/*
 _\_/_%_
  O   O
*/
  TrainThatCarriesPeople* lonelyTrain = new Kaboom ();

/*
"Celine"
 ____   + _\_/_%_
 o  o      O   O
*/
  TrainThatCarriesPeople* balladTrain = new Car ( "Celine", lonelyTrain );

/*
"Luke" "Celine"
 ____ + ____   + _\_/_%_
 o  o   o  o      O   O
*/
  TrainThatCarriesPeople* trainWreck = new Car ( "Luke", balladTrain );

/*
"Jay"  "Luke" "Celine"
____ +  ____ + ____   + _\_/_%_
 o  o    o  o   o  o      O   O
*/
  TrainThatCarriesPeople* startsWithJay = new Car ( "Imaonit", trainWreck );

/*
"Tommy" "Jay"  "Luke" "Celine"
 _____ + ____ +  ____ + ____   + _\_/_%_
 o   o   o  o    o  o   o  o      O   O
*/
  TrainThatCarriesPeople* thisOne = new Car ( "Tommy", startsWithJay );

  printf ( "The answer is %d, but should be %d\n",
           lonelyTrain->howManyPeople(),
           0 ) ;
  printf ( "The answer is %d, but should be %d\n",
           balladTrain->howManyPeople(),
           1 ) ;
  printf ( "The answer is %d, but should be %d\n",
           trainWreck->howManyPeople(),
           2 ) ;
  printf ( "The answer is %d, but should be %d\n",
           startsWithJay->howManyPeople(),
           3 ) ;   
  printf ( "The answer is %d, but should be %d\n",
           thisOne->howManyPeople(),
           4 ) ;

  printf ( "The answer is %d, but should be %d\n",
           (new Car ( "Tommy", (new Car ( "Tommy", (new Car ( "Tommy", (new Car ( "Tommy", (new Car ( "Tommy", (new Car ( "Tommy", (new Car ( "Tommy", (new Car ( "Tommy", (new Car ( "Tommy", (new Car ( "Tommy", (new Car ( "Tommy", (new Car ( "Tommy", (new Car ( "Tommy", (new Car ( "Tommy", (new Car ( "Tommy", (new Car ( "Tommy", (new Car ( "Tommy", (new Car ( "Tommy", (new Car ( "Tommy", (new Car ( "Tommy", startsWithJay )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) )) ))->howManyPeople(),
           23 ) ;

  printf("thisOne is ");
  thisOne->show();
  printf("\n");

  return 0;
}

// XXX: list of planets
// XXX: length
// XXX: sumlength
