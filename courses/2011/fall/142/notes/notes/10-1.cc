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
  // howMuchInk : TrainCarriesPeople -> integer
  // Purpose: to find the number of characters (i.e. letters) used to write the names of everyone on the train
  virtual int howMuchInk ( ) = 0;
  // containsAPersonWhoseNameTakesMoreThan7InkBlotsToWriteHuh : TrainCarriesPeople -> bool
  // Purpose: to return true if the list has someone with more than or equal to 7 letters 
  virtual bool containsAPersonWhoseNameTakesMoreThan7InkBlotsToWriteHuh ( ) = 0 ;
  // containsBobHuh : TrainThatCarriesPeople -> bool
  virtual bool containsBobHuh () = 0;
  // doesntTakeMuchToWriteHuh : TrainThatCarriesPeople int -> bool
  // Purpose: tell me if I can write anyones name with this much ink
  virtual bool doesntTakeMuchToWriteHuh ( int reserves ) = 0;
};

/// Look down below for the use of these
// f : string -> bool
// isThisGuyThatImSendingInTheArgumentBobHuh
bool f ( const char* s ) {
  return strcmp(s, "Bob Loblaw") == 0;
}
// cmb : bool bool -> bool
// areAnyOfTheseGuysBobHuh
bool cmb ( bool isThisGuyBobHuh, bool areAnyOfTheOtherGuysBobHuh ) {
  return isThisGuyBobHuh || areAnyOfTheOtherGuysBobHuh;
}
// unit : bool
bool unit = false;

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

  // howMuchInk : Car -> integer
  // Purpose: to find the number of characters (i.e. letters) used to write the names of everyone on the train
  int howMuchInk ( ) {
    // Template: this, this->Person, this->TheRestOfTheTrain, this->TheRestOfTheTrain->howManyPeople(), this->TheRestOfTheTrain->howMuchInk()

    // Example:
    // this = Tommy:Imaonit:Luke:Celine:!
    // this->howManyPeople() = 4
    // this->Person = Tommy
    // this->TheRest ... = Imaonit:Luke:Celine:!
    // this->TheRest...->howManyPeople() = 3
    // this->TheRest...->howMuchInk() = 17
    //return 22;
    // 22 to 5+17 is hard
    //return 5 + 17;
    //return 5 + this->TheRestOfTheTrain->howMuchInk();
    //return SAF(this->Person) + this->TheRestOfTheTrain->howMuchInk();
    //return howManyLettersInThisString(this->Person) + this->TheRestOfTheTrain->howMuchInk();
    return strlen(this->Person) + this->TheRestOfTheTrain->howMuchInk();
  }

  // containsAPersonWhoseNameTakesMoreThan7InkBlotsToWriteHuh : Car -> bool
  // Purpose: to return true if the list has someone with more than or equal to 7 letters 
  bool containsAPersonWhoseNameTakesMoreThan7InkBlotsToWriteHuh ( ) {
    // Template: this, this->Person, this->TheRestOfTheTrain, this->TheRestOfTheTrain->containsAPersonWhoseNameTakesMoreThan7InkBlotsToWriteHuh()

    // Distinguishes these guys
    if ( strlen(this->Person) < 7 ) {
    // Example
    // this = thisOne = Tommy:Imaonit:Luke:Celine:!
    // this->Person = Tommy
    // this->TheRest.. = Imaonit:Luke:Celine:!
    // this->TheRestOfTheTrain->containsAPersonWhoseNameTakesMoreThan7InkBlotsToWriteHuh() = true
    //return true;

    // Example
    // this = Luke:Celine:!
    // this->Person = Luke
    // this->TheRest.. = Celine:!
    // this->TheRestOfTheTrain->containsAPersonWhoseNameTakesMoreThan7InkBlotsToWriteHuh() = false
    //return false;

    // Generalize:
    return this->TheRestOfTheTrain->containsAPersonWhoseNameTakesMoreThan7InkBlotsToWriteHuh();
    } else {
    // Example
    // this = startsWithJay = Imaonit:Luke:Celine:!
    // this->Person = Imaonit
    // this->TheRest.. = Luke:Celine:!
    // this->TheRestOfTheTrain->containsAPersonWhoseNameTakesMoreThan7InkBlotsToWriteHuh() = false
    return true;
    }
  }

  // containsBobHuh : TrainThatCarriesPeople -> bool
  bool containsBobHuh () {
    // Template: this, this->Person, this->TheRestOfTheTrain

    // f, cmb, unit
    // f : string -> bool
    // cmb : bool bool -> bool
    // unit : bool
    return cmb(f(this->Person), this->TheRestOfTheTrain->containsBobHuh());
  }
  
  // doesntTakeMuchToWriteHuh : Car int -> bool
  bool doesntTakeMuchToWriteHuh ( int reserves ) {
    // Template: this, this->Person, this->TheRestOfTheTrain, reserves
    
    // Distinguishes (1&2) from 3
    //if ( ! (strlen(this->Person) <= reserves) ) {
    if ( (strlen(this->Person) > reserves) ) {
    // Example 1:
    // this = Tommy:Imaonit:Luke:Celine:!
    // this->Person = Tommy
    // this->TheRestOfTheTrain = Imaonit:Luke:Celine:!
    // reserves = 4
    // this->TheRestOfTheTrain->doesntTakeMuchToWriteHuh(999) = true
    // this->TheRestOfTheTrain->doesntTakeMuchToWriteHuh(10) = true
    // this->TheRestOfTheTrain->doesntTakeMuchToWriteHuh(reserves) = true
    //return true;

    // Example 2:
    // this = Tommy:Imaonit:Luke:Celine:!
    // this->Person = Tommy
    // this->TheRestOfTheTrain = Imaonit:Luke:Celine:!
    // reserves = 2
    // this->TheRestOfTheTrain->doesntTakeMuchToWriteHuh(999) = true
    // this->TheRestOfTheTrain->doesntTakeMuchToWriteHuh(10) = true
    // this->TheRestOfTheTrain->doesntTakeMuchToWriteHuh(reserves) = false
    //return false;

    // Generalize 1 & 2:
    return this->TheRestOfTheTrain->doesntTakeMuchToWriteHuh(reserves);
   
    } else {
    // Example 3:
    // this = Luke:Celine:!
    // this->Person = Luke
    // this->TheRestOfTheTrain = Celine:!
    // reserves = 4
    // this->TheRestOfTheTrain->doesntTakeMuchToWriteHuh(999) = true
    // this->TheRestOfTheTrain->doesntTakeMuchToWriteHuh(10) = true
    // this->TheRestOfTheTrain->doesntTakeMuchToWriteHuh(reserves) = false
    return true;
    }
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

  // containsBobHuh : TrainThatCarriesPeople -> bool
  bool containsBobHuh () {
    // Template: this

    // f, cmb, unit
    return unit;
  }

  // show : TrainThatCarriesPeople -> int
  int show () {
    //printf(" the train is over :'( ");
    printf("(new Kaboom ())");
    return 0;
  }


  // howManyPeople : Kaboom -> int
  int howManyPeople ( ) {
    // Template: this

    // Example 1
    // this = lonelyTrain = !
    return 0;
  }

  // howMuchInk : Kaboom -> integer
  // Purpose: to find the number of characters (i.e. letters) used to write the names of everyone on the train
  int howMuchInk ( ) {
    // Template: this,

    // Example 1:
    // this = lonelyTrain = !
    return 0;
  }

  // containsAPersonWhoseNameTakesMoreThan7InkBlotsToWriteHuh : Kaboom -> bool
  // Purpose: to return true if the list has someone with more than or equal to 7 letters 
  bool containsAPersonWhoseNameTakesMoreThan7InkBlotsToWriteHuh ( ) {
    // Template: this,

    // Example 1:
    // this = !
    return false;
  }

  // doesntTakeMuchToWriteHuh : Kaboom int -> bool
  // Purpose: tell me if I can write anyones name with this much ink
  bool doesntTakeMuchToWriteHuh ( int reserves ) {
    // Template: this, reserves

    // Example 8 (sideways):
    // this = !
    // reserves = something
    return false;
  }
};

// howManyPeople : TrainThatCarriesPeople -> int
int howManyPeople ( TrainThatCarriesPeople* t ) {
  return t->howManyPeople();
}

// averageNumberOfInkBlots : TrainThatCarriesPeople -> double
double averageNumberOfInkBlots ( TrainThatCarriesPeople* t ) {
  // Template: t, t->howManyPeople(), t->howMuchInk(), t->containsAPersonWhoseNameTakesMoreThan7InkBlotsToWriteHuh()

  return (1.0 * t->howMuchInk()) / (1.0 * t->howManyPeople());
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
"Tommy" "Imaonit"  "Luke" "Celine"
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

  // Substitution for 
  /*
  trainWreck->howManyPeople()

    (new Car ( "Luke", balladTrain ))->howManyPeople()

[this = (new Car ( "Luke", balladTrain ))]
    return 1 + this->TheRestOfTheTrain->howManyPeople();

    return 1 + (new Car ( "Luke", balladTrain ))->TheRestOfTheTrain->howManyPeople();

    return 1 + balladTrain->howManyPeople();

    return 1 + (new Car ( "Celine", lonelyTrain ))->howManyPeople();

    return 1 + (new Car ( "Celine", lonelyTrain ))->howManyPeople();

    [this = (new Car ( "Celine", lonelyTrain ))]
    return 1 + (1 + this->TheRestOfTheTrain->howManyPeople())

    return 1 + (1 + (new Car ( "Celine", lonelyTrain ))->TheRestOfTheTrain->howManyPeople())

    return 1 + (1 + lonelyTrain->howManyPeople())

    return 1 + (1 + (new Kaboom ())->howManyPeople())

    return 1 + (1 + 0)

    return 1 + 1

    return 2
  */

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

  printf ( "The answer is %d, but should be %d\n",
           lonelyTrain->howMuchInk(),
           0 ) ;
  printf ( "The answer is %d, but should be %d\n",
           thisOne->howMuchInk(),
           22 ) ;

  printf ( "The answer is %s, but should be %s\n",
           booleanToString(lonelyTrain->containsAPersonWhoseNameTakesMoreThan7InkBlotsToWriteHuh()),
           booleanToString(false) ) ;
  printf ( "The answer is %s, but should be %s\n",
           booleanToString(trainWreck->containsAPersonWhoseNameTakesMoreThan7InkBlotsToWriteHuh()),
           booleanToString(false) ) ;
  printf ( "The answer is %s, but should be %s\n",
           booleanToString(startsWithJay->containsAPersonWhoseNameTakesMoreThan7InkBlotsToWriteHuh()),
           booleanToString(true) ) ;
  printf ( "The answer is %s, but should be %s\n",
           booleanToString(thisOne->containsAPersonWhoseNameTakesMoreThan7InkBlotsToWriteHuh()),
           booleanToString(true) ) ;

  // average = total / how many
  printf ( "The answer is %f, but should be %f\n",
           averageNumberOfInkBlots(thisOne),
           5.5) ;

  // List l = (a, b, c, d, ..., n)
  // product l = (a * b * c * d * ... * n)
  // allTrueHuh l = (a && b && c && d && ... && n)

  // List l = a (b (c ()))
  // product l = a * (b * (c * 1))
  // lcl l = len(a) max (len(b) max (len(c) max 1))

  // all list operations (f,cmb,unit) l = f(a) cmb (f(b) cmb (f(c) cmb unit))
  // product, f = identity, cmb = *, unit = 1
  // allTrueHuh, f = identity, cmb = &&, unit = true
  // lcl, f = len, cmb = max, unit = 0
  // these are called... homomorphism
  // MapReduce ... fs run at the same time, but cmb runs afterwards

  printf ( "The answer is %s, but should be %s\n",
           booleanToString(thisOne->containsBobHuh()),
           booleanToString(false) ) ;
  printf ( "The answer is %s, but should be %s\n",
           booleanToString((new Car("Bob Loblaw", thisOne))->containsBobHuh()),
           booleanToString(true) ) ;
  printf ( "The answer is %s, but should be %s\n",
           booleanToString((new Car("Sinker", (new Car("Bob Loblaw", thisOne))))->containsBobHuh()),
           booleanToString(true) ) ;

  printf ( "The answer is %s, but should be %s\n",
           booleanToString(thisOne->doesntTakeMuchToWriteHuh(4)),
           booleanToString(true) ) ;
  printf ( "The answer is %s, but should be %s\n",
           booleanToString(thisOne->doesntTakeMuchToWriteHuh(2)),
           booleanToString(false) ) ;
  printf ( "The answer is %s, but should be %s\n",
           booleanToString(lonelyTrain->doesntTakeMuchToWriteHuh(2)),
           booleanToString(false) ) ;
  printf ( "The answer is %s, but should be %s\n",
           booleanToString(lonelyTrain->doesntTakeMuchToWriteHuh(999999)),
           booleanToString(false) ) ;

  return 0;
}

// DONE: averageLength
// DONE: containsBobHuh
// DONE: containsLessThanHuh
// XXX: make number list
// XXX: containsHuh (on numbers)
// XXX: reference
