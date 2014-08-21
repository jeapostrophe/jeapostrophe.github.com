#include <stdio.h>
#include <math.h>
#include <string.h>

// booleanToString : boolean -> string
const char* booleanToString ( bool it ) {
  if ( it ) { return "true"; } else { return "false"; }
}

// A MaybeNum is either
//  SomeNum
//  NoneNum
class MaybeNum {
public:
  virtual int show () = 0;
};

// A SomeNum is a
//  new SomeNum ( num )
// where
//  num is an int
class SomeNum : public MaybeNum {
public:
  int num;
  SomeNum ( int num0 ) {
      this->num = num0;
  }
  int show ( ) {
    printf ("(%d)", this->num);
  }
};

// A NoneNum is a 
//  new NoneNum ()
// where
class NoneNum : public MaybeNum {
public:
  NoneNum ( ) { }
  int show () {
    printf( "()" );
  }
};

// A ListOfNums is either
//  OneMoreNum
//  EmptyNL
class ListOfNums {
public:
  // show : ListOfNums -> int
  virtual int show () = 0;
  // containsHuh : ListOfNums int -> bool
  // to determine if the given number is in the list
  virtual bool containsHuh ( int target ) = 0; 
  // firstEven : ListOfNums -> int
  // Purpose: to return the first even number in the list (or returns 1 if there are none)
  virtual int firstEven ( ) = 0;
  // firstEvenDash : ListOfNums int -> int
  // Purpose: to return the first even number in the list (or returns error value if there are none)
  virtual int firstEvenDash ( int errval ) = 0;
  // firstEvenSafe : ListOfNums -> MaybeNum
  // Purpose: to return the first even number in the list
  virtual MaybeNum* firstEvenSafe ( ) = 0;
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

  // containsHuh : OneMoreNum int -> bool
  // to determine if the given number is in the list
  bool containsHuh ( int target ) {
    // Template: this, this->first, this->rest

    if ( this->first == target ) {
    // Example:
    // this = 2:5:!
    // this->first = 2
    // this->rest = 5:!
    // target = 2
    // this->first == target = true
    // this->rest->containsHuh( target ) = false
    return true;
    } else {
    // Example
    // this = 5:2:5:!
    // this->first = 5
    // this->rest = 2:5:!
    // target = 2
    // this->first == target = false
    // this->rest->containsHuh( target ) = true
    //return true;

    // Example
    // this = 5:2:5:!
    // this->first = 5
    // this->rest = 2:5:!
    // target = 7
    // this->rest->containsHuh( target ) = false
    //return false;

    // Generalize
    return this->rest->containsHuh( target );
    }
  }

  // firstEven : OneMoreNum -> int
  // Purpose: to return the first even number in the list (or returns 1 if there are none)
  int firstEven ( ) {
    // Template: this, this->first, this->rest

    if ( (this->first % 2) == 1 ) {

    // Example
    // this = 5:2:5:!
    // this->first = 5
    // this->rest = 2:5:!
    // this->rest->firstEven() = 2
    return this->rest->firstEven();

    } else {

    // Example
    // this = 2:5:!
    // this->first = 2
    // this->rest = 5:!
    // this->rest->firstEven() = 1
    return this->first;

    }
  }

  // firstEvenDash : OneMoreNum int -> int
  // Purpose: to return the first even number in the list (or returns errval if there are none)
  int firstEvenDash ( int errval ) {
    // Template: this, this->first, this->rest

    if ( (this->first % 2) == 1 ) {

    // Example
    // this = 5:2:5:!
      // errval = 99
    // this->first = 5
    // this->rest = 2:5:!
    // this->rest->firstEvenDash(errval) = 2
    return this->rest->firstEvenDash(errval);

    } else {

    // Example
    // this = 2:5:!
      // errval = 77
    // this->first = 2
    // this->rest = 5:!
    // this->rest->firstEven(errval) = 1
    return this->first;

    }
  }

  // firstEvenSafe : OneMoreNum -> MaybeNum
  // Purpose: to return the first even number in the list 
  MaybeNum* firstEvenSafe ( ) {
    // Template: this, this->first, this->rest

    if ( (this->first % 2) == 1 ) {

    // Example
    // this = 5:2:5:!
    // this->first = 5
    // this->rest = 2:5:!
    // this->rest->firstEven() = (Some 2)
    return this->rest->firstEvenSafe();

    } else {

    // Example
    // this = 2:5:!
    // this->first = 2
    // this->rest = 5:!
    // this->rest->firstEven() = (None)
      return new SomeNum ( this->first );

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

  // containsHuh : EmptyNL int -> bool
  // to determine if the given number is in the list
  bool containsHuh ( int target ) {
    // Template: this

    // Example:
    // this = !
    // target = 2
    return false;
  }

  // firstEven : EmptyNL -> int
  // Purpose: to return the first even number in the list (or returns 1 if there are none)
  int firstEven ( ) {
    // Template: this

    // Example
    // this = !
    return 1 ;
  }

  // firstEvenDash : EmptyNL int -> int
  // Purpose: to return the first even number in the list (or returns errval if there are none)
  int firstEvenDash ( int errval ) {
    // Template: this

    // Example
    // this = !
    // errval = 99
    return errval ;
  }

  // firstEvenSafe : EmptyNL -> MaybeNum
  // Purpose: to return the first even number in the list 
  MaybeNum* firstEvenSafe ( ) {
    // Template: this

    // Example
    // this = !
    return (new NoneNum ()) ;
  }

};
 
// main : -> number
int main () {
  printf ( "The answer is %f, but should be %f\n",
           1.0/2.0,
           0.5 ) ;
  printf ( "C++ says %s\n",
           booleanToString(strcmp("Jay", "Libby") == 0)) ;

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

  printf ( "The answer is %s, but should be %s\n",
           booleanToString( mt->containsHuh( 2 ) ),
           booleanToString( false ) ) ;
  printf ( "The answer is %s, but should be %s\n",
           booleanToString( twozy->containsHuh( 2 ) ),
           booleanToString( true ) ) ;
  printf ( "The answer is %s, but should be %s\n",
           booleanToString( fivezy->containsHuh( 2 ) ),
           booleanToString( true ) ) ;
  printf ( "The answer is %s, but should be %s\n",
           booleanToString( fivezy->containsHuh( 7 ) ),
           booleanToString( false ) ) ;

  printf ( "The answer is %d, but should be %d\n",
           fivezy->firstEven(),
           2 ) ;
  printf ( "The answer is %d, but should be %d\n",
           (new OneMoreNum( 0, mt ))->firstEven(),
           0 ) ;
  printf ( "The answer is %d, but should be %d\n",
           mt->firstEven(),
           1 ) ;

  printf ( "The answer is %d, but should be %d\n",
           1 / 0,
           76 ) ;
  printf ( "The answer is %f, but should be %f\n",
           1.0 / 0.0,
           76.0 ) ;
  printf ( "The answer is %f, but should be %f\n",
           -1.0 / 0.0,
           76.0 ) ;

  printf ( "The answer is %d, but should be %d\n",
           fivezy->firstEvenDash(99),
           2 ) ;
  printf ( "The answer is %d, but should be %d\n",
           (new OneMoreNum( 0, mt ))->firstEvenDash(101),
           0 ) ;
  printf ( "The answer is %d, but should be %d\n",
           mt->firstEvenDash(77),
           77 ) ;

  printf ( "The answer is %d, but should be %d\n",
           mt->firstEvenDash(4),
           4 ) ;

  fivezy->firstEvenSafe()->show();
  printf("\n");

  mt->firstEvenSafe()->show();
  printf("\n");

  return 0;
}

// XXX: make number list
// XXX: containsHuh (on numbers)
// XXX: reference
// XXX: interleave * and +

// Even = m = 2 * n % 2 = 0
// Odd = m = 2 * n + 1 % 2 = 1
