#include <stdio.h>
#include <math.h>
#include <string.h>

// max : int int -> int
// Purpose: returns the bigger of the two integers
int max (int x , int y ) {
  if ( x > y ) {
    return x;
  } else {
    return y;
  }
}

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

// An Entry is a
//  new Entry(name, number)
// where
//  name is a string
//  number is a string
class Entry {
public:
  const char* name;
  const char* number;

  Entry (const char* name0, const char* number0) {
    this->name = name0;
    this->number = number0;
  }

  int show () {
    printf("new Entry(%s,%s)", 
           this->name,
           this->number);
    return 0;
  }

  bool isThisYourNameHuh(const char* target) {
    return streq(this->name, target);
  }
  const char* whatsYaNumba () {
    return this->number;
  }

};

// A ListofEntry is either
//  EmptyLOE
//  OneLOE
class ListOfEntry {
public:
  virtual int show () = 0;
  virtual const char* lookup (const char* name) = 0;
};

// An EmptyLOE is a
//  new EmptyLOE()
// where
class EmptyLOE : public ListOfEntry {
public:

  EmptyLOE () {}

  int show () {
    printf("new EmptyLOE()");
    return 0;
  }

  const char* lookup (const char* name) {
    return "Not in this book";
  }
};

// A OneLOE is a
//  new OneLOE (first, rest)
// where
//  first is a int
//  rest is a ListOfEntry
class OneLOE : public ListOfEntry {
public:
  Entry* first;
  ListOfEntry* rest;

  OneLOE( Entry* first0, ListOfEntry* rest0 ) {
    this->first = first0;
    this->rest = rest0;
  }

  int show () {
    printf("new OneLOE(");
    this->first->show();
    printf(",");
    this->rest->show();
    printf(")");
    return 0;
  }

  const char* lookup (const char* name) {
    if (this->first->isThisYourNameHuh(name)) {
      return this->first->whatsYaNumba();
    } else {
      return this->rest->lookup(name);
    }
  }
};

// A ListofNum is either
//  EmptyLON
//  OneLON
class ListOfNum {
public:
  virtual int show () = 0;
  // reverse : LON -> LON
  virtual ListOfNum* reverse () = 0;
  // reverseAccum : LON LON -> LON
  virtual ListOfNum* reverseAccum (ListOfNum* answerSoFar) = 0;
};

// An EmptyLON is a
//  new EmptyLON()
// where
class EmptyLON : public ListOfNum {
public:

  EmptyLON () {}

  int show () {
    printf("new EmptyLON()");
    return 0;
  }

  ListOfNum* reverse () {
    return this->reverseAccum(new EmptyLON());
  }
  ListOfNum* reverseAccum (ListOfNum* answerSoFar) {
    return answerSoFar;
  }

};

// A OneLON is a
//  new OneLON (first, rest)
// where
//  first is a int
//  rest is a ListOfNum
class OneLON : public ListOfNum {
public:
  int first;
  ListOfNum* rest;

  OneLON( int first0, ListOfNum* rest0 ) {
    this->first = first0;
    this->rest = rest0;
  }

  int show () {
    printf("new OneLON(%d,", this->first);
    this->rest->show();
    printf(")");
    return 0;
  }

  ListOfNum* reverse () {
    return this->reverseAccum(new EmptyLON());
  }
  ListOfNum* reverseAccum (ListOfNum* answerSoFar) {
    // this, this->first, this->rest
    return this->rest->reverseAccum((new OneLON(this->first, answerSoFar)));
  }

};

// A Natural Number is either
//  0
//  1 + Natural Number
class Nat {
public:
  virtual int toInt () = 0;
  virtual int factorial () = 0;
};

class Zero : public Nat {
public:

  Zero() {}

  int toInt() { return 0; }
  int factorial() { return 1; }
};

class PlusOne : public Nat {
public:
  Nat* rest;

  PlusOne(Nat* rest0) { this->rest = rest0; }

  int toInt() { return 1 + this->rest->toInt(); }
  int factorial () {
    return this->toInt() * this->rest->factorial();
  }
};

// A Natural Number is either
//  0
//  1 + Natural Number

// factorial : nat -> nat
int factorial ( int n ) {
  if ( n == 0 ) {
    // We are in the Zero case
    return 1;
  } else {
    // We are in the PlusOne case
    // n = 1 + rest
    // n - 1 = rest
    // rest = n - 1
    int rest = n - 1;
    return n * factorial(rest);
    //return n * factorial(n-1);
  }
}

// Factorial in ACCUMULATOR style

// normal : problem-specification -> ans 
// accum : problem-specification answer-so-far -> ans

// factorialAccum : nat nat -> nat
int factorialAccum ( int n, int answerSoFar ) {
  if ( n == 0 ) {
    // We are in the Zero case
    // Step 1. Move the zero answer to the first answerSoFar in the wrapper
    //return 1;
    // Step 2. Replace the zero answer with the answerSoFar
    return answerSoFar;
  } else {
    // We are in the PlusOne case
    // n = 1 + rest
    // n - 1 = rest
    // rest = n - 1
    int rest = n - 1;
    // Step 3. Update the function call...
    //return n * factorial(rest);
    // ---->
    //return n * factorialAccum(rest, ...);
    // Step 4. Move the computation from the outside to the inside, replacing the fun call with the answerSoFar
    return factorialAccum(rest, n * answerSoFar);
  }
}
// factorialAccumJZ : nat -> nat
int factorialAccumJZ ( int n ) {
  return factorialAccum( n, 1 );
}

// main : -> number
int main () {
  printf ( "The answer is %f, but should be %f\n",
           1.0/2.0,
           0.5 ) ;
  printf ( "C++ says %s\n",
           booleanToString(strcmp("Jay", "Libby") == 0)) ;
  printf ( "C++ says %s\n",
           booleanToString(streq("Jay", "Libby"))) ;

  // PhoneBooks
  ListOfEntry* mtpb = new EmptyLOE();
  ListOfEntry* lightning = new OneLOE(new Entry("Lightning", "777-123-1452"), mtpb);
  ListOfEntry* snow = new OneLOE(new Entry("Snow", "777-542-1452"), lightning);

  printf("The answer is\n  ");
  snow->show();
  printf("\nbut should be\n  ");
  snow->show();
  printf("\n");

  printf("The answer is %s, but should be %s\n",
         snow->lookup("Lightning"),
         "777-123-1452");

  // Stuff from after the start of class

  // A Natural Number is either
  //  0
  //  1 + Natural Number

  Nat* five = new PlusOne( new PlusOne ( new PlusOne (new PlusOne (new PlusOne (new Zero())))));
  printf ( "The answer is %d, but should be %d\n",
           five->toInt(),
           5 ) ;
  printf ( "The answer is %d, but should be %d\n",
           five->factorial(),
           5 * 4 * 3 * 2 * 1) ;
  printf ( "The answer is %d, but should be %d\n",
           five->factorial(),
           120 ) ;

  // Simulate this definition of natural numbers with ints
  printf ( "The answer is %d, but should be %d\n",
           factorial(5),
           120 ) ;
  printf ( "The answer is %d, but should be %d\n",
           5 * factorial(4),
           120 ) ;
  printf ( "The answer is %d, but should be %d\n",
           5 * (4 * factorial(3)),
           120 ) ;
  printf ( "The answer is %d, but should be %d\n",
           5 * (4 * (3 * factorial(2))),
           120 ) ;
  printf ( "The answer is %d, but should be %d\n",
           5 * (4 * (3 * (2 * factorial(1)))),
           120 ) ;
  printf ( "The answer is %d, but should be %d\n",
           5 * (4 * (3 * (2 * (1 * factorial(0))))),
           120 ) ;
  printf ( "The answer is %d, but should be %d\n",
           5 * (4 * (3 * (2 * (1 * 1)))),
           120 ) ;

  // C++ ran out places for *s
  //printf ( "The answer is %d, but should be %d\n",
  //         factorial(500000),
  //         120 ) ;

  printf ("Using new factorial\n");

  printf ( "The answer is %d, but should be %d\n",
           factorialAccumJZ(5),
           120 ) ;
  printf ( "The answer is %d, but should be %d\n",
           factorialAccum(5, 1),
           120 ) ;
  printf ( "The answer is %d, but should be %d\n",
           factorialAccum(4, (5 * 1)),
           120 ) ;
  printf ( "The answer is %d, but should be %d\n",
           factorialAccum(4, 5),
           120 ) ;
  printf ( "The answer is %d, but should be %d\n",
           factorialAccum(3, (4 * 5)),
           120 ) ;
  printf ( "The answer is %d, but should be %d\n",
           factorialAccum(3, 20),
           120 ) ;
  printf ( "The answer is %d, but should be %d\n",
           factorialAccum(2, 3*20),
           120 ) ;
  printf ( "The answer is %d, but should be %d\n",
           factorialAccum(2, 60),
           120 ) ;
  printf ( "The answer is %d, but should be %d\n",
           factorialAccum(1, 2*60),
           120 ) ;
  printf ( "The answer is %d, but should be %d\n",
           factorialAccum(1, 120),
           120 ) ;
  printf ( "The answer is %d, but should be %d\n",
           factorialAccum(0, 1*120),
           120 ) ;
  printf ( "The answer is %d, but should be %d\n",
           factorialAccum(0, 120),
           120 ) ;
  printf ( "The answer is %d, but should be %d\n",
           120,
           120 ) ;

  // This won't crash if we use "g++ -O3" which means, use all information about the program when compiling
  //printf ( "The answer is %d, but should be %d\n",
  //         factorialAccumJZ(500000),
  //         120 ) ;

  // Bogus ans if answerSoFar is wrong .... garbage in, garbage out
  printf ( "The answer is %d, but should be %d\n",
           factorialAccum(4, 7),
           7 * 4 * 3 * 2 * 1 ) ;

  // Lists of numbers
  ListOfNum* mt = new EmptyLON();
  ListOfNum* l0 = new OneLON(3, mt);
  ListOfNum* l1 = new OneLON(2, l0);
  ListOfNum* l2 = new OneLON(1, l1);

  printf("The answer is\n  ");
  l2->show();
  printf("\nbut should be\n  ");
  l2->show();
  printf("\n");

  printf("The answer is\n  ");
  l2->reverse()->show();
  printf("\nbut should be\n  ");
  printf("3, 2, 1, !");
  printf("\n");


  return 0;
}

// nat num def.
// factorial (fun) : nat -> nat
// factorial (accum) : nat -> nat
// reverse (accum) : LOI -> LOI
// add : str str LOE -> LOE
