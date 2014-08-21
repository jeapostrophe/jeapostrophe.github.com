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
  virtual ListOfNum* addAtEnd(int newGuy) = 0;
  virtual int show () = 0;
  // leftSumList : LON -> LON
  // Purpose: return a list where every position is the sum of all numbers to the left of it in the original list
  virtual ListOfNum* leftSumList () = 0;
  // leftSumListAnHelpMeet : LON nat -> LON
  // Purpose: do leftSumList provided that this number is the sum of things to the left
  virtual ListOfNum* leftSumListAnHelpMeet(int sumSoFar) = 0;
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

  ListOfNum* leftSumList () {
    // Template: this, this->first, this->rest, this->rest->leftSumList()

    // Example:
    // this = 1:2:3:!
    // this->first = 1
    // this->rest = 2:3:!
    // this->rest->leftSumList() = 2:5:!
    
    // leftSumListAnHelpMeet : LON nat -> LON
    // Purpose: do leftSumList provided that this number is the sum of things to the left
    // this->rest->leftSumListAnHelpMeet(this->first) = 3:6:!
    
    // return 1:3:6:!
    // O(n) meaning only looks at each list element once
    return new OneLON(this->first, this->rest->leftSumListAnHelpMeet(this->first));
    

    // Non-accumulator style version
    // addToEverything : LON int -> LON
    // O(n^2) meaning looks every list element for each element
    //return new OneLON(this->first, this->rest->leftSumList()->addToEverything(this->first));
  }

  // leftSumListAnHelpMeet : LON nat -> LON
  // Purpose: do leftSumList provided that this number is the sum of things to the left
  ListOfNum* leftSumListAnHelpMeet(int sumSoFar) {
    // Template: this, this->first, this->rest, this->rest->leftSumListAnHelpMeet(...), sumSoFar

    // Example:
    // this = 2:3:!
    // this->first = 2
    // this->rest = 3:!
    // sumSoFar = 1
    // this->rest->leftSumListAnHelpMeet(sumSoFar) = 4:!
    // this->rest->leftSumListAnHelpMeet(1) = 4:!
    // this->rest->leftSumListAnHelpMeet(2) = 5:!
    // this->rest->leftSumListAnHelpMeet(3) = 6:!
    // return 3:6:!
    // return new OneLON(3, new OneLON(6, new EmptyLON()));
    //return new OneLON(1 + 2, new OneLON(6, new EmptyLON()));
    //return new OneLON(sumSoFar + 2, new OneLON(6, new EmptyLON()));
    //return new OneLON(sumSoFar + this->first, new OneLON(6, new EmptyLON()));
    //return new OneLON(sumSoFar + this->first, this->rest->leftSumListAnHelpMeet(3));
    //return new OneLON(sumSoFar + this->first, this->rest->leftSumListAnHelpMeet(sumSoFar + this->first));
    int nextSum = sumSoFar + this->first;
    return new OneLON(nextSum, this->rest->leftSumListAnHelpMeet(nextSum));
  }

  ListOfNum* addAtEnd(int newGuy) {
    return (new OneLON(this->first, this->rest->addAtEnd(newGuy)));
  }

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

  ListOfNum* addAtEnd(int newGuy) {
    return (new OneLON(newGuy, new EmptyLON()));
  }

  // leftSumList : LON -> LON
  ListOfNum* leftSumList () {
    // Template: this

    // Example:
    // this = !
    return (new EmptyLON());
  }

  // leftSumListAnHelpMeet : LON nat -> LON
  // Purpose: do leftSumList provided that this number is the sum of things to the left
  ListOfNum* leftSumListAnHelpMeet(int sumSoFar) {
    // Template: this

    // Example:
    // this = !
    // sumSoFar = 1729
    return (new EmptyLON());
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

// twoPows : nat -> LON
// Purpose: returns a list of powers of two up to the given number (as the exponent)
ListOfNum* twoPows( int top ) {
  // Template: top

  if ( top == 0 ) {
    // We are in the Zero case

    // Example:
    // top = 0
    // return 1:!
    return new OneLON(1, new EmptyLON());
  } else {
    // We are in the PlusOne case
    int rest = top - 1;

    // Example:
    // top = 4
    // rest = 3
    // twoPows(rest) = 1:2:4:8:!
    // return 1:2:4:8:16:!
    // return twoPows(rest) ++ 16:!
    // return twoPows(rest)->addAtEnd(16);
    //return twoPows(rest)->addAtEnd(2^4);
    //return twoPows(rest)->addAtEnd(2^top);
    return twoPows(rest)->addAtEnd(pow(2,top));
  }  
}

// twoPows : nat -> LON
// Purpose: returns a list of powers of two up to the given number (as the exponent)
ListOfNum* twoPowsAccum( int top, ListOfNum* finalAnswer ) {
  // Template: top

  if ( top == 0 ) {
    // We are in the Zero case

    // Example:
    // top = 0
    // return 1:!
    return new OneLON(1, finalAnswer);
  } else {
    // We are in the PlusOne case
    int rest = top - 1;

    // Example:
    // top = 4
    // rest = 3
    // twoPows(rest) = 1:2:4:8:!
    // return 1:2:4:8:16:!
    // return twoPows(rest) ++ 16:!
    // return twoPows(rest)->addAtEnd(16);
    //return twoPows(rest)->addAtEnd(2^4);
    //return twoPows(rest)->addAtEnd(2^top);
    return twoPowsAccum(rest, new OneLON(pow(2,top), finalAnswer));
  }  
}

ListOfNum* twoPowsKharlyTuna( int top ) {
  return twoPowsAccum(top, new EmptyLON());
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

  // Simulate this definition of natural numbers with ints
  printf ( "The answer is %d, but should be %d\n",
           factorial(5),
           120 ) ;

  printf ("Using new factorial\n");

  printf ( "The answer is %d, but should be %d\n",
           factorialAccumJZ(5),
           120 ) ;

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

  // leftSumList : LON -> LON

  printf("The answer is\n  ");
  l2->leftSumList()->show();
  printf("\nbut should be\n  ");
  printf("1:3:6:!");
  printf("\n");

  printf("The answer is\n  ");
  mt->leftSumList()->show();
  printf("\nbut should be\n  ");
  mt->show();
  printf("\n");

  // printf("The answer is\n  ");
  // l2->rightSumList();
  // printf("\nbut should be\n  ");
  // printf("6:5:3:!");
  // printf("\n");

  printf("The answer is\n  ");
  l1->leftSumListAnHelpMeet(1)->show();
  printf("\nbut should be\n  ");
  printf("3:6:!");
  printf("\n");

  // twoPows : nat -> LON
  // Purpose: returns a list of powers of two up to the given number (as the exponent)

  printf("The answer is\n  ");
  twoPows(4)->show();
  printf("\nbut should be\n  ");
  printf("1:2:4:8:16:!");
  printf("\n");

  printf("The answer is\n  ");
  twoPowsKharlyTuna(4)->show();
  printf("\nbut should be\n  ");
  printf("1:2:4:8:16:!");
  printf("\n");


  return 0;
}

// add : str str LOE -> LOE
// sumList : LON -> LON
// twoPows : nat -> LON
