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

// addEntry : LOE Entry -> LOE
ListOfEntry* addEntry (ListOfEntry* orig, Entry* newGuy) {
  return new OneLOE(newGuy, orig);
}

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

// USING MUTATION

int answerSoFar = 0;
// factorialMutate : nat nat -> nat
int factorialMutate ( int n ) {
  if ( n == 0 ) {
    return answerSoFar;
  } else {
    int rest = n - 1;
    answerSoFar = n * answerSoFar;
    return factorialMutate(rest);
  }
}
// factorialMutateJZ : nat -> nat
int factorialMutateJZ ( int n ) {
  answerSoFar = 1;
  return factorialMutate( n );
}

// Using while
// factorialWhileJZ : nat -> nat
int factorialWhileJZ ( int n ) {
  int answerSoFar = 1;

  while ( ! (n == 0) ) {
    int rest = n - 1;
    answerSoFar = n * answerSoFar;
    n = rest;
  }

  return answerSoFar;
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
  Entry* snowE = new Entry("Snow", "777-542-1452");
  ListOfEntry* snow = new OneLOE(snowE, lightning);

  printf("The answer is\n  ");
  snow->show();
  printf("\nbut should be\n  ");
  snow->show();
  printf("\n");

  printf("The answer is %s, but should be %s\n",
         snow->lookup("Lightning"),
         "777-123-1452");

  printf("The answer is %s, but should be %s\n",
         lightning->lookup("Snow"),
         "Not in this book");

  printf("The answer is %s, but should be %s\n",
         addEntry(lightning, snowE)->lookup("Snow"),
         "777-542-1452");

  // Simulate this definition of natural numbers with ints
  printf ( "The answer is %d, but should be %d\n",
           factorial(5),
           120 ) ;

  printf ( "The answer is %d, but should be %d\n",
           factorialAccum(5, 1),
           120 ) ;

  printf ( "The answer is %d, but should be %d\n",
           factorialAccumJZ(5),
           120 ) ;

  // XXX Start here

  int x = 1;
  printf ( "The answer is %d, but should be %d\n",
           x,
           1 ) ;
  printf ( "The answer is %d, but should be %d\n",
           x+1,
           2 ) ;

  x = 42;
  // 1 = 42

  printf ( "The answer is %d, but should be %d\n",
           x,
           1 ) ;
  printf ( "The answer is %d, but should be %d\n",
           x+1,
           2 ) ;

  printf ( "The answer is %d, but should be %d\n",
           factorialMutateJZ(5),
           120 ) ;

  // Store Tracking
  printf ( "The answer is %d, but should be %d\n",
           factorialMutateJZ(2),
           2 ) ;

  answerSoFar = 0;
  printf ( "The answer is %d, but should be %d\n",
           factorialMutateJZ(2),
           2 ) ;

  answerSoFar = 1;
  printf ( "The answer is %d, but should be %d\n",
           factorialMutate(2),
           2 ) ;
  
  answerSoFar = 2;
  printf ( "The answer is %d, but should be %d\n",
           factorialMutate(1),
           2 ) ;

  answerSoFar = 2;
  printf ( "The answer is %d, but should be %d\n",
           factorialMutate(0),
           2 ) ;

  answerSoFar = 2;
  printf ( "The answer is %d, but should be %d\n",
           answerSoFar,
           2 ) ;

  // You can't write test cases for the accumulator/mutator function
  printf ( "The answer is %d, but should be %d\n",
           factorialMutate(5),
           120 ) ;
  
  // Pre conditions
  answerSoFar = 1;
  printf ( "The answer is %d, but should be %d\n",
           factorialMutate(5),
           120 ) ;
  // Post conditions
  printf ( "The answer is %d, but should be %d\n",
           answerSoFar,
           120 ) ;

  // External mutation things are not functions
  answerSoFar = 1;
  printf ( "The answer is %d, but should be %d\n",
           factorialMutate(5),
           120 ) ;
  printf ( "The answer is %d, but should be %d\n",
           factorialMutate(5),
           120 ) ;
  
  // INTERNAL mutation

  // while

  // while ( condition ) {
  //   code;
  // }
  // // translates to...
  // if ( condition ) {
  //   code;
  //   while ( condition ) {
  //     code;
  //   }
  // } else {
  // }

  // A loop
  // A loop where the condition is never false, is an infinite loop

  int y = 2;
  while ( y != 0 ) {
    printf("\t inside the while. at the begining y is %d\n", y);
    y = y - 1;
    printf("\t inside the while. at the end y is %d\n", y);
  }
  printf ("The answer is %d, but should be %d\n",
          y,
          0);

  y = 2;
  while ( y != 0 ) {
    y = y - 1;
  }
  printf ("The answer is %d, but should be %d\n",
          y,
          0);

  "same as";

  y = 2;
  if ( y != 0 ) {
    y = y - 1;
    while ( y != 0 ) {
      y = y - 1;
    }
  } else {
  }
  printf ("The answer is %d, but should be %d\n",
          y,
          0);

  "same as";

  y = 1;
  while ( y != 0 ) {
    y = y - 1;
  }
  printf ("The answer is %d, but should be %d\n",
          y,
          0);

  // more factorial using INTERNAL MUTATION
  printf ( "The answer is %d, but should be %d\n",
           factorialWhileJZ(5),
           120 ) ;

  // Some built ins use mutation
  
  FILE* f = fopen("18-1.cc", "r");
  // getc : FILE -> character
  // A String is many characters
  printf("The answer is %c, but should be %c\n", getc(f), '#');

  char c = getc(f);
  while (c != -1) {
    printf("%c", c);
    c = getc(f);
  }

  return 0;
}

// basic mutation
// store tracking
// convert factorialAccum
// substitution
// I/O
// external vs internal
// address book
// stop light

  // Basic I/O
  //FILE* f = fopen("18-1.cc", "r");
  //printf("The answer is %c, but should be %c\n", getc(f), '#');
