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

// A LON is either a
//  mtLON
//  consLON
class LON {
public:
  virtual void show () = 0;
  virtual int length () = 0;
};

class mtLON : public LON {
private:

public:
  mtLON () {}

  void show () {
    printf("!");
  }
  int length () {
    return 0;
  }
};

class consLON : public LON {
public:
  int first;
  LON* rest;

public:
  consLON(int first0, LON* rest0) {
    this->first = first0;
    this->rest = rest0;
  }
  void show () {
    printf("%d:", this->first);
    this->rest->show();
  }
  int length () {
    return 1 + this->rest->length();
  }
};

// A Triple is a 
//  new Triple(x, y, z)
// where
//  x is an int
//  y is an int
//  z is an int

// sumArrayAccum : Array(number) number number number -> number
int sumArrayAccum (int someArray[], int howManyThereAre, int sumSoFar, int whereIAm ) {
  // Template: someArray, sumSoFar, whereIAm

  // someArray = {3, 4, 5}
  // sumSoFar = 12
  // whereIAm = 3

  if ( whereIAm == howManyThereAre ) {
    // When we reach the end...
    return sumSoFar;
  } else {
    // When we are in the middle

    // someArray = {3, 4, 5}
    // sumSoFar = 3
    // whereIAm = 1
    // newSum = 7 = sumSoFar + someArray[whereIAm]
    // newPlace = 2 = whereIAm + 1

    int newSum = sumSoFar + someArray[whereIAm];
    int newPlace = whereIAm + 1;
    return sumArrayAccum(someArray, howManyThereAre, newSum, newPlace);
  }
} 

// sumArray : Array(number) number -> number
int sumArray ( int someArray[], int howManyThereAre ) {
  // Template: someArray, someArray[...]

  return sumArrayAccum(someArray, howManyThereAre, 0, 0);
}

// sumArrayWhile : Array(number) number -> number
int sumArrayWhile ( int someArray[], int howManyThereAre ) {
  // Template: someArray, someArray[...]

  int sumSoFar = 0;
  int whereIAm = 0;

  while ( whereIAm != howManyThereAre ) {
    sumSoFar = sumSoFar + someArray[whereIAm];
    whereIAm = whereIAm + 1;
  }

  return sumSoFar;
}

// while loop:
// while ( condition ) { do some stuff }
// // for loop:
// for ( at the beginning; condition to keep going; after each round ) { do some stuff }
// =>
// at the beginning;
// while ( condition to keep going ) {
//   do some stuff;
//   after each round;
//  }

// sumArrayFor : Array(number) number -> number
int sumArrayFor ( int someArray[], int howManyThereAre ) {
  // Template: someArray, someArray[...]

  int sumSoFar = 0;

  for ( int whereIAm = 0; whereIAm != howManyThereAre; whereIAm = whereIAm + 1 ){
    sumSoFar = sumSoFar + someArray[whereIAm];    
  }

  return sumSoFar;
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

  LON* mt = new mtLON();
  consLON* one = new consLON(1, mt);

  one->rest = one;

  printf ( "C++ says: " );
  //one->show();
  printf("\n");

  printf ( "The answer is %s, but should be %d\n",
           "a lot",
           //one->length(),
           1 ) ;

  // A ray
  // Array... kind a list of numbers

  int myFirstArray[] = {3, 4, 5};  // <--- A second class citizen

  printf ( "The answer is %d, but should be %d\n",
           //myFirstArray[2nd], // 2nd is an ORDINAL number
           // ORDINAL = 1st, 2nd, 3rd, .... 11tyfst
           // CARDINAL = 0, 1, 2, .... 112
           //myFirstArray[2], // 2 is a CARDINAL number, but it is the 3rd CARDINAL
           myFirstArray[1], // 1 is a CARDINAL number, but it is the 2nd CARDINAL
           4 ) ;

  // Triple* myFirstTriple = new Triple(3, 4, 5); // <--- First class
  // myFirstTriple->first;
  // myFirstTriple->second;
  // return new Triple(myFirstTriple->first, myFirstTriple->second, 7);
  
  // Illegal
  //return {myFirstArray[0], myFirstArray[1], 7};

  printf ( "The answer is %d, but should be %d\n",
           myFirstArray[2],
           5 ) ;

  // Arrays require mutation
  myFirstArray[2] = 7;

  printf ( "The answer is %d, but should be %d\n",
           myFirstArray[2],
           7 ) ;

  // Arrays are "length polymorphic"
  int mySecondArray[] = {3, 4, 5, 6};

  int myCrazyArray[] = {3, 4, 5, 6,3, 4, 5, 6,3, 4, 5, 6,3, 4, 5, 6,3, 4, 5, 6,3, 4, 5, 6,3, 4, 5, 6,3, 4, 5, 6,3, 4, 5, 6,};

  // sumArray : Array(number) -> number

  printf ( "The answer is %d, but should be %d\n",
           sumArray(myFirstArray, 3),
           14 ) ;

  printf ( "The answer is %d, but should be %d\n",
           sumArray(myFirstArray, 4),
           14 ) ;

  printf ( "The answer is %d, but should be %d\n",
           sumArray(myFirstArray, 2),
           14 ) ;
  
  printf ( "The answer is %d, but should be %d\n",
           sumArrayWhile(myFirstArray, 3),
           14 ) ;
  printf ( "The answer is %d, but should be %d\n",
           sumArrayFor(myFirstArray, 3),
           14 ) ;


  return 0;
}

// DONE cycles require mutation
// DONE infinite require laziness
// DONE array
// DONE sumArrayWhile
// DONE how big is it?
// DONE sumArrayFor
// sumLeft
// andArray
// in place insertion sort with swap
