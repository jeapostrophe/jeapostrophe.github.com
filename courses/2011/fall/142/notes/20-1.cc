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

// sumLeft : Array(number) number -> nothing
// Purpose: to mutate the array such that each element is the sum of those to its left (in the original array)
void sumLeft( int someArray[], int length) {
  // Template: someArray, someArray[...], length

  // Example:
  // someArray = {}
  // length = 0
  // post-condition:
  // someArray = {}
  
  // Example:
  // someArray = {1, 2}
  // length = 2
  // post-condition:
  // someArray = {1, 3}

  int sumSoFar = 0;
  int whereAmI = 0;

  while ( whereAmI < length ) {
    // Example:
    // whereAmI = 1
    int before_whereAmI = whereAmI;
    // sumSoFar = 1
    int before_sumSoFar = sumSoFar;
    // someArray[whereAmI] = 2
    int before_value = someArray[whereAmI];
    
    // post-condition:
    // whereAmI = 2
    int after_whereAmI = before_whereAmI + 1;
    // sumSoFar = 3 = 2 + 1;
    int after_sumSoFar = before_sumSoFar + before_value;
    // someArray[whereAmI] = 3 = 2 + 1 = someArray[whereAmI] + sumSoFar
    int after_value = before_sumSoFar + before_value;

    whereAmI = after_whereAmI;
    sumSoFar = after_sumSoFar;
    someArray[before_whereAmI] = after_value;
  }

  return ;
}

// showArray : Array(number) number -> nothing
// Purpose: to print the array
void showArray ( int someArray[], int length ) {
  int whereAmI = 0;

  while ( whereAmI < length ) {
    printf("%d,", someArray[whereAmI]);
    whereAmI = whereAmI + 1;
  }

  return ;
}

// swap : Array(number) num num -> nothing
// Purpose: swap the two numbers in these two places in the array
void swap (int someArray[], int left, int right ) {
  printf("\t\t\tYou called swap with... ");
  showArray(someArray, max(left+1,right+1));
  printf("\n");

  int before_left = someArray[left];
  int before_right = someArray[right];

  someArray[left] = before_right;
  someArray[right] = before_left;

  return ;
}

// insert : Array(number) number -> nothing
// Purpose: move the element n to the right spot (earlier in the array)
void insert ( int someArray[], int idx_n ) {
  printf("\t\tYou called insert with... ");
  showArray(someArray, idx_n+1);
  printf("\n");

  if ( idx_n == 0 ) {
    return ;
  } else {
    // If this number is smaler than the next one,
    if ( someArray[idx_n] < someArray[idx_n - 1] ) {
      // then move it down and keep inserting
      swap(someArray, idx_n, idx_n - 1);
      // then we keep inserting
      insert(someArray, idx_n - 1);

      return ;
    } else {
      return ;
    }
  }
}

// sortArray : Array(number) number -> nothing
// Purpose: sorts the array in place
void sortArray( int someArray[], int length) {

  printf("\tYou called sortArray with... ");
  showArray(someArray, length);
  printf("\n");

  if ( length == 0 ) {
    return ;
  } else {

    // First, sort [0 .... n-1, n]
    sortArray(someArray, length - 1);
    // Then, put n in the right spot
    insert(someArray, length - 1);

    return ;
  }
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


  int V1array[] = {7, 3, 0, 4, 1};
  int V1len = 5;
  //IntArray* V1 = new IntArray(V1array,V1len);

  sumLeft(V1array, V1len);
  int V1after[] = {7, 10, 10, 14, 15};
  printf("The answer is...\n\t");
  showArray(V1array, V1len);
  printf("\nBut should be\n\t");
  showArray(V1after, V1len);
  printf("\n");

  bool thingsAreGood[] = {true, false, true, true};
  double bigStuff[] = {3.14, 6.77, 99.33};
  double bigStuff2[3] = {3.14, 6.77, 99.33};
  double bigStuff3[5] = {3.14, 6.77, 99.33};
  int arrays[][2] = { {1,2}, {3, 4} };

  int V2array[] = {7, 3, 0, 4, 1};
  int V2len = 5;

  sortArray(V2array, V2len);
  int V2after[] = {0, 1, 3, 4, 7};
  printf("The answer is...\n\t");
  showArray(V2array, V2len);
  printf("\nBut should be\n\t");
  showArray(V2after, V2len);
  printf("\n");


  return 0;
}

// sumLeft
// andArray
// in place insertion sort with swap
