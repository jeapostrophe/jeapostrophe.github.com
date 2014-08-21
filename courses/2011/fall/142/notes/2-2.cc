#include <stdio.h>
#include <math.h>

// f(x) = x^2 * PI
// areaOfDisk ( radius ) = radius * radius * PI

// Contract: areaOfDisk : number with decimals -> number with decimals
// Purpose: to compute the area of a disk
double areaOfDisk ( double radius ) {
  // Template: radius

  // Test 1
  /* printf ( "The answer is %f but it should be %f\n",
           areaOfDisk( hermione ),
           25 * M_PI ); */
  // radius = hermione = 5.0
  // ans = 25 * M_PI
  //return 25 * M_PI ; // radius * radius * M_PI = 5 * 5 * M_PI = 25 * M_PI

  // Test 2
  /*   printf ( "The answer is %f but it should be %f\n",
           areaOfDisk( r ),
           4 * M_PI );  */
  // radius = r = 2.0
  // ans = 4 * M_PI
  //return 4 * M_PI ; // radius * radius * M_PI = 2 * 2 * M_PI = 4 * M_PI

  // Generalize 1 & 2
  //return .... radius ...
  return radius * radius * M_PI ;

  // Test 3
  /* 
  printf ( "The answer is %f but it should be %f\n",
           areaOfDisk( ronald ),
           72 * 72 * M_PI );   
  */
  // radius = ronald = 72
  // ans = 72 * 72 * M_PI
  //return radius * radius * M_PI ;
  // radius = 72, means this is
  //return 72 * 72 * M_PI; 

  // Test 3 does not add new information
  // so we don't change the return
  
}

// volumeOfCylinda : decimal decimal -> decimal
// to compute teh area of this cylinda
double volumeOfCylinda ( double height ,  double radius ) {
  // Template: height radius

  // Test 1
  /*   printf( "The answer is %f but it should be %f\n",
          volumeOfCylinda( 1, 2 ),
          4 * M_PI );
          height = 1
          radius = 2
          ans = 4 *M_PI */
  //  return 4 * M_PI ;

  // Test 2
  /*
  printf( "The answer is %f but it should be %f\n",
          volumeOfCylinda( 2, 2 ),
          8 * M_PI );
          height = 2
          radius = 2
          ans = 8  * M_PI
  */
  //return 8 * M_PI ;

  // Generalize 1 & 2
  //return height * radius * radius * M_PI ;
  return height * areaOfDisk(radius);

  /*
  printf( "The answer is %f but it should be %f\n",
          volumeOfCylinda( 2, 4 ),
          32 * M_PI );
  */
}

int main () {
  printf ( "The answer is %d\n", 1 + 1 ) ;
  printf ( "The answer is %f\n", 1.0 / 2.0 ) ;

  int harry = 7;
  double r = 2.0;
  double hermione = 5.0;
  double ronald = 72.0;

  // f(x) := 25*PI := 4*PI
 
  // f(x) = PI * x * x
  // f(2) = PI * 2 * 2 = 4 * PI

  printf ( "The answer is %f but it should be %f\n",
           areaOfDisk( hermione ),
           25 * M_PI );
  printf ( "The answer is %f but it should be %f\n",
           areaOfDisk( r ),
           4 * M_PI ); 
  printf ( "The answer is %f but it should be %f\n",
           areaOfDisk( ronald ),
           72 * 72 * M_PI );   

  printf ( "Substitution examples\n" );

  /* 
           areaOfDisk( r ),
           =         return radius * radius * M_PI [ radius = r ],
           =            r * r * M_PI,
           = 2 * 2 * PI
           = 4 * PI
           = done
  */

  printf ( "The answer is %f but it should be %f\n",
           areaOfDisk( r ),
           4 * M_PI ); 
  printf ( "The answer is %f but it should be %f\n",
           // radius * radius * M_PI [ radius = r ],
           r * r * M_PI,
           4 * M_PI ); 
  printf ( "The answer is %f but it should be %f\n",
           // radius * radius * M_PI [ radius = r ],
           2.0 * 2.0 * M_PI,
           4 * M_PI ); 
  printf ( "The answer is %f but it should be %f\n",
           // radius * radius * M_PI [ radius = r ],
           4.0 * M_PI,
           4 * M_PI ); 

  printf ( "Cylinda\n" );

  printf( "The answer is %f but it should be %f\n",
          volumeOfCylinda( 1, 2 ),
          4 * M_PI );
  printf( "The answer is %f but it should be %f\n",
          volumeOfCylinda( 2, 2 ),
          8 * M_PI );
  printf( "The answer is %f but it should be %f\n",
          volumeOfCylinda( 2, 4 ),
          32 * M_PI );
}
