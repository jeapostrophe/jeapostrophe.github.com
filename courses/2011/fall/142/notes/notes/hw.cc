/* Jay McCarthy: Assignment 0: Exercise 0 

Write a function named 'volumeOfCylinda' which takes the height of a
cylinder and its base radius (in furlongs) and computes the
volume. Use an auxiliary function named 'areaOfDisk' that takes a
disk's radius and returns its area.  
*/
#include <stdio.h>
#include <math.h>

// Contract: areaOfDisk : number with decimals -> number with decimals
// Purpose: to compute the area of a disk, except if the radius is greater than 10
//          then unlock infinite lives mode
double areaOfDisk ( double radius ) {
  // Template: radius

  // Distinguishes 1&2 from 3
  if ( radius <= 10 ) {
    // Test 1
    // radius = hermione = 5.0
    // ans = 25 * M_PI
    //return 25 * M_PI ; 

    // Test 2
    // radius = r = 2.0
    // ans = 4 * M_PI
    //return 4 * M_PI ; 

    // Generalizes 1 & 2
    return radius * radius * M_PI ;

  } else {
    // Test 3
    // radius = 72
    // ans = 99
    return 99.0;
  }
}

// volumeOfCylinda : decimal decimal -> decimal
// to compute teh area of this cylinda
double volumeOfCylinda ( double height ,  double radius ) {
  // Template: height radius

  // Test 1
  /*      height = 1
          radius = 2
          ans = 4 *M_PI */
  //  return 4 * M_PI ;

  // Test 2
  /*      height = 2
          radius = 2
          ans = 8  * M_PI
  */
  //return 8 * M_PI ;

  // Generalizes 1 & 2
  return height * areaOfDisk(radius);
}

int main () {
  double r = 2.0;
  double hermione = 5.0;
  double ronald = 72.0;

  printf ( "The answer is %f but it should be %f\n",
           areaOfDisk( hermione ),
           25 * M_PI );
  printf ( "The answer is %f but it should be %f\n",
           areaOfDisk( r ),
           4 * M_PI ); 
  /* 
           areaOfDisk( r ),
           = return radius * radius * M_PI [ radius = r ],
           = r * r * M_PI,
           = 2 * 2 * PI
           = 4 * PI
  */
  printf ( "The answer is %f but it should be %f\n",
           areaOfDisk( ronald ),
           99.0 );   


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
  /* 
     volumeOfCylinda( 2, 4 ),
     =
     return height * areaOfDisk(radius); [ height = 2, radius = 4 ]
     =
     return 2 * areaOfDisk(4); 
     =
     return 2 * (return radius * radius * M_PI ) [ radius = 4 ]
     =
     return 2 * (return 4 * 4 * M_PI )
     =
     2 * 4 * 4 * M_PI
     =
     2 * 16 * M_PI
     =
     32 * M_PI
  */

}
