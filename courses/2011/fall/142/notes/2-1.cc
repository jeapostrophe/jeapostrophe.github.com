#include <stdio.h>
#include <math.h>

// Contract: areaOfDisk : number with decimals -> number with decimals
// Purpose: to compute the area of a disk
double areaOfDisk ( double radius ) {
  // XXX This program is incomplete, we will continue next class.
}

int main () {
  printf ( "The answer is %d\n", 1 + 1 ) ;
  printf ( "The answer is %d\n", 8245879 + 8747 ) ;
  printf ( "The answer is %f\n", 1.0 / 2.0 ) ;
  printf ( "The answer is %f but it should be %f\n",
           500.0 / 12.0,
           41.66666 ) ;

  /*
    1. You and your friends decide to order a pizza and share the cost according to how many slices you eat. The pizza is cut into 8 slices. You eat 3 of them. If the pizza costs $12, how much should you contribute? Write a program to perform the computation.
  */
  
  // 12 * 3/8 = 4.5
  printf ( "The answer is %f but it should be %f\n",
           12.0 * 3.0/8.0,
           4.5 );


  // pie arrrr squared
  // Suppose that r = 2 and PI = 3, then the area of the circle is...
  double r = 2.0 ;
  // double PI = 3.1415926535897 ;
  // PI r^2

  // Why is this 
  // PI * r * r
  // = 12.55656526416425

  // Because of substitution
  // PI * r * r
  // PI * 2 * 2
  // 3.14 * 2 * 2
  // 3.14 * 4
  // 12.???


  printf ( "The answer is %f but it should be %f\n",
           M_PI * r * r,
           4 * M_PI );
  // Do substitution in C++
  printf ( "The answer is %f but it should be %f\n",
           M_PI * 2.0 * r,
           4 * M_PI );
  printf ( "The answer is %f but it should be %f\n",
           M_PI * 2.0 * 2.0,
           4 * M_PI );
  printf ( "The answer is %f but it should be %f\n",
           3.14 * 2.0 * 2.0,
           4 * 3.14 );

  // TODO: What are commas for?

  double hermione = 5.0;
  printf ( "The answer is %f but it should be %f\n",
           M_PI * hermione * hermione,
           25 * M_PI );

  double ronald = 72.0;
  printf ( "The answer is %f but it should be %f\n",
           M_PI * ronald * ronald,
           72.0 * 72.0 * M_PI );

  // Commas are for arguments

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

}
