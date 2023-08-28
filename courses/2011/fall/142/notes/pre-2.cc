#include <stdio.h>
#include <math.h>
#include <stdlib.h>

// Problem: Company XYZ & Co. pays all its employees $12 per hour. 
// A typical employee works between 20 and 65 hours per week. Develop 
// a program that determines the wage of an employee from the number
// of hours of work.

// Information (have): hourly wage, typical work load, how many hours they worked
// Information (have, relevant): hourly wage, how many hours they worked
// Information (want): total wage

// Contract : payMe : double -> double [with two decimal places]
// Purpose: to compute the wage given the employee worked 'hours' hours and gets paid $12/hour
// Examples
// payMe( 2 ) = 24
// payMe( 10 ) = 120
// ...
double payMe ( double hours ) {
  return 12 * hours ;
}

// Math function: areaOfDisk(r) = PI*r^2
// Contract: areaOfDisk : double -> double
// Purpose: to compute the area of a disk, with radius 'r'
// Examples:
// areaOfDisk( 2 ) = 3.14159 * 2 * 2 = 12.56636
// areaOfDisk( 16 ) = PI * pow(16,2) = 201.061930
// Function header:
double areaOfDisk ( double r ) {
  // Function body
  return M_PI * pow(r,2) ;
}
    

int main () {
  printf("The answer is: %d\n", 12/8 * 3);
  printf("The answer is: %d\n", 12/8);
  //printf("The answer is: %f\n", 12/8);
  printf("The answer is: %f\n", 12/8 * 3.0);
  printf("The answer is: %f\n", 12/8.0 * 3);
  printf("The answer is: %f\n", 12/8.0 * 3.0);
  printf("The answer is: %f\n", 12.0/8 * 3);
  printf("The answer is: %f\n", 12.0/8 * 3.0);
  printf("The answer is: %f\n", 12.0/8.0 * 3);
  printf("The answer is: %f\n", 12.0/8.0 * 3.0);
  printf("The answer is: %f\n", 3.0/8.0 * 12.0);

  // Problem: What is the area of Jay's imaginary red circle?
  // Information (have): radius (2 units)
  // Information (want): area (unit^2)
  // Data radius: float (decimals)
  // Data area: float (decimals)
  printf("The answer is: %f\n", 3.14159 * 2 * 2);
  printf("The answer is: %f\n", 3.14159 * 4 * 4);
  printf("The answer is: %f\n", 3.14159 * pow(4,2));
  printf("The answer is: %f\n", 3.14159 * pow(8,2));
  printf("The answer is: %f\n", M_PI * pow(8,2));
  printf("The answer is: %f\n", M_PI * pow(16,2));
  printf("The answer is: %f\n", areaOfDisk(32));

  printf("The answer is: %f, but it should be close to... %f\n",
         areaOfDisk(2),
         12.56636);
  printf("The answer is: %f, but it should be... %f\n",
         areaOfDisk(16),
         M_PI * pow(16,2));

  printf("The answer is: %f, but it should be... %f\n",
         payMe(2),
         24.0);

  printf("The answer is: %f, but it should be... %f\n",
         payMe(10),
         120.0);

}
