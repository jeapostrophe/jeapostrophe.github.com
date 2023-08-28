#include <stdio.h>
#include <math.h>


// convert3 : digit digit digit -> number
// Purpose: see below, because I am lazy
int convert3 ( int ones, int tens, int hundreds  ) {
  // Template: ones tens hundreds

  // Example 1
  /*   printf ( "The answer is %d, but should be %d\n",
           convert3(1, 2, 3),
           321 ) ; */
  // ones = 1
  // tens = 2
  // hundreds = 3
  // answer = 321
  // return 321 ;

  // Control x 1 closes the bottom window

  // Example 2
  /*   printf ( "The answer is %d, but should be %d\n",
           convert3(3, 5, 1),
           153 ) ; */
  // ones = 3
  // tens = 5
  // hundreds = 1
  // answer = 153
  // return 153 ;

  // Generalize 1 & 2
  return ones + tens * 10 + hundreds * 10 * 10 ; 
}

// Problem: cheapskateTip represents Jay's brother who doesn't tip if the meal costs more than 40 bucks. Otherwise , tips 10%

// cheapskateTip : decimals -> decimals
// Purpose: simulate Joe
double cheapskateTip ( double price ) {
  // Template: price

  // Distinguishes (1 & 2) from 3
  if (price < 40) {
  // Example 1
  // Ctrl-x 2 splits window
  // Ctrl-x o moves to the other window
  /*   printf ( "The answer is %f, but should be %f\n",
           cheapskateTip( 5.00 ),
           5.5 ) ; */
  // price = 5
  // ans = 5.5
  //return 5.5 ;
  
  // Example 2
  /* 
  printf ( "The answer is %f, but should be %f\n",
           cheapskateTip( 25.00 ),
           27.5 ) ;*/
  // price = 25
  // ans = 27.5
  //return 27.5 ;

  // Generalize 1 & 2
  return price * 1.10 ;

  } else {

  // Example 3
  /* 
  printf ( "The answer is %f, but should be %f\n",
           cheapskateTip( 100.00 ),
           100.00 ) ; */
  // price = 100
  // ans = 100
  //return 100.00 ;
  return price ;

  }


}

// main : -> number
int main () {
  printf ( "The answer is %d\n", 1 + 1 ) ;
  printf ( "The answer is %f\n", 1.0 / 2.0 ) ;

  // convert3
  /* 3. Define the function convert3, which consumes three digits, starting with the least significant (meaning right-most when written positionally) digit, and produces the corresponding number. So, convert3(1,2,3) should produces 321 and convert3(3,5,1) should produces 153. */
  printf ( "The answer is %d, but should be %d\n",
           convert3(1, 2, 3),
           321 ) ;
  printf ( "The answer is %d, but should be %d\n",
           convert3(3, 5, 1),
           153 ) ;
  printf ( "The answer is %d, but should be %d\n",
           convert3(9, 4, 2),
           249 ) ;
  /*            convert3(9, 4, 2),
                =
                return ones + tens * 10 + hundreds * 10 * 10 ; 
                // oh and btw ones = 9, tens = 4, and hundreds = 2
                =
                9 + 4 * 10 + 2 * 10 * 10
                = 
                9 + 40 + 200
                = 49 + 200
                = 249
  */

  /*
  printf ( "The answer is %d\n", 1 / 0 ) ;
  printf ( "The answer is %d\n", 1 / ( 2 - 2 ) ) ;
  printf ( "The answer is %d\n", 1 / ( convert3(0,0,0) ) ) ;
  */
  
  // cheapskateTip

  printf ( "The answer is %f, but should be %f\n",
           cheapskateTip( 5.00 ),
           5.5 ) ;
  printf ( "The answer is %f, but should be %f\n",
           cheapskateTip( 25.00 ),
           27.5 ) ;
  printf ( "The answer is %f, but should be %f\n",
           cheapskateTip( 100.00 ),
           100.00 ) ;
  printf ( "The answer is %f, but should be %f\n",
           cheapskateTip( 400.00 ),
           400.00 ) ;

  // Boolean 
  bool isJayLikeTotallyTheBestProfEvar = true ;
  bool isUtahGoodAtFootball = false ;

  // TODO movie theater
  // TODO movie theater best

  return 0;
}
