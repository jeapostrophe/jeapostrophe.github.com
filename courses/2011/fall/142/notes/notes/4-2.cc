#include <stdio.h>
#include <math.h>
#include <string.h>

// Problem: cheapskateTip represents Jay's brother who doesn't tip if the meal costs more than 40 bucks. Otherwise , tips 10%

// f(x) = x + 1

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

  // Example 4
  // price = 400
  // ans = 400
  //return 400.00 ;

  // Generalize 3 & 4
  return price ;

  }


}

// booleanToString : boolean -> string
const char* booleanToString ( bool it ) {
  // Template: it

  // Distinguishes test 1 from test 2
  if ( it ) {
    // Test 1
    // it = true
    return "true";
  } else {
    // Test 2
    // it = false
    return "false";
  }
}

// insideOpenIntervalFrom5to99Huh : number -> boolean
// Purpose: determine if the number is inside (5,99)
bool insideOpenIntervalFrom5to99Huh ( int cutiepie ) {
  // Template: cutiepie
  
  // Example 1
  // cutie = 4
  // ans = false
  return ( (5 < cutiepie) && (cutiepie < 99)) ;
  
  // not valid
  //5 < cutiepie < 99

  /*
  if ( (5 < cutiepie) ) {
    if (cutiepie < 99) {
      return true;
    } else {
      return false;
    }
  } else {
    return false;
  }
   */
}

// main : -> number
int main () {
  printf ( "The answer is %d\n", 1 + 1 ) ;
  printf ( "The answer is %f\n", 1.0 / 2.0 ) ;

  // cheapskateTip
  printf ( "The answer is %f, but should be %f\n",
           cheapskateTip( 5.00 ),
           5.5 ) ;
  printf ( "The answer is %f, but should be %f\n",
           cheapskateTip( 25.00 ),
           27.5 ) ;
    /*
           cheapskateTip( 25.00 ),
=
>>>>>>> asking question: price < 40
>>>>>>> asking question: 25 < 40
>>>>>>> asking question: true
>>>>>>> asking question: select true side
=
  return price * 1.10 ;
=
  return 25 * 1.10 ;
= i'm tired
    */

  printf ( "The answer is %f, but should be %f\n",
           cheapskateTip( 100.00 ),
           100.00 ) ;
    /*
           cheapskateTip( 100.00 ),
=
>>>>>>> asking question: price < 40
>>>>>>> asking question: 100 < 40
>>>>>>> asking question: false
>>>>>>> asking question: select false side
=
  return price ;
=
  return 100.00 ;
    */
  printf ( "The answer is %f, but should be %f\n",
           cheapskateTip( 400.00 ),
           400.00 ) ;

  // Boolean 
  bool isJayLikeTotallyTheBestProfEvar = true ;
  bool isUtahGoodAtFootball = false ;

  int JayMcCarthy = 26 ;
  printf ( "The answer is %d, but should be %d\n",
           JayMcCarthy - 10,
           16 ) ;
  printf ( "The answer is %d, but should be %d\n",
           26 - 10,
           16 ) ;

  // A String is the CS word for words, actually for all text.
  printf ( "Jay McCarthy is a person, not a number\n" );

  // A String is between " and "
  const char* TheBestName = "Jay McCarthy" ;

  printf ( "The answer is %s, but should be %s\n",
           TheBestName,
           "Jay McCarthy" ) ;
  
  // labs in 1121
  // homework example : http://faculty.cs.byu.edu/~jay/courses/2011/fall/142/code/2-2p.cc
  // Homework for Jay : add ifs to the example hw

  // IDEs

  // Email class about why I am dum about boolean printing
  printf ( "The answer is %s, but should be %s\n",
           booleanToString(isJayLikeTotallyTheBestProfEvar),
           booleanToString(true) ) ;
  printf ( "The answer is %s, but should be %s\n",
           booleanToString(isUtahGoodAtFootball),
           booleanToString(false) ) ;

  printf ( "C++ says %s\n",
           booleanToString(false) ) ;
  // < : number number -> boolean
  printf ( "C++ says %s\n",
           booleanToString(4 < 7) ) ;
  printf ( "C++ says %s\n",
           booleanToString(4 > 7) ) ;
  printf ( "C++ says %s\n",
           booleanToString(7 < 7) ) ;
  printf ( "C++ says %s\n",
           booleanToString(7 <= 7) ) ;
  printf ( "C++ says %s\n",
           booleanToString(7 > 7) ) ;
  printf ( "C++ says %s\n",
           booleanToString(7 >= 7) ) ;

  printf ( "C++ says %s\n",
           booleanToString(7 == 4) ) ;
  printf ( "C++ says %s\n",
           booleanToString(4 == 4) ) ;

  // DONE boolean creating functions

  // DONE comparing strings

  printf ( "C++ says %s\n",
           booleanToString("Jay" == "Libby") ) ;
  printf ( "C++ says %s\n",
           booleanToString("Jay" == "Jay") ) ;

  const char* first = "Jay";
  const char* second = "Jay";
  printf ( "C++ says %s\n",
           booleanToString(first == second) ) ;

  // strcmp : string string -> number
  // where number is 0 if they are equal

  printf ( "C++ says %s\n",
           booleanToString(strcmp(first, second) == 0)) ;
  printf ( "C++ says %s\n",
           booleanToString(strcmp("Jay", "Libby") == 0)) ;

  // DONE how much time to spend, how often
  // DONE TAs in labs
  // DONE missing from class next week
  
  // DONE boolean combining functions
  
  printf ( "C++ says %s\n",
          booleanToString(4 == 4)) ;
  
  printf ( "C++ says %s\n",
          booleanToString(true == true)) ;
  printf ( "C++ says %s\n",
          booleanToString(true == false)) ;
  
  printf ( "C++ says %s\n",
          booleanToString(isJayLikeTotallyTheBestProfEvar
                          == 
                          isUtahGoodAtFootball)) ;
  

  printf ( "C++ says %s\n",
          booleanToString(true && true)) ;
  printf ( "C++ says %s\n",
          booleanToString(true && false)) ;
  printf ( "C++ says %s\n",
          booleanToString(false && true)) ;
  printf ( "C++ says %s\n",
          booleanToString(false && false)) ;
  
  
  printf ( "C++ says %s\n",
          booleanToString((true && true) && true)) ;
  
  // inclusive or
  printf ( "C++ says %s\n",
          booleanToString(true || true)) ;
  printf ( "C++ says %s\n",
          booleanToString(true || false)) ;
  printf ( "C++ says %s\n",
          booleanToString(false || true)) ;
  printf ( "C++ says %s\n",
          booleanToString(false || false)) ;
  
  // exclusive or = xor = ZORRRRR
  // maybe be wrong...
  printf ( "C++ says %s\n",
          booleanToString(true ^ true)) ;

  bool theSkyIsPurpleHuh = true;
  printf ( "C++ says %s\n",
          booleanToString(theSkyIsPurpleHuh));  
  printf ( "C++ says %s\n",
          booleanToString(! theSkyIsPurpleHuh)) ;
  
  printf ( "C++ says %s\n",
          booleanToString(! false)) ;

  printf ( "C++ says %s\n",
          booleanToString(! (4 == 3))) ;
  printf ( "C++ says %s\n",
          booleanToString(4 != 3)) ;

  printf ( "C++ says %s\n",
          booleanToString(!!!!!!! false)) ;

  // DONE returning booleans (intervals, identifiers)
  
  // determining if a number is in an interval
  // Is 4 inside (5,99) => false
  
  printf ( "The answer is %s, but should be %s\n",
          booleanToString(insideOpenIntervalFrom5to99Huh(4)),
          booleanToString(false) ) ;  
  
  
  // TODO when to use aux functions

  return 0;
}
