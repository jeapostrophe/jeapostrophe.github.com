#include <stdio.h>

int main () {
  printf ( "The answer is %d\n", 1 + 1 ) ;
  printf ( "The answer is %d\n", 8245879 + 8747 ) ;
  printf ( "The answer is %f\n", 1.0 / 2.0 ) ;

  // 5 = 101
  // 5634 = 

  // compilation : de-abstracting... going from C++ to the machine

  // A comment starts with // and continues with English until the end of the line
  // 
  //

  /* A really long comment

     that lasts a bunch of lines */
  
  // A string starts with " and ends with another "
  // d = integer
  // \n = press the return key to have a new line
  printf ( "The answer is %d\n", 25 + 99 );
  printf ( "The answer is %d\n", 25 + 99 );

  // Testing:
  // compares two answers
  /// Possibility 1. Two different answers: right first, wrong second
  /// Possibility 2. Two different answers: wrong first, right second
  /// Possi 3. The same answer: did it right both times
  /// Possi 4. The same answer: wrong both times
  /// Possi 5. Different, both wrong
  /// Possi 6. Different, both right (normally because of rounding) [close enough]

  //  11
  //   25
  // + 99
  // ----
  //  124
  printf ( "The answer is %d but it should be %d\n",
           25 + 99,
           124 );

  //       41.6666666
  //    |----
  // 12 | 500
  //      48
  //       20
  //       12
  //        8
  printf ( "The answer is %d but it should be %d\n",
           500 / 12,
           41.66666 ) ;

  printf ( "The answer is %d but it should be %f\n",
           500 / 12,
           41.66666 ) ;

  printf ( "The answer is %f but it should be %f\n",
           500 / 12,
           41.66666 ) ;

  printf ( "The answer is %f but it should be %f\n",
           500.0 / 12.0,
           41.66666 ) ;
}
