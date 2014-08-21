#include <stdio.h>
#include <math.h>

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

  // movie theater
  /*
"Imagine the owner of a movie theater who has complete freedom in
setting ticket prices. The more he charges, the fewer the people who
can afford tickets. In a recent experiment the owner determined a
precise relationship between the price of a ticket and average
attendance. At a price of $5.00 per ticket, 120 people attend a
performance. Decreasing the price by a dime ($.10) increases
attendance by 15. Unfortunately, the increased attendance also comes
at an increased cost. Every performance costs the owner $180. Each
attendee costs another four cents ($0.04). The owner would like to
know the exact relationship between profit and ticket price so that he
can determine the price at which he can make the highest profit."

Information: tells the ticket price, the fixed costs of performance,
gives a relationship between price and attendees, [theater owner is
man], goal: optimize profit, the variable cost of attendees, very
predictable,

Data Relationships:
income expenses -> profit
price attendees -> income
fixed variable attendees -> expenses
price -> attendees

Data:
profit is a double
income is a double
expenses is a double
price is a double
fixed is a double
variable is a double
attendees is a int
  */

  // attendees : ticket-price -> persons
int attendees ( double price ) {
    // Template: price

    // Example 1:
    // price = 5
    // ans = 120
    //return 120;

    // Example 2:
    // price = 4.9
    // ans = 135
    //return 135 ;

    // Example 3:
    // price = 5.1
    // ans = 105
    //return 105;

    // Generalize 1/2/3
    double difference_from_5 = (price - 5.0);
    double number_of_steps = difference_from_5 / .10 ;
    double lost_sheep = number_of_steps * 15.0 ;
    // Compare the output of these two returns:
    //return 120 - ceil(lost_sheep) ;    
    return 120 - floor(lost_sheep) ;    
  }

  // income : ticket-price -> dollars
  double income ( double price ) {
    return price * attendees(price) ;
  }

  // expenses : ticket-price -> dollars
  double fixed = 180.00;
  double variable = 0.04;
  double expenses ( double price ) {
    return fixed + variable * attendees(price) ;
  }

  // profit : ticket-price -> profit
  // To compute the theatre's profit
  double profit ( double price ) {
    return income(price) - expenses(price) ;
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
  /* Annoying, but okay way:

           cheapskateTip( 25.00 ),
=
if (price < 40) {
  return price * 1.10 ;
  } else {
  return price ;
  }
[where price = 25 ]
=
if (25 < 40) {
  return 25 * 1.10 ;
  } else {
  return 25 ;
  }
=
if (true) {
  return 25 * 1.10 ;
  } else {
  return 25 ;
  }
=
  return 25 * 1.10 ;
   */

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

  /*
    There has been a great difficulty in getting anything into the heads
    of this generation. It has been like splitting hemlock knots with a
    corn-dodger for a wedge, and a pumpkin for a beetle. - Joseph Smith
  */

  // substitution with cheapskateTip => data dependency
  // strung

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

  // TODO Email class about why I am dum about boolean printing


  printf ( "The answer is %d, but should be %d\n",
           attendees(5.0),
           120 ) ;
  printf ( "The answer is %d, but should be %d\n",
           attendees(4.9),
           135 ) ;
  printf ( "The answer is %d, but should be %d\n",
           attendees(5.1),
           105 ) ;
  printf ( "The answer is %d, but should be %d\n",
           attendees(5.05),
           112 ) ;

  // TODO movie theater best

  return 0;
}
