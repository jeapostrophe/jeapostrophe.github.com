class scratch {

    // convert3 : integer integer integer -> integer
    // Purpose: to compute the decimal integer given some particular ones places, tens places, and hundreds place
    // Examples:
    // convert3(1,2,3) = 321
    // convert3(7,4,1) = 147 = 1 * 100 + 4 * 10 + 1 * 1
    static int convert3 ( int ones, int tens, int hundreds ) {
	return hundreds * 100 + tens * 10 + ones * 1;
    }

    // areaOfDisk : double -> double
    // Purpose: to compute the area of a disk with radius 'r'
    // Examples:
    // areaOfDisk( 2 ) = pi * 2^2
    // areaOfDisk( 4 ) = pi * 4^2
    static double areaOfDisk ( double r ) {
	return Math.PI * Math.pow( r, 2 );
    }

    // Problem: Find the area of a ring
    // Information (have): radii of the outer and inner circles, maybe we just know the diameters, position, material
    // Information (want): area (in furlongs)
    // Information (have, relevant): radii
    // areaOfRing : double double -> double
    // Purpose: to compute the area of a ring with inner radius 'inner' (it comes first) and outer radius 'outer' (it comes last)
    // Examples:
    // areaOfRing( 2, 4 ) = 4*12.686 - 12.686 = 3*12.686
    static double areaOfRing ( double inner, double outer ) {
	return areaOfDisk(outer) - areaOfDisk(inner);
	//return Math.PI * Math.pow( outer, 2 ) - Math.PI * Math.pow( inner, 2 );
    }

    /*
      Imagine the owner of a movie theater who has complete freedom in setting ticket prices. The more he charges, the fewer the people who can afford tickets. In a recent experiment the owner determined a precise relationship between the price of a ticket and average attendance. At a price of $5.00 per ticket, 120 people attend a performance. Decreasing the price by a dime ($.10) increases attendance by 15. Unfortunately, the increased attendance also comes at an increased cost. Every performance costs the owner $180. Each attendee costs another four cents ($0.04). The owner would like to know the exact relationship between profit and ticket price so that he can determine the price at which he can make the highest profit.
     */
    // Information (have): the relationship between price and cost (on a per person basis) [ticket price - 0.04], the fixed cost [180], a relationship between price and attendance [@$5 we get 120, decrease by .10 we get 15 more], ticket price
    // Information (want): profit
    static double fixedCost = 180;
    
    // profit : double -> double
    // Purpose: Find the profit for a given ticket price
    // Example:
    // profit(4.9) = 661.5 - 185.4 = 476.1
    // profit(5) = 600 - 184.8 = 415.2
    static double profit ( double tp ) {
	return revenue(tp) - cost(tp);
    }
    
    // revenue : double -> double
    // Purpose: Find the revenue for a given ticket price
    // Examples:
    // revenue ( 4.9 ) = 4.9 * 135 = 661.5
    // revenue ( 5 ) = 5 * 120 = 600
    static double revenue ( double tp ) {
	return tp * attendance(tp);
    }

    // cost : double -> double
    // Purpose : Find the cost for a given t p.
    // Examples:
    // cost ( 4.9 ) = 180 + 0.04*135 = 185.4
    // cost ( 5 ) = 180 + 0.04*120 = 184.8
    static double cost ( double tp ) {
	return fixedCost + 0.04 * attendance(tp);
    }

    // attendance : double -> long
    // Purpose : Find the number of attendees at a ticket price
    // Example:
    // attendance ( 4.9 ) = 135
    // attendance ( 5 ) = 120
    // attendance ( 5.1 ) = 105
    static long attendance ( double tp ) {
	double differenceInPrice = 5 - tp;
	double numberOfSteps = (differenceInPrice / .1);
	long differenceInPeople = Math.round(Math.floor(numberOfSteps * 15));
	return 120 + differenceInPeople;
    }

    // Problem: The movie theater owner wants to know the best price between three options.
    // bestPrice : double double double -> double
    // Purpose: to compute the best price
    // Example:
    // bestPrice( 4.9, 5, 5.1 ) = 4.9
    // bestPrice( 5, 4.9, 5.1 ) = 4.9
    // bestPrice( 5, 5.1, 4.9 ) = 4.9
    static double bestPrice ( double first, double second, double third ) {
	// ... first ... second ... third ...
	// ... profit(first) ... profit(second) ... profit(third) ...
	// boolean is true or false
	if ( profit(first) > profit(second) && profit(first) > profit(third) ) {
	    return first;
	} else if ( profit(second) > profit(first) && profit(second) > profit(third) ) {
	    return second;
	} else {
	    return third;
	}
    }

// bestPrice : double double double -> String
    // Purpose: to compute the best price
    // Example:
    // bestPriceS( 4.9, 5, 5.1 ) = "First"
    // bestPriceS( 5, 4.9, 5.1 ) = "Second"
    // bestPriceS( 5, 5.1, 4.9 ) = "Third"
    static String bestPriceS ( double first, double second, double third ) {
	// ... first ... second ... third ...
	// ... profit(first) ... profit(second) ... profit(third) ...
	// boolean is true or false
	if ( profit(first) > profit(second) &&
	     profit(first) > profit(third) ) {
	    return "First";
	} else if ( profit(second) > profit(first) && profit(second) > profit(third) ) {
	    return "Second";
	} else {
	    return "Third";
	}
    }

    public static void main (String[] args) {
	System.out.format("The answer is: %d%n", 12/8 * 3);
	System.out.format("The answer is: %d but should be %d%n", convert3(1,2,3), 321);
	System.out.format("The answer is: %d but should be %d%n", convert3(7,4,1), 147);
	System.out.format("The answer is: %d but should be %d%n", convert3(7,40,1), 507);
	System.out.format("The answer is: %f but should be close to %f%n", areaOfDisk(2), 12.686);
	System.out.format("The answer is: %f but should be close to %f%n", areaOfDisk(4), 4*12.686);
	System.out.format("The answer is: %f but should be close to %f%n", areaOfRing(2,4), 3*12.686);

	System.out.format("The answer is: %d but should be close to %d%n", attendance(4.9), 135);
	System.out.format("The answer is: %d but should be close to %d%n", attendance(5), 120);
	System.out.format("The answer is: %d but should be close to %d%n", attendance(5.1), 105);
	System.out.format("The answer is: %f but should be close to %f%n", cost(4.9), 185.4);
	System.out.format("The answer is: %f but should be close to %f%n", cost(5.0), 184.8);
	System.out.format("The answer is: %f but should be close to %f%n", revenue(4.9), 661.5);
	System.out.format("The answer is: %f but should be close to %f%n", revenue(5), 600.0);


	System.out.format("The answer is: %f but should be close to %f%n", profit(4.9), 476.1);
	System.out.format("The answer is: %f but should be close to %f%n", profit(5), 415.2);


	System.out.format("The answer is: %f but should be close to %f%n", bestPrice(4.9,5,5.1), 4.9);
	System.out.format("The answer is: %f but should be close to %f%n", bestPrice(5,4.9,5.1), 4.9);
	System.out.format("The answer is: %f but should be close to %f%n", bestPrice(5,5.1,4.9), 4.9);

	System.out.format("The answer is: %s but should be close to %s%n", bestPriceS(4.9,5,5.1), "First");
	System.out.format("The answer is: %s but should be close to %s%n", bestPriceS(5,4.9,5.1), "Second");
	System.out.format("The answer is: %s but should be close to %s%n", bestPriceS(5,5.1,4.9), "Third");
    }
}
