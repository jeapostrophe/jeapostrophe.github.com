class scratch {
    static String sign ( double n ) {
	if ( n > 0 ) {
	    return "Positively positive";
	} else if ( n < 0 ) {
	    return "Depressingly negativo";
	} else if ( n == 0 ) {
	    return "Probably zero?";
	} else {
	    return "The nega number";
	}
    }

    // isPositiveHuh : double -> boolean
    // to discover if the number is positive
    static boolean isPositiveHuh ( double n ) {
	/*if ( n > 0 ) {
	    return true;
	} else {
	    return false;
	    }*/
	return n > 0;
	//return ... n ... ;
    }

    // isFiveHuh : integer -> boolean
    // to discover if the integer if 5
    static boolean isFiveHuh ( int might_be_five ) {
	return 5 == might_be_five ;
    }

    // isInIntervalOpen5to23Closed : double -> boolean
    // discovers if the number is in the interval (5,23]
    static boolean isInIntervalOpen5to23Closed ( double n ) {
	return n > 5 && n <= 23;
    }

    // isSolutionToWeirdEquation : double -> boolean
    // discovers if the number is a solution to... ((6n+21)/5) - 2 = 71
    static boolean isSolutionToWeirdEquation ( double n ) {
	return ((6.0*n+21.0)/5.0) - 2.0 == 71.0 ;
    }

    // Problem: Suppose the bank pays 4% for deposits of up to $1,000 (inclusive),
    // 4.5% for deposits of up to $5,000 (inclusive), and 5% for deposits of more
    // than $5,000.
    // Information (have) : how much our deposit is
    // Information (want) : how much interest we'll make
    // Data: doubles that either in [0,1000] or (1000,5000] or (5000, infinity]

    // A bank-deposit is either...
    //  - a double in [0,1000]
    //  - a double in (1000,5000]
    //  - a double in (5000,infinity]

    // Contract : interest : bank-deposit -> double
    // Purpose: to compute the amount of interest due to the depositor
    // Examples:
    // interest ( 400 ) = 0.04 * 400 = 4 * 4 = 16
    // interest ( 1000 ) = 0.04 * 1000 = 40
    // interest ( 1500 ) = 0.045 * 1500 = 67.50
    // interest ( 5500 ) = 0.05 * 5500 = 275
    static double interest ( double deposit ) {
	// Taking stock:
	// return ... deposit ... ;
	// write down the arguments that will be available
	// Now that the input is structured, our "taking stock"
	// has more stuff...
	/*
	if ( 0 <= deposit && deposit <= 1000 ) {
	    // We know it is in the first category
	    return ... deposit ... ;
	} else if ( 1000 < deposit && deposit <= 5000 ) {
	    // We know it is in the second category
	    return ... deposit ... ;
	} else {
	    // We know it is in the third category
	    return ... deposit ... ;
	}
	*/
	// Now that we've done examples... fill in the "..."s
	if ( 0 <= deposit && deposit <= 1000 ) {
	    // We know it is in the first category
	    return 0.04 * deposit ;
	} else if ( 1000 < deposit && deposit <= 5000 ) {
	    // We know it is in the second category
	    return 0.045 * deposit ;
	} else {
	    // We know it is in the third category
	    return 0.05 * deposit ;
	}

    }

    // Problem: The charity credit union needs to identify big spenders for hitting for donations.
    // Contract : shouldCallHuh : bank-deposit -> boolean
    // Purpose : to decide if the depositor should be called
    // Examples:
    // shouldCallHuh( 500 ) = false
    // shouldCallHuh( 3000 ) = false
    // shouldCallHuh( 50000 ) = true
    static boolean shouldCallHuh ( double deposit ) {
	if ( 0 <= deposit && deposit <= 1000 ) {
	    // We know it is in the first category
	    return false ;
	} else if ( 1000 < deposit && deposit <= 5000 ) {
	    // We know it is in the second category
	    return false ;
	} else {
	    // We know it is in the third category
	    return true ;
	}

	//return (5000 < deposit);
    }

    /*
    shouldCallHuh(50000)
=>
    	if ( 0 <= deposit && deposit <= 1000 ) {
	    return false ;
	} else if ( 1000 < deposit && deposit <= 5000 ) {
	    return false ;
	} else {
	    return true ;
	}
=>
    	if ( 0 <= 50000 && 50000 <= 1000 ) {
	    return false ;
	} else if ( 1000 < 50000 && 50000 <= 5000 ) {
	    return false ;
	} else {
	    return true ;
	}
=>
    	if ( true && 50000 <= 1000 ) {
	    return false ;
	} else if ( 1000 < 50000 && 50000 <= 5000 ) {
	    return false ;
	} else {
	    return true ;
	}
=>
    	if ( true && false ) {
	    return false ;
	} else if ( 1000 < 50000 && 50000 <= 5000 ) {
	    return false ;
	} else {
	    return true ;
	}
=>
    	if ( false ) {
	    return false ;
	} else if ( 1000 < 50000 && 50000 <= 5000 ) {
	    return false ;
	} else {
	    return true ;
	}
=>
    	if ( 1000 < 50000 && 50000 <= 5000 ) {
	    return false ;
	} else {
	    return true ;
	}
=>
    	if ( true && 50000 <= 5000 ) {
	    return false ;
	} else {
	    return true ;
	}
=>
    	if ( true && false ) {
	    return false ;
	} else {
	    return true ;
	}
=>
    	if ( false ) {
	    return false ;
	} else {
	    return true ;
	}
=>
    	return true ;
=>
	true
*/


    public static void main (String[] args) {	
	System.out.format("The answer is: %d%n", 12/8 * 3);
	System.out.format("The answer is: %s%n", sign(1));
	System.out.format("The answer is: %s%n", sign(0));
	System.out.format("The answer is: %s%n", sign(-1));
	System.out.format("The answer is: %f%n", Math.pow(400000,40000));
	System.out.format("The answer is: %s%n", sign(Math.pow(400000,40000)));
	System.out.format("The answer is: %f%n", Math.sqrt(-1));
	System.out.format("The answer is: %s%n", sign(Math.sqrt(-1)));
	//System.out.format("The answer is: %d%n", 12/0);

	System.out.format("The answer is: %b%n", true);
	System.out.format("The answer is: %b%n", false);
	double fixedCost = 180;
	boolean jayIsAmaaaazing = true; 
	System.out.format("The answer is: %b%n", jayIsAmaaaazing);
	System.out.format("The answer is: %b%n", 1 == 1);
	System.out.format("The answer is: %b%n", 1 == 5);
	System.out.format("The answer is: %b%n", 1 < 5);
	System.out.format("The answer is: %b%n", 1 <= 5);
	System.out.format("The answer is: %b%n", 1 > 5);
	System.out.format("The answer is: %b%n", 1 >= 5);
	System.out.format("The answer is: %b should be true%n", isPositiveHuh(1));
	System.out.format("The answer is: %b should be false%n", isPositiveHuh(0));
	System.out.format("The answer is: %b should be false%n", isPositiveHuh(-11));
	System.out.format("The answer is: %b%n", true && true);
	System.out.format("The answer is: %b%n", true && false);
	System.out.format("The answer is: %b%n", false && true);
	System.out.format("The answer is: %b%n", false && false);
	System.out.format("The answer is: %b%n", false == false);
	System.out.format("The answer is: %b%n", true || true);
	System.out.format("The answer is: %b%n", true || false);
	System.out.format("The answer is: %b%n", false || true);
	System.out.format("The answer is: %b%n", false || false);
	System.out.format("The answer is: %b%n", ! (1 == 1));
	System.out.format("The answer is: %b%n", ! true);
	System.out.format("The answer is: %b%n", ! false);
	System.out.format("The answer is: %b%n", 1 <= 1);
	System.out.format("The answer is: %b%n", 1 != 1);

	System.out.format("The answer is: %b but should be %b%n", isFiveHuh(5), true);
	System.out.format("The answer is: %b but should be %b%n", isFiveHuh(55), false);
	System.out.format("The answer is: %b but should be %b%n", isFiveHuh(-5), false);
	System.out.format("The answer is: %b but should be %b%n", isFiveHuh(6), false);
	System.out.format("The answer is: %b but should be %b%n", isFiveHuh(2), false);

	System.out.format("The answer is: %b but should be %b%n", 
			  isInIntervalOpen5to23Closed(2),
			  false);
	System.out.format("The answer is: %b but should be %b%n", 
			  isInIntervalOpen5to23Closed(5),
			  false);
	System.out.format("The answer is: %b but should be %b%n", 
			  isInIntervalOpen5to23Closed(5.0000001),
			  true);
	System.out.format("The answer is: %b but should be %b%n", 
			  isInIntervalOpen5to23Closed(17),
			  true);
	System.out.format("The answer is: %b but should be %b%n", 
			  isInIntervalOpen5to23Closed(23.0000001),
			  false);

	System.out.format("The answer is: %b should be %b%n",
			  isSolutionToWeirdEquation(0), false);
	System.out.format("The answer is: %b should be %b%n",
			  isSolutionToWeirdEquation(((5.0 * (71.0 + 2.0)) - 21.0) / 6.0), true);

	System.out.format("The answer is: %f should be %f%n",
			  interest(400), 16.0);
	System.out.format("The answer is: %f should be %f%n",
			  interest(1000), 40.0);
	System.out.format("The answer is: %f should be %f%n",
			  interest(1500), 67.5);
	System.out.format("The answer is: %f should be %f%n",
			  interest(5500), 275.0);

	System.out.format("The answer is: %b should be %b%n",
			  shouldCallHuh(500), false);
	System.out.format("The answer is: %b should be %b%n",
			  shouldCallHuh(3000), false);
	System.out.format("The answer is: %b should be %b%n",
			  shouldCallHuh(50000), true);

    }
}