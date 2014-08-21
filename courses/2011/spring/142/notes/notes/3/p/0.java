class C {
    static double fixed_cost = 180;
    static double variable_cost = .04;

    // profit : double -> double
    // to compute the profit as the difference between revenue and costs
    // at some given ticket price
    static double profit ( double ticket_price ) {
	return revenue(ticket_price) - cost(ticket_price);
    }

    // revenue : double -> double
    // to compute the revenue, given ticket price
    static double revenue ( double ticket_price ) {
	return ticket_price * attendees(ticket_price);
    }

    // cost : double -> double
    // to compute the costs, given ticket price
    static double cost ( double ticket_price ) {
	return fixed_cost + variable_cost * attendees(ticket_price);
    }

    // attendees : double -> int
    // to compute the number of attendees, given ticket price
    static int attendees ( double ticket_price ) {
	return 120 + (int)((15. / .1) * (5. - ticket_price));
    }

    // best_price : double double double -> string
    // to compute the best price from the options
    static String best_price ( double p1, double p2, double p3 ) {
	double pr1 = profit(p1);
	double pr2 = profit(p2);
	double pr3 = profit(p3);
	if ( pr1 < pr2 ) {
	    if ( pr3 < pr2 ) {
		return "Second";
	    } else {
		return "Third";
	    }
	} else {
	    if ( pr3 < pr1 ) {
		return "First";
	    } else {
		return "Third";
	    }
	}
    }
		
    public static void main(String[] args) {
        System.out.format("The answer is %.1f%nThe answer should be 1063.2%n",
			  profit(3.));
        System.out.format("The answer is %.1f%nThe answer should be 889.22%n",
			  profit(4.));
        System.out.format("The answer is %.1f%nThe answer should be 415.2%n",
			  profit(5.));
        System.out.format("The answer is %s%nThe answer should be First%n",
			  best_price(3.,4.,5.));
        System.out.format("The answer is %s%nThe answer should be Second%n",
			  best_price(4.,3.,5.));
        System.out.format("The answer is %s%nThe answer should be Third%n",
			  best_price(4.,5.,3.));
    }
}