class C {
    // wage : double -> doubles
    // Computes the wage at $12 per hour
    static double wage ( double hours ) {
	return 12 * hours;
    }

    // tax : double -> double
    // Computes a tax of 15%
    static double tax ( double gross_pay ) {
	return 0.15 * gross_pay;
    }

    // netpay : double -> double
    // Computes the after-tax pay at $12 per hour
    static double netpay ( double hours ) {
	return wage ( hours ) - tax ( wage (hours) );
    }

    public static void main(String[] args) {
        System.out.format("The answer is %.2f%nThe answer should be %.2f%n",
			  netpay(8.0),
			  8.0*12 - (8.0*12*0.15));
    }
}