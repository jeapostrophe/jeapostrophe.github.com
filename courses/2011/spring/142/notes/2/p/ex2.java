class C {
    // dollarToYen : double -> double
    // Converts a price in dollars to yen
    static double dollarToYen ( double d ) {
	return d*81.65;
    }

    public static void main(String[] args) {
        System.out.format("The answer is %.1f%nThe answer should be 6532.0%n",
			  dollarToYen(80));
    }
}