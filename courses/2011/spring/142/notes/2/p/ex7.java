class C {
    // sum_coins : int int int int -> double
    // Computes the number of dollars inside a bag that contains 'pennies' pennies, " nickels, " dimes, and " quarters
    static double sum_coins ( int pennies, int nickels, int dimes, int quarters ) {
	return 0.01 * pennies + 0.05 * nickels + 0.10 * dimes + 0.25 * quarters;
    }

    public static void main(String[] args) {
        System.out.format("The answer is %.2f%nThe answer should be %.2f%n",
			  sum_coins( 10, 10, 10, 10),
			  4.10);
    }
}