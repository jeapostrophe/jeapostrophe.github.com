class C {
    // fahrenheitToCelsius : double -> double
    // Converts a temperature in fahrenheit to celsius
    static double fahrenheitToCelsius ( double f ) {
	return (f - 32) * 5/9;
    }

    public static void main(String[] args) {
        System.out.format("The answer is %.1f%nThe answer should be -9.0%n",
			  fahrenheitToCelsius(15.8));
    }
}