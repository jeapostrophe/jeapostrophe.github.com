class C {
    // f : int -> double
    // f(x) = 2 - 1/n
    static double f ( int n ) {
	return 2 - 1.0/n;
    }

    public static void main(String[] args) {
        System.out.format("The answer is %.1f%nThe answer should be 1.5%nThe answer is %.1f%nThe answer should be 1.9%n",
			  f(2), f(9));
    }
}