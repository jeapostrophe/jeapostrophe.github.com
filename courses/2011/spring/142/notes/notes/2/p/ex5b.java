class C {
    // f : int -> double
    // f(x) = 1/2n^2 + 20
    static double f ( int n ) {
	return 0.5 * n * n + 20;
    }

    public static void main(String[] args) {
        System.out.format("The answer is %.1f%nThe answer should be 22.0%nThe answer is %.1f%nThe answer should be 60.5%n",
			  f(2), f(9));
    }
}