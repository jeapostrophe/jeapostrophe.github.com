class C {
    // f : int -> int
    // f(x) = n^2 + 10
    static int f ( int n ) {
	return n * n + 10;
    }

    public static void main(String[] args) {
        System.out.format("The answer is %d%nThe answer should be 14%nThe answer is %d%nThe answer should be 91%n",
			  f(2), f(9));
    }
}