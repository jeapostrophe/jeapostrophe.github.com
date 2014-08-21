class C {
    // convert3 : int int int -> int
    // Computes the integer where the ones digit is 'ones', the tens is 'tens', and the hundreds is 'hundreds'
    static int convert3 ( int ones, int tens, int hundreds ) {
	return ones + 10 * tens + 100 * hundreds;
    }

    public static void main(String[] args) {
        System.out.format("The answer is %d%nThe answer should be 321%n",
			  convert3(1, 2, 3));
    }
}