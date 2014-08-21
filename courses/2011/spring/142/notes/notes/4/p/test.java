public class test {
        // 4a
	// Info (have)	:  given equation (2n^2 = 102) to solve.
	// Info (want)	:  to indicate whether a given number is a solution for n
	// solveForN1	:  double -> boolean
	// purpose	:  to test a given number against an equation and return a boolean indicating the result
	// examples	:
	// solveForN1(2) = false
	// solveForN1(20) = false
	// solveForN1(Math.sqrt(51)) = true
	public static boolean solveForN1 (double n) {
	    return Math.abs((Math.pow(n,2.0) * 2.0) - 102.00) <= 0.000001;
	}

        // this is my test I wrote later to make sure my math was right
	public static double test (double n) {
		return (Math.pow(n,2.0) * 2.0);
	}
	public static void main (String[] args) {
       		System.out.format ("---4a---solveForN1---%n");
		System.out.format ("The answer is: %b  but it should be %b%n", solveForN1(2), false);
		System.out.format ("The answer is: %b  but it should be %b%n", solveForN1(20), false);
		System.out.format ("The answer is: %b  but it should be %b%n", solveForN1(Math.sqrt(51.0)), true);
		System.out.format ("The answer is: %b  but it should be %b%n", solveForN1(7.141428429), true);
		System.out.format ("The answer is: %.24f  but it should be %f%n", test(Math.sqrt(51)), 102.0);

	}
}