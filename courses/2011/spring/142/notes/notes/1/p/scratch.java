class Scratch {
    public static void main (String[] args) {
	// x / y = z
	// => y * z = x
	// 91 / 2 = 45
	// => 45 * 2 = 91
	// %d => no decimals, "integer"
	System.out.format("The answer is: %d%n", 91 / 2);
	// %f => decimals, "floating point"
	System.out.format("The answer is: %f%n", 91.0 / 2);
	System.out.format("The answer is: %f%n", Math.sqrt(9));
	System.out.format("The answer is: %f%n", Math.sqrt(11));
	System.out.format("The answer is: %f%n", Math.sqrt(36481));
	System.out.format("The answer is: %f%n", Math.sqrt(-1));

	System.out.format("The answer is: %d%n", Math.abs(-2));
	System.out.format("The answer is: %d%n", Math.abs(2));

	System.out.format("The answer is: %f%n", Math.log(146.3));

	System.out.format("The answer is: %f%n", Math.pow( 23, 1067 ));
	System.out.format("The answer is: %f%n", Math.pow( 23, 6 ));

	System.out.format("The answer is: %d%n", 2 + 3 * 6);
	System.out.format("The answer is: %d%n", (2 + 3) * 6);
	System.out.format("The answer is: %d%n", ((2 + 3) * 6));

	System.out.format("The answer is: %d%n", (((2 + 3) * (6))));
	
	System.out.format("The answer is: %f%n", Math.pow( (20+3), 6 ));
	System.out.format("The answer is: %f%n", Math.pow( (20+3), (2+2+2) ));
	System.out.format("The answer is: %f%n", Math.pow( (20+3), Math.sqrt(36) ));
    }
}