class C {
    // sign : integer -> String
    // Returns a word representing the sign of the number
    static String sign ( int n ) {
	if (n > 0) {
	    return "positive";
	} else if ( n < 0 ) {
	    return "negative";
	} else {
	    return "zero";
	}
    }

    public static void main(String[] args) {
        System.out.format("The answer is %s%nThe answer should be positive%n",
			  sign(1));
        System.out.format("The answer is %s%nThe answer should be negative%n",
			  sign(-1));
        System.out.format("The answer is %s%nThe answer should be zero%n",
			  sign(0));
    }
}