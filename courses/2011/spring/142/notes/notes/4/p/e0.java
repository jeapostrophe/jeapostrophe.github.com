class e0 {
    static String magiclamp ( String password ) {
	if ( password == "open sesame" ) {
	    return "Three Wishes";
	} else {
	    return "Better Luck Next Time";
	}
    }

    public static void main (String[] args) {
	System.out.format("The answer is: %b%n", true);
	System.out.format("The answer is: %b%n", false);
	System.out.format("The answer is: %b%n", 1 > 2);
	System.out.format("The answer is: %b%n", 1 == 2);
	System.out.format("The answer is: %b%n", 1 == 2 || 2 == 2);
	System.out.format("The answer is: %b%n", 1 == 2 && 2 == 2);
	System.out.format("The answer is: %b%n", ! (1 == 2));
	System.out.format("The answer is: %b but should be true%n", "Hey" == "Hey");
	System.out.format("The answer is: %b but should be false%n", "Hey" == "Frog");
	System.out.format("The answer is: %b but should be true%n", "Frog" == "Frog");
	System.out.format("The answer is: %s but should be Three Wishes%n", magiclamp("open sesame"));
	System.out.format("The answer is: %s but should be Better Luck Next Time%n", magiclamp("porridge"));
    }
}