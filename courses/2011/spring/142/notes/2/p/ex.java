class C {
    // tip : double -> double
    // Returns the price with a 15% tip
    static double tip (double price) {
	return price * 1.15;
    }

    // shareWithTip : double int -> double
    // Returns the share of a pizza owed if the pizza costs total, contains 8 slices, and you ate share slices
    static double shareWithTip ( double total, int share ) {
	return (tip(total) / 8) * share;
    }

    public static void main(String[] args) {
        System.out.format("The answer is %.1f%nThe answer should be 5.2%n",
			  shareWithTip(12.0, 3));
    }
}