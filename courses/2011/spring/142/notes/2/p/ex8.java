class C {
    // total_profit : int -> double
    // Computes the total profit for n attendees of a movie where each customer pays $5 and the showing costs $20 + $0.5n
    static double total_profit (int n) {
	return (5 * n) - (20 + 0.5 * n);
    }

    public static void main(String[] args) {
        System.out.format("The answer is %.2f%nThe answer should be %.2f%n",
			  total_profit(5),
			  25 - 22.5);
    }
}