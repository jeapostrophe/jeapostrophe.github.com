class C {
    // areaOfTriangle : double double -> double
    // Computes the area of a right triangle with base b and height h
    static double areaOfTriangle ( double b, double h ) {
	return 0.5 * b * h;
    }

    public static void main(String[] args) {
        System.out.format("The answer is %.1f%nThe answer should be 6.0%n",
			  areaOfTriangle(3, 4));
    }
}