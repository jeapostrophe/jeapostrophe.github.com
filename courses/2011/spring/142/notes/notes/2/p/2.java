class C {
    // areaOfDisk : double -> double
    // Finds the area of a disk of radius radius
    static double areaOfDisk ( double radius ) {
	return 3.14 * radius * radius;
    }

    public static void main(String[] args) {
        System.out.format("The answer is %.1f%nThe answer should be 78.5%n",
			  areaOfDisk(5));
    }
}