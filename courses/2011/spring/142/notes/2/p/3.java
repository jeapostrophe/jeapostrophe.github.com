class C {
    // areaOfDisk : double -> double
    // Finds the area of a disk of radius radius
    static double areaOfDisk ( double radius ) {
	return 3.14 * radius * radius;
    }

    // areaOfRing : double double -> double
    // Finds the area of ring where the outer radius is outer and the inner radius is inner
    static double areaOfRing ( double outer, double inner ) {
	return areaOfDisk(outer) - areaOfDisk(inner);
    }

    public static void main(String[] args) {
        System.out.format("The answer is %.1f%nThe answer should be 50.2%n",
			  areaOfRing(5,3));
    }
}