interface List {
    public int length ();
    public int sum ();
    public String toString ();
}

class Zero implements List {
    public Zero (  ) {
    }

    public int length () {
	return 0;
    }

    public int sum () {
	return 0;
    }

    public String toString () {
	return "[]";
    }
}

class One implements List {
    public int elem;
    public List rest;

    public One ( int elem0, List rest0 ) {
	elem = elem0;
	rest = rest0;
    }

    public int length () {
	return 1 + rest.length();
    }

    public int sum () {
	return elem + rest.sum();
    }

    public String toString () {
	return String.format("%d:%s", this.elem, this.rest);
    }
}

class e {
    static int length ( List l0 ) {
	return l0.length();
    }
    static int sum ( List l0 ) {
	return l0.sum();
    }

    public static void main (String[] args) {	
	List l0 = new Zero ();
	List l1 = new One ( 1, l0 );
	List l2 = new One ( 2, l1 );
	List l3 = new One ( 3, l1 );
	List l4 = new One ( 3, l1 );

	System.out.format("The answer is: %d but should be %d%n", length(l0), 0);
	System.out.format("The answer is: %d but should be %d%n", length(l1), 1);
	System.out.format("The answer is: %d but should be %d%n", length(l2), 2);
	System.out.format("The answer is: %d but should be %d%n", length(l3), 2);

	System.out.format("The answer is: %d but should be %d%n", sum(l0), 0);
	System.out.format("The answer is: %d but should be %d%n", sum(l1), 1);
	System.out.format("The answer is: %d but should be %d%n", sum(l2), 3);
	System.out.format("The answer is: %d but should be %d%n", sum(l3), 4);

	System.out.format("l0 = %s%n", l0);
	System.out.format("l1 = %s%n", l1);
	System.out.format("l2 = %s%n", l2);
	System.out.format("l3 = %s%n", l3);
	System.out.format("l4 = %s%n", l4);
    }
}