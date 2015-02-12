abstract class Abby {
    abstract int g(int a);

    int a;
    int f() {
        return this.g(a);
    }
}

class Baby extends Abby {
    int g (int a) { return a; }
}

class C7 {
    // informal: returns a number bigger than the input
    // more formal: ret > input
    // most formal: \forall x, f(x) > x
    // most formal: \forall x, x + 2 > f(x) > x
    static int f (int x) {
        return x + 1;
    }

    // informal: increments the first value in input by one
    // formal: \forall xs , MEM_LOOKUP(g(MEM, xs), 0) = MEM_LOOKUP(MEM, 0) + 1
    static void g (int[] xs) {
        xs[0]++;
        return;
    }

    // Static means "before the program starts running" aka "at compile time"
    static int arrh = 8; 
    static void h () {
        int arr = 7;
        // arr = "foo"; // <-- statically detected error
        C7.f(8); // <--- statically determined function call
        // BST o = ...; // <-- BST is an interface, so we don't know what class
        // o.f(8); // <--- dynmically determined function call (i.e. what class is "o"?)
    }
}

// distance : num -> num <-------------- contract
// computes the cartesian distance <---- purpose

// f : Even number -> Prime number

// Under-determined System:
// x + y = 6

// x = 1, y = 5
// x = 0, y = 6

// x + y = 6 AND x = 4

class UnderDetermined {
// Under-determined abstraction:
// First, WHAT rather HOW
// Second, some input may not be specified

// f : int -> int
// informal: when input is negative, returns the absolute value of input
// formal: negative(input) -> f(input) = |input|
    static int f (int i) {
        if ( i < 0 ) { return -1 * i; }
        else { throw new RuntimeException("not negative"); }
        // else { return 17; }
    }
}

class Abstraction {
    // f : int -> int
    // f(x) is three more than twice its input
    // computes the birthweight of the nth child of a sparrow
    static int f ( int x ) {
        return 2 * x + 3;
    }

    // af : int -> int
    // af(x) is a more than m times its input
    static int af ( int m, int a, int x ) {
        return m * x + a;
    }
}

class Complexity {
    // sort
    // time is O(n lg n)
    static void sort (int[] r) {
    }
}

class Total {
    // partial means "not defined on all input" (i.e. under-determined)
    // total means "defined on all input"
    // input means "the set specified by the Java type"

    // f : int -> int <---- total
    static int f (int x) {
        return 0;
    }

    // g: Even -> int <---- partial
    static int g (int x) {
        return 0;
    }

    // search : sorted array of ints & int -> int
    // search(xs,g) = -1 OR xs[search(xs,g)] = g
    // sortedness is an invariant of the input
    static int search( int[] s, int g ) {
        // do a binary search
        return -1;
    }
}

// Coq, Alloy, Z, SMT
