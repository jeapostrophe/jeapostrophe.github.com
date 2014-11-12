class C28 {
    // f : int -> int
    // f should take in x and compute the McCarthy coefficient of it.
    public static int f( int x ) {
        // .........
        // ........
        return x;
    }
    // Black box ignores the implmentation.
    // - look up in a book that the MC of an even number is always odd.
    //   forall n, f( 2 * n ) modulo 2 == 1
    // - i know that f(6) is 7.
    //   f(6) == 7

    // contrasts with

    // Glass box that looks at the implementation
    // - is the output always the right type?
    // - don't throw any exceptions on valid input
    // - don't go into infinite loops
    // - line coverage (how many lines of code did tests run)
    // - path coverage (which combinations of if statement directions did you go down)
    public static int g (int x) {
        int r = 0;
        if ( x < 70 ) {
            r = r + 17;
        }
        if ( x > 10 ) {
            r = r - 1;
        }
        return r;
    }
    // Full line coverage
    // f(71)
    // f(9)
    // Full path coverage
    // f(11)
    // Does a program have a finite number of paths?

    // FINITE
    // - Some programs are finite because they have no ifs
    // - All real programs are finite because computers have finite memory
    // INFINITE
    //  - while ( true ) { if ( x < 70 ) { x -= 5 } else if ( x > 60 ) { x += 5 } }
    // A
    // B
    // C
    // ABABABABABABABABA
    // BABABABABABA

    // Approx of full path coverage
    // - schematic paths (also called regular paths because they are regular languages) [ model checking ]
    // (AB)*
    // (BA)*
    // - a finite truncation of the infinite path space
    static int h (int[] a) {
        int r = 0;
        for (int i = 0; i < a.length; i++) {
            // A
            r = r + a[i];
        }
    }
    // Paths = A_0 ... A_n
    // call on arrays from size 0 to size 20.
    static int hp (int[] a) {
        int r = 0;
        for (int i = 0; i < a.length - 1; i++) {
            if ( a[i] < a[i+1] ) {
                // A
                r = r + a[i];
            } else {
                // B
                r = r - a[i];
            }
        }
    }
    // A
    // B
    // AB
    // BA
    // AAB
    // BAA
    // BBA
    // For an array of size n, there are 2^n different ways of going through.
    static int hpp (int[] a) {
        int r = 0;
        for (int i = 0; i < a.length - 1; i++) {
            r = r + hpph(a[i], a[i+1]);
        }
    }
    // spec when l is less than r, return l otherwise -1 * l
    static int hpph(int l, int r) {
        if ( l < r ) {
            return l;
        } else {
            return -1 * l;
        }               
    }

    // Always split big funs into a few little funs?
    // NO, from Ben with love: mutation and lots of locals

    // ..define x...define y..
    // ------ pass x and y as arguments
    // ..use x..use y.....define z..sets x..
    // ------ return z, x and caller needs update x
    // ... work with z ...
    
    public static void main( String[] args ) {
    }
}

// Validation
// - Verification
// - Testing

// Verification
// - Theorems
// - Informal

// Testing
// - Black box: Based on spec
// - Glass box: Based on code

// Glass
// - Aliasing
// - Boundaries

// Testable code
// - Loops or no?
// - Small functions
// - More parameters
// - More returns
// - Error early

// Bugs vs Errors

// Finding errors
// - Build and expand your knowledge
// - Small problems better than big problems

