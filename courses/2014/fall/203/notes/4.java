class C4 {
    // f(x) = x + 3
    // f is a set... { (x, y) | y = x + 3 }
    // (1,4) \in f ?
    // image(f) = ?
    // f in math is a mapping OF input to output

    // g(x) = x + 2 + 1

    public static int f ( int x ) {
        return x + 3;
    }
    public static int g ( int x ) {
        return x + 2 + 1;
    }
    // f in java is a method of going from input to output

    // Good procedural abstractions allow you to think that Java
    // functions ARE Math functions.

    // What vs How is the difference between Math & Java

    // Locality & Modifiability
    // - An abstraction sets up an analysis barrier

    // The contract or type signature or header
    // search : int[] int -> boolean
    // Purpose: searchs for k in a
    public static boolean search ( int[] a, int k ) {
        // EFFECTS: returns true if the integer k is in the array a
        // and returns false otherwise

        // Hoare Logic
        int i = 0;

        // LoopInv = k is not in a[0...i]

        // LoopInv
        while ( i < a.length ) {
            // LoopInv
            if ( a[i] == k ) {
                /* k is in a (met by IF) */ return true; /* SPEC MET */
            }
            // ( a[i] == k ) is false
            i++;
            // LoopInv
        }
        // ( i < a.length ) is false /\ LoopInv
        /* k is not in a */ return false; /* SPEC MET */
    }    

    // \aleph_0 is the infinity of the naturals or "countable infinity"
    // (5 ((1 (2 3)) (4 5)))

    // A specification is a contract between the author of the
    // function and the caller of the function. The author is like a
    // seller and the caller is like a buyer.

    public static void main ( String[] args ) {
        System.out.println("What?");
        System.out.println( search( new int[] {0,1,2,3,4}, 5 ) + " should be " + false );
        System.out.println( search( new int[] {0,1,2,3,4}, 4 ) + " should be " + true );

        // replaced the search() call with implementation
        // int i = 0;
        // while ( i < (new int[] {0,1,2,3,4}).length ) {
        //     if ( (new int[] {0,1,2,3,4})[i] == 4 ) {
        //         return true;
        //     }
        //     i++;
        // }
        // return false;
        // ======>
        // int i = 0;
        // if ( i < (new int[] {0,1,2,3,4}).length ) {
        //     if ( (new int[] {0,1,2,3,4})[i] == 4 ) {
        //         return true;
        //     }
        //     i++;
        //     while ( i < (new int[] {0,1,2,3,4}).length ) {
        //         if ( (new int[] {0,1,2,3,4})[i] == 4 ) {
        //             return true;
        //         }
        //         i++;
        //     }
        // }
        // return false;
        // ======>
        // int i = 1;
        // while ( i < (new int[] {0,1,2,3,4}).length ) {
        //     if ( (new int[] {0,1,2,3,4})[i] == 4 ) {
        //         return true;
        //     }
        //     i++;
        // }
        // return false;

        // while (c) { b } => if (c) { b ; while (c) { b } }

        // ...what java does...

        // replaced the search() call with specification
        System.out.println( true + " should be " + true );
    }
}

// Emacs like me:
// https://github.com/jeapostrophe/exp/blob/master/.emacs.el

// M-x compile
// javac file.java
// M-x compile
// java class
// M-x compile
// javac file.java && java class

// One time:
// M-x global-set-key
// then the key (C-t)
// then type 'compile'

// Procedural Abstraction
