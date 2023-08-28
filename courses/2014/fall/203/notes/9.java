// (* 15 (+ (Problem "X is zero") 10)).GetYouOut(Default) => Default
// (* 15 (+ (Problem "X is zero") 10)) => (Problem "X is zero")

// (+ NoInt 10) => NoInt

interface MaybeInt {
    public MaybeInt MaybeAdd ( int you );
    public boolean AreYouThere () ;
    public int GetYouOut ( int sentinel ) ;
}
class NoInt implements MaybeInt {
    NoInt() {}
    public MaybeInt MaybeAdd ( int you ) { 
        return this; 
    }
    public String toString () { return "I'm from Australia"; }
    public int GetYouOut ( int sentinel ) { 
        return sentinel; 
    }
    public boolean AreYouThere () { return false; };
}
class AnInt implements MaybeInt {
    int me;
    AnInt ( int me ) { this.me = me; }
    public MaybeInt MaybeAdd ( int you ) { 
        int us = you + me;
        // I know my calculus.
        return new AnInt ( us ); 
    }
    public String toString () { return "" + me; }
    public int GetYouOut ( int sentinel ) {
        return me; 
    }
    public boolean AreYouThere () { return true; };
}

interface IntOrProblem {
    public boolean ProblemHuh () ;
    public String WhatsTheDealWithYou ( String sentinel );
    public int GetYouOut ( int sentinel ) ;
}
class IOP_Problem implements IntOrProblem {
    String desc;
    IOP_Problem( String d ) { this.desc = d; }
    public boolean ProblemHuh ()  { return true; }
    public String WhatsTheDealWithYou ( String sentinel ) { return desc; }
    public int GetYouOut ( int sentinel ) { return sentinel; }    
    public String toString () { return "BAD " + desc; }
}
class IOP_Int implements IntOrProblem {
    int me;
    IOP_Int( int m ) { this.me = m; }
    public boolean ProblemHuh ()  { return false; }
    public String WhatsTheDealWithYou ( String sentinel ) { return sentinel; }
    public int GetYouOut ( int sentinel ) { return me; }    
    public String toString () { return "GOOD " + me; }
}


class C9 {
    // REQUIRES: x be not zero
    // RETURNS the reciprocal of x times 2
    static int f ( int x ) {
        return 2 / x;
    }

    // PARTIAL is the opposite of TOTAL

    // Making it total requires a different spec
    static int TOTAL_f ( int x ) {
        if ( x != 0 ) {
            return 2 / x;
        } else {
            while ( true ) {
            }
        }
    }

    // RETURNS if positive the reciproral times 2 or -1 if x is not positive
    static int TOTAL_g ( int x ) {
        if ( x > 0 ) {
            return 2 / x;
        } else {
            return -1;
        }
    }

    // RETURNS if nonzero the reciproral times 2 or 0 if x is zero
    static int TOTAL_h ( int x ) {
        if ( x != 0 ) {
            return 2 / x;
        } else {
            return 0;
        }
    }

    // In these programs 0 and -1 are SENTINEL values.

    static MaybeInt TOTAL_i ( int x, int y ) {
        if ( x != 0 ) {
            return new AnInt ( (1/x) + y );
        } else {
            return new NoInt();
        }
    }

    // i : int -o int
    // ---->
    // i : int -> int + { NONE } a.k.a. int_\bot

    static int TOTAL_i2 ( int x, int y, int sentinel ) {
        if ( x != 0 ) {
            return (1/x) + y;
        } else {
            return sentinel;
        }
    }

    static IntOrProblem TOTAL_j ( int x, int y ) {
        if ( x != 0 && y != 0 ) {
            return new IOP_Int ( (1/x) + (1/y) );
        } else if ( x == 0 ) {
            return new IOP_Problem ( "x is zero" );
        } else {
            return new IOP_Problem ( "y is zero" );
        }
    }

    public static void main( String[] args ) {
        //System.out.println("f(10) is " + f(10));
        //System.out.println("f(0) is " + f(0));

        //System.out.println("f(10) is " + TOTAL_f(10));
        //System.out.println("f(0) is " + TOTAL_f(0));

        System.out.println("f(10) is " + TOTAL_g(10));
        System.out.println("f(0) is " + TOTAL_g(0));

        System.out.println("f(10) is " + TOTAL_i(10, 1));
        System.out.println("f(0) is " + TOTAL_i(0, 1));

        System.out.println("f(10) is " + TOTAL_i(10, 1).MaybeAdd(10));
        System.out.println("f(0) is " + TOTAL_i(0, 1).MaybeAdd(10));

        System.out.println("f(10) is " + TOTAL_i(10, 1).GetYouOut(0));
        System.out.println("f(0) is " + TOTAL_i(0, 1).GetYouOut(0));

        System.out.println("j(10,10) is " + TOTAL_j(10, 10));
        System.out.println("j(0,10) is " + TOTAL_j(0, 10));
        System.out.println("j(10,0) is " + TOTAL_j(10, 0));

        System.out.println("f(10) is " + OTHER_TOTAL_f(10));
        System.out.println("f(0) is " + (15 * (OTHER_TOTAL_f(0) + 10)));

    }

    static int OTHER_TOTAL_f ( int x ) {
        try { 
            return f ( x );
        } catch (RuntimeException exn) {
            return 10;
        }
    }
}
