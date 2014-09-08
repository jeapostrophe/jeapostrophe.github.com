// A Cartesian point is a number and another number
// we call the first X and the second Y
class Point {
    double x;
    double y;

    Point( double x, double y ) {
        this.x = x;
        this.y = y; }

    public static double distance ( Point pd ) {
        return Math.sqrt(pd.x*pd.x + pd.y*pd.y); }

    //    public static double LazyDistance ( Point this ) {
    //        return Math.sqrt(this.x*this.x + this.y*this.y); }

    // A lazy function has no 'static', takes an implicit argument
    // named 'this' of the same kind as the containing class, and is
    // also called a 'method' by not Proto-Jays
    public double LazyDistance () {
        return Math.sqrt(this.x*this.x + this.y*this.y); }

    public double EvenLazierDistance () {
        return Math.sqrt(x*x + y*y); }
}

// An s is a make-s with a f1 that's a something1 ...
// (define-struct s (f1 f2 f3))

// =>

// An s is a new s ( something1, ... )
// class s { 
//  something1 f1;
//  ...
//  S( something1 f1, ... ) {
//    this.f1 = f1;
//    ...
//  }
// }

// A BST is either
//  - an empty tree
//  - a branch with one number and two BSTs called left and right
interface BST {
    boolean isIn ( /* BST this, */ int there ); }

class BST_MT implements BST {
    BST_MT() { } 

    // isIn : BST int -> bool
    public boolean isIn ( /* BST this, */ int there ) {
        return false; } }

class BST_BR implements BST {
    int here;
    BST left;
    BST right;
    BST_BR(BST left, int here, BST right) {
        this.left = left;
        this.here = here;
        this.right = right; }
    
    // isIn : BST int -> bool
    public boolean isIn ( /* BST this, */ int there ) {
        // Test 1: here = 5, left = right = mt, there = 5
        // return true;
        // Test 2: here = 5, left = right = mt, there = 6
        // return false;
        // Generalize tests 1 and 2 into...
        // return (here == there);

        // Split test 1 from tests 2 and 3
        if ( here == there ) {
            // Test 1: here = 5, left = right = mt, there = 5
            // return true;
            return true; } 
        else {
            // Split test 3 from test 2
            if ( there < here ) {
                // Test 3: b_6.isIn( 5 )
                //  here = 6, left = b_5, right = b_7, there = 5
                //  left.IsIn(there) = true
                //  right.IsIn(there) = false
                return left.isIn(there); }
            else {
                // Test 2: here = 5, left = right = mt, there = 6
                //  left.IsIn(there) = false
                //  right.IsIn(there) = false
                // return false;
                return right.isIn(there); } } } }

// BAD: Which BST is wrong? Especially when it was made by Other
// People.

// GOOD: The interface becomes a kind of "TODO" list for fixes and
// changes.

// GOOD: Delayed plans are flexible.

class C3 {
    // f(x) = x + 3

    // f(5) = x + 3 [x = 5] = 5 + 3 = 8

    // f(int x) = x + 3
    // int f(int x) = x + 3
    // static int f(int x) = x + 3
    // public static int f(int x) = x + 3
    public static int f ( int x ) {
        return x + 3; }

    // distance(x,y) = sqrt(x^2 + y^2);
    public static double distance ( double x, double y ) {
        return Math.sqrt(x*x + y*y); }

    public static void main ( String[] args ) {
        // Java has a STATIC type system
        if ( 4 < 5 ) {
            System.out.println( f( 5 ) );
        } else {
            // System.out.println( f( "five" ) );
        }

        System.out.println( 5 + 3 );
        int y = 5 + 3;
        System.out.println( y );
        System.out.println( 8 );
        System.out.println( distance(3,4) + " should be " + 5);
        System.out.println( C3.distance(3,4) + " should be " + 5);
        Point p = new Point(3,4);
        System.out.println( Point.distance(p) + " should be " + 5);
        // A lazy function call (or method invocation) has the first
        // argument to the left of the dot and not class name
        System.out.println( p.LazyDistance() + " should be " + 5);
        System.out.println( p.EvenLazierDistance() + " should be " + 5);

        // BST tests
        BST b_mt = new BST_MT();
        BST b_5 = new BST_BR( b_mt, 5, b_mt );
        
        System.out.println( b_mt.isIn( 5 ) + " should be " + false );
        System.out.println( b_5.isIn( 5 ) + " should be " + true );
        System.out.println( new BST_BR( b_mt, 5, b_mt ).isIn( 5 ) + " should be " + true );
        System.out.println( b_5.isIn( 6 ) + " should be " + false );

        BST b_7 = new BST_BR( b_mt, 7, b_mt );
        BST b_6 = new BST_BR( b_5, 6, b_7 );

        System.out.println( b_6.isIn( 5 ) + " should be " + true );
        System.out.println( b_6.isIn( 6 ) + " should be " + true );
        System.out.println( b_6.isIn( 7 ) + " should be " + true );

        // We aren't "naming" 10 z, instead we are creating a
        // "location" named "z" that has 10 in it RIGHT NOW

        // more on what this does to analysis LATER
        int z = 10;
        System.out.println( z );
        System.out.println( z );
        System.out.println( z );        
        z = 9;
        System.out.println( z );
        System.out.println( z );

        return; } }
