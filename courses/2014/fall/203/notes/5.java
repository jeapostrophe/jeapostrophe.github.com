// Guest account: cs-guest / Fall2014
import java.util.Random;

// A list is either
//  - empty
// empty is NULL // <-- immoral
// empty is a class
//  - one thing(int) and some more(List)
// oneThingAndSomeMore is a class

interface List {
    // length : List -> int
    public int length();

    // remove : List int -> List
    public List remove( int elt );
    // returns a list that does not contain elt (and btw we assume
    // lists don't contain duplicates)
}

class Empty implements List {
    Empty() { }

    public int length() {
        return 0;
    }

    public List remove( int elt ) {
        // Make a new list...
        // new Empty(); <-- verbose because this is already one of those
        // new oneThingAndSomeMore( ..., ... ); <-- doesn't return 0 for length
        return this; // <-- just right!
        // ___.remove( .... ) <-- too much work
    }
}

class oneThingAndSomeMore implements List {
    int oneThing;
    List someMore;

    oneThingAndSomeMore( int oneThing, List someMore ) {
        this.oneThing = oneThing;
        this.someMore = someMore;
    }

    public int length() {
        return 1 + someMore.length();
    }

    public List remove( int elt ) {
        // this this.oneThing this.someMore elt
        // this.someMore;
        // ___.remove( ... );

        // if (cond) { return true-e; } else { return false-e; }
        // =>
        // cond ? true-expr : false-expr;

        // Distinguish test case 1 from test case 2
        if ( this.oneThing != elt ) {
            // First test case
            // this = 5 :: MT
            // this.oneThing = 5
            // this.someMore = MT
            // elt = 6
            // this.someMore.remove(elt) = MT
            // return this; // <-- passes first test

            // Third test case
            // this = 6 :: 5 :: MT
            // this.oneThing = 6
            // this.someMore = 5 :: MT
            // elt = 5
            // this.someMore.remove(elt) = MT
            // return (6 :: MT);
            // Generalizes from the first
            return 
                new oneThingAndSomeMore( this.oneThing,
                                         this.someMore.remove(elt) );

            // The Law of Demeter            
        } else {
            // Second test case
            // this = 5 :: MT
            // this.oneThing = 5
            // this.someMore = MT
            // elt = 5
            return this.someMore;
        }
    }
}


class C5 {
    // For all l and elt,
    //  (remove l elt).length() = (l.length() - 1) \/ l.length()

    public static void checkList_remove_length( List l, int elt ) {
        int left = l.remove(elt).length();
        if ( ! ( left == (l.length() - 1) || left == l.length() ) ) {
            System.out.println("Failure");
        } else {
            System.out.println("Success!");
        }
    }

    // For all l and elt
    //  member (remove l elt) elt = false

    public static void main ( String[] args ) {
        List mt = (new Empty());
        System.out.println( "The length of it is.... " +
                            mt.length() );
        List l5 = (new oneThingAndSomeMore(5, (new Empty())));
        System.out.println( "The length of it is.... " +
                            l5.length() );

        System.out.println( "The length of mt after we remove 6 is... " +
                            mt.remove(6).length() +
                            " should be 0");
        System.out.println( "The length of l5 after we remove 6 is... " +
                            l5.remove(6).length() +
                            " should be 1" );
        System.out.println( "The length of l5 after we remove 5 is... " +
                            l5.remove(5).length() +
                            " should be 0" );

        System.out.println( "The length of l6 after we remove 5 is... " +
                            (new oneThingAndSomeMore( 6, l5) ).remove(5).length() +
                            " should be 1" );

        checkList_remove_length( mt, 0 );
        checkList_remove_length( l5, 5 );
        checkList_remove_length( l5, 6 );
        checkList_remove_length( (new oneThingAndSomeMore( 6, l5) ), 5 );
        checkList_remove_length( (new oneThingAndSomeMore( 6, l5) ), 6 );

        for ( int i = 0; i < 100; i ++ ) {
            int elt = randomInt(0, 100);
            int len = randomInt(0, 20);
            List l = randomList(len);
            // elt is in l
            // E(len) * probability of (rI in rL = elt)
            // 10 * (1/100) = 1/10
            checkList_remove_length( l, elt );
        }
    }

    public static List randomList( int len ) {
        if (len == 0) {
            return new Empty();
        } else {
            return new oneThingAndSomeMore( randomInt(0, 100), randomList( len - 1 ) );
        }
    }

    static Random rand = new Random();
    public static int randomInt( int min, int max ) {
        return rand.nextInt((max - min) + 1) + min; }

}

// Should remove use union?

//     5
//    / \
//   3  7
//  / \
// 1  4

// remove 3

//     5
//    / \
//   X  7
//  / \
// 1  4

//     5
//    / \
//   X  7

// s.t. X is the tree that has only 1 and 4 in it

//  1       4
//   \     /
//   4    1

// Union...

// union could have MT and BR (easy)
//                  MT and MT (easy)
//                  BR and BR

//                  BR(left, here, right) TREE(u)

// Trees = left, right, u
// Element = here
