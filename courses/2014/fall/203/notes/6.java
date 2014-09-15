import java.util.Random;

// A list is either
//  - new Empty ()
//  - new Cons ( int, List )

interface List {
    // length : List -> int
    public int length();

    // remove : List int -> List
    public List remove( /* ListWithoutDuplicates this */ int elt );
    // REQUIRES: the list not contain any duplicates, elt is even
    //           (you put DATA INVARIANTS here)
    // returns a list that does not contain elt

    // public Object LooseRemove( Object elt );
    // REQUIRES: elt is a int and return is a List
}

// If req'd is not met...
// - contract not filed error?
// - doesn't give the right answer?
// - some error later because of data-flow
// - won't compile (we assume the REQUIRES means "Java doesn't enforce")
// - ANYTHING GOES and it's okay
//    if AG is little, then language is SAFE. C is not Safe. Java is pretty Safe.



// CYA

class Empty implements List {
    Empty() { }

    public int length() {
        return 0;
    }

    public List remove( int elt ) {
        if ( 4 < 5 ) {
            return this;
        } else {
            // return 17;
            return this;
            // Godel Incompleteness Theorem
        }
    }

    public String toString () {
        return "";
    }
}

class Cons implements List {
    int first;
    List rest;

    Cons( int first, List rest ) {
        this.first = first;
        this.rest = rest;
    }

    public int length() {
        return 1 + rest.length();
    }

    public List remove( int elt ) {
        if ( this.first != elt ) {
            return new Cons( this.first, this.rest.remove(elt) );
        } else {
            return this.rest;
        }
    }

    public String toString () {
        return "" + first + "::" + rest;
    }
}


class C6 {
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
        List l5 = (new Cons(5, (new Empty())));
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
                            (new Cons( 6, l5) ).remove(5).length() +
                            " should be 1" );

        checkList_remove_length( mt, 0 );
        checkList_remove_length( l5, 5 );
        checkList_remove_length( l5, 6 );
        checkList_remove_length( (new Cons( 6, l5) ), 5 );
        checkList_remove_length( (new Cons( 6, l5) ), 6 );

        for ( int i = 0; i < 100; i ++ ) {
            int elt = randomInt(0, 100);
            int len = randomInt(0, 20);
            List l = randomList(len);
            checkList_remove_length( l, elt );
        }

        System.out.println("The list is " + randomList(20).remove(5) + " and we removed 5 ");

        // Integer xcrazy = 5;
        // List lcrazy = xcrazy.remove(7);

        // List lcrazy = l5.remove( true );
    }

    public static List randomList( int len ) {
        if (len == 0) {
            return new Empty();
        } else {
            return new Cons( randomInt(0, 10), randomList( len - 1 ) );
        }
    }

    static Random rand = new Random();
    public static int randomInt( int min, int max ) {
        return rand.nextInt((max - min) + 1) + min; }

}

/// Remove something from a tree

// If it is there
// Branch( left : Tree, target : Element, right : Tree )

//       target
//      /     \
//     left   right

// The left stays
// The target goes
// The right stays

// Turn two Trees into one Tree

/// Subset

// A leaf and a tree... True

// t = A branch(left : Tree, root, right : Tree) and some tree u....

// All the elements in t are in u
// All the elements in t === root, everything in left, and everything in the right

/// Is "root" in u? ---> u.member
/// ...
/// Is "everything in the right" in u? ---> right.subset(u)

// forall x y s,
// x.union(y).subset(s) = x.subset(s) && y.subset(s)

// forall x y
// max(x.cardinality(), y.cardinality()) <= 
//   x.union(y).cardinality() <= x.cardinality() + y.cardinality()

/*
class Evens implements FiniteSet {
    public boolean member ( int elt ) {
        return ( elt % 2 ) == 0;
    }
}
*/

// interface X; // X = FiniteSet
// class A;     // A = Leaf
// class B;     // B = Branch

// forall a b c d,
// Set x = empty().add(a).add(c);
// Set y = empty().add(b).add(d);
// x.union(y).member(1) == true;
// four times

// member (inter t u) x == true
// <->
// member u x && member t x
