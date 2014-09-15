import java.util.Random;

// A list is either
//  - new Empty ()
//  - new Cons ( int, List )

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
        return this;
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
    }

    public static List randomList( int len ) {
        if (len == 0) {
            return new Empty();
        } else {
            return new Cons( randomInt(0, 100), randomList( len - 1 ) );
        }
    }

    static Random rand = new Random();
    public static int randomInt( int min, int max ) {
        return rand.nextInt((max - min) + 1) + min; }

}
