interface Sumable {
    public int sum();
}

interface Nirvana {
    public int[] letThemPass();
    // Returns the elements of the Nirvana objects "in order" whatever THAT means.
}

interface Nirvana2 {
    public AbstractSequence allYall();
}

interface AbstractSequence {
    public int here();
    public boolean somethingThereHuh();
    public AbstractSequence next();
    // If somethingThereHuh returns false, then next() can return
    // anything and here() can return anything
}

class NothingHere implements AbstractSequence {
    public boolean somethingThereHuh() { return false; }
    public int here() { return -1; }
    public AbstractSequence next() { return this; }
}

class RingBuffer implements Nirvana {
    int size;
    int[] smg;
    int end;

    // returns a rb of the size
    RingBuffer(int size) {
        this.size = size;
        this.smg = new int[size];
        this.end = 0;
    }

    public RingBuffer insert( int elem ) {
        this.smg[ this.end ] = elem;
        this.end = (this.end + 1) % this.size;
        return this;
    }

    public int read ( int idx ) {
        return
            this.smg[ (this.end - idx + this.size - 1)
                      % this.size  ];
    }
    
    public int[] letThemPass() {
        int[] not_the_rep = new int[this.smg.length];
        for (int i = 0; i < not_the_rep.length; i++ ) {
            not_the_rep[i] = this.smg[i];
        }
        return not_the_rep;
    }
}

interface List extends Nirvana, Nirvana2, AbstractSequence {
    public int length();
    public List remove( int elt );
    public int sum();
}

class Empty implements List {
    Empty() { }

    public int length() {
        return 0;
    }

    public List remove( int elt ) {
        return this;
    }
    public int sum () {
        return 0;
    }
    public int[] letThemPass() {
        return (new int[0]);
    }
    public AbstractSequence allYall() {
        return this;
    }

    public boolean somethingThereHuh() { return false; }
    public int here() { return -1; }
    public AbstractSequence next() { return this; }
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
    public int sum () {
        return this.first + this.rest.sum();
    }
    public int[] letThemPass() {
        int[] inner = this.rest.letThemPass();
        int[] outer = new int[1 + inner.length];
        outer[0] = this.first;
        for (int i = 0; i < inner.length; i++) {
            outer[1+i] = inner[i];
        }
        return outer;
    }
    public AbstractSequence allYall() {
        return this;
    }

    public boolean somethingThereHuh() { return true; }
    public int here() { return this.first; }
    public AbstractSequence next() { return this.rest; }
}

interface BST extends Sumable, Nirvana, Nirvana2 {
    boolean isIn ( int there );
    int max ();
 }

class BST_MT implements BST {
    BST_MT() { }

    public boolean isIn ( int there ) {
        return false; } 
    public int sum () { return 0; }
    public int max () { return 0; }
    public int[] letThemPass() {
        return (new int[0]);
    }
    public AbstractSequence allYall() {
        return new NothingHere();
        // return new Empty();
        // return this; // add AbstractSequence to our implements
    }
}

class BST_BR implements BST, AbstractSequence {
    int here;
    BST left;
    BST right;
    BST_BR(BST left, int here, BST right) {
        this.left = left;
        this.here = here;
        this.right = right; }

    public boolean isIn ( int there ) {
        if ( here == there ) {
            return true; }
        else {
            if ( there < here ) {
                return left.isIn(there); }
            else {
                return right.isIn(there); } } } 

    public int sum () {
        return here + left.sum() + right.sum(); 
    }
    public int max () { return Math.max(here, right.max()); }
    public int[] letThemPass() {
        int[] innerL = this.left.letThemPass();
        int[] innerR = this.right.letThemPass();
        int[] outer = new int[innerL.length + 1 + innerR.length];
        for (int i = 0; i < innerL.length; i++) {
            outer[i] = innerL[i];
        }
        outer[innerL.length] = this.here;
        for (int i = 0; i < innerR.length; i++) {
            outer[innerL.length+1+i] = innerR[i];
        }
        return outer;
    }
    public AbstractSequence allYall() {
        return this;
    }

    // AbstractSequence functions
    public boolean somethingThereHuh() { return true; }
    public int here() { 
        return this.here; 
    }
    public AbstractSequence next() {
        // return this.left.allYall(); // not enough right
        // return this.right.allYall(); // not enough left
        return new AS_Union( this.left.allYall(), this.right.allYall() );
    }
}

class AS_Union implements AbstractSequence {
    AbstractSequence left;
    AbstractSequence right;
    
    AS_Union(AbstractSequence l, AbstractSequence r) {
        this.left = l;
        this.right = r;
    }

    public boolean somethingThereHuh() {
        return this.left.somethingThereHuh() || this.right.somethingThereHuh();
    }
    public int here() { 
        if ( this.left.somethingThereHuh() ) {
            return this.left.here();
        } else {
            return this.right.here();
        }
    }
    public AbstractSequence next() {
        if ( this.left.somethingThereHuh() ) {
            // Potential optimizaiton if right is empty
            return new AS_Union( this.left.next(), this.right );
        } else {
            return this.right.next();
        }        
    }
}

// Data-General programming is in tension with exploiting
// representation invariants.

class C15 {
    public static int sumIt ( Nirvana o ) {
        int[] the_contents = o.letThemPass();
        int sum = 0;
        for ( int i = 0; i < the_contents.length; i++ ) {
            sum = the_contents[i] + sum;
        }
        return sum;
    } 
    public static int maxItOut ( Nirvana o ) {
        int[] the_contents = o.letThemPass();
        int most = 0;
        for ( int i = 0; i < the_contents.length; i++ ) {
            most = Math.max(the_contents[i],most);
        }
        return most;
    } 
    public static boolean areYouThere ( Nirvana o, int you ) {
        int[] the_contents = o.letThemPass();
        for ( int i = 0; i < the_contents.length; i++ ) {
            if ( the_contents[i] == you ) {
                return true;
            }
        }
        return false;
    } 

    public static int sumIt2 ( Nirvana2 o ) {
        AbstractSequence as = o.allYall();
        int sum = 0;
        while ( as.somethingThereHuh() ) {
            sum = sum + as.here();
            as = as.next();
        }
        return sum;
    }
    public static boolean areYouThere2 ( Nirvana2 o, int you ) {
        AbstractSequence as = o.allYall();
        while ( as.somethingThereHuh() ) {
            if ( as.here() == you ) {
                return true;
            }
            as = as.next();
        }
        return false;
    }

    public static void main(String[] args) {
        RingBuffer rb = (new RingBuffer(2));
        rb.insert(1);
        rb.insert(2);
        rb.insert(3);
        rb.insert(4);
        System.out.println(rb.read(0) + " should be " + 4 );
        System.out.println(rb.read(1) + " should be " + 3 );
        System.out.println(sumIt(rb) + " should be " + (4 + 3) );
        System.out.println(maxItOut(rb) + " should be " + 4 );

        List mt = (new Empty());
        List l5 = (new Cons(5, mt));
        List l25 = (new Cons(2, l5));
        List l325 = (new Cons(3, l25));
        System.out.println(l325.length() + " should be " + 3);
        System.out.println(l325.remove(2).length() + " should be " + 2);
        System.out.println(l325.remove(8).length() + " should be " + 3);
        System.out.println( l325.sum() + " should be " + (3 + 2 + 5) );
        System.out.println( sumIt(l325) + " should be " + (3 + 2 + 5) );
        System.out.println( maxItOut(l325) + " should be " + 5 );
        System.out.println( areYouThere(l325, 5) + " should be " + true );
        System.out.println( "Nirvana2: " + sumIt2(l325) + " should be " + (3 + 2 + 5) );
        System.out.println( "Nirvana2: " + areYouThere2(l325, 5) + " should be " + true );

        BST b_mt = new BST_MT();
        BST b_5 = new BST_BR( b_mt, 5, b_mt );
        BST b_7 = new BST_BR( b_mt, 7, b_mt );
        BST b_6 = new BST_BR( b_5, 6, b_7 );
        System.out.println( b_6.isIn( 5 ) + " should be " + true );
        System.out.println( b_6.sum() + " should be " + (6 + 5 + 7) );
        System.out.println( b_6.max() + " should be " + 7 );
        System.out.println( sumIt(b_6) + " should be " + (6 + 5 + 7) );
        System.out.println( maxItOut(b_6) + " should be " + 7 );
        System.out.println( areYouThere(b_6, 5) + " should be " + true );
        System.out.println( "Nirvana2: " + sumIt2(b_6) + " should be " + (6 + 5 + 7) );
        System.out.println( "Nirvana2: " + areYouThere2(b_6, 5) + " should be " + true );
        
    }
}

// Next time: implement for ring buffers and continuing amazement
// Are AS just lists? (They are LAZY lists.)
// Other kinds of AS, like Mapped, Naturals, Fibs, Primes
// Type-genericity
// MutableAS
// Fold
