interface Sequenced {
    public Sequence seq();
}

interface Sequence {
    public int here();
    public boolean notEmpty();
    public Sequence next();
    // If notEmpty returns false, then next() can return anything and
    // here() can return anything
}

class NothingHere implements Sequence {
    public boolean notEmpty() { return false; }
    public int here() { return -1; }
    public Sequence next() { return this; }
}

class RingBuffer  {
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
}

interface List extends Sequenced, Sequence {
    public int length();
    public List remove( int elt );
}

class Empty implements List {
    Empty() { }

    public int length() {
        return 0;
    }

    public List remove( int elt ) {
        return this;
    }

    public Sequence seq() { return this; }
    public boolean notEmpty() { return false; }
    public int here() { return -1; }
    public Sequence next() { return this; }
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

    public Sequence seq() { return this; }
    public boolean notEmpty() { return true; }
    public int here() { return this.first; }
    public Sequence next() { return this.rest; }
}

interface BST extends Sequenced {
    boolean isIn ( int there );
}

class BST_MT implements BST, Sequence {
    BST_MT() { }

    public boolean isIn ( int there ) {
        return false; }

    public Sequence seq() { return this; }
    public boolean notEmpty() { return false; }
    public int here() { return -1; }
    public Sequence next() { return this; }
}

class BST_BR implements BST, Sequence {
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

    public Sequence seq() { return this; }
    public boolean notEmpty() { return true; }
    public int here() {  return this.here; }
    public Sequence next() {
        System.out.println("Alloc");
        return new AS_Cat( this.left.seq(), this.right.seq() );
    }
}

class AS_Cat implements Sequence {
    Sequence left;
    Sequence right;

    AS_Cat(Sequence l, Sequence r) {
        this.left = l;
        this.right = r;
    }

    public boolean notEmpty() {
        return this.left.notEmpty() || this.right.notEmpty();
    }
    public int here() {
        if ( this.left.notEmpty() ) {
            return this.left.here();
        } else {
            return this.right.here();
        }
    }
    public Sequence next() {
        if ( this.left.notEmpty() ) {
            // Potential optimizaiton if right is empty
            System.out.println("Alloc");
            return new AS_Cat( this.left.next(), this.right );
        } else {
            return this.right.next();
        }
    }
}

class C16 {
    public static int sumIt ( Sequenced o ) {
        Sequence as = o.seq();
        int sum = 0;
        while ( as.notEmpty() ) {
            sum = sum + as.here();
            as = as.next();
        }
        return sum;
    }
    public static boolean areYouThere ( Sequenced o, int you ) {
        Sequence as = o.seq();
        while ( as.notEmpty() ) {
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

        List mt = (new Empty());
        List l5 = (new Cons(5, mt));
        List l25 = (new Cons(2, l5));
        List l325 = (new Cons(3, l25));
        System.out.println(l325.length() + " should be " + 3);
        System.out.println(l325.remove(2).length() + " should be " + 2);
        System.out.println(l325.remove(8).length() + " should be " + 3);

        BST b_mt = new BST_MT();
        BST b_5 = new BST_BR( b_mt, 5, b_mt );
        BST b_7 = new BST_BR( b_mt, 7, b_mt );
        BST b_6 = new BST_BR( b_5, 6, b_7 );
        System.out.println( b_6.isIn( 5 ) + " should be " + true );

        System.out.println("\nSequenced\n");
        // System.out.println(sumIt(rb) + " should be " + (4 + 3) );
        System.out.println( sumIt(l325) + " should be " + (3 + 2 + 5) );
        System.out.println( areYouThere(l325, 5) + " should be " + true );
        System.out.println( sumIt(b_6) + " should be " + (6 + 5 + 7) );
        System.out.println( areYouThere(b_6, 5) + " should be " + true );
    }
}

// Next time: implement for ring buffers and continuing amazement
// Are AS just lists? (They are LAZY lists.)
// Other kinds of AS, like Mapped, Naturals, Fibs, Primes
// Type-genericity
// MutableAS
// Fold
