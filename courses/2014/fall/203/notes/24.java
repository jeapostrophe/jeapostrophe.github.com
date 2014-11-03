interface Sequenced {
    public Sequence seq();
}

interface Sequence extends Sequenced {
    public int here();
    public boolean notEmpty();
    public Sequence next();
}

class AS_MT implements Sequence {
    AS_MT() {
        System.out.print(".");
    }

    public Sequence seq() { return this; }
    public boolean notEmpty() { return false; }
    public int here() { return -1; }
    public Sequence next() { return this; }
}

class AS_One implements Sequence {
    int here;
    Sequenced next;
    AS_One(int here, Sequenced next) {
        System.out.print(":");
        this.here = here;
        this.next = next;
    }

    public Sequence seq() { return this; }
    public boolean notEmpty() { return true; }
    public int here() { return this.here; }
    public Sequence next() { return this.next.seq(); }
}

class AS_App implements Sequence {
    Sequenced left; Sequence lefts;
    Sequenced right; Sequence rights;

    AS_App(Sequenced l, Sequenced r) {
        System.out.print("+");
        this.left = l; this.lefts = null;
        this.right = r; this.rights = null;
    }

    private void forceLeft() {
        if ( this.lefts == null ) {
            this.lefts = this.left.seq();
        }
    }
    private void forceRight() {
        if ( this.rights == null ) {
            this.rights = this.right.seq();
        }
    }

    public Sequence seq() { return this; }
    public boolean notEmpty() {
        forceLeft();
        if ( this.lefts.notEmpty() ) {
            return true;
        } else {
            forceRight();
            return this.rights.notEmpty();
        }
    }
    public int here() {
        if ( this.lefts.notEmpty() ) {
            return this.lefts.here();
        } else {
            return this.rights.here();
        }
    }
    public Sequence next() {
        if ( this.lefts.notEmpty() ) {
            return new AS_App( this.lefts.next(), this.right );
        } else {
            return this.rights.next();
        }
    }
}

interface Set extends Sequenced {
    boolean isIn ( int there );
}

class Empty implements Set {
    Empty() { }

    public boolean isIn ( int there ) {
        return false;
    }

    public Sequence seq() { return new AS_MT(); }
}

class Cons implements Set {
    int first;
    Set rest;

    Cons( int first, Set rest ) {
        this.first = first;
        this.rest = rest;
    }

    public boolean isIn ( int there ) {
        return this.first == there || this.rest.isIn(there);
    }

    public Sequence seq() { return new AS_One( first, rest ); }
}

class Branch implements Set {
    int here;
    Set left;
    Set right;
    Branch(Set left, int here, Set right) {
        this.left = left;
        this.here = here;
        this.right = right;
    }

    public boolean isIn ( int there ) {
        if ( here == there ) {
            return true; }
        else {
            if ( there < here ) {
                return left.isIn(there);
            } else {
                return right.isIn(there);
            }
        }
    }

    public Sequence seq() {
        return new AS_One( this.here,
                           new AS_App( this.left, this.right ) );
    }
}

class C24 {
    public static void printS ( Sequence as ) {
        while ( as.notEmpty() ) {
            System.out.print(as.here() + " ");
            as = as.next();
        }
        System.out.println();
        return;
    }

    public static void main( String[] args ) {
        Set mt = (new Empty());
        Set l5 = (new Cons(5, mt));
        Set l25 = (new Cons(2, l5));
        Set l325 = (new Cons(3, l25));
        printS(l325.seq());

        Set b_mt = new Empty();
        Set b_5 = new Branch( b_mt, 5, b_mt );
        Set b_7 = new Branch( b_mt, 7, b_mt );
        Set b_6 = new Branch( b_5, 6, b_7 );
        printS(b_6.seq());
    }
}

// Type Hierarchy
// - Super and Sub
// - Sub "extends" Super
// - Sub "implements" Super

// Assignment & Dispatching
// - Apparent vs actual type
// - How does dispatching actually work?

// Extensions & Sharing Code
// - MaxIntSet
// - abstraction function & rep invariant
// - SortedIntSet

// Substitution Principle
// - Compatible types
// - "Same" behavior
// (any code written with a stronger requires is still okay)
// - pre_super => pre_sub [weaken]
// (any code written assuming a weaker provides is still okay)
// - (pre_super && post_sub) => post_super [strengthen]

// - Is int a sub-type of long?
