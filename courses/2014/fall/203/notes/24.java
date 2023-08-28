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
    public int element;
    Sequenced next;
    AS_One(int element, Sequenced next) {
        System.out.print(":");
        this.element = element;
        this.next = next;
    }

    public Sequence seq() { return this; }
    public boolean notEmpty() { return true; }
    public int here() { return this.element; } // <-- AS_One's here
    public Sequence next() { return this.next.seq(); }
}

class AS_Two extends AS_One {
    AS_Two(int element, Sequenced next) {
        super(element, next);
    }
    public int here() { return this.element * 2; }
    public int uniq() { return 42; }
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

    public Sequence seq() {
        if ( first % 2 == 0 ) {
            return new AS_One( first, rest );
        } else {
            return new AS_Two( first, rest );
        }
    }
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

// An AS_One is...
//   VTable = [ Ptr to here() code, Ptr to isEmpty() code, Ptr to next() code ]
//   Fields = [ 32bits for element, 32bits for next ]
//   [ "I am an AS_One", VTable, Fields .... ]

// AS_One's constructor:
//  Java writes this part:
//  this = new memory ( exactly 32 * 5 bits )
//  this.vtable.here = AS_One.here;
//  this.vtable.isEmpty = AS_One.isEmpty;
//  this.vtable.next = AS_One.next;
//  You write this part:
//  this.fields.element = element;
//  this.fields.next = next;

//   [ a bunch of bits for .element, a bunch of bits for .here() ]

// An AS_Two is an AS_One is...
//   VTable = [ Ptr to uniq() code ]
//   Fields = [ 32bit secondElement ]
//   [ "I am an AS_Two", AS_One, VTable, Fields ... ]
//  Java writes this part:
//  this = new memory ( exactly 32 * 5 bits )
//  this.vtable.here = AS_Two.here;
//  this.vtable.isEmpty = AS_One.isEmpty;
//  this.vtable.next = AS_One.next;
//  You write this part:
//  this.fields.element = element;
//  this.fields.next = next;

//   [ a bunch of bits for .element, a bunch of bits for .here() ]

// because .here() is different, bits must be different
//   take the pointer, go to the vtable, and look at the first code pointer
// BUT because .element is same, bits must be similar?
//   take the pointer, go to the fields, and look at the first field
// .secondElement
//   take the pointer, go to the second fields, and look at the first field


class C24 {
    public static void printS ( Sequence as ) {
        while ( as.notEmpty() ) {
            if ( as instanceof AS_One ) {
                System.out.println("Coming from a one!");
                // javac (compiler) vs java (runner)
                // Q1: Does Java know what it actually is?
                // Q1a: Does javac know what it actually is?
                // Q1b: Does java know what it actually is?

                // Q2: Does a cast tell Java "not to worry" and treat it like the casted to type?
                // Q2a: Does a cast have an effect in javac? (javac RELIES on the runtime)
                // Q2b: Does a cast have an effect in java? (i.e. does it print out 3 NOT 6) (java GUARANTEES the javc)
                AS_One it = ((AS_One) as);
                System.out.println("The first is this: " + it.element );
                // here-ness is in the object (runtime) NOT in the type (compiler)
                System.out.println("The here is this: " + ((AS_One) as).here() );
            }
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
