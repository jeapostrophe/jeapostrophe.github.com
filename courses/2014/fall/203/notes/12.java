class C12 {
    public static void main (String[] args) {
        System.out.println("Hey, listen");
    }

    // Interface was like a TODO list
    // I*face is like a schematic for sets
    // Interfaces are necessary when you don't know the kind you will return

    // "Don't have to look at the code"
    /// code might not be yours (thus not available)
    /// HELL is other people.

    // Interfaces HIDE the representation
}

class FiniteSet {
    int content;
    FiniteSet left;
    FiniteSet right;
    boolean content_is_filled;

    public boolean isEmtpyHuh () {
        return ! this.content_is_filled;
    }

    public boolean subset ( FiniteSet u ) {
        if ( this.content_is_filled ) {
            return false;
        } else {
            return true;
        }
    }

}

// How many middles in 100 ?
//  100 / 2 = 50
// log_x y = z
// x^z = y
// 2^z = 100
// log_2 100

// quad-tree is for 2d collision detection and point intersection

// 22222
// 22222
// 22222
// 22222

// A DenseMatrix is ... (n*m or n*n*4 bytes)
// int mat[5][4] be 5*4*sizeof(int) = 5*4*4 = 80 bytes

// 20000
// 02000
// 00200
// 00020
// 00003

// A diagonal matrix or (5-vector) * Identity

// A DiagonalMatrix is... (n*4 bytes)
// int coeffs[5] = 5*sizeof(int) = 5 * 4 = 20 bytes


// Interfaces allow representation freedom and switching


// ListInterface = ( empty, addAtFront, append, addAtBack, atFront, atBack )

// (define (append x y)
//  (if (empty? x)
//    y
//    (cons (first x) (append (rest x) y))))

// (define (snoc l x) (append l (list x)))

// Cons+Mt = ( Free, Free, Linear, Linear, Free, Linear )

// Fast SNOC = ( Free, Linear, Linear, Free, Linear, Free )
// ... store the list reversed (or changing the meaning of Snoc/Cons)

// Both = ( Free, Free, Linear, Free, Free, Free )
// ... store two lists, one front to back and the other back to front

// (first (empty-front-to-back,full-back-to-front)) = (last full-back-to-front)
// (last (full-front-to-back,empty-back-to-front)) = (last full-front-to-back)


/////


/// Good interfaces allow the user to think about the ABSTRACTION not about the REPRESENTATION

// new Branch( new Empty(), 5, new Empty() ) == new Branch( new Empty(), 5, new Empty() );
// allocate thing at address 0x76889 == allocate thing at address 0x7688743;
// 0x76889 == 0x7688743;
// false;

// Java's == is a bad interface, but your .equals is a good one.

// new Branch( new Empty(), 5, new Branch( new Empty(), 6, new Empty() ) )
// new Branch( new Branch( new Empty(), 5, new Empty() ), 6, new Empty() )

// == is false
//.equals is true

// Every interface specifizes an ABSTRACT domain (FiniteSet was "sets")
// Every representation projects REPRESENTED INSTANCES (CONCRETES) into the ABSTRACT domain

// An ABSTRACTION FUNCTION... : CONCRETE -> ABSTRACT
// new Empty() => {};
// new Branch( left, middle, right ) => left U right U { middle };

// If you have n elements, then each could be the middle
//    n !

// Are some sets of abstraction functions "bad"?
// - Is it "complete" meaning that every ABSTRACT value has at least one CONCRETE value?
// - Is it "sound" meaning that every CONCRETE value is really an ABSTRACT value?
// - Is it "well-founded" meaning you can construct a CONCRETE value

// Some property is true in the ABSTRACT and it should always be true on CONCRETE values

// DE-ABSTRACTION-FUNCTION : ABSTRACT -> Set(CONCRETE)
// Some set => Some representations of that set

// FULL ABSTRACTION

// One half
// forall v in the abstract,
//     forall c in DE(v),
//     F(c) = c' ->
//     AB(c') = AF(v)

// Other half
// forall c in the concrete,
//  AF(AB(c)) = c'
//  F(c) \in DE(c')

// Creating a "Galois Connection"

// Chaitin

class LLFS {
}

class BTFS {
}
