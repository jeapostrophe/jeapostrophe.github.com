interface General<X> {
    public X ret(X x);
}

class AGeneral<X> implements General<X> {
    AGeneral() {}
    public X ret(X x) {
        return x;
    }
}

/*
class BGeneral<X> implements General<X> {
    BGeneral() {}
    public X ret(X x) {
        // Java protects us
        return "Hello";
    }
}
*/

/*
class CGeneral<X> implements General<X> {
    CGeneral() {}
    public X ret(X x) {
        // new takes a "Class" not a "Type" (Interfaces are Types,
        // Classes are Types (and Classes), <X> are Types (called Type
        // Parameters)
        return new X();
    }
}
*/

class DGeneral<X> implements General<X> {
    DGeneral() {}
    public X ret(X x) {
        // null is of every type, even X
        // return null;

        // If you never return anything, then you didn't return the
        // wrong thing.
        // while ( true ) { }

        // An exception isn't a return
        // throw new RuntimeException("Back door!");

        // This property is called "parameteric polymorphism" and it
        // is awesome. P1(x) => P2 ( F( G( x ) ) ) and G has "p.p.",
        // then you just need to prove P1(x) => P2(F(x)). "Theorems
        // for Free"

        // Java implements Generics by "erasure" and supports runtime
        // inspection.

        if ( x instanceof String ) {
            return (X)"Gotcha!";
        } else if (x instanceof Integer) {
            return (X) (new Integer(42));
        } else {
            return x;
        }
    }
}

class Arr_int {
    Integer xs[];
    Arr_int() {
        xs = new Integer[25];
    }
}

class Arr<X> {
    X xs[];
    Arr(int sz) {
        // Java doesn't know the size due to Erasure
        // xs = new X[sz];
        xs = (X[]) new Object[sz];
    }
}

class C27 {
    // If you leave out <String>, it assumes it's <Object>
    public static void f(General<String> g) {
        System.out.println("g's x is " + g.ret("Hey"));
    }
    
    public static void main( String[] args ) {
        f(new AGeneral<String>());
        f(new DGeneral<String>());
        System.out.println("g's x is " +
                           new DGeneral<Integer>().ret(25));
    }
}

// (Ch8) Polymorphic Abstraction Limitations
// (Ch9) Specifications: Models vs Theories
// - Technical vs Aesthetic criteria
// - Redundancy helps show errors

interface World {
}

class FightWorld implements World {
    World when_im_done;

    FightWorld(World wid) {
        when_im_done = wid;
    }
}

//////////////////////////////////

// How do we know if a specification is good?

// - If spec says I take Xs, then impls would only works if they took
//   Xs, because if it COULD work with Ys, then it would be overly
//   precise

// A specification has a set of possible implementations: IMPL(S) and
// a set of possible SOUND implementatiosn: SOUND(S)
// a set of possible COMPLETE implementatiosn: COMPLETE(S)
// a set of possible SOUND+COMPLETE implementatiosn: SOUND+COMPLETE(S)

// How do we know if an implementation is good?

// - If spec says I return Xs, then the implm really does

// SOUNDNESS: Forall P, If spec says "P" is true, then the impl says
// "P" is true.
// - If Spec |= P, then any impl I in SOUND(Spec), I |= P

// COMPLETENESS: Forall P, if impl says "P" is true, then the spec
// says "P" is true.
// - If I0 |= P, then by completeness Spec |= P, therefor I1 |= P

// G\"odel Incompleteness Theorem:

// - If Arithmetic \subset Spec, then every implementation is SOUND,
// COMPLETE, or niether, but NOT both.

// Quines




