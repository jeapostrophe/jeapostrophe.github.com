interface Animal {
    public int weight();
    public Animal eat( /*Animal this,*/ Animal that );
    // Java doesn't allow or promise this:
    // public X eat( X this, Y that );
}

interface Cat extends Animal {
    public void meowr();
}

interface Dog extends Animal {
    public void bark();
}

// P1 : X -> Y
// P2 : A -> B

// When does P1 imply P2?

// Attempt 1.
//  H1 : Y = A
//  P1 : X -> Y
// ------
// A -> B

// replace Y with A using H1

//  P1 : X -> A
// ------
// A -> B

// Attemp 2.
// P1 : X -> Y
// ------
// A -> B

// assume A

// P1 : X -> Y
//  a : A
// ------
// B

// hope that (Y -> B)

// P1 : X -> Y
//  a : A
// H1 : Y -> B
// ------
// B

// apply H1

// P1 : X -> Y
//  a : A
// ------
// Y

// apply P1

//  a : A
// ------
// X

// hope that A -> X

//  a : A
// H2 : A -> X
// ------
// X

// apply H2
// exact a

// Two hopes or two conditions on when P1 -> P2, is when (Y -> B) and (A -> X)

// P1 : X -> Y
// P2 : A -> B
// then if
// A -> X
// Y -> B
// then
// P1 -> P2

// WRONG
// Cat.eat : Cat -> Cat
// Animal.eat : Animal -> Animal
// then if
// Animal -> Cat // <--- isn't true
// Cat -> Animal
// then
// Cat.eat -> Animal.eat

// RIGHT
// Cat.eat : Animal -> Cat
// Animal.eat : Animal -> Animal
// then if
// Animal -> Animal
// Cat -> Animal
// then
// Cat.eat -> Animal.eat

// The subtyping of FUNCTIONS (A -> B) is contra-variant

// (A -> B) is a subtype of (X -> Y)
// iff
// X is a subtype of A and B is a subtype of Y

class C25 {
    Animal a;
    Cat g;
    Dog o;

    void example() {
        int i = a.weight();
        Animal g_with_o_inside = g.eat(o);
        Animal g_with_g_inside = g.eat(g);
        // This is bad because of demon cats: (i.e. Java is right that
        // eat only guarantees an animal comes out.)
        Cat d_g_with_g_inside = (Cat) g.eat(g);

        // Zephyr says: The input can be specific but the output
        // cannot.
        
    }

    // REQUIRES: a is sorted.
    boolean fastSearch( int[] a ) {
        // ..... more stuff ....
        return false;
    }

    // EFFECTS: returns a sorted array
    int[] sort( int[] a ) {
        // ... more stuff ...
        return a;
    }

    // In this case "sorted" is like "Animal"
    
    // Suppose we had a fastSearch that was FASTER when the input was
    // monotonically increasing (i.e. mono incr -> sorted but sorted
    // !-> mono incr) called monoFastSearch

    // REQUIRES: a is mono incr.
    boolean monoFastSearch( int[] a ) {
        // ..... more stuff ....
        return false;
    }

    //
    // Suppose the program contained fastSearch(a), can we replace it
    // with monoFastSearch(a)? NO. In this case "sorted" is like "Animal"
    // and "mono incr" is like "Cat".

    // EFFECTS: returns a mono incr array
    int[] monoIncreate( int[] a ) {
        // ... more stuff ...
        return a;
    }

    // Suppose the program contained sort(a), can we replace it with
    // monoIncreate(a)? YES. In this case "sorted" is like "Animal"
    // and "mono incr" is like "Cat".

    // In these examples... replacing one piece of a program with
    // another
    //
    // I would contain
    //        search : int[] -> bool
    //   preprpocess : int[] -> int[]
    // 
    // One "o" would be "sortedIntArr" and the other "m" would be
    // "monoIncrIntArr"
    //
    // o.search(o.preprocess(a)) <-- Allowed
    // m.search(m.preprocess(a)) <-- Allowed
    //
    // o.search(m.preprocess(a)) <-- 8 YES,  2 NO, 4 MAYBE
    //  YES: m returns a monoIncr and o's search allows monoIncr because it wants sorted
    //
    // m.search(o.preprocess(a)) <-- 2 YES, 14 NO, 1 MAYBE
    //   NO: o only returns sorted, but m wants monoIncr 

    /// Example 2

    // RBTInsert : RBTree Key -> RBTree
    // RBTInsert( RBTInsert( MT, X ), Y ) = RBTree? Yes
    
    // DTInsert : DTree Key -> DTree
    // DTInsert( DTInsert( MT, X ), Y ) = DTree? Yes

    // DTInsert( RBInsert( MT, X ), Y ) = DTree?
    // RBTInsert( DTInsert( MT, X ), Y ) = No promises (probably a DTree)

    /// Non-Example 3

    // f : ( Measurements of where the Earth was ) -> int[inches]
    // g : int int[centimeters] int -> int

    // release this much fuel = g( 4, f( M ), 5 )

    // Zephyr says, "Did they mess up metric vs Imperial?"

    // Example 3

    // In a program that contains an "int x" (32 bits) can you change
    // it to "long x" (64 bits)?

    // Every int has a long (every Cat is an Animal) [for lame technial reason, not true]
    // Every long has no int (every Animal is not a Cat)

    // 0123 (LSB) = big-endian
    // 1248
    // 1010 0000 = 1 + 4 = 5

    // 3210 (MSB) = little-endian (how modern computers work)
    // 8421
    // 0000 1010 = 8 + 2 = 10

    // x86 it's A register is 8Bit, EA thats 16bit, and EAX register 32bit
    // totally new stuff 64_A1

    // ia64 (from Intel) didn't do this
    // x86_64 (from AMD) won

    public static void main( String[] args ) {
        System.out.println("Hey!");
    }
}

// Assignment & Dispatching
// - Apparent vs actual type
// - How does dispatching actually work?

// Substitution Principle
// - Compatible types
// - "Same" behavior
// (any code written with a stronger requires is still okay)
// - pre_super => pre_sub [weaken]
// (any code written assuming a weaker provides is still okay)
// - (pre_super && post_sub) => post_super [strengthen]

// - Is int a sub-type of long?
