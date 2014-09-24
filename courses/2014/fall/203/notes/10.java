// These are "checked" meaning they infect the spec
class Impossible extends Exception {
    Impossible(String y) { super(y); }
}
class FunnyImpossible extends Exception {
    FunnyImpossible(String y) { super(y); }
}
class Unwritable extends Exception {
    Unwritable(String y) { super(y); }
}
// This is NOT:
class SpanishInquisition extends RuntimeException {
    SpanishInquisition(String y) { super(y); }
}

class TheAnswer extends Exception {
    public int num;
    TheAnswer(int num) { super(); this.num = num; }
}

interface IntReceiver {
    public int give ( int x );
}
class IdIntReceiver implements IntReceiver {
    IdIntReceiver() {}
    public int give ( int x ) { return x; }
}
class AddIntReceiver implements IntReceiver {
    int number_to_add;
    IntReceiver other_receiver;
    AddIntReceiver( IntReceiver o, int n ) { this.number_to_add = n; this.other_receiver = o;}
    public int give ( int x ) { return other_receiver.give( number_to_add + x ); }
}

interface List {
    public int SumOrFirstOdd_Rnd2( IntReceiver normal, IntReceiver abnormal );
    public int SumOrFirstOdd();
    public int SumOrFirstOdd_H() throws TheAnswer;
}
class MT implements List {
    MT() {}
    public int SumOrFirstOdd_Rnd2( IntReceiver normal, IntReceiver abnormal ) {
        return normal.give( 0 );
    }
    public int SumOrFirstOdd() {
        return 0;
    }
    public int SumOrFirstOdd_H() throws TheAnswer {
        return 0;
    }
}
class Cons implements List {
    int first;
    List rest;
    Cons(int f, List r) { this.first = f; this.rest = r; }
    public int SumOrFirstOdd_Rnd2( IntReceiver normal, IntReceiver abnormal ) {
        if ( first % 2 == 1 ) {
            return abnormal.give( first );
        } else {
            return rest.SumOrFirstOdd_Rnd2( new AddIntReceiver( normal, first ), abnormal );
        }
    }
    public int SumOrFirstOdd() {
        try {
            return SumOrFirstOdd_H();
        } catch (TheAnswer exn) {
            return exn.num;
        }
    }
    public int SumOrFirstOdd_H() throws TheAnswer {
        if ( first % 2 == 1 ) {
            throw new TheAnswer ( first );
        } else {
            return first + rest.SumOrFirstOdd_H();
        }
    }
}

class C10 {

    // Partial to total means change the spec
    // ... specifying a sentinel result for old undefined cases
    // ... changing the type to include more stuff and thus allow a sentinel
    // ....... creating the "maybe" type
    // ... you could have the user specify the sentinel
    // ... Java's Exceptions are like the Maybe type PLUS information about why NONE

    static int RomanSubtraction ( int x, int y ) throws Impossible, Unwritable {
        if ( x == y ) {
            Unwritable exn = new Unwritable("Cannot represent this number");
            throw exn;
        } else if ( x < y ) {
            throw new Impossible("Caesar don't know how to subtract when right number is bigger");
        } else if ( x == 7 ) {
            throw new SpanishInquisition("Nobody expects!");
        } else {
            return x - y;
        }
    }

    static int ArabicSubtraction ( int x, int y ) throws Impossible {
        try {
            return RomanSubtraction( x, y );
        } catch (Unwritable exn) {
            // return RomanSubtraction( 5, 1 );
            return 0;
        }
    }

    static int FunnySubtraction ( int x, int y ) throws FunnyImpossible {
        try {
            return ArabicSubtraction( x, y + 5 );
        } catch (Impossible exn) {
            throw new FunnyImpossible("The right is too big, it must be 5 less than the left");
        }
    }

    public static void main(String[] args) throws Impossible, FunnyImpossible {

        List mt = new MT();
        List l1 = new Cons( 1, mt );
        List l22 = new Cons( 2, mt );
        List l2 = new Cons( 2, l1 );
        List l3 = new Cons( 3, l2 );
        List l4 = new Cons( 4, l3 );
        List l42 = new Cons( 4, l22 );

        System.out.println("l1 is " + l1.SumOrFirstOdd() + " should be 1");
        System.out.println("l2 is " + l2.SumOrFirstOdd() + " should be 1" );
        System.out.println("l3 is " + l3.SumOrFirstOdd() + " should be 3");
        System.out.println("l4 is " + l4.SumOrFirstOdd() + " should be 3");
        System.out.println("l22 is " + l22.SumOrFirstOdd() + " should be 2");
        System.out.println("l42 is " + l42.SumOrFirstOdd() + " should be 6");

        /*
        l4.SumOrFirstOdd();
        try ( l4.SumOrFirstOdd_H() ) catch { x }
        try ( 4 + l3.SumOrFirstOdd_H() ) catch { x }
        try ( 4 + ( throw 3 ) ) catch { x }
        { x = 3 } { x }
        3
        */

        IntReceiver iir = new IdIntReceiver();
        System.out.println("l1 is " + l1.SumOrFirstOdd_Rnd2(iir,iir) + " should be 1");
        System.out.println("l2 is " + l2.SumOrFirstOdd_Rnd2(iir,iir) + " should be 1" );
        System.out.println("l3 is " + l3.SumOrFirstOdd_Rnd2(iir,iir) + " should be 3");
        System.out.println("l4 is " + l4.SumOrFirstOdd_Rnd2(iir,iir) + " should be 3");
        System.out.println("l22 is " + l22.SumOrFirstOdd_Rnd2(iir,iir) + " should be 2");
        System.out.println("l42 is " + l42.SumOrFirstOdd_Rnd2(iir,iir) + " should be 6");

        System.out.println(l4.SumOrFirstOdd_Rnd2(iir,iir));
        System.out.println(l3.SumOrFirstOdd_Rnd2(new AddIntReceiver(iir,4),iir));
        System.out.println(iir.give(3));
        System.out.println(3);

        ArabicSubtraction( 5, 5 );
        FunnySubtraction( 5, 1 );
        ArabicSubtraction( 5, 6 );

        int x = 1 / 0;
    }
}
