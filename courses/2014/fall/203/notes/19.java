interface Action<X,Y> {
    public Y apply( X h );
}

// A listof X is either
//  - empty
/// - (cons X (listof X))
interface ListComputation<X,Y> {
    public Y calledOnEmpty();
    public Y calledOnCons( X first, Y calledOnRest );
}

// A treeof X is either
// - a leaf
// - a (branch (treeof X) X (treeof X))
interface TreeComputation<X,Y> {
    public Y calledOnLeaf();
    public Y calledOnBranch( Y calledOnLeft, X here, Y calledOnRight );
}


interface AssocListComputation<X,Y> {
    public Y assocCalledOnCons( Y calledOnRest, X first );
}

// A Visitor
interface MAssocListComputation<X,Y> {
    public Y getValue();
    public void massocCalledOnCons( X first );
}

interface List<X> {
    public int length();
    public Object doStuffToYourself( ListComputation C );
    public Object doStuffToYourselfAssoc( AssocListComputation C, Object mt );
}

class Empty<X> implements List<X> {
    Empty() { }

    public int length() {
        return 0;
    }
    public Object doStuffToYourself( ListComputation C ) {
        return C.calledOnEmpty();   
    }
    public Object doStuffToYourselfAssoc( AssocListComputation C, Object mt ) {
        return mt;   
    }
}

class Cons<X> implements List<X> {
    X first;
    List<X> rest;

    Cons( X first, List<X> rest ) {
        this.first = first;
        this.rest = rest;
    }

    public int length() {
        return 1 + rest.length();
    }
    // (1 + (1 + (1 + (1 + 0)))) ---> is what we have and it is LINEAR stack space

    // ((((0 + 1) + 1) + 1) + 1) ---> constant stack space

    // Assoc = (x + y) + z = x + (y + z)
    public Object doStuffToYourself( ListComputation C ) {
        return C.calledOnCons( this.first, rest.doStuffToYourself(C) );   
    }
    public Object doStuffToYourselfAssoc( AssocListComputation C, Object mt ) {
        return rest.doStuffToYourselfAssoc(C, C.assocCalledOnCons( mt, this.first ));
    }
}

interface Comparable<X> {
    public boolean eq ( X y );
    public boolean lessThan ( X y );
}

interface BST<X extends Comparable> {
    boolean isIn ( X there );
    public Object doStuffToYourself( TreeComputation C );
}

class BST_MT<X extends Comparable> implements BST<X> {
    BST_MT() { }

    public boolean isIn ( X there ) {
        return false;
    }
    public Object doStuffToYourself( TreeComputation C ) {
        return C.calledOnLeaf();
    }
}

class BST_BR<X extends Comparable> implements BST<X> {
    X here;
    BST left;
    BST right;
    BST_BR(BST left, X here, BST right) {
        this.left = left;
        this.here = here;
        this.right = right;
    }

    public boolean isIn ( X there ) {
        if ( here.eq(there) ) {
            return true;
        } else {
            if ( there.lessThan( here ) ) {
                return left.isIn(there);
            } else {
                return right.isIn(there);
            }
        }
    }
    public Object doStuffToYourself( TreeComputation C ) {
        return C.calledOnBranch( left.doStuffToYourself(C),
                                 here,
                                 right.doStuffToYourself(C) );
    }
}

class Int implements Comparable<Int> {
    int v;
    Int( int x ) { this.v = x; }
    public boolean lessThan( Int y ) {
        return this.v < y.v;
    }
    public boolean eq( Int y ) {
        return this.v == y.v;
    }
    public String toString() {
        return "" + this.v;
    }
}

class C_Length implements ListComputation<Object,Int>,AssocListComputation<Object,Int>,TreeComputation<Object,Int> {
    C_Length() {}
    public Int calledOnLeaf() {
        return new Int(0);
    }
    public Int calledOnBranch( Int left, Object here, Int right ) {
        return new Int( 1 + left.v + right.v );
    }

    public Int calledOnEmpty() {
        return new Int(0);
    }
    public Int calledOnCons( Object first, Int calledOnRest ) {
        return new Int(1 + calledOnRest.v);
    }
    public Int assocCalledOnCons( Int calledOnRest, Object first ) {
        return new Int(1 + calledOnRest.v);
    }
}

class C_Sum implements ListComputation<Int,Int>,AssocListComputation<Int,Int> {
    C_Sum() {}
    public Int calledOnEmpty() {
        return new Int(0);
    }
    public Int calledOnCons( Int first, Int calledOnRest ) {
        return new Int(first.v + calledOnRest.v);
    }
    public Int assocCalledOnCons( Int calledOnRest, Int first ) {
        return new Int(first.v + calledOnRest.v);
    }
}

class Bool {
    boolean v;
    Bool( boolean b ) { this.v = b; }
}

class C_IsEmptyHuh implements ListComputation<Object,Bool> {
    C_IsEmptyHuh() {}
    public Bool calledOnEmpty() {
        return new Bool ( true );
    }
    public Bool calledOnCons( Object first, Bool calledOnRest ) {
        return new Bool ( false );
    }
}

class C_Stringify  implements ListComputation<Object,String>,AssocListComputation<Object,String> {
    C_Stringify() {}
    public String calledOnEmpty() {
        return "";
    }
    public String calledOnCons( Object first, String calledOnRest ) {
        return first.toString() + "," + calledOnRest;
    }
    public String assocCalledOnCons( String calledOnRest, Object first ) {
        return first.toString() + "," + calledOnRest;
    }
}

class C19 {
    static int f (int x) {
        if ( x == 0 ) {
            return 0;
        } else {
            return f(x - 1);
        }
    }

    public static void main(String[] args) {
        // f(32000);

        List<Int> mt = (new Empty<Int>());
        List<Int> l5 = (new Cons<Int>(new Int(5), mt));
        List<Int> l25 = (new Cons<Int>(new Int(2), l5));
        List<Int> l325 = (new Cons<Int>(new Int(3), l25));
        System.out.println(l325.length() + " should be " + 3);
        System.out.println(((Int) l325.doStuffToYourself(new C_Length())).v
                           + " should be " + 3);
        System.out.println(((Bool) l325.doStuffToYourself(new C_IsEmptyHuh())).v
                           + " should be " + false);
        System.out.println(((Int) l325.doStuffToYourself(new C_Sum())).v
                           + " should be " + (5 + 2 + 3));

        System.out.println(((String) l325.doStuffToYourself(new C_Stringify()))
                           + " should be " + "3,2,5,");

        System.out.println(((Int) l325.doStuffToYourselfAssoc(new C_Length(), new Int(0))).v
                           + " should be " + 3);
        System.out.println(((Int) l325.doStuffToYourselfAssoc(new C_Sum(), new Int(0))).v
                           + " should be " + (5 + 2 + 3));

        System.out.println(((String) l325.doStuffToYourselfAssoc(new C_Stringify(), ""))
                           + " should be " + "3,2,5,");


        // doStuffToYourself is a REDUCTION or a FOLD

        BST<Int> b_mt = new BST_MT<Int>();
        BST<Int> b_5 = new BST_BR<Int>( b_mt, new Int(5), b_mt );
        BST<Int> b_7 = new BST_BR<Int>( b_mt, new Int(7), b_mt );
        BST<Int> b_6 = new BST_BR<Int>( b_5, new Int(6), b_7 );
        System.out.println( b_6.isIn( new Int(5) ) + " should be " + true );
        System.out.println(((Int) b_6.doStuffToYourself(new C_Length())).v
                           + " should be " + 3);
    }
}

// Fold

// (tree-fold Leaf1 Branch1 (list-fold Mt1 ConsFunc1 L))
// =>
// (list-fold Leaf1 (compose Branch1 ConsFunc1) L)

// Deforestation
