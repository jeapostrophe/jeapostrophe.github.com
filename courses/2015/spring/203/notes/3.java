// List

/*

(define (total l)
 (cond
  [(empty? l)
   0]
 [else
  (+ (first l)
     (total (rest l)))]))

 */

// A list of ages is either...
interface List {
    // total : List -> int
    int total( /* List this */ );
    // sort : List -> List
    List sort();
    // insert : List int -> List
    List insert(int i);
}

// - an empty list of ages: empty
class Empty implements List {
    Empty() {}

    public int total() {
        return 0;
    }

    public List sort() {
        return this;
    }
    public List insert(int i) {
        return new Cons( i, this );
    }

    public String toString() {
        return "new Empty()";
    }
}

// - a cons of ages: (cons age list-of-ages)
class Cons implements List {
    int age;
    List restOfAges;
    
    Cons(int age, List restOfAges ) {
        this.age = age;
        this.restOfAges = restOfAges;
    }

    public int total() {
        // this : List
        // this.age : int
        // this.restOfAges : List
        // this.restOfAges.total() : int
        return this.age + this.restOfAges.total();
    }

    public List sort() {
        return this.restOfAges.sort().insert(this.age);
    }
    // Notice that insert is PURE meaning that it always
    // returns a NEW value rather than modifying the old
    // one
    public List insert(int i) {
        if ( i < this.age ) {
            return new Cons( i, this );
        } else {
            return new Cons( this.age,
                             this.restOfAges.insert(i) );
        }
    }

    public String toString() {
        return "new Cons(" + this.age + ", "
            + this.restOfAges + ")";
    }
}

// BST

interface BST {
    int smallest();
    // insert : BST int -> BST
    BST insert( int x );
}

class Leaf implements BST {
    Leaf() {}

    public String toString() {
        return "new Leaf()";
    }

    public int smallest() {
        throw
            new RuntimeException
            ("No smallest in a Leaf");
    }

    public BST insert( int x ) {
        return new Branch( this, x, this );
    }
}

class Branch implements BST {
    BST left;
    int key;
    BST right;

    Branch(BST left, int key, BST right) {
        this.left = left;
        this.key = key;
        this.right = right;
    }

    public String toString() {
        return "new Branch(" +
            this.left + ", " +
            this.key + ", " +
            this.right + ")";
    }

    // Whenever F calls G and G throws E,
    // then F will throw E
    // EXCEPT when F "catches" things like E
    
    public int smallest() {
        // left = t2, key = 4, right = t6
        try {
            return this.left.smallest();
        } catch (RuntimeException e) {
            return this.key;
        }
    }

    public BST insert( int x ) {
        if ( x == this.key ) {
            return this ;
        } else if ( x < this.key ) {
            return new Branch( this.left.insert(x),
                               this.key,
                               this.right );
        } else { /* x > this.key */
            return new Branch( this.left,
                               this.key,
                               this.right.insert(x)  );
        }
    }
}

class C3 {
    public static void main( String[] args ) {
        System.out.println("Test");

        // I want to keep track of the ages
        // of my kids: 6, 4, 2
        //
        // (cons 6 (cons 4 (cons 2 empty)))
        List honeymoon = new Empty();
        List baby = new Cons(2, honeymoon);
        List girls = new Cons(4, baby);
        List kids = new Cons(6, girls);

        System.out.println("Total age is: " + kids.total()
                           + " should be: " + 12 );

        System.out.println("Kids is " + kids );
        System.out.println("sorted Kids is "
                           + kids.sort() );

        // BST example
        BST bot = new Leaf();
        BST t1 = new Branch( bot, 1, bot );
        BST t3 = new Branch( bot, 3, bot );
        BST t2 = new Branch( t1, 2, t3 );
        BST t6 = new Branch( bot, 6, bot );
        BST t8 = new Branch( bot, 8, bot );
        BST t7 = new Branch( t6, 7, t8 );
        BST t5 = new Branch( t2, 5, t7 );

        //      5
        //     / \
        //    2   7
        //   / \ / \
        //  1  3 6  8
        // / \/\/\ / \
        //l  lllll l l
        
        // BST example fun call
        System.out.println("The smallest thing is: " +
                           t5.smallest() +
                           " should be: " + 1 );

        System.out.println("Four is: " +
                           t5.insert(4) );
    }
}
