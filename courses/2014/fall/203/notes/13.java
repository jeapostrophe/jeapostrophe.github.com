interface RAList {
    public int getr(int i);
    public RAList setr(int i, int v);
}

class ArrRAList implements RAList {
    int[] arr;
    RAList real_boy;
    boolean am_i_real_mom;
    int what_changed;
    int where;

    ArrRAList() {
        this.real_boy = this;
        this.am_i_real_mom = true;
        this.where = 0;
        this.what_changed = 0;
        this.arr = new int[10];
    }

    private ArrRAList( int[] arr ) {
        this.real_boy = this;
        this.am_i_real_mom = true;
        this.where = 0;
        this.what_changed = 0;
        this.arr = arr;
    }

    public int getr(int i) {
        if ( this.am_i_real_mom ) {
            return arr[i];
        } else if ( i == this.where ) {
            return this.what_changed;
        } else {
            return this.real_boy.getr(i);
        }
    }
    public RAList setr(int i, int v) {
        if ( this.am_i_real_mom ) {
            RAList pinocchio = new ArrRAList( arr );
            this.real_boy = pinocchio;
            this.am_i_real_mom = false;
            this.where = i;
            this.what_changed = arr[i];
            arr[i] = v;
            return pinocchio;
        } else {
            return 1 / 0;
        }
    }
}

class C13 {

    public static void main(String[] args) {
        RAList a = new ArrRAList();
        RAList b = a.setr(0, 1);
        RAList c = b.setr(0, 2);
        System.out.println("A[0] is " + a.getr(0) + " should be 0");
        System.out.println("B[0] is " + b.getr(0) + " should be 1");
        System.out.println("C[0] is " + c.getr(0) + " should be 2");
    }

    // y = (x - x1)/(x0 - x1) * y0 + (x - x0)/(x1 - x0) * y1


    // {Abstract Set} --------- Complement -----> {Other Abstract Set}
    //    |                                                 ^
    //    |                                                 |
    //    |                                                 |
    //    |                                                 |
    //    |                                                 |
    //  concretization                                   abstraction
    //    |                                                 |
    //    |                                                 |
    //    |                                                 |
    //    V                                                 |
    // Set(Binary Tree) --------- this.Complement -----> Set(Binary Trees)

    // INRIA the Cousots

    // Programming Principle: Don't Expose the Representation.
}

// class PirateSet {
//     int[] arr;

//     // PS.member(43433443) == false;
//     // PS.youAndMeMeetInTheArray()[5] = 43433443;
//     // PS.member(43433443) == true;

//     public youAndMeMeetInTheArray() {
//         // return arr.copy();
//         return arr;
//     }
// }
