class RingBuffer {
    int size;
    int[] smg;
    int start;
    int end;

    // returns a rb of the size 
    RingBuffer(int size) { 
        this.size = size;
        this.smg = new int[size];
        this.start = 0;
        this.end = 0;
    }
    // insert : int ->
    public RingBuffer insert( int elem ) {
        this.smg[ this.end ] = elem;
        this.end = (this.end + 1) % this.size;
        return this;
    }

    // read : int -> int
    // REQUIRES: insert() has been called at least idx times
    //           idx < size
    // returns the element inserted idx times in the past
    // EXAMPLE
    //  (new RingBuffer(2)).insert(1).insert(2).insert(3).read(1)
    //   = 2
    public int read ( int idx ) {
        return 
            this.smg[ (this.end - idx + this.size - 1)
                      % this.size  ];
    }

    // BEFORE: start = 0, end = 0, size = 2, smg = { ? , ? }
    // insert(1)
    // AFTER: smg = { 1 , ? }, end = 1
    // insert(2)
    // AFTER: smg = { 1 , 2 }, end = 0
    // insert(3)
    // AFTER: smg = { 3 , 2 }, end = 1
    // read(1)
}

class Box {
    boolean d;

    Box() {
        this.d = true;
    }

    // observe : -> bool
    // EFFECTS: this
    // IMPL: flips the booleanosity of this.d
    // CLIENT: cycles between false and true
    // CLIENT: the next call to observe will return the opposite of what this one does; the first call returns false
    // BIG DEAL: Specs are for clients, not implementers
    public boolean observe() {
        this.d = ! this.d;
        return this.d;
    }
}

class C7 {
    // returns three more than the input
    // REQUIRES: x be even
    static int f ( int x ) {
        return x + 3;
    }

    // g : ->
    // REQUIRES: amanda_before is even
    // EFFECTS: amanda
    // amanda_after is amanda_before + 3
    static int amanda = 0;
    static void g (  ) {
        amanda = amanda + 3;
    }

    static class Store {
        public int amanda;
        Store ( int amanda ) {
            this.amanda = amanda;
        }
    }

    // XXXg : Store -> Store
    // REQUIRES: the store contain an even amanda
    // produces a new store that contains an amanda three degrees
    // cooler
    static Store XXXg ( Store st  ) {
        return new Store ( st.amanda + 3 );
    }

    static int isabella = 0;
    // hm : int -> int
    // REQUIRES: y can't be zero
    // EFFECTS: isabella
    // isabella_after is one more than isabella_before
    // returns isabella_before divided by y
    static int hm ( int y ) {
        int r = isabella / y;
        isabella = isabella + 1;
        return r;
    }

    static class HMStore {
        public int isabella;
        HMStore ( int isabella ) {
            this.isabella = isabella;
        }
    }
    static class IntAndHMStore {
        int r;
        HMStore st;

        IntAndHMStore( int r, HMStore st ) {
            this.r = r;
            this.st = st;
        }
    }

    // XXXh : Store int -> int x Store
    // REQUIRES: y can't be zero
    // returns st.isabella divided by y and a store where isabella is
    // one more than st.isabella
    static IntAndHMStore XXXhm ( HMStore st, int y ) {
        int r = st.isabella / y;
        HMStore stp = new HMStore( st.isabella + 1 );
        return new IntAndHMStore( r, stp );
    }

    // The transformation is called "Store-Passing Style"

    public static void main(String[] args) {
        System.out.println("Yo! Raps!");
        System.out.println(f(4) + " should be " + 7);
        
        int x = 4;
        System.out.println(f(x) + " should be " + 7);
        System.out.println((x + 3) + " should be " + 7);
        System.out.println((4 + 3) + " should be " + 7);
        x = x / 2;
        System.out.println(f(x) + " should be " + 5);
        System.out.println((x + 3) + " should be " + 5);
        System.out.println((2 + 3) + " should be " + 5);
        
        amanda = 4;
        // BEFORE amanda = 4
        g();        
        // AFTER amanda = 7
        System.out.println(amanda + " should be " + 7);
        g();
        System.out.println(amanda + " should be " + 10);

        // From here
        Store st_0 = new Store( 4 );
        Store st_1 = XXXg( st_0 );
        System.out.println(st_1.amanda + " should be " + 7);
        Store st_2 = XXXg( st_1 );
        System.out.println(st_2.amanda + " should be " + 10);
        // to here
        // st_x is used "linearly"
        System.out.println(st_0.amanda + " should be " + 4);
        
        Box sb = new Box();
        System.out.println("The box says, " + sb.observe());
        System.out.println("The box says, " + sb.observe());
        System.out.println("The box says, " + sb.observe());
        System.out.println("The fox says, ?");

        RingBuffer rb = (new RingBuffer(2));
        rb.insert(1);
        System.out.println(rb.read(0) + " should be " + 1 );
        rb.insert(2);
        System.out.println(rb.read(0) + " should be " + 2 );
        System.out.println(rb.read(1) + " should be " + 1 );
        rb.insert(3);
        System.out.println(rb.read(0) + " should be " + 3 );
        System.out.println(rb.read(1) + " should be " + 2 );
        rb.insert(4);
        System.out.println(rb.read(0) + " should be " + 4 );
        System.out.println(rb.read(1) + " should be " + 3 );
    }
}
