import java.util.Arrays;

class scratch {
    // sumArray_accum : int[] int int -> int
    public static int sumArray_accum(int[] ns, int i, int answer) {
        // ... ns ... i ... answer ...
        // ... ns.length ... ns[i] (only if i < ns.length) ...
        // ... sumArray_accum ...
        
        //System.out.format("Answer is %d, should be %d%n", sumArray_accum(nums0, 0, 0), 0);
        // ns = new int[] {}
        // i = 0
        // ns.length = 0
        if (i == ns.length) {
            return answer;
        } else {
            //System.out.format("Answer is %d, should be %d%n", sumArray_accum(nums1, 0, 0), 4);
            // ns = new int[] {4}
            // i = 0
            // ns.length = 1
            // answer = 0
            return sumArray_accum(ns, i + 1, answer + ns[i]);
        }
    }

    // sumArray_while : int[] -> int
    // Returns the sum of the elements of the array
    public static int sumArray_while(int[] ns) {
        // 0. define accumulators
        int answer = 0;
        int i = 0;
        // 1. while loop with inductive case condition
        while (i != ns.length) {
            // 2. update the accumulators
            answer = answer + ns[i];
            i = i + 1;
        }
        // 3. return the base-case answer
        return answer;
    }

    // sumArray_for : int[] -> int
    // Returns the sum of the elements of the array, using FOR!!!1
    public static int sumArray_for(int[] ns) {
        int answer = 0;
        for (int i = 0; i != ns.length; i = i + 1) {
            answer = answer + ns[i];
        }
        // don't care about the value of i
        return answer;
    }

    public static void main(String[] args) {
        int[] nums4 = new int[] {4, 5, 6, 7};
        System.out.format("The element at index 0 is %d%n", nums4[0]);
        System.out.format("The element at index 1 is %d%n", nums4[1]);
        System.out.format("The element at index 2 is %d%n", nums4[2]);
        System.out.format("The element at index 3 is %d%n", nums4[3]);
        System.out.format("The length is %d%n", nums4.length);

        // Doesn't work: get ArrayIndexOutOfBoundsException
        // System.out.format("The element at index 4 is %d%n", nums4[4]);
        // nums4[4] = 8;
        // Can't change the length of an array
        // nums4.length = 5;

        nums4[0] = 40;
        System.out.format("The element at index 0 is %d%n", nums4[0]);
        System.out.format("The array is %s%n", Arrays.toString(nums4));
        
        int[] nums0 = new int[] {};
        int[] nums1 = new int[] {4};
        int[] nums2 = new int[] {4, 5};
        System.out.format("Answer is %d, should be %d%n", sumArray_accum(nums0, 0, 0), 0);
        System.out.format("Answer is %d, should be %d%n", sumArray_accum(nums1, 0, 0), 4);
        System.out.format("Answer is %d, should be %d%n", sumArray_accum(nums2, 0, 0), 9);
        System.out.format("Answer is %d, should be %d%n",
                          sumArray_accum(nums2, 0 + 1, 0 + nums2[0]), 9);
        System.out.format("Answer is %d, should be %d%n",
                          sumArray_accum(nums2, 1, 4), 9);
        System.out.format("Answer is %d, should be %d%n",
                          sumArray_accum(nums2, 1 + 1, 4 + nums2[1]), 9);
        System.out.format("Answer is %d, should be %d%n",
                          sumArray_accum(nums2, 2, 4 + 5), 9);
        System.out.format("Answer is %d, should be %d%n",
                          sumArray_accum(nums2, 2, 9), 9);
        System.out.format("Answer is %d, should be %d%n", 9, 9);

        System.out.format("Answer is %d, should be %d%n", sumArray_while(nums0), 0);
        System.out.format("Answer is %d, should be %d%n", sumArray_while(nums1), 4);
        System.out.format("Answer is %d, should be %d%n", sumArray_while(nums2), 9);

        System.out.format("Answer is %d, should be %d%n", sumArray_for(nums0), 0);
        System.out.format("Answer is %d, should be %d%n", sumArray_for(nums1), 4);
        System.out.format("Answer is %d, should be %d%n", sumArray_for(nums2), 9);
    }
}

class scratch2 {
    public static void main(String[] args) {
        System.out.format("The command-line arguments are %s%n",
                          Arrays.toString(args));
    }
}

class Posn {
    public int x;
    public int y;

    public Posn(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public String toString() {
        return String.format("(%d, %d)", this.x, this.y);
    }

    // reflect : Posn -> Posn
    public Posn reflect() {
        return new Posn(this.y, this.x);
    }

    // reflect_this : Posn -> void
    public void reflect_this() {
        // Doesn't work:
        /*
        // store: this.x = a, this.y = b
        this.x = this.y;
        // store: this.x = b, this.y = b
        this.y = this.x;
        // store: this.x = b, this.y = b
        */

        // store: this.x = a, this.y = b
        int temp = this.x;
        // store: this.x = a, this.y = b, temp = a
        this.x = this.y;
        // store: this.x = b, this.y = b, temp = a
        this.y = temp;
        // store: this.x = b, this.y = a, temp = a
    }
}

class scratch3 {
    // reflectAll : Posn[] -> void
    // Reflects all positions in the array, by mutating the array
    public static void reflectAll(Posn[] ps) {
        for (int i = 0; i < ps.length; i = i + 1) {
            ps[i] = ps[i].reflect();
        }
    }

    // reflectAllNoNew : Posn[] -> void
    // Reflects all positions in the array, by mutating the positions
    public static void reflectAllNoNew(Posn[] ps) {
        for (int i = 0; i < ps.length; i++) {
            ps[i].reflect_this();
        }
    }

    public static void main(String[] args) {
        Posn[] posns = new Posn[] {new Posn(1, 2), new Posn(10, 20)};
        reflectAll(posns);
        System.out.format("Answer is %s, should be %s%n",
                          Arrays.toString(posns),
                          Arrays.toString(new Posn[] {new Posn(2, 1), new Posn(20, 10)}));
    }
}

// Unrolling for loops

class scrach4 {
    // sum4 : -> void
    // Computes 0 + (0 + 1 + 2 + 3), the sum of the first four natural numbers,
    // and prints it.
    public static void sum4() {
        int answer = 0;
        for (int i = 0; i < 4; i = i + 1) {
            answer = answer + i;
        }
        // Prints "Answer is 6\n"
        System.out.format("Answer is %d%n", answer);
    }
    
    // sum4_unrolled : -> void
    // This is equivalent to sum4 above, but the for loop is unrolled.
    // Fortunately, the for loop is simple enough that we can unroll it without
    // turning it into a while loop first, by copying the statement inside and
    // replacing i with the values it will have.
    // We could do store-tracking on this version of sum4.
    public static void sum4_unrolled() {
        int answer = 0;
        answer = answer + 0;
        answer = answer + 1;
        answer = answer + 2;
        answer = answer + 3;
        // Prints "Answer is 6\n"
        System.out.format("Answer is %d%n", answer);
    }
    
    // ------------------------------------------------------------------------
    
    // We can't always unroll a for loop without turning it into a while loop
    // first. The loop in badSum4 is one example:

    // badSum4 : -> void
    // Apparently tries to compute 0 + (0 + 1 + 2 + 3) but actually never
    // returns.
    public static void badSum4() {
        int answer = 0;
        for (int i = 0; i < 4; i = i + 1) {
            answer = answer + i;
            i = i - 1;  // WTH? This changes i, which we're looping over...
        }
        // The loop never stops, so Java never prints an answer. Bummer!
        
        System.out.format("Answer is %d%n", answer);
    }
    
    // If we tried to unroll the for loop in badSum4, we would never stop
    // unrolling. But we can unroll a while loop one iteration at a time.
    // First, we change the for loop into a while loop:

    // badSum4_while : -> void
    // Equivalent to badSum4, but the for loop is changed to a while loop. Take
    // some time to figure out where, in sum4, the statements were copied from.
    public static void badSum4_while() {
        int answer = 0;
        int i = 0;
        while (i < 4) {
            answer = answer + i;
            i = i - 1;            
            i = i + 1;
        }
        System.out.format("Answer is %d%n", answer);
    }
    
    // Then we unroll one iteration, by copying the loop and changing "while"
    // to "if" in the upper copy.
    
    // badSum4_while_unrolled : -> void
    // Equivalent to badSum4_while, but with the while loop unrolled one
    // iteration. Also has store-tracking comments.
    public static void badSum4_while_unrolled() {
        int answer = 0;
        int i = 0;
        // store: answer = 0, i = 0
        
        if (i < 4) {
            answer = answer + i;
            // store: answer = 0, i = 0
            i = i - 1;
            // store: answer = 0, i = -1
            i = i + 1;
            // store: answer = 0, i = 0
        }
        
        // Ah. That's the problem: i = 0 in the store after every iteration.
        // Because 0 < 4, the while condition will always be true.
        
        while (i < 4) {
            answer = answer + i;
            i = i - 1;
            i = i + 1;
        }
        System.out.format("Answer is %d%n", answer);
    }
    
    // ------------------------------------------------------------------------
    
    // In the case of badSum4_while, we don't actually have to unroll the loop
    // to figure out why it loops forever. We can do store-tracking in the loop
    // by using names for unknown values, as in reflect_this in scratch3.java.

    // badSum4_while2 : -> void
    // Equivalent to badSum4_while. Has store-tracking comments within the
    // while loop.
    public static void badSum4_while2() {
        int answer = 0;
        int i = 0;
        while (i < 4) {
            // store: answer = a, i = b
            answer = answer + i;
            // store: answer = a + b, i = b
            i = i - 1;
            // store: answer = a + b, i = b - 1
            i = i + 1;
            // store: answer = a + b, i = b - 1 + 1 = b
            
            // If i < 4 in any iteration, then i < 4 in the next iteration.
            // Therefore, the loop will never stop.
        }
        System.out.format("Answer is %d%n", answer);
    }

    public static void main(String[] args) {
        sum4();
        sum4_unrolled();
        
        // Don't call any versions of badSum4, because they loop forever
        //badSum4();
    }
}

