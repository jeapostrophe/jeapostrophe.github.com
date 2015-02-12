import java.util.Random;

class MT {
    int[] state;
    int currentIndex;

    MT() {
        this.state = new int[624];
        for (int i = 0; i < 624; i++) {
            this.state[i] = i;
        }
        this.currentIndex = 0;
    }
    MT(int[] state) {
        this.state = state;
        this.currentIndex = 0;
    }

    int nextInt() {
        int tmp = state[currentIndex];
        tmp ^= (tmp >>> 11);
        tmp ^= (tmp << 7) & 0x9d2c5680;
        tmp ^= (tmp << 15) & 0xefc60000;
        tmp ^= (tmp >>> 18);

        currentIndex++;
        if ( currentIndex == 624 ) {
            currentIndex = 0;
            this.nextState();
        }

        return tmp;
    }

    void nextState() {
        // Iterate through the state
        for (int i = 0; i < 624; i++) {
            // y is the first bit of the current number,
            // and the last 31 bits of the next number
            int y = (state[i] & 0x80000000) + (state[(i + 1) % 624] & 0x7fffffff);
            // first bitshift y by 1 to the right
            int next = y >>> 1;
            // xor it with the 397th next number
            next ^= state[(i + 397) % 624];
            // if y is odd, xor with magic number
            if ((y & 1L) == 1L) {
                next ^= 0x9908b0df;
            }
            // now we have the result
            state[i] = next;
        }
    }
}

class HackMT {
    static int unBitshiftRightXor(int value, int shift) {
        // we part of the value we are up to (with a width of shift bits)
        int i = 0;
        // we accumulate the result here
        int result = 0;
        // iterate until we've done the full 32 bits
        while (i * shift < 32) {
            // create a mask for this part
            int partMask = (-1 << (32 - shift)) >>> (shift * i);
            // obtain the part
            int part = value & partMask;
            // unapply the xor from the next part of the integer
            value ^= part >>> shift;
            // add the part to the result
            result |= part;
            i++;
        }
        return result;
    }

    static int unBitshiftLeftXor(int value, int shift, int mask) {
        // we part of the value we are up to (with a width of shift bits)
        int i = 0;
        // we accumulate the result here
        int result = 0;
        // iterate until we've done the full 32 bits
        while (i * shift < 32) {
            // create a mask for this part
            int partMask = (-1 >>> (32 - shift)) << (shift * i);
            // obtain the part
            int part = value & partMask;
            // unapply the xor from the next part of the integer
            value ^= (part << shift) & mask;
            // add the part to the result
            result |= part;
            i++;
        }
        return result;
    }

    static int undoOne( int output ) {
        int value = output;
        value = unBitshiftRightXor(value, 18);
        value = unBitshiftLeftXor(value, 15, 0xefc60000);
        value = unBitshiftLeftXor(value, 7, 0x9d2c5680);
        value = unBitshiftRightXor(value, 11);
        return value;
    }

    static void doIt() {
        MT mt = new MT();

        int[] observed = new int[624];
        for (int i = 0; i < 624; i++) {
            int obs = mt.nextInt();
            int undo = undoOne(obs);
            System.out.println("got " + obs + " calculated " + undo);
            observed[i] = undo;
        }
    }
}

class HackJavaRandom {
    static void doIt() {
        Random random = new Random();
        long multiplier = 0x5DEECE66DL;
        long mask = (1L << 48) - 1;
        long addend = 11;
        int x1 = random.nextInt();
        int x2 = random.nextInt();
        long v1 = x1 & 0x00000000ffffffffL;
        long v2 = x2 & 0x00000000ffffffffL;
        for (int i = 0; i < Math.pow(2, 16); i++) {
            long seed = v1 * ((long)Math.pow(2, 16)) + i;
            if ((((seed * multiplier + addend) & mask) >>> 16) == v2) {
                System.out.println("Seed found: " + seed);
                break;
            }
        }
    }
}

class C5 {
    public static void main( String[] args ) {
        HackJavaRandom.doIt();
        HackMT.doIt();
    }
}
