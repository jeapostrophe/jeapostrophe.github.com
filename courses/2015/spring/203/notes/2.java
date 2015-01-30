import java.util.Random;

class Cart {
    // ;; (define pie 3)
    public String puller;
    private String contents;

    public static int f ( int x ) {
        return x + 2;
    }
}

// (define-struct posn (x y))
// (define o (posn 0 0))
// (posn-x o)

class Posn {
    public int x;
    public int y;

    // A "constructor" is a special category in
    // Java SEPARATE from functions.
    public Posn ( int x, int y ) {
        this.x = x;
        this.y = y;
    }

    // manhattanDistance : Posn -> int
    public static int manhattanDistance ( Posn p ) {
        return p.x + p.y;
    }    
    
    // lazyManhattanDistance : Posn -> int
    public int lazyManhattanDistance ( /* Posn this */ ) {
        if ( 5000 <= this.x && this.x <= 6000 ) {
            return -1;
        } else {
            return Math.abs(this.x) + Math.abs(this.y);
        }
    }
}

class C2 {
    // ;; f : list(String) -> void
    // (define (f x) (+ x 2))
    public static void main( String[] arrrrghs ) {
        System.out.println("Helllooo nurse!");
        System.out.println("f(4) = " + Cart.f(4) );

        Posn o = new Posn( 0, 0 );
        System.out.println("o's x is = " + o.x );
        System.out.println("o's md is = " + Posn.manhattanDistance(o) + " this should have been: " + 0 );
        System.out.println("o's md is = " + o.lazyManhattanDistance() + " this should have been: " + 0 );        

        // System.out.println("o's md is = " + Posn.manhattanDistance(42) + " this should have been: " + 0 );
        // System.out.println("o's md is = " + arrrrghs[0].lazyManhattanDistance() + " this should have been: " + 0 );

        if ( ! (test(o)) ) {
            System.out.println("DANGER! test fails on o");
        }

        for ( int x = -25; x <= 25; x++ ) {
            for ( int y = -25; y <= 25; y++ ) {
                Posn t = new Posn( x, y );
                if ( ! (test(t)) ) {
                    System.out.println("DANGER! test fails on t where x = " + x + " and y = " + y);
                }
            }
        }

        Random r = new Random();
        for ( int i = 0; i <= 1000; i++ ) {
            double x = (r.nextDouble() - 0.5) * 30000.0;
            double y = (r.nextDouble() - 0.5) * 30000.0;
            Posn t = new Posn( (int)x, (int)y );
            if ( ! (test(t)) ) {
                System.out.println("DANGER! test fails on t where x = " + x + " and y = " + y);
            }
        }
    }

    // A PROPERTY on lazyManhattanDistance
    // ∀p . p.lazyManhattanDistance() >= 0 
    static boolean test ( Posn p ) {
        int md = p.lazyManhattanDistance();
        return (md >= 0);
    }
}
