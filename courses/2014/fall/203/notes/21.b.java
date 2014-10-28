interface PHash<K extends Comparable,V> {
    V lookup( K k, V def );
    PHash<K,V> add ( K k, V v );
}

class PHash_MT<K extends Comparable,V> implements PHash<K,V> {
    public PHash_MT() { }
    public V lookup( K k, V def ) { return def; }
    public PHash<K,V> add ( K k, V v ) {
        return new PHash_Cons<K,V>( k, v, this );
    }
}

class PHash_Cons<K extends Comparable,V> implements PHash<K,V> {
    K ck;
    V cv;
    PHash<K,V> next;

    public PHash_Cons(K ck, V cv, PHash<K,V> next) {
        this.ck = ck; this.cv = cv; this.next = next;
    }
    public V lookup( K k, V def ) {
        System.out.println("Comparing " + k + " to " + this.ck);
        if ( this.ck.compareTo(k) == 0 ) {
            return this.cv;
        } else {
            return this.next.lookup(k, def);
        }
    }
    public PHash<K,V> add ( K k, V v ) {
        return new PHash_Cons<K,V>( k, v, this );
    }
}

interface IHash<K extends Comparable,V> {
    V lookup( K k, V def );
    void add ( K k, V v );
}

class IHash_Array<K extends Comparable,V> implements IHash<K,V> {
    // Actually PHash<K,V>
    Object data[];

    public IHash_Array(int size) {
        this.data = new Object[size];
        PHash<K,V> mt = new PHash_MT<K,V>();
        for ( int i = 0; i < this.data.length; i++ ) {
            this.data[i] = mt;
        }
    }

    private int indexOf( K k ) {
        return k.hashCode() % this.data.length;
    }
    private PHash<K,V> littleHash ( int idx ) {
        return ((PHash<K,V>) (this.data[idx]));
    }

    public V lookup( K k, V def ) {
        return this.littleHash( this.indexOf(k) ).lookup( k, def);
    }
    public void add ( K k, V v ) {
        int idx = this.indexOf(k);
        this.data[idx] = this.littleHash( idx ).add( k, v );
        return;
    }
}

class C21 {
    public static void main( String[] args ) {
        PHash<Integer,String> ht0 = new PHash_MT<Integer,String>();
        PHash<Integer,String> ht1 = ht0.add( 0, "Answer" );
        PHash<Integer,String> ht2 = ht1;
        for ( int i = 1; i < 100; i++ ) {
            ht2 = ht2.add( i, "Not Answer" );
        }

        System.out.println(ht2.lookup( 0, "Not Answer" ) + " should be " + "Answer" );

        IHash<Integer,String> iht = new IHash_Array<Integer,String>(20);
        iht.add( 0, "Answer" );
        for ( int i = 1; i < 100; i++ ) {
            iht.add( i, "Not Answer" );
        }
        System.out.println(iht.lookup( 0, "Not Answer" ) + " should be " + "Answer" );
        System.out.println(iht.lookup( 3, "Default" ) + " should be " + "Not Answer" );

    }
}
