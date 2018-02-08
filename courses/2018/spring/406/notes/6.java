import java.util.Iterator;
import java.util.Arrays;
import java.util.Scanner;
import java.util.function.Function;
import java.util.function.BiFunction;
import java.util.function.Supplier;
import java.util.HashMap;

// Helpers
class IMap<K, V> {
    HashMap<K, V> hm;
    IMap () {
        this( new HashMap<K, V>() ); }
    private IMap (HashMap<K, V> hm) {
        this.hm = hm; }
    public V ref(K x) {
        V r = hm.get(x);
        if ( r == null ) {
            throw new RuntimeException(x + " not found in hash map"); }
        return r; }
    public IMap<K,V> set(K x, V v) {
        HashMap<K,V> hmp = new HashMap<K,V>(hm);
        hmp.put(x, v);
        return new IMap<K,V>(hmp); }
    public IMap<K,V> merge( IMap<K,V> o ) {
        HashMap<K,V> hmp = new HashMap<K,V>(hm);
        hmp.putAll( o.hm );
        return new IMap<K,V>(hmp); }
}

class ISet<K> {
    IMap<K,Boolean> m;
    ISet( ) {
        this( new IMap<K,Boolean>() ); }
    ISet( K k ) {
        this( new IMap<K,Boolean>().set(k, true) ); }
    private ISet( IMap<K,Boolean> m ) {
        this.m = m; }
    ISet<K> add( K k ) {
        return new ISet<K>( m.set(k, true) ); }
    ISet<K> merge( ISet<K> o ) {
        return new ISet<K>( m.merge(o.m) ); }
}

interface List<X> {
    public <Acc> Acc fold(Acc mt, BiFunction<X,Acc,Acc> f);
}
class Mt<X> implements List<X> {
    Mt () {}

    public <Acc> Acc fold(Acc mt, BiFunction<X,Acc,Acc> f) {
        return mt; }
}
class Cons<X> implements List<X> {
    X a; List<X> d;
    Cons (X a) {
        this(a, new Mt<X>() ); }
    Cons (X a, List<X> d) {
        this.a = a;
        this.d = d; }

    public <Acc> Acc fold(Acc mt, BiFunction<X,Acc,Acc> f) {
        return d.fold(f.apply(a, mt), f); }
}
class Snoc<X> implements List<X> {
    List<X> d; X a;
    Snoc (X a) {
        this(new Mt<X>(), a ); }
    Snoc (List<X> d, X a) {
        this.d = d;
        this.a = a; }

    public <Acc> Acc fold(Acc mt, BiFunction<X,Acc,Acc> f) {
        return f.apply(a, d.fold(mt, f)); }
}
class Append<X> implements List<X> {
    List<X> l; List<X> r;
    Append( List<X> l, List<X> r ) {
        this.l = l;
        this.r = r ; }

    public <Acc> Acc fold(Acc mt, BiFunction<X,Acc,Acc> f) {
        return r.fold( l.fold(mt, f), f ); }
}

// Real Code
class Var {
    String label;
    static int counter = 0;
    static void reset() { counter = 0; }
    Var (String label) { this.label = label + "_" + counter++; }
    Var (Var base) { this.label = base.label + "_" + counter++; }

    public String toString() { return label; }
};

interface RExpr {
    public int eval(Iterator<String> in, IMap<Var,Integer> env);
    public RExpr uniquify(IMap<Var, Var> rho);
    public CProgram flatten();
}

class RInt implements RExpr {
    int n;
    RInt( int n ) { this.n = n; }
    public String toString() { return new Integer(n).toString(); }
    public int eval(Iterator<String> in, IMap<Var,Integer> env) {
        return n; }
    public RExpr uniquify(IMap<Var, Var> rho) {
        return this; }
    public CProgram flatten() {
        return new CProgram( new CAInt(n) ); }
}

class RRead implements RExpr {
    RRead( ) { }
    public String toString() { return "(read)"; }
    public int eval(Iterator<String> in, IMap<Var,Integer> env) {
        return Integer.parseInt(in.next()); }
    public RExpr uniquify(IMap<Var, Var> rho) {
        return this; }
    public CProgram flatten() {
        Var rv = new Var("read");
        return new CProgram( new ISet<Var>(rv),
                             new Cons<CStmt>( new CSSet( rv, new CERead() ) ),
                             new CARef(rv) ); }
}

class RNeg implements RExpr {
    RExpr arg;
    RNeg( RExpr arg ) { this.arg = arg; }
    public String toString() { return "(- " + arg + ")"; }
    public int eval(Iterator<String> in, IMap<Var,Integer> env) {
        return -1 * arg.eval(in, env); }
    public RExpr uniquify(IMap<Var, Var> rho) {
        return new RNeg(arg.uniquify(rho)); }
    public CProgram flatten() {
        Var nv = new Var("neg");
        CProgram ep = arg.flatten();
        return new CProgram
            ( ep.vs.add(nv),
              new Snoc<CStmt>(ep.ss, new CSSet( nv, new CENeg( ep.ret ) ) ),
              new CARef(nv) ); }
}

class RAdd implements RExpr {
    RExpr lhs;
    RExpr rhs;
    RAdd( RExpr lhs, RExpr rhs ) { this.lhs = lhs; this.rhs = rhs; }
    public String toString() { return "(+ " + lhs + " " + rhs + ")"; }
    public int eval(Iterator<String> in, IMap<Var,Integer> env) {
        return lhs.eval(in, env) + rhs.eval(in, env); }
    public RExpr uniquify(IMap<Var, Var> rho) {
        return new RAdd( lhs.uniquify(rho), rhs.uniquify(rho) ); }
    public CProgram flatten() {
        Var av = new Var("add");
        CProgram lp = lhs.flatten();
        CProgram rp = rhs.flatten();
        return new CProgram
            ( lp.vs.merge(rp.vs).add(av),
              new Snoc<CStmt>
              (new Append<CStmt>( lp.ss, rp.ss ),
               new CSSet( av,
                          new CEAdd( lp.ret, rp.ret ) ) ),
              new CARef(av) ); }
}

class RRef implements RExpr {
    Var x;
    RRef ( Var x ) { this.x = x; }
    public String toString() { return x.toString(); }
    public int eval(Iterator<String> in, IMap<Var,Integer> env) {
        return env.ref(x); }
    public RExpr uniquify(IMap<Var, Var> rho) {
        return new RRef( rho.ref(x) ); }
    public CProgram flatten() {
        return new CProgram( new CARef( x ) ); }
}

class RLet implements RExpr {
    Var x;
    RExpr xe;
    RExpr be;
    RLet ( Var x, RExpr xe, RExpr be ) {
        this.x = x; this.xe = xe; this.be = be; }
    public String toString() { return "(let " + x + " " + xe + " " + be + ")"; }
    public int eval(Iterator<String> in, IMap<Var,Integer> env) {
        int what_is_x = xe.eval(in, env);
        IMap<Var,Integer> env_with_x = env.set(x, what_is_x);
        return be.eval(in, env_with_x); }
    public RExpr uniquify(IMap<Var, Var> rho) {
        Var xp = new Var(x);
        return new RLet( xp, xe.uniquify(rho), be.uniquify(rho.set(x, xp)) ); }
    public CProgram flatten() {
        CProgram xep = xe.flatten();
        CProgram bep = be.flatten();
        return new CProgram
            ( xep.vs.merge(bep.vs).add(x),
              new Append<CStmt>
              ( new Snoc<CStmt>( xep.ss,
                                 new CSSet( x, new CEArg( xep.ret ) ) ),
                bep.ss ),
              bep.ret ); }
}

// C Programs

interface CArg {
}
class CAInt implements CArg {
    int n;
    CAInt( int n ) { this.n = n; }
    public String toString() { return new Integer(n).toString(); }
}
class CARef implements CArg {
    Var x;
    CARef( Var x ) { this.x = x; }
    public String toString() { return x.toString(); }
}

interface CExpr {
}
class CEArg implements CExpr {
    CArg a;
    CEArg( CArg a ) { this.a = a; }
    public String toString() { return a.toString(); }
}
class CERead implements CExpr {
    public String toString() { return "(read)"; }
}
class CENeg implements CExpr {
    CArg a;
    CENeg( CArg a ) { this.a = a; }
    public String toString() { return "(- " + a + ")"; }
}
class CEAdd implements CExpr {
    CArg l; CArg r;
    CEAdd( CArg l, CArg r ) { this.l = l; this.r = r; }
    public String toString() { return "(+ " + l + " " + r + ")"; }
}

interface CStmt {
}
class CSSet implements CStmt {
    Var v; CExpr e;
    CSSet( Var v, CExpr e ) {
        this.v = v;
        this.e = e; }
    public String toString() { return "(set! " + v + " " + e + ")"; }
}

class CProgram {
    ISet<Var> vs; List<CStmt> ss; CArg ret;
    CProgram( CArg ret ) {
        this( new ISet<Var>(), new Mt<CStmt>(), ret); }
    CProgram( ISet<Var> vs, List<CStmt> ss, CArg ret ) {
        this.vs = vs;
        this.ss = ss;
        this.ret = ret; }
    public String toString() {
        String ss_str =
            (ss.fold("", ((s, pre) -> pre + s.toString() + "\n       ")));
        return "(begin " + ss_str + ret + ")"; }
}

class C6 {
    // Test system
    static int tests_passed = 0;
    static void check ( String which, Supplier<Integer> te, int expected ) {
        try {
            int actual = te.get();
            if ( actual != expected ) {
                System.out.println
                    (which + ": got " + actual + ", expected " + expected);
                System.exit(1); }
            else {
                tests_passed++; } }
        catch (RuntimeException x) {
            System.out.println
                (which + ": threw exception [" + x + "], expected " + expected);
            System.exit(1); } }
    static void summary() {
        System.out.println(tests_passed + " test(s) passed!"); }

    // Evaluation functions
    static Iterator<String> stdin() {
        if (true) {
            String inv[] = { "42", "12" };
            return Arrays.asList(inv).listIterator();
        } else {
            return new Scanner(System.in);
        }
    }
    static int eval( RExpr e ) {
        IMap<Var,Integer> env = new IMap<Var,Integer>();
        return e.eval(stdin(), env); }

    static RExpr uniquify( RExpr e ) {
        IMap<Var,Var> rho = new IMap<Var,Var>();
        return e.uniquify(rho); }

    //static int eval( CProgram cp ) {
    //    return cp.eval(stdin()); }

    // Run all tests at once
    static void t ( int ans, RExpr e ) {
        Var.reset();
        System.out.println(e + " =>");
        check( "eval", () -> eval(e), ans );
        RExpr u = uniquify(e);
        System.out.println(u + " =>");
        check( "uniq", () -> eval(u), ans );
        CProgram f = u.flatten();
        System.out.println(f + " =>");
        // check( "flatten", () -> eval(f), ans );
        System.out.println(ans + "\n");
    }

    // Fast constructors
    static RExpr I(int n) { return new RInt(n); }
    static RExpr R(Var x) { return new RRef(x); }
    static RExpr N(RExpr e) { return new RNeg(e); }
    static RExpr A(RExpr l, RExpr r) { return new RAdd(l, r); }
    static RExpr Let(Var x, RExpr xe, RExpr be) { return new RLet(x, xe, be); }

    static RExpr Pow2(int n) {
        if ( n == 0 ) { return I(1); }
        else {
            RExpr sub = Pow2(n-1);
            return A(sub, sub); }
    }

    public static void main( String args[] ) {
        RExpr Rd = new RRead();
        Var x = new Var("x");
        Var y = new Var("y");

        t( 8, I(8) );
        t( 42, Rd );
        t( -8, (N(I(8))) );
        t( 2, (A(N(I(8)), I(10))) );
        t( 2, (Let(x, I(8), A(N(R(x)), I(10)))) );
        t( 18, Let(x, I(8), A( Let(x, I(10), R(x)), R(x) ) ) );
        t( 13, Let(x, I(7), Let( y, I(6), A( R(x), R(y) ) ) ) );
        t( 16, Pow2(4) );

        summary(); }
}
