import java.util.function.Function;
import javafx.util.Pair;

class Char {
    public char c;
    public Char(char c) { this.c = c; } }

interface Alphabet {
    public int size();
    public Char ith(int i); }

class EmptyAlphabet implements Alphabet {
    public EmptyAlphabet() {}
    public int size() { return 0; }
    public Char ith(int i) {
        throw new RuntimeException("Empty alphabet has no letters!"); } }

class AlphabetWithOneLetter implements Alphabet {
    Char f; Alphabet r;
    public AlphabetWithOneLetter(Char f, Alphabet r) {
        this.f = f; this.r = r; }
    public int size() { return 1 + r.size(); }
    public Char ith(int i) {
        if ( i == 0 ) { return this.f; }
        else { return this.r.ith(i-1); } } }

interface CString {
    public boolean emptyHuh();
    public Char firstChar();
    public CString after();
    public void print(); }

class Epsilon implements CString {
    public Epsilon() {}
    public boolean emptyHuh() { return true; }
    public Char firstChar() { throw new RuntimeException("No first char in epsilon!"); }
    public CString after() { throw new RuntimeException("No after in epsilon!"); }
    public void print() { System.out.println("ϵ"); } }

class StringWithOneChar implements CString {
    Char f; CString r;
    public StringWithOneChar(Char f, CString r) {
        this.f = f; this.r = r; }
    public Char firstChar() { return this.f; }
    public CString after() { return this.r; }
    public boolean emptyHuh() { return false; }
    public void print() {
        System.out.print(this.f.c);
        this.r.print(); } }


class DFA {
    // Q, Σ, q0, δ, F
    public String label;
    Function<Object, Boolean> Q;
    Alphabet Σ;
    Object q0;
    Function<Pair<Object, Char>, Object> δ;
    Function<Object, Boolean> F;

    public DFA( String label,
                Function<Object, Boolean> Q,
                Alphabet Σ,
                Object q0,
                Function<Pair<Object, Char>, Object> δ,
                Function<Object, Boolean> F ) {
        this.label = label; this.Q = Q; this.Σ = Σ; this.q0 = q0; this.δ = δ; this.F = F; }

    public boolean accepts( CString input ) {
        Object qi = this.q0;
        while ( ! input.emptyHuh() ) {
            qi = this.δ.apply( new Pair<Object,Char>(qi, input.firstChar()) );
            input = input.after();
        }
        return this.F.apply( qi ); } }

interface FFunction {
    public boolean member( Object st ); }

class Tests {
    static CString lexi( Alphabet a, int n ) {
        int sz = a.size();

        int t = n;
        int i = 0;
        while (true) {
            int lim = (int)Math.pow(sz, i);
            if ( t < lim ) { break; }
            else {
                t = t - lim;
                i++; } }

        System.out.println("i = " + i);

        CString r = new Epsilon();
        while ( i > 0 ) {
            int which = t % sz;
            r = new StringWithOneChar( a.ith( which ), r );
            t = t / sz;
            i--; }

        return r; }

    Char Zero = new Char('0');
    Char One = new Char('1');

    Alphabet Binary =
        new AlphabetWithOneLetter( Zero,
                                   new AlphabetWithOneLetter( One, new EmptyAlphabet() ) );

    Object State0 = new Char('0');
    Object State1 = new Char('1');
    Object State2 = new Char('2');

    class FFunction_for_DFA_Char implements FFunction {
        public boolean member( Object st ) { return st == State1; } }

    DFA DFA_EmptySet() {
        return
            new DFA( "EmptySet",
                     (qi -> qi == State0),
                     Binary,
                     State0,
                     (p -> {
                         Object qi = p.getKey();
                         Char c = p.getValue();
                         return State0; } ),
                     (qi -> false) ); }

    DFA DFA_Epsilon() {
        return
            new DFA( "Epsilon",
                     (qi -> qi == State0 || qi == State1 ),
                     Binary,
                     State0,
                     (p -> {
                         Object qi = p.getKey();
                         Char c = p.getValue();
                         return State1; } ),
                     (qi -> qi == State0 ) ); }

    DFA DFA_Char(Char given_c) {
        return
            new DFA( "Char(" + given_c.c + ")",
                     (qi -> qi == State0 || qi == State1 || qi == State2 ),
                     Binary,
                     State0,
                     (p -> {
                         Object qi = p.getKey();
                         Char c = p.getValue();
                         if ( qi == State0 && c == given_c ) {
                             return State1; }
                         else {
                             return State2; } } ),
                     (qi -> qi == State1 ) ); }

    void test_dfa(DFA d, CString s) {
        System.out.println("DFA " + d.label + " on String...");
        s.print();
        System.out.println("...returns: " + d.accepts(s)); }

    public void runTests() {
        CString ZOO = new StringWithOneChar( Zero, new StringWithOneChar( One, new StringWithOneChar( One, new Epsilon() ) ) );
        CString Z = new StringWithOneChar( Zero, new Epsilon() );

        ZOO.print();

        lexi( Binary, 100 ).print();

        test_dfa( DFA_EmptySet(), ZOO );
        test_dfa( DFA_Epsilon(), ZOO );
        test_dfa( DFA_Char(Zero), ZOO );
        test_dfa( DFA_EmptySet(), Z );
        test_dfa( DFA_Epsilon(), Z );
        test_dfa( DFA_Char(Zero), Z );
        test_dfa( DFA_EmptySet(), new Epsilon() );
        test_dfa( DFA_Epsilon(), new Epsilon() );
        test_dfa( DFA_Char(Zero), new Epsilon() );

    } }

class C8 {
    public static void main(String[] args) {
        new Tests().runTests(); } }
