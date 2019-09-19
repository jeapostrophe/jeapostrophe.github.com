import java.util.function.Function;
import javafx.util.Pair;

class Char {
  public char c;
  public Char(char c) {
    this.c = c; } }

interface Zing {
  public Boolean isEmpty();
  public Char first();
  public Zing rest(); }

/*
class Zing {
 public:
 virtual bool isEmpty() = 0; }
*/

// Zing have three methods: a, b, c

class ZingThatsEmpty implements Zing {
  public ZingThatsEmpty() {}
  public Boolean isEmpty() { return true; };
  public Char first() { throw new RuntimeException("none!"); }
  public Zing rest() { throw new RuntimeException("none!"); } }

class ZingWithAChar implements Zing {
  public Char c;
  public Zing more;
  public ZingWithAChar(Char c, Zing more) {
    this.c = c;
    this.more = more; }
  public Boolean isEmpty() { return false; };
  public Char first() { return this.c; }
  public Zing rest() { return this.more; } }

interface Alphabet {}

class AlphabetThatsEmpty implements Alphabet {
  public AlphabetThatsEmpty() {} }

class AlphabetWithAChar implements Alphabet {
  public Char c;
  public Alphabet more;
  public AlphabetWithAChar(Char c, Alphabet more) {
    this.c = c;
    this.more = more; } }

// class Function<X, Y> {
//   public Y apply(X x) { ....; } }

class DFA<State> {
  public String label;
  public Function<State,Boolean> Q;
  public Alphabet Σ;
  public State q0;
  public Function<Pair<State,Char>, State> δ;
  // public Function<Pair<State,CharOrEpsilon>, List<State>> δ';
  public Function<State,Boolean> F;

  public DFA( String label,
              Function<State,Boolean> Q,
              Alphabet Σ,
              State q0,
              Function<Pair<State,Char>, State> δ,
              Function<State,Boolean> F ) {
    this.label = label;
    this.Q = Q;
    this.Σ = Σ;
    this.q0 = q0;
    this.δ = δ;
    this.F = F; }

  public Boolean accepts( Zing w ) {
    State qi = this.q0;
    while ( ! w.isEmpty() ) {
      qi = δ.apply(new Pair<State,Char>(qi, w.first()));
      w = w.rest(); }
    return F.apply(qi); } }

class Tests {
  public Tests() {};

  int tests_that_passed = 0;
  void test_dfa(DFA d, Zing w, Boolean expected) {
    Boolean actual = d.accepts(w);
    if ( actual != expected ) {
      System.out.println("DFA d(" + d.label + ") returns " + actual + " on " + w + " but should return " + expected); }
    else { tests_that_passed++; } }

  DFA DFA_Union(DFA<Object> A, DFA<Object> B) {
    return
      new DFA<Pair>("union of" + A.label + " & " + B.label,
                    ( qp -> (A.Q.apply( qp.getKey() )
                             && B.Q.apply( qp.getValue() )) ),
                    // [&](Pair<State,Char> qp) {
                    //  .... }
                    A.Σ,
                    new Pair( A.q0, B.q0 ),
                    ( p -> {
                      Pair qp = p.getKey();
                      Char c = p.getValue();
                      return new Pair( A.δ.apply( new Pair(qp.getKey(), c) ),
                                       B.δ.apply( new Pair(qp.getValue(), c) ) ); } ),
                    ( qp -> A.F.apply( qp.getKey() )
                      || B.F.apply( qp.getValue() ) )
                    ); }

  public void runTests() {
    Char zero = new Char('0');
    Char one = new Char('1');
    Alphabet binary = new AlphabetWithAChar( zero, new AlphabetWithAChar( one, new AlphabetThatsEmpty()));

    Zing ϵ = new ZingThatsEmpty();
    Zing ZOO = new ZingWithAChar(zero, new ZingWithAChar(one, new ZingWithAChar( one, ϵ ) ) );
    Zing OO = new ZingWithAChar(one, new ZingWithAChar( one, ϵ ) );
    Zing OOZ = new ZingWithAChar(one, new ZingWithAChar( one, new ZingWithAChar(zero, ϵ) ) );

    // Even length:
    Char A = new Char('A');
    Char B = new Char('B');
    DFA even_lengthed =
      new DFA<Char>("even_lengthed",
                    // Q = A, B
                    (qi -> (qi == A) || (qi == B)),
                    binary,
                    // q0 = A
                    A,
                    // δ(A, 0) = B
                    // δ(A, 1) = B
                    // δ(B, 0) = A
                    // δ(B, 1) = A
                    (p -> {
                      Char qi = p.getKey();
                      Char c = p.getValue();
                      if ( qi == A ) { return B; }
                      else { return A; }
                    }),
                    // F = A
                    (qi -> (qi == A)));

    test_dfa( even_lengthed, ϵ, true );
    test_dfa( even_lengthed, ZOO, false );
    test_dfa( even_lengthed, OO, true );
    test_dfa( even_lengthed, OOZ, false );

    DFA is_even =
      new DFA<Char>("is_even",
                    // Q = A, B
                    (qi -> (qi == A) || (qi == B)),
                    binary,
                    // q0 = A
                    A,
                    (p -> {
                      Char qi = p.getKey();
                      Char c = p.getValue();
                      if ( c == one ) { return B; }
                      else { return A; }
                    }),
                    // F = A
                    (qi -> (qi == A)));
    test_dfa( is_even, ϵ, true );
    test_dfa( is_even, ZOO, false );
    test_dfa( is_even, OO, false );
    test_dfa( is_even, OOZ, true );

    DFA is_even_len_or_num =
       DFA_Union( is_even, even_lengthed );

    test_dfa( is_even_len_or_num, ϵ, true );
    test_dfa( is_even_len_or_num, ZOO, false );
    test_dfa( is_even_len_or_num, OO, true );
    test_dfa( is_even_len_or_num, OOZ, true );

    System.out.println(tests_that_passed + " tests passed!");
  } }

class C5 {
  public static void main(String args[]) {
    new Tests().runTests(); } }
