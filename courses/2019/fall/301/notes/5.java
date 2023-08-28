// Sexpr
// se = atom | (cons se se) | empty

interface Sexpr {
  public String show(); }

class SE_Str implements Sexpr {
  public String s;
  public String show() { return s; }
  public SE_Str(String s) { this.s = s; } }

class SE_Num implements Sexpr {
  public int n;
  public String show() { return "" + n; }
  public SE_Num(int n) { this.n = n; } }

class SE_MT implements Sexpr {
  public String show() { return "⊥"; }
  public SE_MT() { } }

class SE_Cons implements Sexpr {
  public Sexpr lhs, rhs;
  public String show() { return "(" + this.lhs.show() + " " + this.rhs.show() + ")"; }
  public SE_Cons(Sexpr lhs, Sexpr rhs) {
    this.lhs = lhs;
    this.rhs = rhs; } }

// J1
// e = v | (if e e e) | (e e ...)
// v = number | bool | prim

interface JExpr {
  public Boolean isValue();
  public String show();
  public JExpr interp(); }

class JNull implements JExpr {
  public String show() { return "⊥"; }
  public JNull() { }
  public Boolean isValue() { return true; }
  public JExpr interp() { return this; } }

class JCons implements JExpr {
  public JExpr lhs, rhs;
  public String show() { return "(" + this.lhs.show() + " " + this.rhs.show() + ")"; }
  public JCons(JExpr lhs, JExpr rhs) {
    this.lhs = lhs;
    this.rhs = rhs; }
  public Boolean isValue() { return false; }
  public JExpr interp() {
    return new JCons(this.lhs.interp(), this.rhs.interp()); }}

class JPrim implements JExpr {
  public String p;
  public JPrim(String p) {
    this.p = p; }
  public Boolean isValue() { return true; }
  public String show() {
    return "" + this.p; }
  public JExpr interp() {
    return this; } }

class JNumber implements JExpr {
  public int n;
  public JNumber(int n) {
    this.n = n; }
  public Boolean isValue() { return true; }
  public String show() {
    return "" + this.n; }
  public JExpr interp() {
    return this; } }

class JBool implements JExpr {
  public Boolean b;
  public JBool(Boolean b) {
    this.b = b; }
  public Boolean isValue() { return true; }
  public String show() {
    return "" + this.b; }
  public JExpr interp() {
    return this; } }

class JIf implements JExpr {
  public JExpr cond, tbr, fbr;
  public JIf(JExpr cond, JExpr tbr, JExpr fbr) {
    this.cond = cond;
    this.tbr = tbr;
    this.fbr = fbr; }
  public Boolean isValue() { return false; }
  public String show() {
    return "(if " + this.cond.show() + " " + this.tbr.show() + " " + this.fbr.show() + ")"; }
  public JExpr interp() {
    JExpr condv = this.cond.interp();
    if ( condv instanceof JBool
         && ((JBool)condv).b == false ) {
      return this.fbr.interp(); }
    else {
      return this.tbr.interp(); } } }

class JApp implements JExpr {
  public JExpr fun, args;
  public JApp(JExpr fun, JExpr args) {
    this.fun = fun;
    this.args = args; }
  public Boolean isValue() { return false; }
  public String show() {
    return "(@ " + this.fun.show() + " " + this.args.show() + ")"; }
  public JExpr interp() {
    JExpr which_fun = this.fun.interp();
    JExpr arg_vals = this.args.interp();

    String p = ((JPrim)which_fun).p;
    int lhs = ((JNumber)((JCons)arg_vals).lhs).n;
    int rhs = ((JNumber)((JCons)((JCons)arg_vals).rhs).lhs).n;
    if ( p.equals("+") ) { return new JNumber(lhs + rhs); }
    if ( p.equals("*") ) { return new JNumber(lhs * rhs); }
    if ( p.equals("/") ) { return new JNumber(lhs / rhs); }
    if ( p.equals("-") ) { return new JNumber(lhs - rhs); }
    if ( p.equals("<") ) { return new JBool(lhs < rhs); }
    if ( p.equals("<=") ) { return new JBool(lhs <= rhs); }
    if ( p.equals("==") ) { return new JBool(lhs == rhs); }
    if ( p.equals(">") ) { return new JBool(lhs > rhs); }
    if ( p.equals(">=") ) { return new JBool(lhs >= rhs); }
    if ( p.equals("!=") ) { return new JBool(lhs != rhs); }

    return new JNumber(666); }}

class C5 {
  static JExpr JN(int n) {
    return new JNumber(n); }
  static JExpr JA(JExpr lhs, JExpr rhs) {
    return new JApp(new JPrim("+"), new JCons(lhs, new JCons(rhs, new JNull()))); }
  static JExpr JM(JExpr lhs, JExpr rhs) {
    return new JApp(new JPrim("*"), new JCons(lhs, new JCons(rhs, new JNull()))); }

  static Sexpr SApp(String op, Sexpr lhs, Sexpr rhs) {
    return new SE_Cons(new SE_Str(op),
                       new SE_Cons(lhs,
                                   new SE_Cons(rhs,
                                               new SE_MT()))); }

  static Sexpr SN(int n) {
    return new SE_Num(n); }
  static Sexpr SA(Sexpr lhs, Sexpr rhs) {
    return SApp("+", lhs, rhs); }
  static Sexpr SM(Sexpr lhs, Sexpr rhs) {
    return SApp("*", lhs, rhs); }
  static Sexpr SIf(Sexpr cond, Sexpr lhs, Sexpr rhs) {
    return new SE_Cons(new SE_Str("if"),
                       new SE_Cons(cond,
                                   new SE_Cons(lhs,
                                               new SE_Cons(rhs,
                                                           new SE_MT())))); }

  static JExpr desugar(Sexpr se) {
    // desugar 'n = n
    if ( se instanceof SE_Num ) {
      return JN(((SE_Num) se).n); }
    // desugar ('+) = 0
    if ( se instanceof SE_Cons
         && ((SE_Cons)se).lhs instanceof SE_Str
         && ((SE_Str)((SE_Cons)se).lhs).s.equals("+")
         && ((SE_Cons)se).rhs instanceof SE_MT ) {
      return JN(0); }
    // desugar ('+ lhs rhs ...) = (+ desugar(lhs) desugar(('+ rhs ...)))
    if ( se instanceof SE_Cons
         && ((SE_Cons)se).lhs instanceof SE_Str
         && ((SE_Str)((SE_Cons)se).lhs).s.equals("+")
         && ((SE_Cons)se).rhs instanceof SE_Cons ) {
      return JA( desugar(((SE_Cons)((SE_Cons)se).rhs).lhs),
                 desugar(new SE_Cons(((SE_Cons)se).lhs, ((SE_Cons)((SE_Cons)se).rhs).rhs)) ); }
    // desugar ('*) = 1
    if ( se instanceof SE_Cons
         && ((SE_Cons)se).lhs instanceof SE_Str
         && ((SE_Str)((SE_Cons)se).lhs).s.equals("*")
         && ((SE_Cons)se).rhs instanceof SE_MT ) {
      return JN(1); }
    // desugar ('* lhs rhs) = (* desugar(lhs) desugar(rhs))
    if ( se instanceof SE_Cons
         && ((SE_Cons)se).lhs instanceof SE_Str
         && ((SE_Str)((SE_Cons)se).lhs).s.equals("*")
         && ((SE_Cons)se).rhs instanceof SE_Cons ) {
      return JM( desugar(((SE_Cons)((SE_Cons)se).rhs).lhs),
                 desugar(new SE_Cons(((SE_Cons)se).lhs, ((SE_Cons)((SE_Cons)se).rhs).rhs)) ); }

    if ( se instanceof SE_Cons
         && ((SE_Cons)se).lhs instanceof SE_Str
         && ((SE_Str)((SE_Cons)se).lhs).s.equals("-")
         && ((SE_Cons)se).rhs instanceof SE_Cons
         && ((SE_Cons)((SE_Cons)se).rhs).rhs instanceof SE_MT) {
      return JM( JN(-1), desugar(((SE_Cons)((SE_Cons)se).rhs).lhs) ); }

    if ( se instanceof SE_Cons
         && ((SE_Cons)se).lhs instanceof SE_Str
         && ((SE_Cons)se).rhs instanceof SE_Cons
         && ((SE_Cons)((SE_Cons)se).rhs).rhs instanceof SE_Cons
         && ((SE_Cons)((SE_Cons)((SE_Cons)se).rhs).rhs).rhs instanceof SE_MT) {
      return new JApp( new JPrim(((SE_Str)((SE_Cons)se).lhs).s),
                       new JCons(desugar(((SE_Cons)((SE_Cons)se).rhs).lhs),
                                 new JCons(desugar(((SE_Cons)((SE_Cons)((SE_Cons)se).rhs).rhs).lhs), new JNull()))); }

    if ( se instanceof SE_Cons
         && ((SE_Cons)se).lhs instanceof SE_Str
         && ((SE_Str)((SE_Cons)se).lhs).s.equals("if")
         && ((SE_Cons)se).rhs instanceof SE_Cons
         && ((SE_Cons)((SE_Cons)se).rhs).rhs instanceof SE_Cons
         && ((SE_Cons)((SE_Cons)((SE_Cons)se).rhs).rhs).rhs instanceof SE_Cons
         && ((SE_Cons)((SE_Cons)((SE_Cons)((SE_Cons)se).rhs).rhs).rhs).rhs instanceof SE_MT ) {
      return new JIf( desugar(((SE_Cons)((SE_Cons)se).rhs).lhs),
                      desugar(((SE_Cons)((SE_Cons)((SE_Cons)se).rhs).rhs).lhs),
                      desugar(((SE_Cons)((SE_Cons)((SE_Cons)((SE_Cons)se).rhs).rhs).rhs).lhs) ); }
    // XXX
    return JN(666); }

  static int tests_passed = 0;
  static void test(Sexpr se, JExpr expected) {
    JExpr e = desugar(se);
    // System.out.println(se.show() + " desugars to " + e.show());
    JExpr actual = e.interp();
    if ( ! actual.show().equals(expected.show()) ) {
      System.out.println(e.show() + " = " + actual.show() + " but should = " + expected.show()); }
    else { tests_passed++; } }

  static void test_num(Sexpr se, int n) {
    test(se, JN(n)); }

  public static void main(String args[]) {
    test_num(SN(42), 42);
    test_num(SN(7), 7);
    test_num(SA(SN(42),SN(0)), 42);
    test_num(SM(SN(42),SN(0)), 0);
    test_num(SA(SM(SN(42),SN(0)),SN(0)), 0);
    test_num(SA(SM(SN(42),SN(0)),SA(SM(SN(42),SN(0)),SN(0))), 0);

    test_num(SA(SN(42),SN(1)), 43);
    test_num(SM(SN(42),SN(1)), 42);
    test_num(SA(SM(SN(42),SN(1)),SN(1)), 43);
    test_num(SA(SM(SN(42),SN(1)),SA(SM(SN(42),SN(1)),SN(1))), 85);

    test_num(new SE_Cons(new SE_Str("+"), new SE_MT()), 0);
    test_num(new SE_Cons(new SE_Str("*"), new SE_MT()), 1);
    Sexpr three_things =
      new SE_Cons(new SE_Num(1),
                  new SE_Cons(new SE_Num(2),
                              new SE_Cons(new SE_Num(4),
                                          new SE_MT())));
    test_num(new SE_Cons(new SE_Str("+"), three_things), 7);
    test_num(new SE_Cons(new SE_Str("*"), three_things), 8);

    test_num(new SE_Cons(new SE_Str("-"), new SE_Cons(new SE_Num(4), new SE_MT())), -4);
    test_num(new SE_Cons(new SE_Str("-"), new SE_Cons(new SE_Num(4), new SE_Cons(new SE_Num(2), new SE_MT()))), 2);

    test(new SE_Cons(new SE_Str("=="), new SE_Cons(new SE_Num(4), new SE_Cons(new SE_Num(2), new SE_MT()))), new JBool(false));
    test(new SE_Cons(new SE_Str("=="), new SE_Cons(new SE_Num(4), new SE_Cons(new SE_Num(4), new SE_MT()))), new JBool(true));

    test(SApp("==", new SE_Num(4), new SE_Num(4)), new JBool(true));
    test(SIf(SApp("==", new SE_Num(4), new SE_Num(4)),
             new SE_Num(5),
             new SE_Num(6)), JN(5));
    test(SIf(SApp("==", new SE_Num(4), new SE_Num(2)),
             new SE_Num(5),
             new SE_Num(6)), JN(6));

    System.out.println(tests_passed + " tests passed!");
    return; } }
