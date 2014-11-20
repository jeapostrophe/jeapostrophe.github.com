class C31 {
    public static void main( String[] args ) {
        AE a1 = new Mult(new Add(new Num(1), new Num(2)),
                         new Mult(new Num(4), new Num(3)));
        System.out.println("a1 is " + a1.value() + " should be " + 36);
        AEVisitor av1 = new AEV_Compute();
        a1.host(av1);
        System.out.println("a1 is " + av1.value() + " should be " + 36);
        System.out.println("a1 is " + a1.friendlyHost(new AEFV_Compute()) + " should be " + 36);
        AEVisitor av2 = new AEV_Max();
        a1.host(av2);
        System.out.println("a1 is " + av2.value() + " should be " + 4);
    }
}

// Composite Pattern
// - Trees of elements of same super-type

interface AEVisitor {
    public void Num( int n );
    public void Add_Start();
    public void Add_End();
    public void Mult_Start();
    public void Mult_End();
    public int value();
}

interface List {
    public List push( int x );
    public int first();
    public List rest();
}
class Empty implements List {
    Empty() {}
    public List push( int x ) {
        return new Cons( x, this );
    }
    public int first() { throw new RuntimeException("No first here"); }
    public List rest() { throw new RuntimeException("No rest here"); }
}
class Cons implements List {
    int f;
    List r;
    Cons(int f, List r) { this.f = f; this.r = r; }
    public List push( int x ) {
        return new Cons( x, this );
    }
    public int first() { return this.f; };
    public List rest() { return this.r; };
}

class AEV_Compute implements AEVisitor {
    List vals;
    public AEV_Compute() {
        this.vals = new Empty();
    }
    public int pop() {
        int x = this.vals.first();
        this.vals = this.vals.rest();
        return x;
    }
    public void push(int x) {
        this.vals = this.vals.push(x);
    }
    public int value() {
        return this.pop();
    }    
    
    public void Num( int n ) {
        this.push(n);
    }
    public void Add_Start() {
    }
    public void Add_End()  {
        int r = this.pop();
        int l = this.pop();
        this.push(l + r);
    }
    public void Mult_Start()  {
    }
    public void Mult_End()  {
        int r = this.pop();
        int l = this.pop();
        this.push(l * r);
    }
}

class AEV_Max implements AEVisitor {
    int max;
    public AEV_Max() {
        this.max = 0;
    }
    public int value() {
        return this.max;
    }    
    
    public void Num( int n ) {
        if ( n > max ) { max = n; }
    }
    public void Add_Start() {}
    public void Add_End()  {}
    public void Mult_Start()  {}
    public void Mult_End()  {}
}

// Not just a Friendly Visitor but a FOLDING visitor
interface AEFVisitor<X> {
    public X Num( int n );
    public X Add( X l, X r );
    public X Mult( X l, X r );
}

class AEFV_Compute implements AEFVisitor<Integer> {
    public Integer Num( int n ) { return n; }
    public Integer Add( Integer l, Integer r ) { return l + r; }
    public Integer Mult( Integer l, Integer r ) { return l * r; } 
}

interface AE { // ArithmeticExpression
    // Interpreter pattern - structural induction
    public int value();
    public void host( AEVisitor av );
    public Object friendlyHost( AEFVisitor afv );
}

class Num implements AE {
    int n;
    Num(int n) { this.n = n; }
    public int value() { return this.n; }
    public void host( AEVisitor av ) { av.Num(this.n); }
    public Object friendlyHost( AEFVisitor afv ) { return afv.Num(n); }
}
class Add implements AE {
    AE l;
    AE r;
    Add(AE l, AE r) { this.l = l; this.r = r; };
    public int value() { return this.l.value() + this.r.value(); }
    public void host( AEVisitor av ) {
        av.Add_Start();
        this.l.host(av);
        this.r.host(av);
        av.Add_End();
    }
    public Object friendlyHost( AEFVisitor afv ) {
        return afv.Add(this.l.friendlyHost(afv), this.r.friendlyHost(afv));
    }
}
class Mult implements AE {
    AE l;
    AE r;
    Mult(AE l, AE r) { this.l = l; this.r = r; };
    public int value() { return this.l.value() * this.r.value(); }
    public void host( AEVisitor av ) {
        av.Mult_Start();
        this.l.host(av);
        this.r.host(av);
        av.Mult_End();
    }
    public Object friendlyHost( AEFVisitor afv ) {
        return afv.Mult(this.l.friendlyHost(afv), this.r.friendlyHost(afv));
    }
}

// Composite Traverseal: Interpreter Pattern
// - A manually implemented fold of a composite

// Composite Traverseal: Visitor Pattern
// - A single fold but with a Strategy Pattern with N methods for the N types

// Interposition: Adapter Pattern
// - Wraps another object with the same interfaces and gives different behavior

// Cache
// o.m( g ) = n // 2 seconds
// A = new a ( o )
// (A).m(g) = n // 2 seconds
// (A).m(g) = n // 0 seconds

// Negativity
// o.m( g ) = n
// A = new a ( o )
// (A).m(g) = -n

// new BlackAndWhiteImage( img ).drawOntoScreen( scr )

// Interposition: Proxy Pattern
// - Wraps another object with the same interfaces and gives same behavior, but does something "more"

// Interposition: Decorator Pattern
// - Wraps another object with the same interfaces and gives same behavior, but adds more too

// Observer Pattern
// - An object stores listeners
// - When the object changes it notifies the listeners
// - Pull: Listeners then ask for info
// - Push: Notification contains all info

interface Subject {
    public void addListener(Listener l);
    // push
    public void broadcastNotificiation(Notification n);
    // pull
    public void broadcast();
    public Notification getNotification();
}
interface Notification {
}
interface Listener {
    // push
    public void receiveNotification(Notification n);
    // pull
    public void receive(Subject s);
}

// Mediator Pattern
// - Abstracts the observers and can implement a new protocol, such as "first responder"

// Whiteboard Pattern
// - Abstracts the identities of observation subjects (like a mega mediator)
// - Asynchronous alternative to function calls

// MORE
