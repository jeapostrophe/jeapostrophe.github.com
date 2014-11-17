class C30 {
    public static void main( String[] args ) {
    }
}

// Ch 15 - Design Patterns
// - BIG topic
// - Examples: Iterators, Generics

interface Sequence<X> {
    public boolean nextableHuh();
    public Sequence<X> next();
    public X here();
}

// Sequence<Pair<X,Y>>

interface TwoSequence<X,Y> {
    public boolean nextableHuh();
    public TwoSequence<X,Y> next();
    public X x();
    public Y y();
}

// interface ManySequence<X_0 ... X_n> {
//     public boolean nextableHuh();
//     public ManySequence<X_0 ... X_n> next();
//     public X_0 ... X_n here();
// }

// Template Patern
// - super class calls abstract sub-class methods

abstract class Parent<X,Y> {
    public X method() {
        if ( this.switcher() ) {
            return this.combine( this.left(), this.right() );
        } else {
            return this.left();
        }
    }
    abstract public X combine( X x, Y y );
    abstract public X left();
    abstract public Y right();
    abstract public boolean switcher();
}

class Child extends Parent<Integer,Integer> {
    Child() {}
    public Integer combine( Integer x, Integer y ) { return x + y; }
    public Integer left() { return 42; }
    public Integer right() { return 47; }
    public boolean switcher() { return true; }
}

// Factory Pattern
// - do not depend on class, but call a method of a factory to get a class (empty and add)
// - Allows you more easily to change implementations in future

interface Posn {
}

class RectPosn implements Posn {
    RectPosn(int x, int y) {}
}

class PolarPosn implements Posn {
    PolarPosn(double x, double y) {}
}

class PosnFactory {
    PosnFactory() {}
    public Posn newPosnFromRect(int x, int y) {
        return newPosnFromPolar( x / 90, y * 3.14 );
    }
    public Posn newPosnFromPolar(double x, double y) {
        return new PolarPosn(x, y);
    }
}

class FactoryUse {
    public void m() {
        PosnFactory pf = new PosnFactory();
        Posn p5 = pf.newPosnFromRect(5, 5);
        Posn pdiddy = pf.newPosnFromRect(3, 5);
    }
}

// Builder Pattern
// - hides the object AND the collection of them

// if the factory makes a type X, then the builder makes Collection<X>
// and hides the collection type

abstract class Builder<Collection, X> {
    abstract public Collection empty();
    abstract public Collection addOne(Collection collect, X thing);
}

// Prototype Pattern
// - like a factory where an object of type T can produce more Ts (like all your finite set code)

interface Listy<X> {
    // new Cons( x, this )
    public Listy putAtFront( X x );
}

// Flyweight Pattern
// - ensure that identical objects are created once
// - requires immutable objects
// - table indexed by constructor arguments

class Fish {
    public Fish( int color ) {}
}

class FishFactory {
    Fish theFish[];
    public FishFactory() {
        this.theFish = new Fish[10];
    }
    
    public Fish newFish(int color) {
        if ( theFish[color] == null ) {
            theFish[color] = new Fish(color);
        }
        return theFish[color];
    }
}

class FishUse {
    public void m() {
        FishFactory ff = new FishFactory();
        Fish fHarry = ff.newFish(5);
        Fish fTom = ff.newFish(5);
    }
}

// Singleton Pattern
// - flyweight pattern applied to an object with no arguments in constructor

class NoStuff {
    static NoStuff theNoStuff;
    static NoStuff getTheNoStuff() {
        if ( theNoStuff == null ) {
            theNoStuff = new NoStuff();
        }
        return theNoStuff;
    }
    
    NoStuff() {}
}

class StuffUse {
    public void m() {
        NoStuff.getTheNoStuff();
        NoStuff.getTheNoStuff();
    }
}

// State Pattern
// - all methods of X implemented by A or B and X has a field of what is doing the work
// - switches from A to B (or back) at some point
// - typically assumes mutability

interface ASet {
}

class EfficientForSparseSets implements ASet {
}
class EfficientForDenseSets implements ASet {
}
class GeneralSets implements ASet {
    boolean amSparse;
    ASet underlyingSet;

    /*
    public ASet add(int x) {
        if ( amSparse && this.decideToSwitch() ) {
            return (new GeneralSets( false, EfficientForDenseSets.fromList( underlyingSet.toList ) )).add( x );
        } else {
            return new GeneralSets( true, underlyingSet.add(x) );
        }
    }
    */
}

// Procedure Pattern
// - Manually implemented Closures
// - Action and Filter from Iteration examples

interface Strategy<X,Y> {
    public Y convert( X x );
}
interface Command<X> {
    public void doIt( X x );
}
interface Seq<X, Y> {
    public Seq<Y, X> convert( Stratgey<X,Y> a );
    public void doIt( Command<X> a );
}

// - When there are constraints, called Strategy Pattern
// - When there are none (void -> void), called Command Pattern

// Composite Pattern
// - Trees of elements of same super-type

// Composite Traverseal: Interpreter Pattern
// - A manually implemented fold of a composite

// Composite Traverseal: Visitor Pattern
// - A single fold but with a Strategy Pattern with N methods for the N types

// Interposition: Adapter Pattern
// - Wraps another object with the same interfaces and gives different behavior

// Interposition: Proxy Pattern
// - Wraps another object with the same interfaces and gives same behavior, but does something "more"

// Interposition: Decorator Pattern
// - Wraps another object with the same interfaces and gives same behavior, but adds more too

// Observer Pattern
// - An object stores listeners
// - When the object changes it notifies the listeners
// - Pull: Listeners then ask for info
// - Push: Notification contains all info

// Mediator Pattern
// - Abstracts the observers and can implement a new protocol, such as "first responder"

// Whiteboard Pattern
// - Abstracts the identities of observation subjects (like a mega mediator)
// - Asynchronous alternative to function calls

// MORE
