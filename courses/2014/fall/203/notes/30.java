class C30 {
    public static void main( String[] args ) {
    }
}

// Ch 15 - Design Patterns
// - BIG topic
// - Examples: Iterators, Generics

// Template Patern
// - super class calls abstract sub-class methods

// Factory Pattern
// - do not depend on class, but call a method of a factory to get a class (empty and add)
// - Allows you more easily to change implementations in future

// Builder Pattern
// - hides the object AND the collection of them

// Prototype Pattern
// - like a factory where an object of type T can produce more Ts (like all your finite set code)

// Flyweight Pattern
// - ensure that identical objects are created once
// - requires immutable objects
// - table indexed by constructor arguments

// Singleton Pattern
// - flyweight pattern applied to an object with no arguments in constructor

// State Pattern
// - all methods of X implemented by A or B and X has a field of what is doing the work
// - switches from A to B (or back) at some point
// - typically assumes mutability

// Procedure Pattern
// - Manually implemented Closures
// - Action and Filter from Iteration examples
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
