// A InventoryRecord is a
//  new InventoryRecord ( name , price )
// where
//  name is a String
//  price is a double
class InventoryRecord {
    public String name;
    public double price;

    public InventoryRecord ( String name0, double price0 ) {
	name = name0;
	price = price0;
    }

    // toString : InventoryRecord -> String
    public String toString () {
	return String.format("%s ($%f)", this.name, this.price );
    }


    // isButzHuh : InventoryRecord -> boolean
    public boolean isButzHuh ( ) {
	// this.name ... this.price ...
	return this.name.equals("Butz Action Figure");
    }

    // isExpensiveHuh : InventoryRecord -> boolean
    public boolean isExpensiveHuh () {
	// this.name ... this.price ...
	return this.price >= 1000.00 ;
    }

    // tax : InventoryRecord double -> InventoryRecord
    public InventoryRecord tax ( double rate ) {
	// this.name : String
	// this.price : double
	return new InventoryRecord ( this.name , this.price * (1.0+rate) ) ;
    }
}

// A Inventory is either
//  - EmptyInventory
//  - OneMoreItem
interface Inventory {
    // totalPrice : Inventory -> double
    public double totalPrice ( );
    // containsButzHuh : Inventory -> boolean
    public boolean containsButzHuh ( ) ;
    // priceOf : Inventory String -> double
    public double priceOf ( String name ) ;
    // expensive : Inventory -> Inventory[expensive]
    public Inventory expensive ( );
    // SmootHawley : Inventory -> Inventory
    public Inventory SmootHawley () ;
}

// A EmptyInventory is a..
//  new EmptyInventory ( )
// where
class EmptyInventory implements Inventory {
    
    public EmptyInventory ( ) {
    }

    // toString : EmptyInventory -> String
    public String toString () {
	return "!";
    }

    // totalPrice : Inventory -> double
    public double totalPrice ( ) {
	return 0.0;
    }

    // containsButzHuh : Inventory -> boolean
    public boolean containsButzHuh ( ) {
	return false ;
    }

    // priceOf : Inventory String -> double
    public double priceOf ( String name ) {
	// ... ... 
	return -1.0 ; // pick something that always makes sense
	// throw new Error("We don't have that, ma'am"); // throw like a monkey
	// return notTherePrice; // let the caller decide
    }

    // expensive : Inventory -> Inventory[expensive]
    public Inventory expensive ( ) {
	// ... ...
	return new EmptyInventory() ;
    }

    // SmootHawley : Inventory -> Inventory
    public Inventory SmootHawley ( ) {
	// ... ...
	return new EmptyInventory() ;
    }

}

// A OneMoreItem is a...
//  new OneMoreItem ( first, rest )
// where
//  first is a InventoryRecord
//  rest is a Inventory
class OneMoreItem implements Inventory {
    public InventoryRecord first;
    public Inventory rest;

    public OneMoreItem ( InventoryRecord first0, Inventory rest0 ) {
	first = first0;
	rest = rest0;
    }

    // toString : OneMoreItem -> String
    public String toString () {
	return String.format("%s:%s", this.first, this.rest );
    }

    // totalPrice : Inventory -> double
    public double totalPrice ( ) {
	// this.first ... this.rest.totalPrice() ... this.rest
	// (this.first).name (this.first).price ... this.rest.totalPrice() ... this.rest
	/*
	  new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new EmptyInventory() ).totalPrice(),
	  500.00);

	  this.first == new InventoryRecord(this.first.name, this.first.price)
	  new OneMoreItem( this.first, this.rest ).totalPrice(),
	  this.rest.totalPrice() == 0.0
	  this.first.price + this.rest.totalPrice() );
	*/
	return this.first.price + this.rest.totalPrice() ; 
    }

    // containsButzHuh : Inventory -> boolean
    public boolean containsButzHuh ( ) {
	// this.first ... this.rest ... this.rest.containsButzHuh() ...
	// either this.first is Butz or this.rest.containsButzHuh()
	// this.first.name.equals("Butz Action Figure") || this.rest.containsButzHuh()
	// this.first.isButzHuh() || this.rest.containsButzHuh()
	return this.first.isButzHuh() || this.rest.containsButzHuh() ;
    }

    // priceOf : Inventory String -> double
    public double priceOf ( String someName ) {
	// this.first ... this.rest ... this.rest.priceOf(name) ...
	// ... this.first.name ... this.first.price ...
	if ( ((this.first).name).equals(someName) ) {
	    return this.first.price; 
	} else {
	    return this.rest.priceOf(someName);
	}
    }

    // expensive : Inventory -> Inventory[expensive]
    public Inventory expensive ( ) {
	// this.first : InventoryRecord
	// this.rest : Inventory
	// this.rest.expensive() : Inventory[expensive]
	if ( this.first.isExpensiveHuh() ) {
	    return new OneMoreItem( this.first, this.rest.expensive() ) ;
	} else {
	    return this.rest.expensive() ;
	}
    }

    // SmootHawley : Inventory -> Inventory
    public Inventory SmootHawley ( ) {
	// ... this.first ... this.rest ... this.rest.SmootHawley() ...
	// ... this.first.tax(0.1) ... this.rest ... this.rest.SmootHawley() ...
	return new OneMoreItem( this.first.tax(0.1) , this.rest.SmootHawley() );
    }
}

class scratch {
    public static void main ( String[] args ) {
	Inventory mt = new EmptyInventory();
	InventoryRecord kefka = new InventoryRecord("Kefka Action Figure", 500.00);
	Inventory andKefka = new OneMoreItem( kefka, mt );
	InventoryRecord butz = new InventoryRecord("Butz Action Figure", 1000.00);
	Inventory andButz = new OneMoreItem( butz, andKefka );
	Inventory andLightning = new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), andButz );

	System.out.format("The answer is %s, but should be %s%n",
			  mt,
			  new EmptyInventory());
	System.out.format("The answer is %s%n",
			  andKefka);
	System.out.format("The answer is %s%n",
			  andButz);
	System.out.format("The answer is %s%n",
			  andLightning);

	System.out.format("The answer is %f, but should be %f%n",
			  andKefka.totalPrice(),
			  500.00);
	System.out.format("The answer is %f, but should be %f%n",
			  new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new EmptyInventory() ).totalPrice(),
			  500.00);
	System.out.format("The answer is %f, but should be %f%n",
			  andLightning.totalPrice(),
			  1550.00);

	System.out.format("The answer is %b, but should be %b%n",
			  kefka.isButzHuh(),
			  false);
	System.out.format("The answer is %b, but should be %b%n",
			  butz.isButzHuh(),
			  true);

	System.out.format("The answer is %b, but should be %b%n",
			  mt.containsButzHuh(),
			  false);
	System.out.format("The answer is %b, but should be %b%n",
			  andKefka.containsButzHuh(),
			  false);
	System.out.format("The answer is %b, but should be %b%n",
			  andButz.containsButzHuh(),
			  true);
	System.out.format("The answer is %b, but should be %b%n",
			  andLightning.containsButzHuh(),
			  true);

	System.out.format("The answer is %f, but should be %f%n",
			  andLightning.priceOf("Kefka Action Figure"),
			  500.00);
	System.out.format("The answer is %f, but should be %f%n",
			  andLightning.priceOf("Butz Action Figure"),
			  1000.00);


	System.out.format("The answer is %s, but should be %s%n",
			  andLightning.expensive(),
			  new OneMoreItem(butz,mt) );

	System.out.format("The answer is %s, but should be %s%n",
			  butz.tax(0.1),
			  new InventoryRecord( "Butz Action Figure", 1100.00 ) );

	System.out.format("The answer is %s, but should be %s%n",
			  andLightning.SmootHawley(),
			  new OneMoreItem( new InventoryRecord("Lightning Action Figure", 55.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1100.00), new OneMoreItem( new InventoryRecord("Kefka Action Figure", 550.00), mt ) ) ) );


    }
}