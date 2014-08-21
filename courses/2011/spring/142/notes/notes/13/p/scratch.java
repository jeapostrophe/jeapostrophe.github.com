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

    // isAnActionFigure : InventoryRecord -> boolean
    public boolean isAnActionFigure () {
	// this.name ... this.price
	return this.name.contains("Action Figure");
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
    // append : Inventory Inventory -> Inventory
    public Inventory append ( Inventory inv1 );
    // reverse : Inventory -> Inventory
    public Inventory reverse () ;
    // noMoreOtaku : Inventory -> Inventory
    public Inventory noMoreOtaku () ;
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

    // append : Inventory Inventory -> Inventory
    public Inventory append ( Inventory inv1 ) {
	return inv1;
    }

    // reverse : Inventory -> Inventory
    public Inventory reverse () {
	return new EmptyInventory ();
    }


    // noMoreOtaku : Inventory -> Inventory
    public Inventory noMoreOtaku () {
	// ... ...
	return new EmptyInventory ();
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

    // append : Inventory Inventory -> Inventory
    public Inventory append ( Inventory inv1 ) {
	return new OneMoreItem(this.first, this.rest.append(inv1));
    }

    // reverse : Inventory -> Inventory
    public Inventory reverse () {
	// this.first this.rest this.rest.reverse()

	// input: new OneMoreNumber( 1, new EmtpyListOfNumbers () ).revese()
	// output: new OneMoreNumber( 1, new EmtpyListOfNumbers () )

	// input: new OneMoreNumber( this.first, this.rest ).revese()
	// this.first = 1
	// this.rest = !
	// this.rest.reverse() = !
	// output: new OneMoreNumber( 1, new EmtpyListOfNumbers () )
	// output: new OneMoreNumber( this.first, this.rest.reverse() )

	// ---

	// input: new OneMoreNumber( 1, new OneMoreNumber(2, new EmtpyListOfNumbers ()) ).revese()
	// output: new OneMoreNumber(2, new OneMoreNumber( 1, new EmtpyListOfNumbers () ) )
       
	// this.first = 1
	// this.rest = 2:!
	// this.rest.reverse() = 2:!
	// not output = 1:2:!
	// output = 2:1:! = f( this.first, this.rest.reverse() ) = f ( 1, 2:! )
	// output = this.rest.putAtEnd(this.first) = 2:! . putAtEnd(1) = 2:1:!
	// putAtEnd( l , e ) = append( l, e:! )
	// output = this.rest.append( new OneMoreNumber( this.first, new EmptyListOfNumbers () ) )

	// input: new OneMoreNumber( 1, new OneMoreNumber(2, new OneMoreNumber(3, new EmtpyListOfNumbers ())) ).revese()
	// output: new OneMoreNumber(3, new OneMoreNumber(2, new OneMoreNumber( 1, new EmtpyListOfNumbers () ) ) )
	// this.first = 1
	// this.rest = 2:3:!
	// this.rest.reverse() = 3:2:!
	// not output = 2:3:1:!
	// output = 3:2:1:!
	// output = this.rest.reverse().append( new OneMoreNumber( this.first, new EmptyListOfNumbers () ) )
	return this.rest.reverse().append(new OneMoreItem(this.first, new EmptyInventory()));
    }

    // noMoreOtaku : Inventory -> Inventory
    public Inventory noMoreOtaku () {
	// this.first ... this.rest ... this.rest.noMoreOtaku()
	if ( ! this.first.isAnActionFigure() ) {
	    return new OneMoreItem( this.first , this.rest.noMoreOtaku() );
	} else {
	    return this.rest.noMoreOtaku() ;
	}
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

	// L1 = a:b:c:...:!
	// L2 = alpha:beta:gamma:...:!

	// append ( mt, k ) = k
	// append ( e:l, k ) = e : append(l,k) 

	System.out.format("The answer is %s, but should be %s%n",
			  andLightning.append(mt),
			  andLightning );
	System.out.format("The answer is %s, but should be %s%n",
			  mt.append(andLightning),
			  andLightning );
	System.out.format("The answer is %s, but should be %s%n",
			  andLightning.append(andLightning),
			  new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), mt ) ) ) ) ) ) );

	// What if I want to reverse a list?
	// reverse : List -> List

	System.out.format("The answer is %s, but should be %s%n",
			  andLightning.reverse(),
			  new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), mt ) ) ) );

	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), mt ) ) ).reverse(),
			  new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), mt ) ) ) );

	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), mt ) ).reverse().append(new OneMoreItem(new InventoryRecord("Lightning Action Figure", 50.00), new EmptyInventory())),
			  new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), mt ) ) ) );


	System.out.format("The answer is %s, but should be %s%n",
			 (new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), mt ).reverse().append(new OneMoreItem(new InventoryRecord("Butz Action Figure", 1000.00), new EmptyInventory()))).append(new OneMoreItem(new InventoryRecord("Lightning Action Figure", 50.00), new EmptyInventory())),
			  new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), mt ) ) ) );

	System.out.format("The answer is %s, but should be %s%n",
			 ((mt.reverse().append(new OneMoreItem(new InventoryRecord("Kefka Action Figure", 500.00), new EmptyInventory()))).append(new OneMoreItem(new InventoryRecord("Butz Action Figure", 1000.00), new EmptyInventory()))).append(new OneMoreItem(new InventoryRecord("Lightning Action Figure", 50.00), new EmptyInventory())),
			  new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), mt ) ) ) );

	System.out.format("The answer is %s, but should be %s%n",
			 ((new EmptyInventory ().append(new OneMoreItem(new InventoryRecord("Kefka Action Figure", 500.00), new EmptyInventory()))).append(new OneMoreItem(new InventoryRecord("Butz Action Figure", 1000.00), new EmptyInventory()))).append(new OneMoreItem(new InventoryRecord("Lightning Action Figure", 50.00), new EmptyInventory())),
			  new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), mt ) ) ) );

	System.out.format("The answer is %s, but should be %s%n",
			 (new OneMoreItem(new InventoryRecord("Kefka Action Figure", 500.00), new EmptyInventory())).append(new OneMoreItem(new InventoryRecord("Butz Action Figure", 1000.00), new EmptyInventory())).append(new OneMoreItem(new InventoryRecord("Lightning Action Figure", 50.00), new EmptyInventory())),
			  new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), mt ) ) ) );

	System.out.format("The answer is %s, but should be %s%n",
			  (new OneMoreItem(new InventoryRecord("Kefka Action Figure", 500.00),  new EmptyInventory().append((new OneMoreItem(new InventoryRecord("Butz Action Figure", 1000.00), new EmptyInventory()))))).append(new OneMoreItem(new InventoryRecord("Lightning Action Figure", 50.00), new EmptyInventory())),
			  new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), mt ) ) ) );

	System.out.format("The answer is %s, but should be %s%n",
			  (new OneMoreItem(new InventoryRecord("Kefka Action Figure", 500.00), (new OneMoreItem(new InventoryRecord("Butz Action Figure", 1000.00), new EmptyInventory())))).append(new OneMoreItem(new InventoryRecord("Lightning Action Figure", 50.00), new EmptyInventory())),
			  new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), mt ) ) ) );

	System.out.format("The answer is %s, but should be %s%n",
			 new OneMoreItem(new InventoryRecord("Kefka Action Figure", 500.00), (new OneMoreItem(new InventoryRecord("Butz Action Figure", 1000.00), new EmptyInventory().append(new OneMoreItem(new InventoryRecord("Lightning Action Figure", 50.00), new EmptyInventory()))))),
			  new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), mt ) ) ) );

	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreItem(new InventoryRecord("Kefka Action Figure", 500.00), (new OneMoreItem(new InventoryRecord("Butz Action Figure", 1000.00), (new OneMoreItem(new InventoryRecord("Lightning Action Figure", 50.00), new EmptyInventory()))))),
			  new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), mt ) ) ) );


	/// noMoreOtaku
	System.out.format("The answer is %s, but should be %s%n",
			  mt.noMoreOtaku(),
			  mt );
	System.out.format("The answer is %s, but should be %s%n",
			  andLightning.noMoreOtaku(),
			  mt );
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreItem( new InventoryRecord("Football", 12.00), andLightning).noMoreOtaku(),
			  new OneMoreItem( new InventoryRecord("Football", 12.00), mt ) );

	// Tree stuff

	FamilyTree unknown = new MissingPerson () ;
	FamilyTree grandPaMcC = new Person( "Joe McCarthy", "brown", unknown, unknown );
	FamilyTree jo = new Person( "Jo McCarthy", "blue", unknown, unknown );
	FamilyTree jaysDad = new Person( "Jim McCarthy", "brown", grandPaMcC, jo );
	FamilyTree jaysMum = new Person( "Pam McCarthy", "brown", unknown, unknown );
	FamilyTree jay = new Person( "Jay McCarthy", "blue", jaysDad, jaysMum );

	System.out.format("The answer is %s%n", jay);

	System.out.format("The answer is %b but should be %b%n",
			  unknown.hasBluedEyedAncestors(),
			  false );
	System.out.format("The answer is %b but should be %b%n",
			  jaysMum.hasBluedEyedAncestors(),
			  false );
	System.out.format("The answer is %b but should be %b%n",
			  jo.hasBluedEyedAncestors(),
			  true );
	System.out.format("The answer is %b but should be %b%n",
			  jaysDad.hasBluedEyedAncestors(),
			  true );
	System.out.format("The answer is %b but should be %b%n",
			  jay.hasBluedEyedAncestors(),
			  true );

    }
}

// A FamilyTree is either
//  MissingPerson
//  Person
interface FamilyTree {
    // hasBluedEyedAncestors : FamilyTree -> boolean
    public boolean hasBluedEyedAncestors ( );
}

// A MissingPerson is a...
//  new MissingPerson ()
// where
class MissingPerson implements FamilyTree {
    
    public MissingPerson () {
    }

    public String toString () {
	return "...HF";
    }

    // hasBluedEyedAncestors : FamilyTree -> boolean
    public boolean hasBluedEyedAncestors ( ) {
	// ... ...
	return false ;
    }
}

// A Person is a...
//  new Person( name, eyeColor, father, mother )
// where
//  name is a String
//  eyeColor is a String
//  father is a FamilyTree
//  mother is a FamilyTree
class Person implements FamilyTree {
    public String name;
    public String eyeColor;
    public FamilyTree father;
    public FamilyTree mother;

    public Person ( String name0, String eyeColor0, FamilyTree father0, FamilyTree mother0 ) {
	name = name0;
	eyeColor = eyeColor0;
	father = father0;
	mother = mother0;
    }

    public String toString () {
	return
	    String.format("%s (%s):%n Father: %s%n Mother: %s%n",
			  this.name,
			  this.eyeColor,
			  this.father,
			  this.mother );
    }

    // hasBluedEyedAncestors : FamilyTree -> boolean
    public boolean hasBluedEyedAncestors ( ) {
	// ... this.name ... this.eyeColor ... this.father ... this.father.hasBluedEyedAncestors() ... this.mother ... this.mother.hasBluedEyedAncestors()

	// input: new Person( "Pam McCarthy", "brown", unknown, unknown ).hasBluedEyedAncestors(),
	// this.name = Pam
	// this.eyeColor = brown
	// this.father = unknown
	// this.father.hasBluedEyedAncestors() = false
	// this.mother = unknown
	// this.mother.hasBluedEyedAncestors() = false
	// output: false because BOTH don't have blueEyed...
	// return ! ( ! this.father.hasBluedEyedAncestors() ) && ( ! this.mother.hasBluedEyedAncestors() ) ;
	// return : this.father.hasBluedEyedAncestors() || this.mother.hasBluedEyedAncestors() ;

	// De Morgan's Laws:
	//  (A /\ B) == ! ( ! A \/ ! B )
	//  (A \/ B) == ! ( ! A /\ ! B ) <-- we used this one!
	// return this.father.hasBluedEyedAncestors() || this.mother.hasBluedEyedAncestors() ;

	// input: new Person( "Jim McCarthy", "brown", new Person( "Joe McCarthy", "brown", unknown, unknown ), new Person( "Jo McCarthy", "blue", unknown, unknown ) ).hasBluedEyedAncestors(),

	// this.name = Jim ...
	// this.eyeColor = brown
	// this.father = new Person( "Joe McCarthy", "brown", unknown, unknown )
	// this.father.hasBluedEyedAncestors() = false
	// this.mother = new Person( "Jo McCarthy", "blue", unknown, unknown )
	// this.mother.hasBluedEyedAncestors() = true

	// output: true
	// return : this.father.hasBluedEyedAncestors() || this.mother.hasBluedEyedAncestors() ;
	// return : false || false || this.mother.hasBluedEyedAncestors() ;
	// return : false || true ;
	// return : true ;
	// return this.father.hasBluedEyedAncestors() || this.mother.hasBluedEyedAncestors() ;


	// input: new Person( "Jo McCarthy", "blue", unknown, unknown ).hasBluedEyedAncestors(),
	// this.name = Jo
	// this.eyeColor = blue
	// this.father = unknown
	// this.father.hasBluedEyedAncestors() = false
	// this.mother = unknown
	// this.mother.hasBluedEyedAncestors() = false
	
	// output: true
	// not return : this.father.hasBluedEyedAncestors() || this.mother.hasBluedEyedAncestors() ;
	// not return : false || false = false
	// return : this.eyeColor.equals("blue") || false || false = true
	return (this.eyeColor.equals("blue") || this.father.hasBluedEyedAncestors()) || this.mother.hasBluedEyedAncestors() ;
	
    }
}
