 /*
1. Develop structure and data definitions for a collection of zoo animals. The collection includes

* spiders, whose relevant attributes are the number of remaining legs (we assume that spiders can lose legs in accidents) and the space they need in case of transport;
* elephants, whose only attributes are the space they need in case of transport;
* monkeys, whose attributes are intelligence and space needed for transportation.

Develop the function fitsHuh. The function consumes a zoo animal and the volume of a cage. It determines whether the cage is large enough for the animal. 

4. Extend the data definition of #1 so that animals endorse products, which are described by a product name and a product company. Write a function called 'endorse' that consumes an animal and a product and produces an animal that endorses that product.

5. Develop the function 'biggest', which consumes two animals from #1 and returns the one that takes up more space. (Hint: This requires a new function in the interface that ISN'T 'biggest'.)
*/

class Product {
    public String name;
    public String company;

    public Product ( String name0, String company0 ) {
	name = name0;
	company = company0;
    }
}

class Spider implements Animal {
    public int legs;
    public double space;
    public Product endorsed;

    public Spider ( int legs0, double space0, Product endorsed0 ) {
	legs = legs0;
	space = space0;
	endorsed = endorsed0;
    }

    public boolean fitsHuh ( double cage ) {
	return this.space <= cage ;
    }


    public Animal endorse ( Product newProduct ) {
	return new Spider ( this.legs, this.space, newProduct );
    }
    
    public Product endorses () {
	return this.endorsed ;
    }

    public double howBig () {
	return this.space ;
    }
}

class Monkey implements Animal {
    public int smarts;
    public double space;
    public Product endorsed;

    public Monkey ( int smarts0, double space0, Product endorsed0 ) {
	smarts = smarts0;
	space = space0;
	endorsed = endorsed0;
    }

    public boolean fitsHuh ( double cage ) {
	return this.space <= cage ;
    }


    public Animal endorse ( Product newProduct ) {
	return new Monkey ( this.smarts, this.space, newProduct );
    }
    
    public Product endorses () {
	return this.endorsed ;
    }

    public double howBig () {
	return this.space ;
    }
}

class Elephant implements Animal {
    public double space;
    public Product endorsed;

    public Elephant ( double space0, Product endorsed0 ) {
	space = space0;
	endorsed = endorsed0;
    }

    public boolean fitsHuh ( double cage ) {
	return this.space <= cage ;
    }

    public Animal endorse ( Product newProduct ) {
	return new Elephant ( this.space, newProduct );
    }
    
    public Product endorses () {
	return this.endorsed ;
    }

    // howBig : Elephant -> double
    public double howBig () {
	return this.space ;
    }
}

interface Animal {
    public boolean fitsHuh ( double cage ) ;
    public Animal endorse ( Product newProduct ) ;
    public Product endorses () ;
    // howBig : Animal -> double
    public double howBig () ;
}

class e {
    static boolean fitsHuh ( Animal a, double cage ) {
	return a.fitsHuh( cage );
    }

    static Animal endorse ( Animal a, Product newProduct ) {
	return a.endorse( newProduct );
    }

    static Animal biggest ( Animal a, Animal b ) {
	if ( a.howBig () > b.howBig () ) {
	    return a;
	} else {
	    return b;
	}
    }

    public static void main ( String[] args ) {
	Product iMonkey = new Product ( "iMonkey", "Apple" );
	Product spuddies = new Product ( "Spuddies", "Blam-co!" );
	Product famicom = new Product( "Famicom", "Nintendo" );

	Spider ron = new Spider ( 6, 50, famicom );
	Monkey charles = new Monkey ( 100, 60, iMonkey );
	Elephant rupert = new Elephant ( 100, spuddies );

	System.out.format( "The answer is %b, but should be %b%n",
			   fitsHuh( ron, 60 ),
			   true );
	System.out.format( "The answer is %b, but should be %b%n",
			   fitsHuh( ron, 30 ),
			   false );
	System.out.format( "The answer is %b, but should be %b%n",
			   fitsHuh( charles, 60 ),
			   true );
	System.out.format( "The answer is %b, but should be %b%n",
			   fitsHuh( charles, 30 ),
			   false );
	System.out.format( "The answer is %b, but should be %b%n",
			   fitsHuh( rupert, 110 ),
			   true );
	System.out.format( "The answer is %b, but should be %b%n",
			   fitsHuh( rupert, 30 ),
			   false );

	Product nextBig = new Product ( "The Next Big Thing", "Microsoft" );
	System.out.format( "The answer is %s by %s, but should be %s by %s%n",
			   endorse( ron, nextBig ).endorses().name,
			   endorse( ron, nextBig ).endorses().company,
			   nextBig.name,
			   nextBig.company );
	System.out.format( "The answer is %s by %s, but should be %s by %s%n",
			   endorse( charles, nextBig ).endorses().name,
			   endorse( charles, nextBig ).endorses().company,
			   nextBig.name,
			   nextBig.company );
	System.out.format( "The answer is %s by %s, but should be %s by %s%n",
			   endorse( rupert, nextBig ).endorses().name,
			   endorse( rupert, nextBig ).endorses().company,
			   nextBig.name,
			   nextBig.company );
	System.out.format( "The answer is %s by %s, but should be %s by %s%n",
			   rupert.endorses().name,
			   rupert.endorses().company,
			   spuddies.name,
			   spuddies.company );

	//4 == 5;
	//2 + 3 == 5;
	//f(27) == 5;

	System.out.format( "The answer is %b but should be %b%n",
			   biggest( ron, ron ) == ron,
			   true );
	System.out.format( "The answer is %b but should be %b%n",
			   biggest( ron, charles ) == charles,
			   true );
	System.out.format( "The answer is %b but should be %b%n",
			   biggest( ron, rupert ) == rupert,
			   true );

	System.out.format( "The answer is %b but should be %b%n",
			   biggest( charles, ron ) == charles,
			   true );
	System.out.format( "The answer is %b but should be %b%n",
			   biggest( charles, charles ) == charles,
			   true );
	System.out.format( "The answer is %b but should be %b%n",
			   biggest( charles, rupert ) == rupert,
			   true );

	System.out.format( "The answer is %b but should be %b%n",
			   biggest( rupert, ron ) == rupert,
			   true );
	System.out.format( "The answer is %b but should be %b%n",
			   biggest( rupert, charles ) == rupert,
			   true );
	System.out.format( "The answer is %b but should be %b%n",
			   biggest( rupert, rupert ) == rupert,
			   true );

	Animal bigger = biggest( rupert, charles );

    }
}