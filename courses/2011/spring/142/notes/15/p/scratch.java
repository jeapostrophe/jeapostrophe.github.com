// A ListOfString is either
//  Empty
//  Cons
interface ListOfString {
    // append : ListOfString ListOfString -> ListOfString
    public ListOfString append ( ListOfString after ) ;
}

// An EmptyLOS is a
//   new EmptyLOS ()
// where
class EmptyLOS implements ListOfString {
    public EmptyLOS ( ) {
    }
    public String toString ( ) {
	return "!";
    }
    // append : ListOfString ListOfString -> ListOfString
    public ListOfString append ( ListOfString after ) {
	return after;
    }
}

// A ConsLOS is a
//  new ConsLOS ( first, rest )
// where
//  first is a String
//  rest is a ListOfString
class ConsLOS implements ListOfString {
    public String first;
    public ListOfString rest;

    public ConsLOS ( String first0, ListOfString rest0 ) {
	first = first0;
	rest = rest0;
    }

    public String toString () {
	return String.format("%s:%s", this.first, this.rest );
    }

    // append : ListOfString ListOfString -> ListOfString
    public ListOfString append ( ListOfString after ) {
	return new ConsLOS ( this.first, this.rest.append(after) );
    }
}

// A PersonRecord is a...
//  new PersonRecord( name, eyeColor )
// where
//  name is a String
//  eyeColor is a String
class PersonRecord {
    public String name;
    public String eyeColor;

    public PersonRecord ( String name0, String eyeColor0 ) {
	name = name0;
	eyeColor = eyeColor0;
    }
    public String toString () {
	return String.format("new PersonRecord( \"%s\", \"%s\" )",
			     this.name,
			     this.eyeColor );
    }
}

// A DFamilyTree is a...
//  new DFamilyTree ( info, children )
// where
//  info is a PersonRecord
//  children is a ListOfDFT
class DFamilyTree {
    public PersonRecord info;
    public ListOfDFT children;

    public DFamilyTree ( PersonRecord info0, ListOfDFT children0 ) {
	info = info0;
	children = children0;
    }

    // howManyBrownEyedDescendents : DFT -> int
    public int howManyBrownEyedDescendents ( ) {
	// this.info ... this.children ...
	// this.info.eyeColor ...
	// this.children.howManyBrownEyedDescendents() ...

	if ( this.info.eyeColor.equals("brown") ) {
	// input: patT
	// output: 1
	// this.info.eyeColor = brown
	// this.children.howManyBrownEyedDescendents() = 0
	    // return 1;

	// i: new DFamilyTree( new PersonRecord ( "John LaBonte", "brown" ), new ConsLDFT( joT, new EmptyLDFT () ) )
	// o: 3
	// this.info.eyeColor = brown
	// this.children.howManyBrownEyedDescendents() = 2
	return 1 + this.children.howManyBrownEyedDescendents() ;

	} else {
	// input: new DFamilyTree( jay, new EmptyLDFT () ).howManyBrownEyedDescendents()
	// this.info.eyeColor = blue
	// this.children.howManyBrownEyedDescendents() = 0
	// output: 0
	    //return 0;

	// i: new DFamilyTree( jo, josKids )
	// o: 2
	// this.info.eyeColor = blue
	// this.children.howManyBrownEyedDescendents() = 2
	    return this.children.howManyBrownEyedDescendents() ;

	}

	
    }

    // descendents : DFamilyTree -> ListOfString
    public ListOfString descendents ( ) {
	// this.info.name 
	// this.children.descendents ()
	return new ConsLOS( this.info.name , this.children.descendents () );
    }
}

// A ListOfDFT is either...
//  EmptyLDFT
//  ConsLDFT
interface ListOfDFT {
    // howManyBrownEyedDescendents : ListOfDFT -> int
    public int howManyBrownEyedDescendents ( ) ;
    // descendents : ListOfDFT -> ListOfString
    public ListOfString descendents ( ) ;
}

// A EmptyLDFT is a...
//  new EmptyLDFT ()
// where
class EmptyLDFT implements ListOfDFT {

    public EmptyLDFT () {
    }

    // howManyBrownEyedDescendents : ListOfDFT -> int
    public int howManyBrownEyedDescendents ( ) {
	// ... ...
	return 0;
    }

    // descendents : ListOfDFT -> ListOfString
    public ListOfString descendents ( ) {
	return new EmptyLOS();
    }

}

// A ConsLDFT is a...
//  new ConsLDFT( first, rest )
// where
//  first is DFamilyTree
//  rest is ListOfDFT
class ConsLDFT implements ListOfDFT {
    public DFamilyTree first;
    public ListOfDFT rest;

    public ConsLDFT ( DFamilyTree first0, ListOfDFT rest0 ) {
	first = first0;
	rest = rest0;
    }

    // howManyBrownEyedDescendents : ListOfDFT -> int
    public int howManyBrownEyedDescendents ( ) {
	// this.first ... this.rest ...
	// this.first.howManyBrownEyedDescendents()
	// this.rest.howManyBrownEyedDescendents()

	// input: new ConsLDFT( jayT, new ConsLDFT( kamaT, new EmptyLDFT () ) )
	// output: 0
	// this.first = jayT
	// this.first.howManyBrownEyedDescendents() = 0
	// this.rest = new ConsLDFT( kamaT, new EmptyLDFT () )	
	// this.rest.howManyBrownEyedDescendents() = 0
	// return 0;

	// i: new ConsLDFT( patT, new EmptyLDFT() ).howManyBrownEyedDescendents(),
	// o: 1
	// this.first = patT
	// this.first.howManyBrownEyedDescendents() = 1
	// this.rest = new EmptyLDFT ()
	// this.rest.howManyBrownEyedDescendents() = 0
	// return this.first.howManyBrownEyedDescendents() ;

	// i: josKids
	// o: 2
	// this.first = jimT
	// this.first.howManyBrownEyedDescendents() = 1
	// this.rest = patT : !
	// this.rest.howManyBrownEyedDescendents() = 1
	return this.first.howManyBrownEyedDescendents() + this.rest.howManyBrownEyedDescendents() ;
	
    }

    // descendents : ListOfDFT -> ListOfString
    public ListOfString descendents ( ) {
	// this.first.descendents ( )
	// this.rest.descendents ( )
	return this.first.descendents().append( this.rest.descendents() );
    }

}

class scratch {
    public static void main ( String[] args ) {
	System.out.format("The answer is%n   %s%n", "Spaghetti");
	
	//PersonRecord joe = new PersonRecord( "Joe McCarthy", "brown" ); 
       	PersonRecord jo = new PersonRecord( "Jo McCarthy", "blue" );
	PersonRecord jim = new PersonRecord( "Jim McCarthy", "brown" );
	//PersonRecord pam = new PersonRecord( "Pam McCarthy", "brown" );
	PersonRecord jay = new PersonRecord( "Jay McCarthy", "blue" );
	PersonRecord kama = new PersonRecord( "Kama McCarthy", "chartreuse" );
	PersonRecord pat = new PersonRecord( "Pat Thistle", "brown" );

	DFamilyTree jayT = new DFamilyTree( jay, new EmptyLDFT () );
	DFamilyTree kamaT = new DFamilyTree( kama, new EmptyLDFT () );
	DFamilyTree patT = new DFamilyTree( pat, new EmptyLDFT () );
	ListOfDFT jimsKids = new ConsLDFT( jayT, new ConsLDFT( kamaT, new EmptyLDFT () ) );
	DFamilyTree jimT = new DFamilyTree( jim, jimsKids );
	ListOfDFT josKids = new ConsLDFT( jimT, new ConsLDFT( patT, new EmptyLDFT() ) );
	DFamilyTree joT = new DFamilyTree( jo, josKids );
	DFamilyTree johnT = new DFamilyTree( new PersonRecord ( "John LaBonte", "brown" ), new ConsLDFT( joT, new EmptyLDFT () ) ); 

	System.out.format("The answer is%n   %d%nBut should be%n   %d%n",
			  new EmptyLDFT().howManyBrownEyedDescendents(),
			  0 );	
	System.out.format("The answer is%n   %d%nBut should be%n   %d%n",
			  jimsKids.howManyBrownEyedDescendents(),
			  0 );
	System.out.format("The answer is%n   %d%nBut should be%n   %d%n",
			  patT.howManyBrownEyedDescendents(),
			  1 );	
	System.out.format("The answer is%n   %d%nBut should be%n   %d%n",
			  new ConsLDFT( patT, new EmptyLDFT() ).howManyBrownEyedDescendents(),
			  1 );	
	System.out.format("The answer is%n   %d%nBut should be%n   %d%n",
			  josKids.howManyBrownEyedDescendents(),
			  2 );	

	System.out.format("The answer is%n   %d%nBut should be%n   %d%n",
			  jayT.howManyBrownEyedDescendents(),
			  0 );	
	System.out.format("The answer is%n   %d%nBut should be%n   %d%n",
			  joT.howManyBrownEyedDescendents(),
			  2 );	
	System.out.format("The answer is%n   %d%nBut should be%n   %d%n",
			  johnT.howManyBrownEyedDescendents(),
			  3 );

	
	System.out.format("The answer is%n   %s%nBut should be%n   %s%n",
			  johnT.descendents(),
			  "john:jo:jim:jay:kama:pat:!");
	System.out.format("The answer is%n   %s%nBut should be%n   %s%n",
			  new DFamilyTree( new PersonRecord ( "John LaBonte", "brown" ), new ConsLDFT( joT, new EmptyLDFT () ) ).descendents(),
			  "john:jo:jim:jay:kama:pat:!");
	System.out.format("The answer is%n   %s%nBut should be%n   %s%n",
			  new ConsLOS( "John LaBonte" , new ConsLDFT( joT, new EmptyLDFT () ).descendents () ),
			  "john:jo:jim:jay:kama:pat:!");
	System.out.format("The answer is%n   %s%nBut should be%n   %s%n",
			  new ConsLOS( "John LaBonte" , joT.descendents().append( new EmptyLDFT ().descendents() ) ),
			  "john:jo:jim:jay:kama:pat:!");
	System.out.format("The answer is%n   %s%nBut should be%n   %s%n",
			  new ConsLOS( "John LaBonte" , new DFamilyTree( jo, josKids ).descendents().append( new EmptyLDFT ().descendents() ) ),
			  "john:jo:jim:jay:kama:pat:!");
	System.out.format("The answer is%n   %s%nBut should be%n   %s%n",
			  new ConsLOS( "John LaBonte" , new DFamilyTree( jo, josKids ).descendents().append( new EmptyLOS() ) ),
			  "john:jo:jim:jay:kama:pat:!");
	System.out.format("The answer is%n   %s%nBut should be%n   %s%n",
			  new ConsLOS( "John LaBonte" , new DFamilyTree( jo, josKids ).descendents() ),
			  "john:jo:jim:jay:kama:pat:!");
	System.out.format("The answer is%n   %s%nBut should be%n   %s%n",
			  new ConsLOS( "John LaBonte" , new ConsLOS( "Jo", josKids.descendents() ) ),
			  "john:jo:jim:jay:kama:pat:!");
	System.out.format("The answer is%n   %s%nBut should be%n   %s%n",
			  new ConsLOS( "John LaBonte" , new ConsLOS( "Jo", new ConsLDFT( jimT, new ConsLDFT( patT, new EmptyLDFT() ) ).descendents() ) ),
			  "john:jo:jim:jay:kama:pat:!");

    }
}