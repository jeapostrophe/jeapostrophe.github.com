// A ListOfNumbers is either
//  - EmptyListOfNumbers
//  - OneMoreNumber
interface ListOfNumbers {
    // insert : ListOfNumbers[sorted] double -> ListOfNumbers[sorted]
    public ListOfNumbers insert ( double n );
    // sort : ListOfNumbers -> ListOfNumbers[sorted]
    public ListOfNumbers sort ( );
    // sort2 : ListOfNumbers -> ListOfNumbers[sorted]
    public ListOfNumbers sort2 ( );
}

// A EmptyListOfNumbers is a..
//  new EmptyListOfNumbers ( )
// where
class EmptyListOfNumbers implements ListOfNumbers {
    
    public EmptyListOfNumbers ( ) {
    }

    // toString : EmptyListOfNumbers -> String
    public String toString () {
	return "!";
    }

    // insert : EmptyListOfNumbers[sorted] double -> ListOfNumbers[sorted]
    public ListOfNumbers insert ( double n ) {
	// ... ...
	/*
	  new EmptyListOfNumbers().insert(1.0),
	  new OneMoreNumber(1.0, new EmptyListOfNumbers())

	  this.insert(n),
	  new OneMoreNumber(n, new EmptyListOfNumbers())
	*/
	return new OneMoreNumber(n, new EmptyListOfNumbers()) ;
    }

    // sort : EmptyListOfNumbers -> ListOfNumbers[sorted]
    public ListOfNumbers sort ( ) {
	return new EmptyListOfNumbers();
    }

    // sort2 : EmptyListOfNumbers -> ListOfNumbers[sorted]
    public ListOfNumbers sort2 ( ) {
	return new EmptyListOfNumbers();
    }
}

// A OneMoreNumber is a...
//  new OneMoreNumber ( first, rest )
// where
//  first is a double
//  rest is a ListOfNumbers
class OneMoreNumber implements ListOfNumbers {
    public double first;
    public ListOfNumbers rest;

    public OneMoreNumber ( double first0, ListOfNumbers rest0 ) {
	first = first0;
	rest = rest0;
    }

    // toString : OneMoreNumber -> String
    public String toString () {
	return String.format("%f:%s", this.first, this.rest );
    }

    // insert : OneMoreNumber[sorted] double -> ListOfNumbers[sorted]
    public ListOfNumbers insert ( double n ) {
	// this.first ... this.rest ... n ... this.rest.insert(n) ...

	if ( n > this.first ) {
	/*
	  new OneMoreNumber(1.0, new EmptyListOfNumbers()).insert(2.0),
	  new OneMoreNumber(1.0, new OneMoreNumber(2.0, new EmptyListOfNumbers())));

	  new OneMoreNumber(this.first, this.rest).insert(n),
	  new OneMoreNumber(this.first, new OneMoreNumber(n, this.rest)));
	*/
	    // return new OneMoreNumber(this.first, new OneMoreNumber(n, this.rest));

	    /*
	      new OneMoreNumber(0.0, new OneMoreNumber(1.0, new EmptyListOfNumbers())).insert(2.0),
	      new OneMoreNumber(0.0, new OneMoreNumber(1.0, new OneMoreNumber(2.0, new EmptyListOfNumbers()))));
	      
	      new OneMoreNumber(this.first, this.rest).insert(n),
	      new OneMoreNumber(this.first, this.rest.insert(n)));
	    */
	    return new OneMoreNumber(this.first, this.rest.insert(n));
	} else {
	/*
	  ---

	  new OneMoreNumber(1.0, new EmptyListOfNumbers).insert(0.0),
	  new OneMoreNumber(0.0, new OneMoreNumber(1.0, new EmptyListOfNumbres)));

	  new OneMoreNumber(this.first, this.rest).insert(n),
	  new OneMoreNumber(n, new OneMoreNumber(this.first, this.rest)));
	*/
	return new OneMoreNumber(n, new OneMoreNumber(this.first, this.rest));
	}       
    }

    // sort : ListOfNumbers -> ListOfNumbers[sorted]
    public ListOfNumbers sort ( ) {
	// this.first : double
	// this.rest : ListOfNumbers
	// this.rest.sort() : ListOfNumbers[sorted]

	/*
	  new OneMoreNumber(2.0, new EmptyListOfNumbers()).sort(),
	  new OneMoreNumber(2.0, new EmptyListOfNumbers())

	  new OneMoreNumber(this.first, this.rest).sort(),
	  new OneMoreNumber(this.first, this.rest.sort())
	*/
	// return new OneMoreNumber(this.first, this.rest.sort()) ;

	/*
	  new OneMoreNumber(2.0, new OneMoreNumber(1.0, new EmptyListOfNumbers())).sort(),
	  new OneMoreNumber(1.0, new OneMoreNumber(2.0, new EmptyListOfNumbers())));

	  new OneMoreNumber(this.first, this.rest).sort(),
	  new OneMoreNumber(1.0, new OneMoreNumber(this.first, new EmptyListOfNumbers())));
	  this.rest.insert(this.first);

	  // insert : ListOfNumbers[sorted] double -> ListOfNumbers[sorted]

	  this.rest.sort().insert(this.first);
	*/
	return (this.rest.sort()).insert(this.first);
	// Nicolais:
	//return this.rest.insert(this.first).sort();
 
    }	

    // sort2 : ListOfNumbers -> ListOfNumbers[sorted]
    public ListOfNumbers sort2 ( ) {
	// this.first : double
	// this.rest : ListOfNumbers
	// this.rest.sort2() : ListOfNumbers[sorted]

	// QuickSort
	/*
	List.append(
		    this.rest.smallerThanOrEqualTo(this.first).sort2(),
		    new OneMoreNumber( this.first,
				       this.rest.biggerThan(this.first).sort2() ) );
	*/
	// XXX This is borken
	return this.rest;

    }

}

/*
5. Write a function called insert that takes a number and a sorted list of numbers and returns a new sort list of number where the new number is added in the correct spot. [Sorted meaning the numbers go from smallest to largest.]

6. Write a function called sort that takes an unsorted list of numbers and returns a sorted list of numbers. [Hint: Use #5!]
*/

class sort {
    public static void main ( String[] args ) {
	System.out.format("The answer is %s, but should be %s%n",
			  new EmptyListOfNumbers(),
			  new EmptyListOfNumbers());

	System.out.format("%nInsert%n%n");

	System.out.format("The answer is %s, but should be %s%n",
			  new EmptyListOfNumbers().insert(1.0),
			  new OneMoreNumber(1.0, new EmptyListOfNumbers()));
	System.out.format("The answer is %s, but should be %s%n",
			  new EmptyListOfNumbers().insert(2.0),
			  new OneMoreNumber(2.0, new EmptyListOfNumbers()));


	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber(1.0, new EmptyListOfNumbers()).insert(2.0),
			  new OneMoreNumber(1.0, new OneMoreNumber(2.0, new EmptyListOfNumbers())));
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber(1.0, new EmptyListOfNumbers()).insert(0.0),
			  new OneMoreNumber(0.0, new OneMoreNumber(1.0, new EmptyListOfNumbers())));

	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber(0.0, new OneMoreNumber(1.0, new EmptyListOfNumbers())).insert(2.0),
			  new OneMoreNumber(0.0, new OneMoreNumber(1.0, new OneMoreNumber(2.0, new EmptyListOfNumbers()))));

	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber(0.0, new OneMoreNumber(1.0, new EmptyListOfNumbers())).insert(0.5),
			  new OneMoreNumber(0.0, new OneMoreNumber(0.5, new OneMoreNumber(1.0, new EmptyListOfNumbers()))));

	// Why does Nicolai's sort run forever?
	
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber(2.0, new EmptyListOfNumbers()).sort(),
			  new OneMoreNumber(2.0, new EmptyListOfNumbers()));
	System.out.format("The answer is %s, but should be %s%n",
			  new EmptyListOfNumbers().insert(2.0).sort(),
			  new OneMoreNumber(2.0, new EmptyListOfNumbers()));
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber(2.0, new EmptyListOfNumbers()).sort(),
			  new OneMoreNumber(2.0, new EmptyListOfNumbers()));
	System.out.format("The answer is %s, but should be %s%n",
			  new EmptyListOfNumbers().insert(2.0).sort(),
			  new OneMoreNumber(2.0, new EmptyListOfNumbers()));
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber(2.0, new EmptyListOfNumbers()).sort(),
			  new OneMoreNumber(2.0, new EmptyListOfNumbers()));

	//

	System.out.format("%nSort%n%n");

	System.out.format("The answer is %s, but should be %s%n",
			  new EmptyListOfNumbers().sort(),
			  new EmptyListOfNumbers());
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber(2.0, new EmptyListOfNumbers()).sort(),
			  new OneMoreNumber(2.0, new EmptyListOfNumbers()));

	System.out.format("The answer is %s, but should be %s%n",
			  (new EmptyListOfNumbers().sort()).insert(2.0),
			  new OneMoreNumber(2.0, new EmptyListOfNumbers()));
	System.out.format("The answer is %s, but should be %s%n",
			  new EmptyListOfNumbers().insert(2.0),
			  new OneMoreNumber(2.0, new EmptyListOfNumbers()));
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber(2.0, new EmptyListOfNumbers()),
			  new OneMoreNumber(2.0, new EmptyListOfNumbers()));

	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber(1.0, new OneMoreNumber(2.0, new EmptyListOfNumbers())).sort(),
			  new OneMoreNumber(1.0, new OneMoreNumber(2.0, new EmptyListOfNumbers())));
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber(2.0, new OneMoreNumber(1.0, new EmptyListOfNumbers())).sort(),
			  new OneMoreNumber(1.0, new OneMoreNumber(2.0, new EmptyListOfNumbers())));
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber(3.0, new OneMoreNumber(2.0, new OneMoreNumber(1.0, new EmptyListOfNumbers()))).sort(),
			  new OneMoreNumber(1.0, new OneMoreNumber(2.0, new OneMoreNumber(3.0, new EmptyListOfNumbers()))));

	// Insertion Sort
	// "Algorithms"

	System.out.format("%nSort2%n%n");

	System.out.format("The answer is %s, but should be %s%n",
			  new EmptyListOfNumbers().sort2(),
			  new EmptyListOfNumbers());
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber(2.0, new EmptyListOfNumbers()).sort2(),
			  new OneMoreNumber(2.0, new EmptyListOfNumbers()));

	System.out.format("The answer is %s, but should be %s%n",
			  (new EmptyListOfNumbers().sort2()).insert(2.0),
			  new OneMoreNumber(2.0, new EmptyListOfNumbers()));
	System.out.format("The answer is %s, but should be %s%n",
			  new EmptyListOfNumbers().insert(2.0),
			  new OneMoreNumber(2.0, new EmptyListOfNumbers()));
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber(2.0, new EmptyListOfNumbers()),
			  new OneMoreNumber(2.0, new EmptyListOfNumbers()));

	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber(1.0, new OneMoreNumber(2.0, new EmptyListOfNumbers())).sort2(),
			  new OneMoreNumber(1.0, new OneMoreNumber(2.0, new EmptyListOfNumbers())));
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber(2.0, new OneMoreNumber(1.0, new EmptyListOfNumbers())).sort2(),
			  new OneMoreNumber(1.0, new OneMoreNumber(2.0, new EmptyListOfNumbers())));
	System.out.format("The answer is %s, but should be %s%n",
			  new OneMoreNumber(3.0, new OneMoreNumber(2.0, new OneMoreNumber(1.0, new EmptyListOfNumbers()))).sort2(),
			  new OneMoreNumber(1.0, new OneMoreNumber(2.0, new OneMoreNumber(3.0, new EmptyListOfNumbers()))));

    }
}