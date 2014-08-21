// A Number is either
//  Zero
//  Successor of another number

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

// append ( l0, l1 ) = l0 ... l1 ...
// append ( a:b:c:...:!, z:x:y:...:! ) = a:b:c:...:z:x:y:...:!
// append ( !, l ) = l
// append ( e:l0, l1 ) = e : append( l0, l1 )
// 0 = l
// : = :

// MapReduce

// A FamilyTree is either
//  MissingPerson
//  Person
interface FamilyTree {
    // hasBluedEyedAncestors : FamilyTree -> boolean
    public boolean hasBluedEyedAncestors ( );
    // hasProperBluedEyedAncestors : FamilyTree -> boolean
    public boolean hasProperBluedEyedAncestors ( );
    // countPeople : FamilyTree -> int
    public int countPeople ( ) ;
    // generations : FamilyTree -> int
    public int generations ( ) ;
    // names : FamilyTree -> ListOfString
    public ListOfString names ( ) ;
    // oldestMatriarch : FamilyTree -> PersonRecord
    public PersonRecord oldestMatriarch ( );
}

// A MissingPerson is a...
//  new MissingPerson ()
// where
class MissingPerson implements FamilyTree {
    
    public MissingPerson () {
    }

    public String toString () {
	return "new MissingPerson ()";
    }

    // hasBluedEyedAncestors : FamilyTree -> boolean
    public boolean hasBluedEyedAncestors ( ) {
	// ... ...
	return false ;
    }

    // hasProperBluedEyedAncestors : FamilyTree -> boolean
    public boolean hasProperBluedEyedAncestors ( ) {
	// ... ...
	return false ;
    }

    // countPeople : FamilyTree -> int
    public int countPeople ( ) {
	// ... ...
	return 0;
    }

    // generations : FamilyTree -> int
    public int generations ( ) {
	// ... ...
	return 0;
    }

    // names : FamilyTree -> ListOfString
    public ListOfString names ( ) {
	// ... ...
	return new EmptyLOS () ;
    }

    // oldestMatriarch : FamilyTree -> PersonRecord
    public PersonRecord oldestMatriarch ( ) {
	return new PersonRecord ( "No person", "No eye color" );
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

// A Person is a...
//  new Person( info, father, mother )
// where
//  info is a PersonRecord
//  father is a FamilyTree
//  mother is a FamilyTree
class Person implements FamilyTree {
    public PersonRecord info;
    public FamilyTree father;
    public FamilyTree mother;

    public Person ( PersonRecord info0, FamilyTree father0, FamilyTree mother0 ) {
	info = info0;
	father = father0;
	mother = mother0;
    }

    /*
    public return template ( Input in ... ) {
	// this.name ... this.eyeColor ... this.father ... this.mother ... in ...
	// this.name ... this.eyeColor ... this.father.template() ... this.mother.template() ... in ...
	return .... ;
    }
    */

    public String toString () {
	return
	    String.format("new Person( %s, %s, %s)",
			  this.info,
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
	return (this.info.eyeColor.equals("blue") || this.father.hasBluedEyedAncestors()) || this.mother.hasBluedEyedAncestors() ;
    }

    // hasProperBluedEyedAncestors : FamilyTree -> boolean
    public boolean hasProperBluedEyedAncestors ( ) {
	// this.info ... this.father ... this.mother
	// this.info.name ... this.info.eyeColor ...
	// this.father.hasProperBluedEyedAncestors() 
	// this.father.countPeople()
	// this.father.hasBlueEyedAncestor()
	// this.mother.hasProperBluedEyedAncestors() 
	// this.mother.countPeople()
	// this.mother.hasBlueEyedAncestor()

	return this.father.hasBluedEyedAncestors()
	    || this.mother.hasBluedEyedAncestors() ;

    }


    // countPeople : FamilyTree -> int
    public int countPeople ( ) {
	// this.name ... this.eyeColor ... this.father ... this.mother
	// this.name ... this.eyeColor ... (this.father).countPeople() ... (this.mother).countPeople()
	
	// input: new Person( "Jo McCarthy", "blue", unknown, unknown ).countPeople()
	// output: 1
	// this.name = Jo
	// this.eyeColor = blue
	// this.father = unknown
	// this.father.countPeople() = 0
	// this.mother = unknown
	// this.mother.countPeople() = 0
	// return 1;

	// input: new Person( "Jay McCarthy", "blue", jaysDad, jaysMum ).countPeople()
	// output: 5
	// this.name = Jay
	// this.eyeColor = blue
	// this.father = jaysDad
	// this.father.countPeople() = 3
	// this.mother = jaysMum
	// this.mother.countPeople() = 1
	return 1 + this.father.countPeople() + this.mother.countPeople() ;
    }

    // generations : FamilyTree -> int
    public int generations ( ) {
	// this.name ... this.eyeColor ... this.father ... this.mother
	// this.name ... this.eyeColor ... (this.father).generations() ... (this.mother).generations()

	// input: new Person( "Jo McCarthy", "blue", unknown, unknown ).generations()
	// output: 1
	// this.name = Jo
	// this.eyeColor = blue
	// this.father = unknown
	// this.father.generations() = 0
	// this.mother = unknown
	// this.mother.generations() = 0
	//return 1;

	// input: new Person( "Jay McCarthy", "blue", jaysDad, jaysMum ).generations()
	// output: 3
	// this.name = Jay
	// this.eyeColor = blue
	// this.father = jaysDad
	// this.father.generations() = 2
	// this.mother = jaysMum
	// this.mother.generations() = 1
	return 1 + Math.max( this.father.generations(), this.mother.generations() );
    }

    // names : FamilyTree -> ListOfString
    public ListOfString names ( ) {
	// this.name ... this.eyeColor ... this.father ... this.mother
	// this.name ... this.eyeColor ... (this.father).names() ... (this.mother).names()

	// input: new Person( "Jo McCarthy", "blue", unknown, unknown ).names()
	// output: new ConsLOS ( "Jo McCarthy", new EmptyLOS () )
	// this.name = Jo
	// this.eyeColor = blue
	// this.father = unknown
	// this.father.names() = !
	// this.mother = unknown
	// this.mother.names() = !
	//return new ConsLOS( this.name, new EmptyLOS () );

	// input: new Person( "Jay McCarthy", "blue", jaysDad, jaysMum ).names()
	// output: jay:jim:pam:joe:jo:!
	// this.name = Jay
	// this.eyeColor = blue
	// this.father = jaysDad
	// this.father.names() = jim:joe:jo:!
	// this.mother = jaysMum
	// this.mother.names() = pam:!
	return new ConsLOS( this.info.name, (this.father.names()).append(this.mother.names()) );
    }

    // oldestMatriarch : FamilyTree -> PersonRecord
    public PersonRecord oldestMatriarch ( ) {
	// this.info ... this.father ... this.mother
	// this.father.oldestMatriarch()
	// this.mother.oldestMatriarch()

	if ( this.mother.countPeople() == 0 ) {
	// jo.oldestMatriarch()
	// return: joPR
	// this.info = joPR
	return this.info ;
	} else {
	// jay.oldestMatriarch()
	// this.info = jayPR
	return this.mother.oldestMatriarch();
	}
    }

}

class hw {
    public static void main ( String[] args ) {
	FamilyTree unknown = new MissingPerson () ;
	PersonRecord joe = new PersonRecord( "Joe McCarthy", "brown" ); 
	FamilyTree grandPaMcC = new Person( joe, unknown, unknown );
	PersonRecord joPR = new PersonRecord( "Jo McCarthy", "blue" );
	FamilyTree jo = new Person( joPR, unknown, unknown );
	PersonRecord jim = new PersonRecord( "Jim McCarthy", "brown" );
	FamilyTree jaysDad = new Person( jim, grandPaMcC, jo );
	PersonRecord pam = new PersonRecord( "Pam McCarthy", "brown" );
	FamilyTree jaysMum = new Person( pam, unknown, unknown );
	PersonRecord jayPR = new PersonRecord( "Jay McCarthy", "blue" );
	FamilyTree jay = new Person( jayPR, jaysDad, jaysMum );

	System.out.format("The answer is%n   %s%n", jay);

	System.out.format("The answer is%n   %b%nbut should be%n   %b%n",
			  unknown.hasBluedEyedAncestors(),
			  false );
	System.out.format("The answer is%n   %b%nbut should be%n   %b%n",
			  jaysMum.hasBluedEyedAncestors(),
			  false );
	System.out.format("The answer is%n   %b%nbut should be%n   %b%n",
			  jo.hasBluedEyedAncestors(),
			  true );
	System.out.format("The answer is%n   %b%nbut should be%n   %b%n",
			  jaysDad.hasBluedEyedAncestors(),
			  true );
	System.out.format("The answer is%n   %b%nbut should be%n   %b%n",
			  jay.hasBluedEyedAncestors(),
			  true );
	// ASCII / UTF-8

	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  unknown.countPeople(),
			  0 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  jo.countPeople(),
			  1 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  jay.countPeople(),
			  5 );

	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  new Person( jayPR, jaysDad, jaysMum ).countPeople(),
			  5 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  1 + jaysDad.countPeople() + jaysMum.countPeople(),
			  5 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  1 + new Person( jim, grandPaMcC, jo ).countPeople() + jaysMum.countPeople(),
			  5 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  1 + 1 + grandPaMcC.countPeople() + jo.countPeople() + jaysMum.countPeople(),
			  5 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  1 + 1 + new Person( joe, unknown, unknown ).countPeople() + jo.countPeople() + jaysMum.countPeople(),
			  5 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  1 + 1 + 1 + unknown.countPeople() + unknown.countPeople() + jo.countPeople() + jaysMum.countPeople(),
			  5 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  1 + 1 + 1 + 0 + 0 + jo.countPeople() + jaysMum.countPeople(),
			  5 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  1 + 1 + 1 + 0 + 0 + 1 + 0 + 0 + jaysMum.countPeople(),
			  5 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  1 + 1 + 1 + 0 + 0 + 1 + 0 + 0 + 1 + 0 + 0,
			  5 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  1 + (1 + (1 + 0 + 0) + (1 + 0 + 0)) + (1 + 0 + 0),
			  5 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  5,
			  5 );

	// List a = 1:2:3:9:10:!; = a
	// a.sum() = 1+2+3+9+10+0 = 25
	// sum( ! ) = 0
	// sum( e : l ) = identity(e) + sum(l)

	// f(x) = isEvenHuh(x)
	// a.containsEvenHuh() = f(1) || f(2) || f(3) || f(9) || f(10) || false
	// containsEvenHuh( ! ) = false
	// containsEvenHuh( e : l ) = isEvenHuh(e) || containsEvenHuh(l)

	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  unknown.generations(),
			  0 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  jo.generations(),
			  1 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  jay.generations(),
			  3 );

	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  new Person( jayPR, jaysDad, jaysMum ).generations(),
			  3 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  1 + Math.max( jaysDad.generations(), jaysMum.generations() ),
			  3 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  1 + Math.max( new Person( jim, grandPaMcC, jo ).generations(), jaysMum.generations() ),
			  3 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  1 + Math.max( (1 + Math.max(grandPaMcC.generations(), jo.generations())), jaysMum.generations() ),
			  3 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  1 + Math.max( (1 + Math.max(new Person( joe, unknown, unknown ).generations(), jo.generations())), jaysMum.generations() ),
			  3 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  1 + Math.max( (1 + Math.max( (1 + Math.max( unknown.generations(), unknown.generations() ) ), jo.generations())), jaysMum.generations() ),
			  3 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  1 + Math.max( (1 + Math.max( (1 + Math.max( 0, 0 ) ), jo.generations())), jaysMum.generations() ),
			  3 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  1 + Math.max( (1 + Math.max( (1 + Math.max( 0, 0 ) ), (1 + Math.max( 0, 0 ) ))), (1 + Math.max( 0, 0 ) ) ),
			  3 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  1 + Math.max( (1 + Math.max( (1 + 0 ), (1 + 0 ))), (1 + 0 ) ),
			  3 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  1 + Math.max( (1 + Math.max( 1, 1)), 1 ),
			  3 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  1 + Math.max( (1 + 1), 1 ),
			  3 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  1 + Math.max( 2, 1 ),
			  3 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  1 + 2,
			  3 );
	System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
			  3,
			  3 );

	System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
			  unknown.names(),
			  new EmptyLOS () );
	System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
			  jo.names(),
			  new ConsLOS ( "Jo McCarthy", new EmptyLOS () ) );
	System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
			  jay.names(),
			  new ConsLOS ( "Jay McCarthy", new ConsLOS ( "Jim McCarthy", new ConsLOS ("Joe McCarthy", new ConsLOS ( "Jo McCarthy", new ConsLOS ( "Pam McCarthy", new EmptyLOS () ) ) ) ) ) );

	// jay.names()
	// Jay McC : append( dad.names() , mom.names() )
	// Jay McC : append( Jim : append( gdad.names(), gma.names() ) , mom.names() )
	// Jay McC : append( Jim : append( Joe : unknown.names(), gma.names() ) , mom.names() )
	// Jay McC : append( Jim : append( Joe : !, gma.names() ) , mom.names() )
	// Jay McC : append( Jim : Joe : gma.names() , mom.names() )
	// Jay McC : append( Jim : Joe : Jo : ! , mom.names() )
	// Jay McC : Jim : Joe : Jo : mom.names()
	// Jay McC : Jim : Joe : Jo : Pam : append( unknown.names(), unknown.names() )
	// Jay McC : Jim : Joe : Jo : Pam : append( !, unknown.names() )
	// Jay McC : Jim : Joe : Jo : Pam : unknown.names() 
	// Jay McC : Jim : Joe : Jo : Pam : ! 

	System.out.format("The answer is%n   %b%nbut should be%n   %b%n",
			  unknown.hasProperBluedEyedAncestors(),
			  false );
	System.out.format("The answer is%n   %b%nbut should be%n   %b%n",
			  jaysMum.hasProperBluedEyedAncestors(),
			  false );
	System.out.format("The answer is%n   %b%nbut should be%n   %b%n",
			  jo.hasProperBluedEyedAncestors(),
			  false );
	System.out.format("The answer is%n   %b%nbut should be%n   %b%n",
			  jaysDad.hasProperBluedEyedAncestors(),
			  true );
	System.out.format("The answer is%n   %b%nbut should be%n   %b%n",
			  jay.hasProperBluedEyedAncestors(),
			  true );


	System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
			  unknown.oldestMatriarch(),
			  new PersonRecord( "No person", "No eye color" ) );
	System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
			  jaysMum.oldestMatriarch(),
			  pam );
	System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
			  jo.oldestMatriarch(),
			  joPR );
	System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
			  grandPaMcC.oldestMatriarch(),
			  joe
			  // new PersonRecord( "No person", "No eye color" ) 
			  );
	System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
			  jaysDad.oldestMatriarch(),
			  joPR );
	System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
			  jay.oldestMatriarch(),
			  pam );

	//
	BST mt = new MtBST () ;
	BST two = new Node ( 2, "ni", mt, mt );
	BST seven = new Node ( 7, "nana", mt, mt );
	BST five = new Node ( 5, "go", two, seven );
	BST twelve = new Node ( 12, "jyuuni", mt, mt );
	BST huge = new Node ( 100000000, "oku", twelve, mt );
	BST ten = new Node ( 10, "jyuu", five, huge );

	System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
			  mt.searchBST(2, "mu"),
			  "mu" );
	System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
			  ten.searchBST(2, "mu"),
			  "ni" );
	System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
			  two.searchBST(2, "mu"),
			  "ni" );
	System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
			  ten.searchBST(12, "mu"),
			  "jyuuni" );


	System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
			  ten.inorder(),
			  "ni:go:nana:jyuu:jyuuni:oku:!" );
    }
}

// A BST is either a
//  - MtBST
//  - Node
interface BST {
    // searchBST : BST int String -> String
    public String searchBST ( int key, String missing ) ;
    // inorder : BST -> ListOfString
    public ListOfString inorder ( );
}

// A MtBST is a
//  new MtBST ()
// where
class MtBST implements BST {

    public MtBST ( ) {
    }

    // searchBST : BST int String -> String
    public String searchBST ( int key, String missing ) {
	return missing ;
    }

    // inorder : BST -> ListOfString
    public ListOfString inorder ( ) {
	return new EmptyLOS () ;
    }

}

// A Node is a
//  new Node ( key, value, left, right )
// where
//  key is a int
//  value is a String
//  left is a BST[all keys are less than 'key']
//  right is a BST[all keys are greater than 'key']
class Node implements BST {
    public int key;
    public String value;
    public BST left;
    public BST right;

    public Node ( int key0, String value0, BST left0, BST right0 ) {
	key = key0;
	value = value0;
	left = left0;
	right = right0;
    }

    // searchBST : BST int String -> String
    public String searchBST ( int key, String missing ) {
	// this.key ... this.value .. this.left .. this.right
	// this.left.searchBST(key, missing)
	// this.right.searchBST(key, missing)

	if ( this.key == key ) {
	    return this.value;
	} else if ( key < this.key ) {
	    return this.left.searchBST(key, missing);
	} else /* if ( this.key < key ) */ {
	    return this.right.searchBST(key, missing);
	}
    }

    // inorder : BST -> ListOfString
    public ListOfString inorder ( ) {
	// this.key .. this.value .. this.left ... this.right
	// this.value = jyuu
	// this.left.inorder() = ni:go:nana:!
	// this.right.inorder() = jyuuni:oku:!
	// jyuu:jyuuni:oku:! = new ConsLOS( this.value , this.right.inorder() )
	return this.left.inorder().append( new ConsLOS( this.value , this.right.inorder() ) );
    }	
}

