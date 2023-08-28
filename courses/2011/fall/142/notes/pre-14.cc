#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

int max ( int a , int b ) {
  if (a > b) {
    return a;
  } else {
    return b;
  }
}

// A Number is either
//  Zero
//  Successor of another number

// A ListOfString is either
//  Empty
//  Cons
class ListOfString {
public:
  // append : ListOfString ListOfString -> ListOfString
  virtual ListOfString* append ( ListOfString* after ) = 0 ;
  virtual int print () = 0;
};

// An EmptyLOS is a
//   new EmptyLOS ()
// where
class EmptyLOS : public ListOfString {
public:
  EmptyLOS ( ) {
  }
  int print ( ) {
    return printf("!");
  }
  // append : ListOfString ListOfString -> ListOfString
  ListOfString* append ( ListOfString* after ) {
	return after;
  }
};

// A ConsLOS is a
//  new ConsLOS ( first, rest )
// where
//  first is a String
//  rest is a ListOfString
class ConsLOS : public ListOfString {
public:
  const char* first;
  ListOfString* rest;

  ConsLOS ( const char* first0, ListOfString* rest0 ) {
	first = first0;
	rest = rest0;
  }

  int print () {
    printf("%s:", this->first);
    return this->rest->print();
  }

  // append : ListOfString ListOfString -> ListOfString
  ListOfString* append ( ListOfString* after ) {
	return new ConsLOS ( this->first, this->rest->append(after) );
  }
};

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
class FamilyTree {
public:
  // hasBluedEyedAncestors : FamilyTree -> boolean
  virtual bool hasBluedEyedAncestors ( ) = 0;
  // countPeople : FamilyTree -> int
  virtual int countPeople ( ) = 0;
  // generations : FamilyTree -> int
  virtual int generations ( ) = 0;
  // names : FamilyTree -> ListOfString
  virtual ListOfString* names ( ) = 0;
  virtual int print () = 0;
};

// A MissingPerson is a...
//  new MissingPerson ()
// where
class MissingPerson : public FamilyTree {
public:
  MissingPerson () {
  }

  int print () {
    return printf("new MissingPerson ()");
  }

  // hasBluedEyedAncestors : FamilyTree -> bool
  bool hasBluedEyedAncestors ( ) {
	// ... ...
	return false ;
  }

  // countPeople : FamilyTree -> int
  int countPeople ( ) {
	// ... ...
	return 0;
  }

  // generations : FamilyTree -> int
  int generations ( ) {
	// ... ...
	return 0;
  }

  // names : FamilyTree -> ListOfString
  ListOfString* names ( ) {
	// ... ...
	return new EmptyLOS () ;
  }
};

// A PersonRecord is a...
//  new PersonRecord( name, eyeColor )
// where
//  name is a String
//  eyeColor is a String
class PersonRecord {
public:
  const char* name;
  const char* eyeColor;

  PersonRecord ( const char* name0, const char* eyeColor0 ) {
	name = name0;
	eyeColor = eyeColor0;
  }
  int print () {
	return printf("new PersonRecord( \"%s\", \"%s\" )",
                  this->name,
                  this->eyeColor );
  }
};

// A Person is a...
//  new Person( info, father, mother )
// where
//  info is a PersonRecord
//  father is a FamilyTree
//  mother is a FamilyTree
class Person : public FamilyTree {
public:
  PersonRecord* info;
  FamilyTree* father;
  FamilyTree* mother;

  Person ( PersonRecord* info0, FamilyTree* father0, FamilyTree* mother0 ) {
	info = info0;
	father = father0;
	mother = mother0;
  }

  /*
    return template ( Input in ... ) {
	// this->name ... this->eyeColor ... this->father ... this->mother ... in ...
	// this->name ... this->eyeColor ... this->father->template() ... this->mother->template() ... in ...
	return .... ;
    }
  */

  int print () {
    printf("new Person( ");
    this->info->print();
    printf(", ");
    this->father->print();
    printf(", ");
    this->mother->print();
    return printf(")");
  }

  // hasBluedEyedAncestors : FamilyTree -> bool
  bool hasBluedEyedAncestors ( ) {
	// ... this->name ... this->eyeColor ... this->father ... this->father->hasBluedEyedAncestors() ... this->mother ... this->mother->hasBluedEyedAncestors()

	// input: new Person( "Pam McCarthy", "brown", unknown, unknown )->hasBluedEyedAncestors(),
	// this->name = Pam
	// this->eyeColor = brown
	// this->father = unknown
	// this->father->hasBluedEyedAncestors() = false
	// this->mother = unknown
	// this->mother->hasBluedEyedAncestors() = false
	// output: false because BOTH don't have blueEyed...
	// return ! ( ! this->father->hasBluedEyedAncestors() ) && ( ! this->mother->hasBluedEyedAncestors() ) ;
	// return : this->father->hasBluedEyedAncestors() || this->mother->hasBluedEyedAncestors() ;

	// De Morgan's Laws:
	//  (A /\ B) == ! ( ! A \/ ! B )
	//  (A \/ B) == ! ( ! A /\ ! B ) <-- we used this one!
	// return this->father->hasBluedEyedAncestors() || this->mother->hasBluedEyedAncestors() ;

	// input: new Person( "Jim McCarthy", "brown", new Person( "Joe McCarthy", "brown", unknown, unknown ), new Person( "Jo McCarthy", "blue", unknown, unknown ) )->hasBluedEyedAncestors(),

	// this->name = Jim ...
	// this->eyeColor = brown
	// this->father = new Person( "Joe McCarthy", "brown", unknown, unknown )
	// this->father->hasBluedEyedAncestors() = false
	// this->mother = new Person( "Jo McCarthy", "blue", unknown, unknown )
	// this->mother->hasBluedEyedAncestors() = true

	// output: true
	// return : this->father->hasBluedEyedAncestors() || this->mother->hasBluedEyedAncestors() ;
	// return : false || false || this->mother->hasBluedEyedAncestors() ;
	// return : false || true ;
	// return : true ;
	// return this->father->hasBluedEyedAncestors() || this->mother->hasBluedEyedAncestors() ;


	// input: new Person( "Jo McCarthy", "blue", unknown, unknown )->hasBluedEyedAncestors(),
	// this->name = Jo
	// this->eyeColor = blue
	// this->father = unknown
	// this->father->hasBluedEyedAncestors() = false
	// this->mother = unknown
	// this->mother->hasBluedEyedAncestors() = false
	
	// output: true
	// not return : this->father->hasBluedEyedAncestors() || this->mother->hasBluedEyedAncestors() ;
	// not return : false || false = false
	// return : this->eyeColor.equals("blue") || false || false = true
	return ((strcmp(this->info->eyeColor, "blue") == 0) || this->father->hasBluedEyedAncestors()) || this->mother->hasBluedEyedAncestors() ;
  }

  // countPeople : FamilyTree -> int
  int countPeople ( ) {
	// this->name ... this->eyeColor ... this->father ... this->mother
	// this->name ... this->eyeColor ... (this->father)->countPeople() ... (this->mother)->countPeople()
	
	// input: new Person( "Jo McCarthy", "blue", unknown, unknown )->countPeople()
	// output: 1
	// this->name = Jo
	// this->eyeColor = blue
	// this->father = unknown
	// this->father->countPeople() = 0
	// this->mother = unknown
	// this->mother->countPeople() = 0
	// return 1;

	// input: new Person( "Jay McCarthy", "blue", jaysDad, jaysMum )->countPeople()
	// output: 5
	// this->name = Jay
	// this->eyeColor = blue
	// this->father = jaysDad
	// this->father->countPeople() = 3
	// this->mother = jaysMum
	// this->mother->countPeople() = 1
	return 1 + this->father->countPeople() + this->mother->countPeople() ;
  }

  // generations : FamilyTree -> int
  int generations ( ) {
	// this->name ... this->eyeColor ... this->father ... this->mother
	// this->name ... this->eyeColor ... (this->father)->generations() ... (this->mother)->generations()

	// input: new Person( "Jo McCarthy", "blue", unknown, unknown )->generations()
	// output: 1
	// this->name = Jo
	// this->eyeColor = blue
	// this->father = unknown
	// this->father->generations() = 0
	// this->mother = unknown
	// this->mother->generations() = 0
	//return 1;

	// input: new Person( "Jay McCarthy", "blue", jaysDad, jaysMum )->generations()
	// output: 3
	// this->name = Jay
	// this->eyeColor = blue
	// this->father = jaysDad
	// this->father->generations() = 2
	// this->mother = jaysMum
	// this->mother->generations() = 1
	return 1 + max( this->father->generations(), this->mother->generations() );
  }

  // names : FamilyTree -> ListOfString
  ListOfString* names ( ) {
	// this->name ... this->eyeColor ... this->father ... this->mother
	// this->name ... this->eyeColor ... (this->father)->names() ... (this->mother)->names()

	// input: new Person( "Jo McCarthy", "blue", unknown, unknown )->names()
	// output: new ConsLOS ( "Jo McCarthy", new EmptyLOS () )
	// this->name = Jo
	// this->eyeColor = blue
	// this->father = unknown
	// this->father->names() = !
	// this->mother = unknown
	// this->mother->names() = !
	//return new ConsLOS( this->name, new EmptyLOS () );

	// input: new Person( "Jay McCarthy", "blue", jaysDad, jaysMum )->names()
	// output: jay:jim:pam:joe:jo:!
	// this->name = Jay
	// this->eyeColor = blue
	// this->father = jaysDad
	// this->father->names() = jim:joe:jo:!
	// this->mother = jaysMum
	// this->mother->names() = pam:!
	return new ConsLOS( this->info->name, (this->father->names())->append(this->mother->names()) );
  }
};

int main ( ) {
  FamilyTree* unknown = new MissingPerson () ;
  PersonRecord* joe = new PersonRecord( "Joe McCarthy", "brown" ); 
  FamilyTree* grandPaMcC = new Person( joe, unknown, unknown );
  PersonRecord* joPR = new PersonRecord( "Jo McCarthy", "blue" );
  FamilyTree* jo = new Person( joPR, unknown, unknown );
  PersonRecord* jim = new PersonRecord( "Jim McCarthy", "brown" );
  FamilyTree* jaysDad = new Person( jim, grandPaMcC, jo );
  PersonRecord* pam = new PersonRecord( "Pam McCarthy", "brown" );
  FamilyTree* jaysMum = new Person( pam, unknown, unknown );
  PersonRecord* jayPR = new PersonRecord( "Jay McCarthy", "blue" );
  FamilyTree* jay = new Person( jayPR, jaysDad, jaysMum );

  printf("The answer is\n   %d\n", jay->print());

  printf("The answer is\n   %d\nbut should be\n   %d\n",
         unknown->hasBluedEyedAncestors(),
         false );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         jaysMum->hasBluedEyedAncestors(),
         false );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         jo->hasBluedEyedAncestors(),
         true );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         jaysDad->hasBluedEyedAncestors(),
         true );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         jay->hasBluedEyedAncestors(),
         true );
  // ASCII / UTF-8

  printf("The answer is\n   %d\nbut should be\n   %d\n",
         unknown->countPeople(),
         0 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         jo->countPeople(),
         1 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         jay->countPeople(),
         5 );

  printf("The answer is\n   %d\nbut should be\n   %d\n",
         (new Person( jayPR, jaysDad, jaysMum ))->countPeople(),
         5 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         1 + jaysDad->countPeople() + jaysMum->countPeople(),
         5 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         1 + (new Person( jim, grandPaMcC, jo ))->countPeople() + jaysMum->countPeople(),
         5 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         1 + 1 + grandPaMcC->countPeople() + jo->countPeople() + jaysMum->countPeople(),
         5 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         1 + 1 + (new Person( joe, unknown, unknown ))->countPeople() + jo->countPeople() + jaysMum->countPeople(),
         5 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         1 + 1 + 1 + unknown->countPeople() + unknown->countPeople() + jo->countPeople() + jaysMum->countPeople(),
         5 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         1 + 1 + 1 + 0 + 0 + jo->countPeople() + jaysMum->countPeople(),
         5 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         1 + 1 + 1 + 0 + 0 + 1 + 0 + 0 + jaysMum->countPeople(),
         5 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         1 + 1 + 1 + 0 + 0 + 1 + 0 + 0 + 1 + 0 + 0,
         5 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         1 + (1 + (1 + 0 + 0) + (1 + 0 + 0)) + (1 + 0 + 0),
         5 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         5,
         5 );

  // List a = 1:2:3:9:10:!; = a
  // a.sum() = 1+2+3+9+10+0 = 25
  // sum( ! ) = 0
  // sum( e : l ) = identity(e) + sum(l)

  // f(x) = isEvenHuh(x)
  // a->containsEvenHuh() = f(1) || f(2) || f(3) || f(9) || f(10) || false
  // containsEvenHuh( ! ) = false
  // containsEvenHuh( e : l ) = isEvenHuh(e) || containsEvenHuh(l)

  printf("The answer is\n   %d\nbut should be\n   %d\n",
         unknown->generations(),
         0 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         jo->generations(),
         1 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         jay->generations(),
         3 );

  printf("The answer is\n   %d\nbut should be\n   %d\n",
         (new Person( jayPR, jaysDad, jaysMum ))->generations(),
         3 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         1 + max( jaysDad->generations(), jaysMum->generations() ),
         3 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         1 + max( (new Person( jim, grandPaMcC, jo ))->generations(), jaysMum->generations() ),
         3 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         1 + max( (1 + max(grandPaMcC->generations(), jo->generations())), jaysMum->generations() ),
         3 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         1 + max( (1 + max((new Person( joe, unknown, unknown ))->generations(), jo->generations())), jaysMum->generations() ),
         3 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         1 + max( (1 + max( (1 + max( unknown->generations(), unknown->generations() ) ), jo->generations())), jaysMum->generations() ),
         3 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         1 + max( (1 + max( (1 + max( 0, 0 ) ), jo->generations())), jaysMum->generations() ),
         3 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         1 + max( (1 + max( (1 + max( 0, 0 ) ), (1 + max( 0, 0 ) ))), (1 + max( 0, 0 ) ) ),
         3 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         1 + max( (1 + max( (1 + 0 ), (1 + 0 ))), (1 + 0 ) ),
         3 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         1 + max( (1 + max( 1, 1)), 1 ),
         3 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         1 + max( (1 + 1), 1 ),
         3 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         1 + max( 2, 1 ),
         3 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         1 + 2,
         3 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         3,
         3 );

  printf("The answer is\n   %d\nbut should be\n   %d\n",
         unknown->names()->print(),
         (new EmptyLOS ())->print() );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         jo->names()->print(),
         (new ConsLOS ( "Jo McCarthy", new EmptyLOS () ))->print() );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         jay->names()->print(),
         (new ConsLOS ( "Jay McCarthy", new ConsLOS ( "Jim McCarthy", new ConsLOS ("Joe McCarthy", new ConsLOS ( "Jo McCarthy", new ConsLOS ( "Pam McCarthy", new EmptyLOS () ) ) ) ) ))->print() );

  // jay->names()
  // Jay McC : append( dad->names() , mom->names() )
  // Jay McC : append( Jim : append( gdad->names(), gma->names() ) , mom->names() )
  // Jay McC : append( Jim : append( Joe : unknown->names(), gma->names() ) , mom->names() )
  // Jay McC : append( Jim : append( Joe : !, gma->names() ) , mom->names() )
  // Jay McC : append( Jim : Joe : gma->names() , mom->names() )
  // Jay McC : append( Jim : Joe : Jo : ! , mom->names() )
  // Jay McC : Jim : Joe : Jo : mom->names()
  // Jay McC : Jim : Joe : Jo : Pam : append( unknown->names(), unknown->names() )
  // Jay McC : Jim : Joe : Jo : Pam : append( !, unknown->names() )
  // Jay McC : Jim : Joe : Jo : Pam : unknown->names() 
  // Jay McC : Jim : Joe : Jo : Pam : ! 

	

}
