#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

// A InventoryRecord is a
//  new InventoryRecord ( name , price )
// where
//  name is a String
//  price is a double
class InventoryRecord {
public:
  const char* name;
  double price;

  InventoryRecord ( const char* name0, double price0 ) {
	name = name0;
	price = price0;
  }

  // print : InventoryRecord -> int
  int print () {
	return printf("%s ($%f)", this->name, this->price );
  }


  // isButzHuh : InventoryRecord -> bool
  bool isButzHuh ( ) {
	// this->name ... this->price ...
    return strcmp(this->name,("Butz Action Figure"))==0;
  }

  // isExpensiveHuh : InventoryRecord -> bool
  bool isExpensiveHuh () {
	// this->name ... this->price ...
	return this->price >= 1000.00 ;
  }

  // tax : InventoryRecord double -> InventoryRecord
  InventoryRecord* tax ( double rate ) {
	// this->name : String
	// this->price : double
	return new InventoryRecord ( this->name , this->price * (1.0+rate) ) ;
  }

  // isAnActionFigure : InventoryRecord -> bool
  bool isAnActionFigure () {
	// this->name ... this->price
    return strcmp(this->name,("Action Figure"))==0;
  }
};

// A Inventory is either
//  - EmptyInventory
//  - OneMoreItem
class Inventory {
public:
  // totalPrice : Inventory -> double
  virtual double totalPrice ( ) = 0;
  // containsButzHuh : Inventory -> bool
  virtual bool containsButzHuh ( ) = 0;
  // priceOf : Inventory String -> double
  virtual double priceOf ( const char* name ) = 0;
  // expensive : Inventory -> Inventory[expensive]
  virtual Inventory* expensive ( ) = 0;
  // SmootHawley : Inventory -> Inventory
  virtual Inventory* SmootHawley () = 0;
  // append : Inventory Inventory -> Inventory
  virtual Inventory* append ( Inventory* inv1 ) = 0;
  // reverse : Inventory -> Inventory
  virtual Inventory* reverse () = 0;
  // noMoreOtaku : Inventory -> Inventory
  virtual Inventory* noMoreOtaku () = 0;
  virtual int print () = 0;
};

// A EmptyInventory is a..
//  new EmptyInventory ( )
// where
class EmptyInventory : public Inventory {
public:
  EmptyInventory ( ) {
  }

  // print : EmptyInventory -> int
  int print () {
    return printf("!");
  }

  // totalPrice : Inventory -> double
  double totalPrice ( ) {
	return 0.0;
  }

  // containsButzHuh : Inventory -> bool
  bool containsButzHuh ( ) {
	return false ;
  }

  // priceOf : Inventory String -> double
  double priceOf ( const char* name ) {
	// ... ... 
	return -1.0 ; // pick something that always makes sense
	// return notTherePrice; // let the caller decide
  }

  // expensive : Inventory -> Inventory[expensive]
  Inventory* expensive ( ) {
	// ... ...
	return new EmptyInventory() ;
  }

  // SmootHawley : Inventory -> Inventory
  Inventory* SmootHawley ( ) {
	// ... ...
	return new EmptyInventory() ;
  }

  // append : Inventory Inventory -> Inventory
  Inventory* append ( Inventory* inv1 ) {
	return inv1;
  }

  // reverse : Inventory -> Inventory
  Inventory* reverse () {
	return new EmptyInventory ();
  }


  // noMoreOtaku : Inventory -> Inventory
  Inventory* noMoreOtaku () {
	// ... ...
	return new EmptyInventory ();
  }
};

// A OneMoreItem is a...
//  new OneMoreItem ( first, rest )
// where
//  first is a InventoryRecord
//  rest is a Inventory
class OneMoreItem : public Inventory {
public:
  InventoryRecord* first;
  Inventory* rest;

  OneMoreItem ( InventoryRecord* first0, Inventory* rest0 ) {
	first = first0;
	rest = rest0;
  }

  // print : OneMoreItem -> int
  int print () {
    this->first->print();
    printf(":");
    return this->rest->print();
  }

  // totalPrice : Inventory -> double
  double totalPrice ( ) {
	// this->first ... this->rest->totalPrice() ... this->rest
	// (this->first)->name (this->first)->price ... this->rest->totalPrice() ... this->rest
	/*
	  new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new EmptyInventory() )->totalPrice(),
	  500.00);

	  this->first == new InventoryRecord(this->first->name, this->first->price)
	  new OneMoreItem( this->first, this->rest )->totalPrice(),
	  this->rest->totalPrice() == 0.0
	  this->first->price + this->rest->totalPrice() );
	*/
	return this->first->price + this->rest->totalPrice() ; 
  }

  // containsButzHuh : Inventory -> bool
  bool containsButzHuh ( ) {
	// this->first ... this->rest ... this->rest->containsButzHuh() ...
	// either this->first is Butz or this->rest->containsButzHuh()
	// this->first->name->equals("Butz Action Figure") || this->rest->containsButzHuh()
	// this->first->isButzHuh() || this->rest->containsButzHuh()
	return this->first->isButzHuh() || this->rest->containsButzHuh() ;
  }

  // priceOf : Inventory String -> double
  double priceOf ( const char* someName ) {
	// this->first ... this->rest ... this->rest->priceOf(name) ...
	// ... this->first->name ... this->first->price ...
	if ( strcmp(((this->first)->name),(someName))==0 ) {
      return this->first->price; 
	} else {
      return this->rest->priceOf(someName);
	}
  }

  // expensive : Inventory -> Inventory[expensive]
  Inventory* expensive ( ) {
	// this->first : InventoryRecord
	// this->rest : Inventory
	// this->rest->expensive() : Inventory[expensive]
	if ( this->first->isExpensiveHuh() ) {
      return new OneMoreItem( this->first, this->rest->expensive() ) ;
	} else {
      return this->rest->expensive() ;
	}
  }

  // SmootHawley : Inventory -> Inventory
  Inventory* SmootHawley ( ) {
	// ... this->first ... this->rest ... this->rest->SmootHawley() ...
	// ... this->first->tax(0.1) ... this->rest ... this->rest->SmootHawley() ...
	return new OneMoreItem( this->first->tax(0.1) , this->rest->SmootHawley() );
  }

  // append : Inventory Inventory -> Inventory
  Inventory* append ( Inventory* inv1 ) {
	return new OneMoreItem(this->first, this->rest->append(inv1));
  }

  // reverse : Inventory -> Inventory
  Inventory* reverse () {
	// this->first this->rest this->rest->reverse()

	// input: new OneMoreNumber( 1, new EmtpyListOfNumbers () )->revese()
	// output: new OneMoreNumber( 1, new EmtpyListOfNumbers () )

	// input: new OneMoreNumber( this->first, this->rest )->revese()
	// this->first = 1
	// this->rest = !
	// this->rest->reverse() = !
	// output: new OneMoreNumber( 1, new EmtpyListOfNumbers () )
	// output: new OneMoreNumber( this->first, this->rest->reverse() )

	// ---

	// input: new OneMoreNumber( 1, new OneMoreNumber(2, new EmtpyListOfNumbers ()) )->revese()
	// output: new OneMoreNumber(2, new OneMoreNumber( 1, new EmtpyListOfNumbers () ) )
       
	// this->first = 1
	// this->rest = 2:!
	// this->rest->reverse() = 2:!
	// not output = 1:2:!
	// output = 2:1:! = f( this->first, this->rest->reverse() ) = f ( 1, 2:! )
	// output = this->rest->putAtEnd(this->first) = 2:! . putAtEnd(1) = 2:1:!
	// putAtEnd( l , e ) = append( l, e:! )
	// output = this->rest->append( new OneMoreNumber( this->first, new EmptyListOfNumbers () ) )

	// input: new OneMoreNumber( 1, new OneMoreNumber(2, new OneMoreNumber(3, new EmtpyListOfNumbers ())) )->revese()
	// output: new OneMoreNumber(3, new OneMoreNumber(2, new OneMoreNumber( 1, new EmtpyListOfNumbers () ) ) )
	// this->first = 1
	// this->rest = 2:3:!
	// this->rest->reverse() = 3:2:!
	// not output = 2:3:1:!
	// output = 3:2:1:!
	// output = this->rest->reverse()->append( new OneMoreNumber( this->first, new EmptyListOfNumbers () ) )
	return this->rest->reverse()->append(new OneMoreItem(this->first, new EmptyInventory()));
  }

  // noMoreOtaku : Inventory -> Inventory
  Inventory* noMoreOtaku () {
	// this->first ... this->rest ... this->rest->noMoreOtaku()
	if ( ! this->first->isAnActionFigure() ) {
      return new OneMoreItem( this->first , this->rest->noMoreOtaku() );
	} else {
      return this->rest->noMoreOtaku() ;
	}
  }

};

// A FamilyTree is either
//  MissingPerson
//  Person
class FamilyTree {
public:
  // hasBluedEyedAncestors : FamilyTree -> bool
  virtual bool hasBluedEyedAncestors ( ) = 0;
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
	return printf("..->hF");
  }

  // hasBluedEyedAncestors : FamilyTree -> bool
  bool hasBluedEyedAncestors ( ) {
	// ... ...
	return false ;
  }
};

// A Person is a...
//  new Person( name, eyeColor, father, mother )
// where
//  name is a String
//  eyeColor is a String
//  father is a FamilyTree
//  mother is a FamilyTree
class Person : public FamilyTree {
public:
  const char* name;
  const char* eyeColor;
  FamilyTree* father;
  FamilyTree* mother;

  Person ( const char* name0, const char* eyeColor0, FamilyTree* father0, FamilyTree* mother0 ) {
	name = name0;
	eyeColor = eyeColor0;
	father = father0;
	mother = mother0;
  }

  int print () {
    printf("%s (%s):\n",
           this->name,
           this->eyeColor );
    this->father->print();
    return this->mother->print();
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
	// return : this->eyeColor->equals("blue") || false || false = true
	return (strcmp((this->eyeColor),("blue"))==0 || this->father->hasBluedEyedAncestors()) || this->mother->hasBluedEyedAncestors() ;
	
  }
};

int main ( ) {
  Inventory* mt = new EmptyInventory();
  InventoryRecord* kefka = new InventoryRecord("Kefka Action Figure", 500.00);
  Inventory* andKefka = new OneMoreItem( kefka, mt );
  InventoryRecord* butz = new InventoryRecord("Butz Action Figure", 1000.00);
  Inventory* andButz = new OneMoreItem( butz, andKefka );
  Inventory* andLightning = new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), andButz );

  printf("The answer is %d, but should be %d\n",
         mt->print(),
         (new EmptyInventory())->print());
  printf("The answer is %d\n",
         andKefka->print());
  printf("The answer is %d\n",
         andButz->print());
  printf("The answer is %d\n",
         andLightning->print());

  printf("The answer is %f, but should be %f\n",
         andKefka->totalPrice(),
         500.00);
  printf("The answer is %f, but should be %f\n",
         (new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new EmptyInventory() ))->totalPrice(),
         500.00);
  printf("The answer is %f, but should be %f\n",
         andLightning->totalPrice(),
         1550.00);

  printf("The answer is %d, but should be %d\n",
         kefka->isButzHuh(),
         false);
  printf("The answer is %d, but should be %d\n",
         butz->isButzHuh(),
         true);

  printf("The answer is %d, but should be %d\n",
         mt->containsButzHuh(),
         false);
  printf("The answer is %d, but should be %d\n",
         andKefka->containsButzHuh(),
         false);
  printf("The answer is %d, but should be %d\n",
         andButz->containsButzHuh(),
         true);
  printf("The answer is %d, but should be %d\n",
         andLightning->containsButzHuh(),
         true);

  printf("The answer is %f, but should be %f\n",
         andLightning->priceOf("Kefka Action Figure"),
         500.00);
  printf("The answer is %f, but should be %f\n",
         andLightning->priceOf("Butz Action Figure"),
         1000.00);


  printf("The answer is %d, but should be %d\n",
         andLightning->expensive()->print(),
         (new OneMoreItem(butz,mt))->print() );

  printf("The answer is %d, but should be %d\n",
         butz->tax(0.1)->print(),
         (new InventoryRecord( "Butz Action Figure", 1100.00 ))->print() );

  printf("The answer is %d, but should be %d\n",
         andLightning->SmootHawley()->print(),
         (new OneMoreItem( new InventoryRecord("Lightning Action Figure", 55.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1100.00), new OneMoreItem( new InventoryRecord("Kefka Action Figure", 550.00), mt ) ) ))->print() );

  // L1 = a:b:c:...:!
  // L2 = alpha:beta:gamma:...:!

  // append ( mt, k ) = k
  // append ( e:l, k ) = e : append(l,k) 

  printf("The answer is %d, but should be %d\n",
         andLightning->append(mt)->print(),
         andLightning->print() );
  printf("The answer is %d, but should be %d\n",
         mt->append(andLightning)->print(),
         andLightning->print() );
  printf("The answer is %d, but should be %d\n",
         andLightning->append(andLightning)->print(),
         (new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), mt ) ) ) ) ) ))->print() );

  // What if I want to reverse a list?
  // reverse : List -> List

  printf("The answer is %d, but should be %d\n",
         andLightning->reverse()->print(),
         (new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), mt ) ) ))->print() );

  printf("The answer is %d, but should be %d\n",
         (new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), mt ) ) ))->reverse()->print(),
         (new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), mt ) ) ))->print() );

  printf("The answer is %d, but should be %d\n",
         (new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), mt ) ))->reverse()->append(new OneMoreItem(new InventoryRecord("Lightning Action Figure", 50.00), new EmptyInventory()))->print(),
         (new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), mt ) ) ))->print() );


  printf("The answer is %d, but should be %d\n",
         ((new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), mt ))->reverse()->append(new OneMoreItem(new InventoryRecord("Butz Action Figure", 1000.00), new EmptyInventory())))->append(new OneMoreItem(new InventoryRecord("Lightning Action Figure", 50.00), new EmptyInventory()))->print(),
         (new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), mt ) ) ))->print() );

  printf("The answer is %d, but should be %d\n",
         ((mt->reverse()->append(new OneMoreItem(new InventoryRecord("Kefka Action Figure", 500.00), new EmptyInventory())))->append(new OneMoreItem(new InventoryRecord("Butz Action Figure", 1000.00), new EmptyInventory())))->append(new OneMoreItem(new InventoryRecord("Lightning Action Figure", 50.00), new EmptyInventory()))->print(),
         (new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), mt ) ) ))->print() );

  printf("The answer is %d, but should be %d\n",
         (((new EmptyInventory ())->append(new OneMoreItem(new InventoryRecord("Kefka Action Figure", 500.00), new EmptyInventory())))->append(new OneMoreItem(new InventoryRecord("Butz Action Figure", 1000.00), new EmptyInventory())))->append(new OneMoreItem(new InventoryRecord("Lightning Action Figure", 50.00), new EmptyInventory()))->print(),
         (new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), mt ) ) ))->print() );

  printf("The answer is %d, but should be %d\n",
         (new OneMoreItem(new InventoryRecord("Kefka Action Figure", 500.00), new EmptyInventory()))->append(new OneMoreItem(new InventoryRecord("Butz Action Figure", 1000.00), new EmptyInventory()))->append(new OneMoreItem(new InventoryRecord("Lightning Action Figure", 50.00), new EmptyInventory()))->print(),
         (new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), mt ) ) ))->print() );

  printf("The answer is %d, but should be %d\n",
         ((new OneMoreItem(new InventoryRecord("Kefka Action Figure", 500.00), (new EmptyInventory())->append((new OneMoreItem(new InventoryRecord("Butz Action Figure", 1000.00), new EmptyInventory()))))))->append(new OneMoreItem(new InventoryRecord("Lightning Action Figure", 50.00), new EmptyInventory()))->print(),
         (new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), mt ) ) ))->print() );

  printf("The answer is %d, but should be %d\n",
         (new OneMoreItem(new InventoryRecord("Kefka Action Figure", 500.00), (new OneMoreItem(new InventoryRecord("Butz Action Figure", 1000.00), new EmptyInventory()))))->append(new OneMoreItem(new InventoryRecord("Lightning Action Figure", 50.00), new EmptyInventory()))->print(),
         (new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), mt ) ) ))->print() );

  printf("The answer is %d, but should be %d\n",
         (new OneMoreItem(new InventoryRecord("Kefka Action Figure", 500.00), (new OneMoreItem(new InventoryRecord("Butz Action Figure", 1000.00), (new EmptyInventory())->append(new OneMoreItem(new InventoryRecord("Lightning Action Figure", 50.00), new EmptyInventory()))))))->print(),
         (new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), mt ) ) ))->print() );

  printf("The answer is %d, but should be %d\n",
         (new OneMoreItem(new InventoryRecord("Kefka Action Figure", 500.00), (new OneMoreItem(new InventoryRecord("Butz Action Figure", 1000.00), (new OneMoreItem(new InventoryRecord("Lightning Action Figure", 50.00), new EmptyInventory()))))))->print(),
         (new OneMoreItem( new InventoryRecord("Kefka Action Figure", 500.00), new OneMoreItem( new InventoryRecord("Butz Action Figure", 1000.00), new OneMoreItem( new InventoryRecord("Lightning Action Figure", 50.00), mt ) ) ))->print() );


  /// noMoreOtaku
  printf("The answer is %d, but should be %d\n",
         mt->noMoreOtaku()->print(),
         mt->print() );
  printf("The answer is %d, but should be %d\n",
         andLightning->noMoreOtaku()->print(),
         mt->print() );
  printf("The answer is %d, but should be %d\n",
         (new OneMoreItem( new InventoryRecord("Football", 12.00), andLightning))->noMoreOtaku()->print(),
         (new OneMoreItem( new InventoryRecord("Football", 12.00), mt ))->print() );

  // Tree stuff

  FamilyTree* unknown = new MissingPerson () ;
  FamilyTree* grandPaMcC = new Person( "Joe McCarthy", "brown", unknown, unknown );
  FamilyTree* jo = new Person( "Jo McCarthy", "blue", unknown, unknown );
  FamilyTree* jaysDad = new Person( "Jim McCarthy", "brown", grandPaMcC, jo );
  FamilyTree* jaysMum = new Person( "Pam McCarthy", "brown", unknown, unknown );
  FamilyTree* jay = new Person( "Jay McCarthy", "blue", jaysDad, jaysMum );

  printf("The answer is %d\n", jay->print());

  printf("The answer is %d but should be %d\n",
         unknown->hasBluedEyedAncestors(),
         false );
  printf("The answer is %d but should be %d\n",
         jaysMum->hasBluedEyedAncestors(),
         false );
  printf("The answer is %d but should be %d\n",
         jo->hasBluedEyedAncestors(),
         true );
  printf("The answer is %d but should be %d\n",
         jaysDad->hasBluedEyedAncestors(),
         true );
  printf("The answer is %d but should be %d\n",
         jay->hasBluedEyedAncestors(),
         true );

}

