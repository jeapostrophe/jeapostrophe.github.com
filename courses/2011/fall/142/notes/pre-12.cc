#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

// A InventoryRecord is a
//  new InventoryRecord ( name , price )
// where
//  name is a const char*
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
	// this->name : const char*
	// this->price : double
	return new InventoryRecord ( this->name , this->price * (1.0+rate) ) ;
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
  // priceOf : Inventory const char* -> double
  virtual double priceOf ( const char* name ) = 0;
  // expensive : Inventory -> Inventory[expensive]
  virtual Inventory* expensive ( ) = 0;
  // SmootHawley : Inventory -> Inventory
  virtual Inventory* SmootHawley () = 0;
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
    return printf( "!" );
  }

  // totalPrice : Inventory -> double
  double totalPrice ( ) {
	return 0.0;
  }

  // containsButzHuh : Inventory -> bool
  bool containsButzHuh ( ) {
	return false ;
  }

  // priceOf : Inventory const char* -> double
  double priceOf ( const char* name ) {
	// ... ... 
	return -1.0 ; // pick something that "always makes sense"
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
    printf(":" );
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

  // priceOf : Inventory const char* -> double
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


}
