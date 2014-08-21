#include <stdio.h>
#include <math.h>
#include <string.h>

// max : int int -> int
// Purpose: returns the bigger of the two integers
int max (int x , int y ) {
  if ( x > y ) {
    return x;
  } else {
    return y;
  }
}

// booleanToString : boolean -> string
// Purpose: convert a boolean into a string for printing
const char* booleanToString ( bool it ) {
  if ( it ) { return "true"; } else { return "false"; }
}

// streq : string string -> boolean
// Purpose: compares to strings for equality, use this rather than == to compare strings
bool streq ( const char* l, const char* r ) {
  return strcmp(l,r) == 0;
}

// An Entry is a
//  new Entry(name, number)
// where
//  name is a string
//  number is a string
class Entry {
public:
  const char* name;
  const char* number;

  Entry (const char* name0, const char* number0) {
    this->name = name0;
    this->number = number0;
  }

  int show () {
    printf("new Entry(%s,%s)", 
           this->name,
           this->number);
    return 0;
  }

  bool isThisYourNameHuh(const char* target) {
    return streq(this->name, target);
  }
  const char* whatsYaNumba () {
    return this->number;
  }

};

// A ListofEntry is either
//  EmptyLOE
//  OneLOE
class ListOfEntry {
public:
  virtual int show () = 0;
  virtual const char* lookup (const char* name) = 0;
};

// An EmptyLOE is a
//  new EmptyLOE()
// where
class EmptyLOE : public ListOfEntry {
public:

  EmptyLOE () {}

  int show () {
    printf("new EmptyLOE()");
    return 0;
  }

  const char* lookup (const char* name) {
    return "Not in this book";
  }
};

// A OneLOE is a
//  new OneLOE (first, rest)
// where
//  first is a int
//  rest is a ListOfEntry
class OneLOE : public ListOfEntry {
public:
  Entry* first;
  ListOfEntry* rest;

  OneLOE( Entry* first0, ListOfEntry* rest0 ) {
    this->first = first0;
    this->rest = rest0;
  }

  int show () {
    printf("new OneLOE(");
    this->first->show();
    printf(",");
    this->rest->show();
    printf(")");
    return 0;
  }

  const char* lookup (const char* name) {
    if (this->first->isThisYourNameHuh(name)) {
      return this->first->whatsYaNumba();
    } else {
      return this->rest->lookup(name);
    }
  }
};

// addEntry : LOE Entry -> LOE
ListOfEntry* addEntry (ListOfEntry* orig, Entry* newGuy) {
  return new OneLOE(newGuy, orig);
}

// The single address
ListOfEntry* LBB = new EmptyLOE();

// theLookup : name -> phone number
const char* theLookup(const char* name) {
  // Template: name, LBB

  // Example:
  // name = Snow
  // LBB = !
  // return "Not in this book"
  //return "Not in this book";

  // Example:
  // name = Snow
  // LBB = SnowE : LightningE : !
  // return "777-542-1452"
  return LBB->lookup(name);
}

// theAddEntry : Entry -> nothing
void theAddEntry ( Entry* newGuy ) {
  // Template: LBB, newGuy

  // Example
  // >>>Before
  // LBB = LightingE : !
  // >>>Arguments
  // newGuy = SnowE
  // >>Effect
  // LBB = SnowE : LightningE : !
  // LBB = addEntry( LBB, SnowE ); 

  // Mutation like this is called...
  //  .... assignment
  //  .... setting, setAddressBook
  //  .... set-bang, or set!
  //  .... modifying variables
  LBB = addEntry( LBB, newGuy ); 
  // >>>Answer
  // return
  return ;

  // Example:
  // >>BEFORE
  // LBB = SnowE : LightningE : !
  // >>ARGUMENTS
  // newGuy = FangE
  // >>EFFECT
  // LBB = addEntry( LBB, newGuy ); 
  // >>ANSWER
  // return
}

// theShow : nothing -> nothing
void theShow () {
  LBB->show();
  return ;
}

// main : -> number
int main () {
  printf ( "The answer is %f, but should be %f\n",
           1.0/2.0,
           0.5 ) ;
  printf ( "C++ says %s\n",
           booleanToString(strcmp("Jay", "Libby") == 0)) ;
  printf ( "C++ says %s\n",
           booleanToString(streq("Jay", "Libby"))) ;

  // PhoneBooks
  ListOfEntry* mtpb = new EmptyLOE();
  ListOfEntry* lightning = new OneLOE(new Entry("Lightning", "777-123-1452"), mtpb);
  Entry* snowE = new Entry("Snow", "777-542-1452");
  ListOfEntry* snow = new OneLOE(snowE, lightning);

  printf("The answer is %s, but should be %s\n",
         snow->lookup("Lightning"),
         "777-123-1452");

  printf("The answer is\n  ");
  snow->show();
  printf("\nbut should be\n  ");
  printf("Snow : Lighting : !");
  printf("\n");

  printf("The answer is %s, but should be %s\n",
         addEntry(lightning, snowE)->lookup("Snow"),
         "777-542-1452");

  printf("The answer is %s, but should be %s\n",
         lightning->lookup("Snow"),
         "Not in this book");


  // Basic mutation
  int x = 4;

  x = 6;

  // PhoneBooks
  Entry* newlightningE = new Entry("Lightning", "777-123-1452");
  Entry* newsnowE = new Entry("Snow", "777-542-1452");

  // lookup : PhoneBook name -> number
  // theLookup : name -> number

  theAddEntry(newlightningE);

  printf("The answer is %s, but should be %s\n",
         theLookup("Snow"),
         "Not in this book");

  // addEntry : PhoneBook Entry -> PhoneBook
  //addEntry(lightning, snowE);

  // theAddEntry : Entry -> nothing
  theAddEntry(newsnowE);

  printf("The answer is %s, but should be %s\n",
         theLookup("Snow"),
         "777-542-1452");

  printf("The answer is %s, but should be %s\n",
         theLookup("Lightning"),
         "777-123-1452");

  printf("The answer is\n  ");
  theShow();
  printf("\nbut should be\n  ");
  printf("Snow : Lighting : !");
  printf("\n");

  return 0;
}

// address book
// stop light

// A Function means
//  f(x) = y
//  f(x) = z
//  then y = z
// Only internal mutation is "function"-like

