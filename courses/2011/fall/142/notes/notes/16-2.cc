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

// A ListofInfo is either
//  EmptyLOI
//  OneLOI
class ListOfInfo {
public:
  virtual int show () = 0;
  virtual ListOfInfo* append (ListOfInfo* atTheEnd) = 0;
};

// An EmptyLOI is a
//  new EmptyLOI()
// where
class EmptyLOI : public ListOfInfo {
public:

  EmptyLOI () {}

  int show () {
    printf("new EmptyLOI()");
    return 0;
  }

  ListOfInfo* append (ListOfInfo* atTheEnd) {
    return atTheEnd;
  }
};

// A PersonInfo is a
//  new PersonInfo( name, eyes, age, livingness )
// where
//  name is a string
//  eyes is a string
//  age is a int
//  livingness is a bool
class PersonInfo {
public:
  const char* name;
  const char* eyes;
  int age;
  bool livingness;

  PersonInfo(const char* name0, const char* eyes0, int age0, bool livingness0){
    this->name = name0;
    this->eyes = eyes0;
    this->age = age0;
    this->livingness = livingness0;
  }
  
  int show () {
    printf("new PersonInfo(\"%s\", \"%s\", %d, %s)",
           this->name,
           this->eyes,
           this->age,
           booleanToString(this->livingness));
    return 0;
  }

  PersonInfo* advanceAges () {
    // Template: this, this->name, this->eyes, this->age, this->livingness

    // ! this->livingness
    if ( this->livingness == false ){
    // Example
    // this = OndoherI
    // this->name = Ondoher
    // this->eyes = Bluey
    // this->age = 60
    // this->livingness = false
    // return OndoherI
    return this;
    } else {
    // Example
    // this = AragornI
    // this->name = Aragorn
    // this->eyes = Green
    // this->age = 20
    // this->livingness = true
    //return (new PersonInfo(Aragorn, Green, 21, true));
    //return (new PersonInfo(Aragorn, Green, 21, this->livingness));
    //return (new PersonInfo(this->name, Green, 21, this->livingness));
    //return (new PersonInfo(this->name, this->eyes, 21, this->livingness));
    return (new PersonInfo(this->name, this->eyes, this->age + 1, this->livingness));
    }
  }

};

// A OneLOI is a
//  new OneLOI (first, rest)
// where
//  first is a PersonInfo
//  rest is a ListOfInfo
class OneLOI : public ListOfInfo {
public:
  PersonInfo* first;
  ListOfInfo* rest;

  OneLOI( PersonInfo* first0, ListOfInfo* rest0 ) {
    this->first = first0;
    this->rest = rest0;
  }

  int show () {
    printf("new OneLOI(");
    this->first->show();
    printf(",");
    this->rest->show();
    printf(")");
    return 0;
  }

  ListOfInfo* append ( ListOfInfo* atTheEnd ) {
    return new OneLOI( this->first, this->rest->append(atTheEnd));
  }
};

// A ListOfDFTs is either
//  mtLoDFTs
//  oneDFT
class ListOfDFTs {
public:
  virtual int show () = 0;
  // advanceAges : ListOfDFTs -> ListOfDFTs
  // Purpose: to simulate time on this family forest (advanced living ages by 1)
  virtual ListOfDFTs* advanceAges () = 0;
};

// A DescendantFamilyTree (DFT) is
//  new Person( info, children )
// where
//  info is a PersonInfo
//  children is a ListOfDFTs
class Person {
public:
  PersonInfo* info;
  ListOfDFTs* children;

  Person ( PersonInfo* info0 ,
           ListOfDFTs* children0 ) {
    this->info = info0;
    this->children = children0;
  }

  int show () {
    printf("new Person(");
    this->info->show();
    printf(",");
    this->children->show();
    printf(")");
    return 0;
  }

  // advanceAges : Person -> Person
  // Purpose: to simulate time on this family tree (advanced living ages by 1)
  Person* advanceAges () {
    // Template: this, this->info, this->children, this->info->advanceAges(), this->children->advanceAges()

    // Example:
    // this = OndoherD
    // this->info = OndoherI
    // this->info->advanceAges() = OndoherI
    // this->children = OndoherK
    // this->children->advanceAges() = Ondoher's kids with new ages
    // return OndoherD's tree with updated kids
    return new Person(this->info->advanceAges(), this->children->advanceAges());
  }

};

// A mtLoDFTs is a
//  new mtLoDFTs ()
// where
class mtLoDFTs : public ListOfDFTs {
public:

  mtLoDFTs () {}
  
  int show () {
    printf ("new mtLoDFTs()");
  }

  // advanceAges : ListOfDFTs -> ListOfDFTs
  // Purpose: to simulate time on this family forest (advanced living ages by 1)
  ListOfDFTs* advanceAges () {
    return (new mtLoDFTs());
  }

};

// A oneDFT is a
//  new oneDFT( first, rest )
// where
//  first is a DFT
//  rest is a ListOfDFTs
class oneDFT : public ListOfDFTs {
public:
  Person* first;
  ListOfDFTs* rest;

  oneDFT( Person* first0, ListOfDFTs* rest0 ) {
    this->first = first0;
    this->rest = rest0;
  }
  
  int show () {
    printf("new oneDFT(");
    this->first->show();
    printf(",");
    this->rest->show();
    printf(")");
  }

  // advanceAges : ListOfDFTs -> ListOfDFTs
  // Purpose: to simulate time on this family forest (advanced living ages by 1)
  ListOfDFTs* advanceAges () {
    // Template: this, this->first, this->rest, this->first->advanceAges(), this->rest->advanceAges()

    return (new oneDFT(this->first->advanceAges(), this->rest->advanceAges()));
  }

};

// main : -> number
int main () {
  printf ( "The answer is %f, but should be %f\n",
           1.0/2.0,
           0.5 ) ;
  printf ( "C++ says %s\n",
           booleanToString(strcmp("Jay", "Libby") == 0)) ;
  printf ( "C++ says %s\n",
           booleanToString(streq("Jay", "Libby"))) ;

  PersonInfo* jbI = new PersonInfo("Jeremy Bieber", "Blue", 35, true);
  
  printf("The answer is\n  ");
  jbI->show();
  printf("\nbut should be\n  ");
  jbI->show();
  printf("\n");

  PersonInfo* OndoherI = new PersonInfo("Ondoher", "Blue", 60, false);
  PersonInfo* FirielI = new PersonInfo("Firiel", "Black", 30, false);
  PersonInfo* ArtamirI = new PersonInfo("Artamir", "Yellow", 28, true);
  PersonInfo* FaramirI = new PersonInfo("Faramir", "Blue", 25, false);
  PersonInfo* AragornI = new PersonInfo("Aragorn", "Green", 20, true);
  PersonInfo* EldarionI = new PersonInfo("Eldarion", "Blue", 1, true);

  Person* EldarionD = new Person( EldarionI, (new mtLoDFTs()));
  Person* ArtamirD = new Person( ArtamirI, (new mtLoDFTs()));
  Person* FaramirD = new Person( FaramirI, (new mtLoDFTs()));
  // Aragorn -> Eldarion
  Person* AragornD = new Person( AragornI, (new oneDFT( EldarionD, (new mtLoDFTs()))));
  // Firiel -> Aragorn
  Person* FirielD = new Person( FirielI, (new oneDFT( AragornD, (new mtLoDFTs()))));
  // Ondoher -> Firiel, Artamir, Faramir
  ListOfDFTs* mt = (new mtLoDFTs());
  ListOfDFTs* OndoherK3 = (new oneDFT( FaramirD, mt));
  ListOfDFTs* OndoherK2 = (new oneDFT( ArtamirD, OndoherK3));
  ListOfDFTs* OndoherK = (new oneDFT( FirielD, OndoherK2 ));
  Person* OndoherD = new Person( OndoherI, OndoherK );

  printf("The whole tree is\n  ");
  OndoherD->show();
  printf("\n");

  printf ( "The answer is %f, but should be %f\n",
           INFINITY,
           999.99 ) ;

  // advanceAges : Person -> Person
  // Purpose: to simulate time on this family tree (advanced living ages by 1)
  printf("The answer is\n  ");
  OndoherD->advanceAges()->show();
  printf("\nbut it should be\n  ");
  printf("Arty, Ely, and Ary are older (29,21,2)");
  printf("\n");


  return 0;
}

// advancedAges : Person -> Person
// kinds of data structures

// [!]Atomic === numbers, booleans, strings
// [!]Compound === multiple pieces of atomic
// Nested Compound === multiple pieces where some were compound
// [!]Mixed data === multiple kinds of compound data
//  ------ aside, C++ doesn't allow "Point or number"
//  ------ so, you can put a number inside a compound
//  ------ this is called "boxing"
// Self refential data === compound data that includes an instance of itself
//  ------ example is a list
//  ------ Name And Conquer
//  ------ trees are also example
// Mutually referential data === two self refential structures that refer to each other
//  ------ dfts are an example (person contains onedft contains person)
//  ------ it is possible to have A contains B contains C contains A
// Inductive data === data that has a smaller amount of data inside it

// 1 : 2 : 3 : 4 : !
// find what the sum is for: 2 : 3 : 4 : ! ====> 9
// 1 + 9 = 10
// informations goes from right to left (from smaller to larger)

// f(x) = 1 + x
// g(x,y) = x * y + 2

// g(f(4), f(7))
// g(5, 8) = 5 * 8 + 2 = 42
// g(f(4), f(7)) = g(5, f(7)) = 5 * f(7) + 2 = 5 * 8 + 2 = 42
// g(f(4), f(7)) = f(4) * f(7) + 2 = 5 * 8 + 2 = 42

/// here be dragons...

// Cyclic data === data that actually contains itself inside it
//  ------- programming is hard because you don't know when to stop
//  ------- so detect the cycle OR compute a fixed point

// Co-inductive data === falutin' math term
//  ------- data that has a bigger amount of data inside it

// Transfinite data === even more falutin'
//  ------- data that more data AND actually reaches infinity
