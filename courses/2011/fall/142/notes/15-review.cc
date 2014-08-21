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
//  new PersonInfo( name, eyes)
// where
//  name is a string
//  eyes is a string
class PersonInfo {
public:
  const char* name;
  const char* eyes;

  PersonInfo(const char* name0, const char* eyes0){
    this->name = name0;
    this->eyes = eyes0;
  }
  
  int show () {
    printf("new PersonInfo(\"%s\", \"%s\")",
           this->name,
           this->eyes);
    return 0;
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
  // truncate : ListOfDFTs nat -> ListOfDFTs
  virtual ListOfDFTs* truncate (int which) = 0;
  // generationDescendents : ListOfDFTs nat -> ListOfInfo
  virtual ListOfInfo* generationDescendents (int which) = 0;
  virtual int countProperDescendents () = 0;
  virtual int countDescendents () = 0;
};

// A mtLoDFTs is a
//  new mtLoDFTs ()
// where
class mtLoDFTs : public ListOfDFTs {
public:

  mtLoDFTs () {}
  
  int show () {
    printf ("new mtLoDFTS()");
  }

  // truncate : ListOfDFTs nat -> ListOfDFTs
  ListOfDFTs* truncate (int which) {
    return (new mtLoDFTs());
  }

  // generationDescendents : ListOfDFTs nat -> ListOfInfo
  ListOfInfo* generationDescendents (int which) {
    return (new EmptyLOI());
  }

  int countProperDescendents () { return 0; }
  int countDescendents () { return 0; }

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

  // countProperDescendents : Person -> nat
  int countProperDescendents () {
    // Template: this,  this->info, this->children, this->children->countProperDescendent()
    
    // Example
    // this = OndoherD
    // this->info = OndoherI
    // this->children = OndoherK
    // this->children->countProperDescendents() = 2
    // return 5
    // return SAF(this->children)
    return this->children->countDescendents();
  } 

  // countDescendents : Person -> nat
  int countDescendents () {
    // Template: this,  this->info, this->children, this->children->countDescendent()
    
    // Example
    // this = OndoherD
    // this->info = OndoherI
    // this->children = OndoherK
    // this->children->countDescendents() = 5
    // return 6
    return 1 + this->children->countDescendents();
  } 

  // truncate : Person nat -> Person
  Person* truncate ( int which ) {
    // Template: this, which, this->info, this->children, this->children->truncate(...)

    if ( which == 1 ) {
    // Example
    // this = Ondoher's tree
    // which = 1
    // this->info = Ondoher's info
    // this->children = his three kids
    // return just Ondoher's info (not his kids)
    return (new Person(this->info, (new mtLoDFTs())));
    } else {
    // Example
    // this = Ondoher's tree
    // which = 2
    // this->info = Ondoher's info
    // this->children = his three kids
    // this->children->truncate(1) = Firiel, Artamir, Faramir
    // this->children->truncate(2) = Firiel (and Aragorn, but not Eldarian), Artamir, Faramir
    // this->children->truncate(3) = Firiel (and Aragorn and Eldarian), Artamir, Faramir
    // return Ondoher and his three kids, but not his grand kids
    //return (new Person(this->info, this->children->truncate(1)));
    return (new Person(this->info, this->children->truncate(which - 1)));
    }
  }

  // generationDescendents : Person nat -> ListOfInfo
  ListOfInfo* generationDescendents ( int which ) {
    // Template: this, this->info, this->children, this->children->generationDescendents(...)

    if ( which != 0 ) {
    // Example
    // this = Ondoher's tree
    // which = 1
    // this->info = Ondoher's info
    // this->children = his three kids' trees
    // this->children->generationDescendents(2) =  Eldarian : !
    // this->children->generationDescendents(1) =  Aragorn : !
    // this->children->generationDescendents(0) =  Firiel, Artamir, Faramir : !
    // this->children->generationDescendents(which) =  Aragorn : !
    // return his three kids
    // return SAF(this->children)
    //return this->children->generationDescendents(0);
    return this->children->generationDescendents(which - 1);
    } else {
    // Example:
    // this = Ondoher's tree
    // which = 0
    // this->info = Ondoher's info
    // this->children = his three kids' trees
    // this->children->generationDescendents(2) =  Eldarian : !
    // this->children->generationDescendents(1) =  Aragorn : !
    // this->children->generationDescendents(0) =  Firiel, Artamir, Faramir : !
    // this->children->generationDescendents(which) = Firiel, Artamir, Faramir : !
    // return Ondoher : !
    return (new OneLOI(this->info, (new EmptyLOI())));
    }
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

  int countProperDescendents () { 
    return this->first->countProperDescendents() + this->rest->countProperDescendents();
  }
  int countDescendents () { 
    return this->first->countDescendents() + this->rest->countDescendents();
  }

  // truncate : ListOfDFTs nat -> ListOfDFTs
  ListOfDFTs* truncate (int which) {
    // Template: this, which, this->first, this->first->truncate(...), this->rest, this->rest->truncate(...)

    // Distinguish the last example
    if (which == 0) {
      return new mtLoDFTs();
    } else {
      return new oneDFT((this->first)->truncate(which), this->rest->truncate(which));
    }
  }

  // generationDescendents : ListOfDFTs nat -> ListOfInfo
  ListOfInfo* generationDescendents (int which) {
    // Template: this, this->first, this->rest, which, this->first->generationDescendents(...), this->rest->generationDescendents(...)

    // Example
    // this = OndoherK
    // this->first = FirielD
    // this->rest = OndoherK2
    // which = 0
    // this->first->generationDescendents(0) = FirielI : !
    // this->rest->generationDescendents(0) = Artamir : Faramir : !
    // return Firiel : Artamir : Faramir : !
    // return Firiel : this->rest->generationDescendents(0)
    // return this->first->generationDescendents(0) ++ this->rest->generationDescendents(0)
    //return this->first->generationDescendents(0)->append(this->rest->generationDescendents(0));
    return this->first->generationDescendents(which)->append(this->rest->generationDescendents(which));   
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

  PersonInfo* jbI = new PersonInfo("Jeremy Bieber", "Blue");
  
  printf("The answer is\n  ");
  jbI->show();
  printf("\nbut should be\n  ");
  jbI->show();
  printf("\n");

  PersonInfo* OndoherI = new PersonInfo("Ondoher", "Blue");
  PersonInfo* FirielI = new PersonInfo("Firiel", "Black");
  PersonInfo* ArtamirI = new PersonInfo("Artamir", "Yellow");
  PersonInfo* FaramirI = new PersonInfo("Faramir", "Blue");
  PersonInfo* AragornI = new PersonInfo("Aragorn", "Green");
  PersonInfo* EldarionI = new PersonInfo("Eldarion", "Blue");

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

  // Generation 0 = Joe (the person)
  // Generation 1 = Jim and Pat (kids)
  // Generation 2 = Jay, Joe, Kama, Kennedy, ...., (grand-kids)
  // Generation 3 = Frog, Peach, Kama's kids, new one (great grand-kids)
  // Generation 4 = none (great great grand-kids)

  // truncate : Person nat -> Person
  printf("The answer is\n  ");
  OndoherD->truncate(1)->show();
  printf("\nbut should be\n  ");
  printf("just Ondoher");
  printf("\n");

  printf("The answer is\n  ");
  OndoherD->truncate(2)->show();
  printf("\nbut should be\n  ");
  printf("Ondoher -> Firiel, Artamir, Faramir");
  printf("\n");

  printf("The answer is\n  ");
  OndoherK->truncate(0)->show();
  printf("\nbut should be\n  ");
  printf("mt");
  printf("\n");

  printf("The answer is\n  ");
  OndoherK->truncate(1)->show();
  printf("\nbut should be\n  ");
  printf("Firiel, Artamir, Faramir");
  printf("\n");

  // generationDescendents : Person nat -> ListOfInfo
  printf("The answer is\n  ");
  OndoherD->generationDescendents(1)->show();
  printf("\nbut should be\n  ");
  printf("Artamir, Faramir, Firiel");
  printf("\n");

  printf("The answer is\n  ");
  OndoherD->generationDescendents(0)->show();
  printf("\nbut should be\n  ");
  printf("Ondoher");
  printf("\n");

  // countProperDescendents : Person -> nat
  printf ( "The answer is %d, but should be %d\n",
           OndoherD->countProperDescendents(),
           5 ) ;
  // countProperDescendents : ListOfDFTs -> nat
  printf ( "The answer is %d, but should be %d\n",
           OndoherK->countProperDescendents(),
           2 ) ;

  return 0;
}
