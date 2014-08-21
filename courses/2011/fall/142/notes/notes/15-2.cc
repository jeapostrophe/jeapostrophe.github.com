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

  // countBlue : PersonInfo -> int
  // Purpose: returns 1 if the person has blue eyes and 0 otherwise
  int countBlue () {
    // Template: this, this->name, this->eyes

    if (streq(this->eyes, "Blue")) {
      return 1;
    } else {
      return 0;
    }
  }

  // blueEyes : PersonInfo -> ListOfInfo
  // Purpose: return an empty list if the person doesn't have blue eyes and return a singleton list contain this guy's info if she does
  // ListOfInfo* blueEyes () {
  //   if (streq(this->eyes, "Blue")) {
  //     return (new OneLOI(this, new EmptyLOI()));
  //   } else {
  //     return new EmptyLOI();
  //   }
  // }
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
  // countBlue : ListOfDFTs -> int
  // Purpose: to compute how many people in this list have blue eyes
  virtual int countBlue () = 0;
  virtual ListOfInfo* blueEyes() = 0;
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

  // blueEyes : Person -> ListOfInfo
  ListOfInfo* blueEyes () {
    // Template: this, this->info, this->children, this->children->blueEyes(), this->info->blueEyes()
    
    // Example:
    // this = Ondoher's family
    // this->info = Ondoher (blue)

    // blueEyes : PersonInfo -> ListOfInfo
    // Purpose: return an empty list if the person doesn't have blue eyes and return a singleton list contain this guy's info if she does
    // this->info->blueEyes() = Ondoher : !

    // this->children = some list
    // this->children->blueEyes() = Eldarian : Faramir : !

    if ( this->info->countBlue() == 1 ) {
      return (new OneLOI(this->info, this->children->blueEyes()));
    } else {
      return this->children->blueEyes();
    }
    
    // We can't actually write a blueEyes like this without using ugly
    // C++ stuff (forward references from lecture 13-2), so, we'll 
    // distinguish the cases here.

    //return this->info->blueEyes()->append(this->children->blueEyes());
  }

  // countBlue : Person -> int
  // Purpose: to compute how many descendants have blue eyes
  int countBlue () {
    // Template: this, this->info, this->children, this->children->countBlue(), this->info->countBlue()
    
    // Example:
    // this = Ondoher's family
    // this->info = Ondoher (blue)

    // countBlue : PersonInfo -> int
    // Purpose: returns 1 if the person has blue eyes and 0 otherwise
    // this->info->countBlue() = 1

    // this->children = some list
    
    // countBlue : ListOfDFTs -> int
    // Purpose: to compute how many people in this list have blue eyes
    // this->children->countBlue() = 2

    //return 3;
    //return 1 + 2;
    return this->info->countBlue() + this->children->countBlue();
  }

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

  // blueEyes : ListOfDFTs -> ListOfInfo
  ListOfInfo* blueEyes () {
    return (new EmptyLOI());
  }

  // countBlue : ListOfDFTs -> int
  // Purpose: to compute how many people in this list have blue eyes
  int countBlue () {
    return 0;
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

  // blueEyes : ListOfDFTs -> ListOfInfo
  ListOfInfo* blueEyes () {
    // Template: this, this->first, this->rest, this->rest->blueEyes(), this->first->blueEyes()

    // Example:
    // this = Aramir, Faramir, Boromir, !
    // this->first = Aramir
    // this->first->blueEyes() = Eladarion : !
    // this->rest = Faramir, Boromir, !
    // this->rest->blueEyes() = Faramir : !
    // return both guys;
    return this->first->blueEyes()->append(this->rest->blueEyes());
  }

  // countBlue : ListOfDFTs -> int
  // Purpose: to compute how many people in this list have blue eyes
  int countBlue () {
    // Template: this, this->first, this->rest, this->rest->countBlue(), this->first->countBlue()

    // Example:
    // this = Aramir, Faramir, Boromir, !
    // this->first = Aramir
    // this->first->countBlue() = 1
    // this->rest = Faramir, Boromir, !
    // this->rest->countBlue() = 1
    // return 2;
    //return 2;
    //return 1+1;
    return this->first->countBlue() + this->rest->countBlue();
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

  // N-ary (in contrast to BI-nary or U-nary)

  printf("The whole tree is\n  ");
  OndoherD->show();
  printf("\n");

  // countBlue : Person -> int
  // Purpose: to compute how many descendants have blue eyes
  printf ( "The answer is %d, but should be %d\n",
           OndoherD->countBlue(),
           3 ) ;
  printf ( "The answer is %d, but should be %d\n",
           (new Person( OndoherI, OndoherK ))->countBlue(),
           3 ) ;
  printf ( "The answer is %d, but should be %d\n",
           OndoherI->countBlue() + OndoherK->countBlue(),
           3 ) ;
  printf ( "The answer is %d, but should be %d\n",
           1 + OndoherK->countBlue(),
           3 ) ;
  printf ( "The answer is %d, but should be %d\n",
           1 + (new oneDFT( FirielD, OndoherK2 ))->countBlue(),
           3 ) ;
  printf ( "The answer is %d, but should be %d\n",
           1 + (FirielD->countBlue() + OndoherK2->countBlue()),
           3 ) ;
  printf ( "The answer is %d, but should be %d\n",
           1 + ((new Person( FirielI, (new oneDFT( AragornD, (new mtLoDFTs())))))->countBlue() + OndoherK2->countBlue()),
           3 ) ;
  printf ( "The answer is %d, but should be %d\n",
           1 + ((FirielI->countBlue() + (new oneDFT( AragornD, (new mtLoDFTs())))->countBlue()) + OndoherK2->countBlue()),
           3 ) ;
  printf ( "The answer is %d, but should be %d\n",
           1 + ((0 + (new oneDFT( AragornD, (new mtLoDFTs())))->countBlue()) + OndoherK2->countBlue()),
           3 ) ;
  printf ( "The answer is %d, but should be %d\n",
           1 + ((0 + (AragornD->countBlue() + mt->countBlue())) + OndoherK2->countBlue()),
           3 ) ;
  printf ( "The answer is %d, but should be %d\n",
           1 + ((0 + ((new Person( AragornI, (new oneDFT( EldarionD, (new mtLoDFTs())))))->countBlue() + mt->countBlue())) + OndoherK2->countBlue()),
           3 ) ;
  printf ( "The answer is %d, but should be %d\n",
           1 + ((0 + ((AragornI->countBlue() + (new oneDFT( EldarionD, (new mtLoDFTs())))->countBlue()) + mt->countBlue())) + OndoherK2->countBlue()),
           3 ) ;
  printf ( "The answer is %d, but should be %d\n",
           1 + ((0 + ((0 + (new oneDFT( EldarionD, (new mtLoDFTs())))->countBlue()) + mt->countBlue())) + OndoherK2->countBlue()),
           3 ) ;
  printf ( "The answer is %d, but should be %d\n",
           1 + ((0 + ((0 + (1 + 0)) + mt->countBlue())) + OndoherK2->countBlue()),
           3 ) ;
  printf ( "The answer is %d, but should be %d\n",
           1 + ((0 + ((0 + (1 + 0)) + 0)) + OndoherK2->countBlue()),
           3 ) ;
  printf ( "The answer is %d, but should be %d\n",
           1 + ((0 + ((0 + (1 + 0)) + 0)) + ((1 + 0) + (0 + 0) + 0)),
           3 ) ;
  printf ( "The answer is %d, but should be %d\n",
           3,
           3 ) ;

  // blueEyes : Person -> ListOfInfo
  printf("The answer is\n  ");
  OndoherD->blueEyes()->show();
  printf("\nbut should be\n  ");
  printf("Ondoher, Eladarion, Faramir\n");
  printf("\n");

  printf ( "The answer is %f, but should be %f\n",
           INFINITY,
           999.99 ) ;

  return 0;
}
