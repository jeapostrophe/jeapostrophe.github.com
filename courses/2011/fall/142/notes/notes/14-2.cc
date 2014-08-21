#include <stdio.h>
#include <math.h>
#include <string.h>

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

// A PersonInfo is a
//  new PersonInfo( name, bloodtype, eyes)
// where
//  name is a string
//  bloodtype is a string
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

  // dyeEyes : PersonInfo string string -> PersonInfo
  PersonInfo* dyeEyes ( const char* from, const char* to ) {
    // Template: this, this->name, this->eyes

    if ( streq(this->eyes, from) ) {
    // Example
    // this = (Justin, Dreamy)
    // this->name = Justin
    // this->eyes = Dreamy
    // from = Dreamy
    // to = Snake
    // return (Justin, Snake)
    //return (new PersonInfo("Justin", "Snake"));
    return (new PersonInfo(this->name, to));
    } else {
    // Example
    // this = (Jay, Adorable)
    // this->name = Jay
    // this->eyes = Adorable
    // from = Dreamy
    // to = Snake
    // return (Jay, Adorable)
    //return (new PersonInfo("Jay", "Adorable"));
    //return (new PersonInfo(this->name, this->eyes));
    return this;
    }
  }

};

// A FamilyTree is either
//   VisitTheFamilyHistoryCenter
//   Person
class FamilyTree {
public:
  virtual int show () = 0;
  // dyeEyes : tree string string -> tree
  // Purpose: change people who have x eyes to y eyes
  virtual FamilyTree* dyeEyes ( const char* from, const char* to ) = 0;
  // farthestBack : tree -> info
  // Purpose: return the information about the farthest back ancestor
  virtual PersonInfo* farthestBack ( ) = 0;
  virtual int depth () = 0;
};

// A VisitTheFamilyHistoryCenter is a
//  new VisitTheFamilyHistoryCenter()
// where
class VisitTheFamilyHistoryCenter : public FamilyTree {
public:

  VisitTheFamilyHistoryCenter () {}

  int show () {
    printf("new VisitTheFamilyHistoryCenter()");
    return 0;
  }

  // dyeEyes : tree string string -> tree
  // Purpose: change people who have x eyes to y eyes
  FamilyTree* dyeEyes ( const char* from, const char* to ) {
    // Template: this, from, to
    
    //return this;
    return (new VisitTheFamilyHistoryCenter());
  }

  // farthestBack : tree -> info
  // Purpose: return the information about the farthest back ancestor
  PersonInfo* farthestBack ( ) {
    // Template: this

    // Example:
    // this = <!>
    return (new PersonInfo("Eventual Eve", "Blue"));
  }

  int depth () { return 0; }
};

// A Person is a
//  new Person( info, mother, father )
// where
//  info is a PersonInfo
//  mother is a FamilyTree
//  father is a FamilyTree
class Person : public FamilyTree {
public:
  PersonInfo* info;
  FamilyTree* mother;
  FamilyTree* father;

  Person ( PersonInfo* info0, FamilyTree* mother0, FamilyTree* father0 ) {
    this->info = info0;
    this->mother = mother0;
    this->father = father0;
  }

  int show () {
    printf("new Person(");
    this->info->show();
    printf(", ");
    this->mother->show();
    printf(", ");
    this->father->show();
    printf(")");
    return 0;
  }

  int depth () {
    return 1 + max(this->father->depth(), this->mother->depth());
  }

  // dyeEyes : tree string string -> tree
  // Purpose: change people who have x eyes to y eyes
  FamilyTree* dyeEyes ( const char* from, const char* to ) {
    // Template: this, this->info, this->mother, this->father, from, to, this->mother->dyeEyes(...), this->father->dyeEyes(...)

    // Example
    // this = jbP
    // this->info = jbI
    // this->mother = patriciaP
    // this->father = jeremyP
    // from = Dreamy
    // to = Snake
    // this->mother->dyeEyes(from, to) = patriciaP
    // this->father->dyeEyes(from, to) = jeremyP
    // return (Snakey jbI, patriciaP, jeremyP)
    return (new Person(this->info->dyeEyes(from, to),
                       this->mother->dyeEyes(from, to),
                       this->father->dyeEyes(from, to)));
  }

  // farthestBack : tree -> info
  // Purpose: return the information about the farthest back ancestor
  PersonInfo* farthestBack ( ) {
    // Template: this, this->info, this->mother, this->father, this->mother->farthestBack(), this->father->farthestBack()
    
    if ( this->father->depth() == 0 && this->mother->depth() == 0 ){
      // this = dianneP
      // this->info = dianneI
      // this->mother = <!>
      // this->mother->farthestBack() = eveI
      // this->father = <!>
      // this->father->farthestBack() = eveI
      // return dianneI;
      return this->info;
    } else {
      if ( this->father->depth() <= this->mother->depth() ) {
        // Example:
        // this = jbP
        // this->info = jbI
        // this->mother = patriciaP
        // this->mother->farthestBack() = dianneI
        // this->father = jeremyP
        // this->father->farthestBack() = jeremyI
        // return dianneI;
        return this->mother->farthestBack();
      } else {
        // todo
        return this->father->farthestBack();
      }   
    }

    // this = patriciaP
    // this->info = patriciaI
    // this->mother = dianneP
    // this->mother->farthestBack() = dianneI
    // this->father = bruceP
    // this->father->farthestBack() = bruceI
    // return dianneI;
    //return this->mother->farthestBack();


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

  FamilyTree* nomore = new VisitTheFamilyHistoryCenter();

  PersonInfo* jeremyI = new PersonInfo("Jeremy Bieber", "Blue");
  FamilyTree* jeremyP = new Person(jeremyI, nomore, nomore);

  PersonInfo* dianneI = new PersonInfo("Dianne", "Brown");
  FamilyTree* dianneP = new Person(dianneI, nomore, nomore);

  PersonInfo* bruceI = new PersonInfo("Bruce", "Hazel");
  FamilyTree* bruceP = new Person(bruceI, nomore, nomore);

  PersonInfo* patriciaI = new PersonInfo("Patricia", "Brown");
  FamilyTree* patriciaP = new Person( patriciaI, dianneP, bruceP );

  PersonInfo* jbI = new PersonInfo("Justin", "Dreamy");
  FamilyTree* jbP = new Person( jbI, patriciaP, jeremyP);
  
  jbP->show();
  printf("\n");

  // dyeEyes : PersonInfo string string -> PersonInfo
  printf("The answer is\n  ");
  jbI->dyeEyes("Dreamy","Snake")->show();
  printf("\nbut should be\n  ");
  (new PersonInfo("Justin", "Snake"))->show();
  printf("\n");

  // dyeEyes : tree string string -> tree
  // Purpose: change people who have x eyes to y eyes
  printf("The answer is\n  ");
  jbP->dyeEyes("Dreamy","Snake")->show();
  printf("\nbut should be\n  ");
  (new Person( new PersonInfo("Justin", "Snake"), patriciaP, jeremyP))->show();
  printf("\n");

  // farthestBack : tree -> info
  // Purpose: return the information about the farthest back ancestor
  printf("The answer is\n  ");
  jbP->farthestBack()->show();
  printf("\nbut should be\n  ");
  dianneI->show();
  printf("\n");

  printf("The answer is\n  ");
  patriciaP->farthestBack()->show();
  printf("\nbut should be\n  ");
  dianneI->show();
  printf("\n");

  printf("The answer is\n  ");
  dianneP->farthestBack()->show();
  printf("\nbut should be\n  ");
  dianneI->show();
  printf("\n");

  return 0;
}

// XXX dyeEyes : tree string string -> tree
// XXX farthestBack : tree -> info

// Built-in C functions: strlen, strcmp, printf, +, %

// LDBL_MANT_DIG - fake? REAL
// cwicm - real? FAKE
// strtok - real? REAL (kinda)
// |= - real? REAL
// vaargs_init - fake? REAL
// %= - real? FAKE
// <#> - fake? FAKE
// ynot - fake? FAKE
// sprintf - fake? REAL
// ispunc - real? REAL
// lngjmp - real? FAKE, but its longjmp
// sscanf - real? REAL

// LIBRARY
