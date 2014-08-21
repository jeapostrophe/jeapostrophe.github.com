#include <stdio.h>
#include <math.h>
#include <string.h>

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

class ListOfColors {
public:
  virtual int show () = 0;
  virtual ListOfColors* append ( ListOfColors* that ) = 0;
};
class OneMoreColor : public ListOfColors {
public:
  const char* first;
  ListOfColors* rest;

  OneMoreColor(const char* first0, ListOfColors* rest0) {
    this->first = first0;
    this->rest = rest0;
  }

  int show () {
    printf("new OneMoreColor(\"%s\",", this->first);
    this->rest->show();
    printf(")");
    return 0;
  }
  ListOfColors* append ( ListOfColors* that ) {
    return new OneMoreColor(this->first, this->rest->append(that));
  }
};
class EmptyCL : public ListOfColors {
public:

  EmptyCL () {  }
  int show () { 
    printf("new EmptyCL()"); 
    return 0;
  }
  ListOfColors* append ( ListOfColors* that ) {
    return that;
  }
};

// A PersonInfo is a
//  new PersonInfo( name, bloodtype, eyes)
// where
//  name is a string
//  bloodtype is a string
//  eyes is a string
class PersonInfo {
public:
  const char* name;
  const char* bloodtype;
  const char* eyes;

  PersonInfo(const char* name0, const char* bloodtype0, const char* eyes0){
    this->name = name0;
    this->bloodtype = bloodtype0;
    this->eyes = eyes0;
  }
  
  int show () {
    printf("new PersonInfo(\"%s\", \"%s\", \"%s\")",
           this->name,
           this->bloodtype,
           this->eyes);
    return 0;
  }

  // areYouBlueEyedHuh : PersonInfo -> bool
  bool areYouBlueEyedHuh () {
    // Template: this, this->name, this->bloodtype, this->eyes

    return streq(this->eyes, "Blue");
  }

  // addYourEyeColorToThis : PersonInfo ListOfColors -> ListOfColors
  ListOfColors* addYourEyeColorToThis ( ListOfColors* that ) {
    // XXX Jay is bad
    return new OneMoreColor(this->eyes, that);
  }
};

// A FamilyTree is either
//   VisitTheFamilyHistoryCenter
//   Person
class FamilyTree {
public:
  virtual int show () = 0;
  // blueEyedAncestorsHuh : FamilyTree -> bool
  // Purpose: return true if the given FamilyTree contains blued peoples
  virtual bool blueEyedAncestorsHuh () = 0;
  // eyeColors : FamilyTree -> ListOfColors
  virtual ListOfColors* eyeColors () = 0;
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

  // blueEyedAncestorsHuh : FamilyTree -> bool
  // Purpose: return true if the given FamilyTree contains blued peoples
  bool blueEyedAncestorsHuh () {
    // Template: this

    // Example
    // this = bot
    return false;
  }

  // eyeColors : FamilyTree -> ListOfColors
  ListOfColors* eyeColors () {
    // Template: this

    // Example
    // this = bot
    // return !
    return (new EmptyCL());
  }
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

  // blueEyedAncestorsHuh : FamilyTree -> bool
  // Purpose: return true if the given FamilyTree contains blued peoples
  bool blueEyedAncestorsHuh () {
    // Template: this, this->info, this->mother, this->father, this->mother->blueEyedAncestorsHuh(), this->father->blueEyedAncestorsHuh()
    
    // Example:
    // this = jbP
    // this->info = jbI (not blued eyed)
    // this->mother = patriciaP
    // this->mother->blueEyedAncestorsHuh() = false
    // this->father = jeremyP
    // this->father->blueEyedAncestorsHuh() = true
    // return true
    //return this->father->blueEyedAncestorsHuh();

    // Example:
    // this = jeremyP
    // this->info = jeremyI (blued eyed)
    // this->mother = nomore
    // this->mother->blueEyedAncestorsHuh() = false
    // this->father = nomore
    // this->father->blueEyedAncestorsHuh() = false
    // return true
    //return SAF(this->info);
    //return areYouBlueEyedHuh(this->info);
    //return this->info->areYouBlueEyedHuh();

    // Generalize
    return this->info->areYouBlueEyedHuh() || this->father->blueEyedAncestorsHuh() || this->mother->blueEyedAncestorsHuh();
  }

  // eyeColors : FamilyTree -> ListOfColors
  ListOfColors* eyeColors () {
    // Template: this, this->info, this->mother, this->father, this->mother->eyeColors(), this->father->eyeColors()

    // Example:
    // this = jbP
    // this->info = jbI (Dreamy)
    // this->mother = patriciaP
    // this->mother->eyeColors() = Brown:Brown:Hazel:!
    // this->father = jeremyP
    // this->father->blueEyedAncestorsHuh() = Blue:!
    // return Dreamy:Brown:Brown:Hazel:Blue:!
    // return Dreamy:append(Brown:Brown:Hazel:!, Blue:!)

    // append : List(X) List(X) -> List(X)
    // addYourEyeColorToThis : PersonInfo ListOfColors -> ListOfColors
    return this->info->addYourEyeColorToThis( this->mother->eyeColors()->append(this->father->eyeColors()) );

    // cmb : thing inside x f applied to the rest
    // cmb : thing inside x f applied to the rest on the left x f applied to the rest on right
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

  PersonInfo* jeremyI = new PersonInfo("Jeremy Bieber", "O", "Blue");
  FamilyTree* jeremyP = new Person(jeremyI, nomore, nomore);

  PersonInfo* dianneI = new PersonInfo("Dianne", "AB", "Brown");
  FamilyTree* dianneP = new Person(dianneI, nomore, nomore);

  PersonInfo* bruceI = new PersonInfo("Bruce", "O+", "Hazel");
  FamilyTree* bruceP = new Person(bruceI, nomore, nomore);

  PersonInfo* patriciaI = new PersonInfo("Patricia", "B", "Brown");
  FamilyTree* patriciaP = new Person( patriciaI, dianneP, bruceP );

  PersonInfo* jbI = new PersonInfo("Justin", "A", "Dreamy");
  FamilyTree* jbP = new Person( jbI, patriciaP, jeremyP);
  
  jbP->show();
  printf("\n");

  // blueEyedAncestorsHuh : FamilyTree -> bool
  // Purpose: return true if the given FamilyTree contains blued peoples
  printf ( "The answer is %s, but should be %s\n",
           booleanToString(jeremyP->blueEyedAncestorsHuh()),
           booleanToString(true) ) ;

  printf ( "The answer is %s, but should be %s\n",
           booleanToString(patriciaP->blueEyedAncestorsHuh()),
           booleanToString(false) ) ;

  printf ( "The answer is %s, but should be %s\n",
           booleanToString(jbP->blueEyedAncestorsHuh()),
           booleanToString(true) ) ;

  // eyeColors : FamilyTree -> ListOfStrings
  printf("The answer is\n  ");
  jbP->eyeColors()->show();
  printf("\nbut should be\n  ");
  (new OneMoreColor("Dreamy", new OneMoreColor("Brown", new OneMoreColor("Brown", new OneMoreColor("Hazel", new OneMoreColor("Blue", (new EmptyCL())))))))->show();
  printf("\n");

  return 0;
}

// XXX A child is a father, mother, name, blood type, eyes
// XXX blueEyedAncestorHuh : tree -> boolean
// XXX countPersons : tree -> int
// XXX eyeColors : tree -> ListOfString
