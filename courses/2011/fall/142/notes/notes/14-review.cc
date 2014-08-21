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

  // hasBloodType : PersonInfo string -> bool
  bool hasBloodType ( const char* target ) {
    return streq(this->bloodtype, target);
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
  // properBlueEyedAncestorsHuh : FamilyTree -> bool
  virtual bool properBlueEyedAncestorsHuh () = 0;
  // countType : FamilyTree string -> int
  virtual int countType (const char* target) = 0;
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

  // properBlueEyedAncestorsHuh : FamilyTree -> bool
  bool properBlueEyedAncestorsHuh () {
    // Template: this

    // Example
    // this = bot
    return false;
  }

  // countType : FamilyTree string -> int
  int countType (const char* target) {
    // Template: this, target

    // Example
    // this = <!>
    // target = A
    return 0;
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

  // properBlueEyedAncestorsHuh : FamilyTree -> bool
  bool properBlueEyedAncestorsHuh () {
    // Template: this, this->info, this->mother, this->father, this->mother->blueEyedAncestorsHuh(), this->father->blueEyedAncestorsHuh()
    
    // Example:
    // this = jbP
    // this->info = jbI (not blued eyed)
    // this->mother = patriciaP
    // this->mother->properBlueEyedAncestorsHuh() = false
    // this->mother->blueEyedAncestorsHuh() = false
    // this->father = jeremyP
    // this->father->properBlueEyedAncestorsHuh() = false
    // this->father->blueEyedAncestorsHuh() = true
    // return true
    //return this->father->blueEyedAncestorsHuh();

    // Example:
    // this = jeremyP
    // this->info = jeremyI (blued eyed)
    // this->mother = nomore
    // this->mother->properBlueEyedAncestorsHuh() = false
    // this->mother->blueEyedAncestorsHuh() = false
    // this->father = nomore
    // this->father->properBlueEyedAncestorsHuh() = false
    // this->father->blueEyedAncestorsHuh() = false
    // return false

    // Generalize
    return this->father->blueEyedAncestorsHuh() || this->mother->blueEyedAncestorsHuh();
  }

  // countType : FamilyTree string -> int
  int countType (const char* target) {
    // Template: this, this->info, this->father, this->mother, target, this->father->countType(..). this->mother->countType(..)

    // hasBloodType : PersonInfo string -> bool
    if ( this->info->hasBloodType(target) ){
      // Example
      // this = jbP
      // this->info = jbI (A)
      // this->father = jeremyP
      // this->mother = patriciaP
      // target = A
      // this->father->countType(target) = 0
      // this->mother->countType(target) = 1
      // return 2
      //return 2;
      //return 1 + 1;
      //return 1 + 1 + 0;
      return 1 + this->mother->countType(target) + this->father->countType(target);
    } else {
      // Example
      // this = jeremyP
      // this->info = jeremyI (O)
      // this->father = nomore
      // this->mother = nomore
      // target = A
      // this->father->countType(target) = 0
      // this->mother->countType(target) = 0
      // return 0
      return this->mother->countType(target) + this->father->countType(target);
    }
  }

};

// A BST is either
//   mtBST
//   nodeBST
class BST {
public:
  virtual int show () = 0;
  // searchBST : BST int string -> string
  virtual const char* searchBST ( int target, const char* missing) = 0;
  // inorder : BST -> ListOfString
  virtual ListOfColors* inorder ( ) = 0;
};

// A mtBST is a
//  new mtBST()
// where
class mtBST : public BST {
public:

  mtBST () {}

  int show () {
    printf("new mtBST()");
    return 0;
  }

  // searchBST : BST int string -> string
  const char* searchBST ( int target, const char* missing) {
    // Template: this, target, missing

    // Example
    // this = bot
    // target = 55
    // missing = Atlantis
    return missing;
  }

  // inorder : BST -> ListOfString
  ListOfColors* inorder ( ) {
    return (new EmptyCL ());
  }

};


// A nodeBST is a
//  new nodeBST( key, value, left, right )
// where
//  key is a int
//  value is a string
//  left is a BST
//  right is a BST
class nodeBST : public BST {
public:
  int key;
  const char* value;
  BST* left;
  BST* right;

  nodeBST ( int key0, const char* value0, BST* left0, BST* right0 ) {
    this->key = key0;
    this->value = value0;
    this->left = left0;
    this->right = right0;
  }

  int show () {
    printf("new nodeBST(%d, \"%s\", ", this->key, this->value);
    this->left->show();
    printf(", ");
    this->right->show();
    printf(")");
    return 0;
  }

  // searchBST : BST int string -> string
  const char* searchBST ( int target, const char* missing) {
    // Template: this, this->key, this->value, this->left->searchBST(..), this->right->searchBST(...), target, missing

    if ( this->key == target ) {
    // Example
    // this = aTree
    // this->key = 1
    // this->string = Jay
    // this->left = nomore
    // this->left->searchBST(target, missing) = missing
    // this->right = nomore
    // this->left->searchBST(target, missing) = missing
    // target = 1
    // missing = Atlantis
    //return "Jay";
    return this->value;
      
    } else {

      if ( target < this->key ){
    // Example
    // this = aTree
    // this->key = 8
    // this->string = Jerry
    // this->left = big
    // this->left->searchBST(target, missing) = "Jay"
    // this->right = nothing
    // target = 1
    // missing = Atlantis
    return this->left->searchBST(target, missing);

    // Example
    // this = aTree
    // this->key = 4
    // this->string = Harold
    // this->left = big
    // this->left->searchBST(target, missing) = "Jay"
    // this->right = also big
    // this->right->searchBST(target, missing) = missing
    // target = 1
    // missing = Atlantis
      } else {
    // Example
    // this = ...
    // this->key = 4
    // this->value = Harold
    // this->left = big
    // this->left->searchBST(target, missing) = missing
    // this->right = also big
    // this->right->searchBST(target, missing) = Yo
    // target = 6
    // missing = Atlantis
    return this->right->searchBST(target, missing);
      }
    }
  }

  // inorder : BST -> ListOfString
  ListOfColors* inorder ( ) {
    // Template: this, this->key, this->value, this->left, this->right

    // Example:
    // this = ...
    // this->key = 4
    // this->value = Harold
    // this->left = contains jay
    // this->left->inorder = jay : bro : lib : !
    // this->right = contains snay
    // this->right->inorder = snay : yo : trib : !
    // return jay : bro : lib : harold : snay : yo : trib : !
    //return jay : bro : lib : harold : this->right->inorder();
    //return jay : bro : lib : this->value : this->right->inorder();
    //return jay : bro : lib : (new OneMoreColor(this->value, this->right->inorder()));
    //return this->left->inorder() ++ (new OneMoreColor(this->value, this->right->inorder()));
    return this->left->inorder()->append(new OneMoreColor(this->value, this->right->inorder()));
    
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

  PersonInfo* dianneI = new PersonInfo("Dianne", "A", "Brown");
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

  // proper

  printf ( "The answer is %s, but should be %s\n",
           booleanToString(jeremyP->properBlueEyedAncestorsHuh()),
           booleanToString(false) ) ;

  printf ( "The answer is %s, but should be %s\n",
           booleanToString(patriciaP->properBlueEyedAncestorsHuh()),
           booleanToString(false) ) ;

  printf ( "The answer is %s, but should be %s\n",
           booleanToString(jbP->properBlueEyedAncestorsHuh()),
           booleanToString(true) ) ;


  // countType : FamilyTree string -> int
  printf ( "The answer is %d, but should be %d\n",
           (jbP->countType("A")),
           2 ) ;
  printf ( "The answer is %d, but should be %d\n",
           (jbP->countType("O+")),
           1 ) ;

  // BSTs

  BST* mt = (new mtBST());
  BST* aTree =
    (new nodeBST (8,
                  "Jerry",
                  (new nodeBST (4,
                                "Harold",
                                (new nodeBST(2, 
                                             "Bro",
                                             (new nodeBST( 1, "Jay", mt, mt)),
                                             (new nodeBST( 3, "Lib", mt, mt)))),
                                (new nodeBST(6, 
                                             "Yo",
                                             (new nodeBST( 5, "Snay", mt, mt)),
                                             (new nodeBST( 7, "Trib", mt, mt)))))),
                  (new mtBST())));

  printf("The answer is\n  ");
  aTree->show();
  printf("\nbut should be\n  ");
  aTree->show();
  printf("\n");

  // searchBST : BST int string -> string
  printf ( "The answer is %s, but should be %s\n",
           (aTree->searchBST(1, "Atlantis")),
           "Jay" ) ;
  printf ( "The answer is %s, but should be %s\n",
           (aTree->searchBST(6, "Atlantis")),
           "Yo" ) ;
  printf ( "The answer is %s, but should be %s\n",
           (aTree->searchBST(14, "Atlantis")),
           "Atlantis" ) ;

  // inorder : BST -> ListOfString
  printf("The answer is\n  ");
  aTree->inorder()->show();
  printf("\nbut should be\n  ");
  printf("Jay, Bro, Lib, Harold, Snay, Yo, Trib, Jerry : !");
  printf("\n");


  return 0;
}

