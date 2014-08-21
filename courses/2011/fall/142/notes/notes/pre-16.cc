#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

int max (int a, int b) {
  if(a > b) {
    return a;
  } else {
    return b;
  }
}

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

// A ListOfPersonRecord is either
//  Empty
//  Cons
class ListOfPersonRecord {
public:
  // append : ListOfPersonRecord ListOfPersonRecord -> ListOfPersonRecord
  virtual ListOfPersonRecord* append ( ListOfPersonRecord* after ) = 0;
  virtual int print () = 0;
};

// An EmptyLOPR is a
//   new EmptyLOPR ()
// where
class EmptyLOPR : public ListOfPersonRecord {
public:
  EmptyLOPR ( ) {
  }
  int print ( ) {
    return printf("!");
  }
  // append : ListOfPersonRecord ListOfPersonRecord -> ListOfPersonRecord
  ListOfPersonRecord* append ( ListOfPersonRecord* after ) {
    return after;
  }
};

// A ConsLOPR is a
//  new ConsLOPR ( first, rest )
// where
//  first is a PersonRecord
//  rest is a ListOfPersonRecord
class ConsLOPR : public ListOfPersonRecord {
public:
  PersonRecord* first;
  ListOfPersonRecord* rest;

  ConsLOPR ( PersonRecord* first0, ListOfPersonRecord* rest0 ) {
    first = first0;
    rest = rest0;
  }

  int print () {
    this->first->print();
    printf(":");
    return this->rest->print();
  }

  // append : ListOfPersonRecord ListOfPersonRecord -> ListOfPersonRecord
  ListOfPersonRecord* append ( ListOfPersonRecord* after ) {
    return new ConsLOPR ( this->first, this->rest->append(after) );
  }
};


// A ListOfDFT is either...
//  EmptyLDFT
//  ConsLDFT
class ListOfDFT {
public:
  // countGenerations : ListOfDFT -> int
  virtual int countGenerations ( ) = 0;
  // howFarRemoved : ListOfDFT -> int
  virtual int howFarRemoved ( ) = 0;
  // generationDescendants : ListOfDFT int -> ListOfPersonRecord
  virtual ListOfPersonRecord* generationDescendants ( int genNum ) = 0;
  virtual int print () = 0;
};

// A DFamilyTree is a...
//  new DFamilyTree ( info, children )
// where
//  info is a PersonRecord
//  children is a ListOfDFT
class DFamilyTree {
public:
  PersonRecord* info;
  ListOfDFT* children;

  DFamilyTree ( PersonRecord* info0, ListOfDFT* children0 ) {
    info = info0;
    children = children0;
  }

  int print () {
    printf("new DFamilyTree( ");
    this->info->print();
    printf(", ");
    this->children->print();
    return printf(")");
  }

  // countGenerations : DFamilyTree -> int
  int countGenerations ( ) {
    // this->info
    // this->children
    // this->children->countGenerations()

    // Example 1:
    //  printf("The answer is\n   %s\nbut should be\n   %s\n",
    //        johnT->countGenerations(),
    //        4 );
    // this->info = new PersonRecord ( "John LaBonte", "brown" )
    // this->children = new ConsLDFT( joT, new EmptyLDFT () )
    // this->children->countGenerations() = 3
    //return 4;

    // Example 2:
    //  printf("The answer is\n   %s\nbut should be\n   %s\n",
    //        joT->countGenerations(),
    //        3 );
    // this->info = jo
    // this->children = new ConsLDFT( jimT, new ConsLDFT( patT, new EmptyLDFT() ) )
    // this->children->countGenerations() = 2
    //return 3;

    // Generalize 1 & 2 to:
    return 1 + this->children->countGenerations();

  }

  // howFarRemoved : DFamilyTree -> int
  int howFarRemoved ( ) {
    // this->info
    // this->info->name
    // this->info->eyeColor
    // this->children
    // this->children->howFarRemoved()

    if ( strcmp(this->info->eyeColor, "blue") == 0 ) {
      // Example 1:
      //  printf("The answer is\n   %s\nbut should be\n   %s\n",
      //        jayT->howFarRemoved(),
      //        0 );
      // this->info->name = "Jay ..."
      // this->info->eyeColor = "blue"
      // this->children = !
      // this->children->howFarRemoved() = -1
      return 0;
    } else {
      if ( this->children->howFarRemoved() == -1 ) {
        // Example 2:
        //  printf("The answer is\n   %s\nbut should be\n   %s\n",
        //        kamaT->howFarRemoved(),
        //        -1 );
        // this->info->name = "Kama ..."
        // this->info->eyeColor = "chartreuse"
        // this->children = !
        // this->children->howFarRemoved() = -1
        return -1;
      } else {
        // Example 3:
        // printf("The answer is\n   %s\nbut should be\n   %s\n",
        //        jimT->howFarRemoved(),
        //        1 );
        // this->info->name = "Jim ..."
        // this->info->eyeColor = "brown"
        // this->children = jayT : kamaT : !
        // this->children->howFarRemoved() = 0
        //return 1;

        // Example 4:
        //  printf("The answer is\n   %s\nbut should be\n   %s\n",
        //        joT->howFarRemoved(),
        //        2 );
        // this->info->name = "Jo ..."
        // this->info->eyeColor = "brown"
        // this->children = jimT : patT : !
        // this->children->howFarRemoved() = 1
        //return 2;

        // Generalize 3 & 4:
        return this->children->howFarRemoved() + 1;
      }
    }
  }

  // generationDescendants : DFamilyTree int -> ListOfPersonRecord
  ListOfPersonRecord* generationDescendants ( int genNum ) {
    // genNum
    // this->info
    // this->children
    // this->children->generationDescendants( ... genNum ... )

    if ( genNum != 0 ) {
      // Example 1:
      // printf("The answer is\n   %s\nbut should be\n   %s\n",
      //        jayT->generationDescendants(17),
      //        new EmptyLOPR() );
      // genNum = 17
      // this->info = jay
      // this->children = !
      // this->children->generationDescendants( 0 ) = !
      // this->children->generationDescendants( 17 ) = !
      // this->children->generationDescendants( 10 ) = !
      // this->children->generationDescendants( 1024 ) = !
      // this->children->generationDescendants( ... genNum ... ) = !
      // return new EmptyLOPR();

      // Example 3:
      // printf("The answer is\n   %s\nbut should be\n   %s\n",
      //        jimT->generationDescendants(1),
      //        new ConsLOPR( jay, new ConsLOPR( kama, new EmptyLOPR() ) ) );
      // genNum = 1
      // this->info = jim
      // this->children = jayT : kamaT : !
      // this->children->generationDescendants( 0 ) = jay : kama : !
      // this->children->generationDescendants( 1 ) = !
      // this->children->generationDescendants( 2 ) = !
      // return this->children->generationDescendants( 0 );

      // Generalize 1 & 3:
      //return this->children->generationDescendants( 0 );

      // Example 4:
      // printf("The answer is\n   %s\nbut should be\n   %s\n",
      //        johnT->generationDescendants(3),
      //        new ConsLOPR( jay, new ConsLOPR( kama, new EmptyLOPR() ) ) );
      // genNum = 3
      // this->info = john
      // this->children = joT : !
      // this->children->generationDescendants( 0 ) = jo : !
      // this->children->generationDescendants( 1 ) = jim : pat : !
      // this->children->generationDescendants( 2 ) = jay : kama : !
      // this->children->generationDescendants( 3 or higher ) = !
      //return this->children->generationDescendants( 2 ) ;

      // Generalize (1 & 3) & 4
      return this->children->generationDescendants( genNum - 1 ) ;

    } else {
      // Example 2:
      // printf("The answer is\n   %s\nbut should be\n   %s\n",
      //        jayT->generationDescendants(0),
      //        new ConsLOPR( jay, new EmptyLOPR() ) );
      // genNum = 0
      // this->info = jay
      // this->children = !
      // this->children->generationDescendants( 17 ) = !
      // this->children->generationDescendants( 10 ) = !
      // this->children->generationDescendants( 1024 ) = !
      // this->children->generationDescendants( ... genNum ... ) = !
      return new ConsLOPR( this->info, new EmptyLOPR() ) ;
    }
  }
};


// A EmptyLDFT is a...
//  new EmptyLDFT ()
// where
class EmptyLDFT : public ListOfDFT {
public:
  EmptyLDFT () {
  }

  int print () {
    return printf("!");
  }

  // countGenerations : ListOfDFT -> int
  int countGenerations ( ) {
    // ... ...

    // Example 1:
    //  printf("The answer is\n   %s\nbut should be\n   %s\n",
    //        new EmptyLDFT()->countGenerations(),
    //        0 );
    return 0;
  }

  // howFarRemoved : ListOfDFT -> int
  int howFarRemoved ( ) {
    // ... ...

    // Example 1:
    //  printf("The answer is\n   %s\nbut should be\n   %s\n",
    //        new EmptyLDFT()->howFarRemoved(),
    //        -1 );
    return -1;
  }

  // generationDescendants : ListOfDFT int -> ListOfPersonRecord
  ListOfPersonRecord* generationDescendants ( int genNum ) {
    // ... genNum ...

    // Example 1:
    // printf("The answer is\n   %s\nbut should be\n   %s\n",
    //        new EmptyLDFT()->generationDescendants(1),
    //        new EmptyLOPR() );
    return new EmptyLOPR() ;
  }

};

// smallerOfOnesNotNegativeOne : int int -> int
// Purpose: to return the number that is smaller, between the two, but don't count -1, as smaller.
// Examples:
// smallerOfOnesNotNegativeOne( -1, 31 ) = 31
// smallerOfOnesNotNegativeOne( 4, 5 ) = 4
// smallerOfOnesNotNegativeOne( 7, 1 ) = 1
// smallerOfOnesNotNegativeOne( 0, -1 ) = 0
// smallerOfOnesNotNegativeOne( -1, 0 ) = 0
int smallerOfOnesNotNegativeOne ( int lhs, int rhs ) {
  if ( lhs == -1 ) {
    // smallerOfOnesNotNegativeOne( -1, 31 ) = 31
    return rhs;
    // smallerOfOnesNotNegativeOne( -1, 0 ) = 0
    // return rhs;
  } else {
    if ( lhs < rhs ) {
      // smallerOfOnesNotNegativeOne( 4, 5 ) = 4
      return lhs;
    } else {
      if ( rhs != -1 ) {
        // smallerOfOnesNotNegativeOne( 7, 1 ) = 1
        return rhs;
      } else {
        // smallerOfOnesNotNegativeOne( 0, -1 ) = 0
        return lhs;
      }
    }
  }
}

// A ConsLDFT is a...
//  new ConsLDFT( first, rest )
// where
//  first is DFamilyTree
//  rest is ListOfDFT
class ConsLDFT : public ListOfDFT {
public:
  DFamilyTree* first;
  ListOfDFT* rest;

  ConsLDFT ( DFamilyTree* first0, ListOfDFT* rest0 ) {
    first = first0;
    rest = rest0;
  }
  int print () {
    this->first->print();
    printf(":");
    return this->rest->print();
  }

  // countGenerations : ListOfDFT -> int
  int countGenerations ( ) {
    // this->first
    // this->rest
    // this->first->countGenerations()
    // this->rest->countGenerations()

    // Example 1:
    //  printf("The answer is\n   %s\nbut should be\n   %s\n",
    //        new ConsLDFT( kamaT, new EmptyLDFT () )->countGenerations(),
    //        1 );
    // this->first = kamaT
    // this->rest = !
    // this->first->countGenerations() = 1
    // this->rest->countGenerations() = 0
    //return 1;

    // Example 2:
    //          printf("The answer is\n   %s\nbut should be\n   %s\n",
    //        new ConsLDFT( jimT, new ConsLDFT( patT, new EmptyLDFT() ) )->countGenerations(),
    //        2 );
    // this->first = jimT
    // this->rest = new ConsLDFT( patT, new EmptyLDFT() )
    // this->first->countGenerations() = 2
    // this->rest->countGenerations() = 1
    //return 2;

    // Generalize 1 & 2:
    //return this->first->countGenerations() ;

    // Example 3:
    // printf("The answer is\n   %s\nbut should be\n   %s\n",
    //        new ConsLDFT( patT, new ConsLDFT( jimT, new EmptyLDFT() ) )->countGenerations(),
    //        2 );
    // this->first = patT
    // this->rest = new ConsLDFT( jimT, new EmptyLDFT() )
    // this->first->countGenerations() = 1
    // this->rest->countGenerations() = 2
    //return 2;
    //return this->rest->countGenerations();

    // Generalize (1 & 2) & 3:
    return max( this->first->countGenerations(), this->rest->countGenerations() );


  }

  // howFarRemoved : ListOfDFT -> int
  int howFarRemoved ( ) {
    // this->first
    // this->rest
    // this->first->howFarRemoved()
    // this->rest->howFarRemoved()

    // Example 1:
    // printf("The answer is\n   %s\nbut should be\n   %s\n",
    //        new ConsLDFT( kamaT, new EmptyLDFT () )->howFarRemoved(),
    //        -1 );
    // this->first = kamaT
    // this->rest = !
    // this->first->howFarRemoved() = -1
    // this->rest->howFarRemoved() = -1
    //return -1;

    // Example 2:
    // printf("The answer is\n   %s\nbut should be\n   %s\n",
    //        new ConsLDFT( jayT, new ConsLDFT( kamaT, new EmptyLDFT () ) )->howFarRemoved(),
    //        0 );
    // this->first = jayT
    // this->rest = kama : !
    // this->first->howFarRemoved() = 0
    // this->rest->howFarRemoved() = -1
    //return 0;

    // Generalize 1 & 2;
    //return this->first->howFarRemoved() ;

    // Example 3:
    // printf("The answer is\n   %s\nbut should be\n   %s\n",
    //        new ConsLDFT( kamaT, new ConsLDFT( jayT, new EmptyLDFT () ) )->howFarRemoved(),
    //        0 );
    // this->first = kamaT
    // this->rest = jayT : !
    // this->first->howFarRemoved() = -1
    // this->rest->howFarRemoved() = 0
    // return 0;
    //return this->rest->howFarRemoved();

    // Generalize (1 & 2) & 3:
    return smallerOfOnesNotNegativeOne( this->first->howFarRemoved(), this->rest->howFarRemoved() );

  }

  // generationDescendants : ListOfDFT int -> ListOfPersonRecord
  ListOfPersonRecord* generationDescendants ( int genNum ) {
    // genNum
    // this->first
    // this->rest
    // this->first->generationDescendants( ... genNum ... )
    // this->rest->generationDescendants( ... genNum ... )

    // Example 1:
    //
    // printf("The answer is\n   %s\nbut should be\n   %s\n",
    //        new ConsLDFT( jayT, new ConsLDFT( kamaT, new EmptyLDFT () ) )->generationDescendants(0),
    //        new ConsLOPR( jay, new ConsLOPR( kama, new EmptyLOPR() ) ) );
    // genNum = 0
    // this->first = jayT
    // this->rest = kamaT : !
    // this->first->generationDescendants( genNum ) = jay : !
    // this->rest->generationDescendants( genNum ) = kama : !
    // output : jay : kama : !
    // return new ConsLOPR( jay, new ConsLOPR( kama, new EmptyLOPR() ) ) ;
    return (this->first->generationDescendants( genNum ))->append( this->rest->generationDescendants( genNum ) );
  }
};

int main ( ) {
  printf("The answer is\n   %s\n", "Spaghetti");

  //PersonRecord joe = new PersonRecord( "Joe McCarthy", "brown" );
  PersonRecord* jo = new PersonRecord( "Jo McCarthy", "brown" ); // <-- changed her eyes to brown
  PersonRecord* jim = new PersonRecord( "Jim McCarthy", "brown" );
  //PersonRecord pam = new PersonRecord( "Pam McCarthy", "brown" );
  PersonRecord* jay = new PersonRecord( "Jay McCarthy", "blue" );
  PersonRecord* kama = new PersonRecord( "Kama McCarthy", "chartreuse" );
  PersonRecord* pat = new PersonRecord( "Pat Thistle", "brown" );

  DFamilyTree* jayT = new DFamilyTree( jay, new EmptyLDFT () );
  DFamilyTree* kamaT = new DFamilyTree( kama, new EmptyLDFT () );
  DFamilyTree* patT = new DFamilyTree( pat, new EmptyLDFT () );
  ListOfDFT* jimsKids = new ConsLDFT( jayT, new ConsLDFT( kamaT, new EmptyLDFT () ) );
  DFamilyTree* jimT = new DFamilyTree( jim, jimsKids );
  ListOfDFT* josKids = new ConsLDFT( jimT, new ConsLDFT( patT, new EmptyLDFT() ) );
  DFamilyTree* joT = new DFamilyTree( jo, josKids );
  DFamilyTree* johnT = new DFamilyTree( new PersonRecord ( "John LaBonte", "brown" ), new ConsLDFT( joT, new EmptyLDFT () ) );

  printf("The answer is\n   %s\n", "countGenerations");

  printf("The answer is\n   %d\nbut should be\n   %d\n",
         johnT->countGenerations(),
         4 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         joT->countGenerations(),
         3 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         (new EmptyLDFT())->countGenerations(),
         0 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         (new ConsLDFT( kamaT, new EmptyLDFT () ))->countGenerations(),
         1 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         (new ConsLDFT( jimT, new ConsLDFT( patT, new EmptyLDFT() ) ))->countGenerations(),
         2 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         (new ConsLDFT( patT, new ConsLDFT( jimT, new EmptyLDFT() ) ))->countGenerations(),
         2 );

  printf("The answer is\n   %s\n", "howFarRemoved");

  printf("The answer is\n   %d\nbut should be\n   %d\n",
         jayT->howFarRemoved(),
         0 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         kamaT->howFarRemoved(),
         -1 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         jimT->howFarRemoved(),
         1 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         (new EmptyLDFT())->howFarRemoved(),
         -1 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         (new ConsLDFT( kamaT, new EmptyLDFT () ))->howFarRemoved(),
         -1 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         (new ConsLDFT( jayT, new ConsLDFT( kamaT, new EmptyLDFT () ) ))->howFarRemoved(),
         0 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         (new ConsLDFT( kamaT, new ConsLDFT( jayT, new EmptyLDFT () ) ))->howFarRemoved(),
         0 );


  printf("The answer is\n   %d\nbut should be\n   %d\n",
         smallerOfOnesNotNegativeOne( -1, 31 ), 31 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         smallerOfOnesNotNegativeOne( 4, 5 ), 4 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         smallerOfOnesNotNegativeOne( 7, 1 ), 1 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         smallerOfOnesNotNegativeOne( 0, -1 ), 0 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         smallerOfOnesNotNegativeOne( -1, 0 ), 0 );

  printf("The answer is\n   %d\nbut should be\n   %d\n",
         joT->howFarRemoved(),
         2 );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         johnT->howFarRemoved(),
         3 );

  printf("The answer is\n   %s\n", "generationDescendants");

  printf("The answer is\n   %d\nbut should be\n   %d\n",
         jayT->generationDescendants(17)->print(),
         (new EmptyLOPR())->print() );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         jayT->generationDescendants(0)->print(),
         (new ConsLOPR( jay, new EmptyLOPR() ))->print() );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         jimT->generationDescendants(0)->print(),
         (new ConsLOPR( jim, new EmptyLOPR() ))->print() );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         jimT->generationDescendants(1)->print(),
         (new ConsLOPR( jay, new ConsLOPR( kama, new EmptyLOPR() ) ))->print() );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         (new EmptyLDFT())->generationDescendants(1)->print(),
         (new EmptyLOPR())->print() );
  printf("The answer is\n   %d\nbut should be\n   %d\n",
         (new ConsLDFT( jayT, new ConsLDFT( kamaT, new EmptyLDFT () ) ))->generationDescendants(0)->print(),
         (new ConsLOPR( jay, new ConsLOPR( kama, new EmptyLOPR() ) ))->print() );

  printf("The answer is\n   %d\nbut should be\n   %d\n",
         johnT->generationDescendants(3)->print(),
         (new ConsLOPR( jay, new ConsLOPR( kama, new EmptyLOPR() ) ))->print() );

  printf("The answer is\n   %d\n", johnT->print());
}
