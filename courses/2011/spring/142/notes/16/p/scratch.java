// A ListOfPersonRecord is either
//  Empty
//  Cons
interface ListOfPersonRecord {
    // append : ListOfPersonRecord ListOfPersonRecord -> ListOfPersonRecord
    public ListOfPersonRecord append ( ListOfPersonRecord after ) ;
}

// An EmptyLOPR is a
//   new EmptyLOPR ()
// where
class EmptyLOPR implements ListOfPersonRecord {
    public EmptyLOPR ( ) {
    }
    public String toString ( ) {
        return "!";
    }
    // append : ListOfPersonRecord ListOfPersonRecord -> ListOfPersonRecord
    public ListOfPersonRecord append ( ListOfPersonRecord after ) {
        return after;
    }
}

// A ConsLOPR is a
//  new ConsLOPR ( first, rest )
// where
//  first is a PersonRecord
//  rest is a ListOfPersonRecord
class ConsLOPR implements ListOfPersonRecord {
    public PersonRecord first;
    public ListOfPersonRecord rest;

    public ConsLOPR ( PersonRecord first0, ListOfPersonRecord rest0 ) {
        first = first0;
        rest = rest0;
    }

    public String toString () {
        return String.format("%s:%s", this.first, this.rest );
    }

    // append : ListOfPersonRecord ListOfPersonRecord -> ListOfPersonRecord
    public ListOfPersonRecord append ( ListOfPersonRecord after ) {
        return new ConsLOPR ( this.first, this.rest.append(after) );
    }
}

// A PersonRecord is a...
//  new PersonRecord( name, eyeColor )
// where
//  name is a String
//  eyeColor is a String
class PersonRecord {
    public String name;
    public String eyeColor;

    public PersonRecord ( String name0, String eyeColor0 ) {
        name = name0;
        eyeColor = eyeColor0;
    }
    public String toString () {
        return String.format("new PersonRecord( \"%s\", \"%s\" )",
                             this.name,
                             this.eyeColor );
    }
}

// A DFamilyTree is a...
//  new DFamilyTree ( info, children )
// where
//  info is a PersonRecord
//  children is a ListOfDFT
class DFamilyTree {
    public PersonRecord info;
    public ListOfDFT children;

    public DFamilyTree ( PersonRecord info0, ListOfDFT children0 ) {
        info = info0;
        children = children0;
    }

    public String toString () {
        return String.format("new DFamilyTree( %s, %s )",
                             this.info,
                             this.children );
    }

    // countGenerations : DFamilyTree -> int
    public int countGenerations ( ) {
        // this.info
        // this.children
        // this.children.countGenerations()

        // Example 1:
        //  System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
        //        johnT.countGenerations(),
        //        4 );
        // this.info = new PersonRecord ( "John LaBonte", "brown" )
        // this.children = new ConsLDFT( joT, new EmptyLDFT () )
        // this.children.countGenerations() = 3
        //return 4;

        // Example 2:
        //  System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
        //        joT.countGenerations(),
        //        3 );
        // this.info = jo
        // this.children = new ConsLDFT( jimT, new ConsLDFT( patT, new EmptyLDFT() ) )
        // this.children.countGenerations() = 2
        //return 3;

        // Generalize 1 & 2 to:
        return 1 + this.children.countGenerations();

    }


    // howFarRemoved : DFamilyTree -> int
    public int howFarRemoved ( ) {
        // this.info
        // this.info.name
        // this.info.eyeColor
        // this.children
        // this.children.howFarRemoved()

        if ( this.info.eyeColor.equals("blue") ) {
            // Example 1:
            //  System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
            //        jayT.howFarRemoved(),
            //        0 );
            // this.info.name = "Jay ..."
            // this.info.eyeColor = "blue"
            // this.children = !
            // this.children.howFarRemoved() = -1
            return 0;
        } else {
            if ( this.children.howFarRemoved() == -1 ) {
                // Example 2:
                //  System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
                //        kamaT.howFarRemoved(),
                //        -1 );
                // this.info.name = "Kama ..."
                // this.info.eyeColor = "chartreuse"
                // this.children = !
                // this.children.howFarRemoved() = -1
                return -1;
            } else {
                // Example 3:
                // System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
                //        jimT.howFarRemoved(),
                //        1 );
                // this.info.name = "Jim ..."
                // this.info.eyeColor = "brown"
                // this.children = jayT : kamaT : !
                // this.children.howFarRemoved() = 0
                //return 1;

                // Example 4:
                //  System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
                //        joT.howFarRemoved(),
                //        2 );
                // this.info.name = "Jo ..."
                // this.info.eyeColor = "brown"
                // this.children = jimT : patT : !
                // this.children.howFarRemoved() = 1
                //return 2;

                // Generalize 3 & 4:
                return this.children.howFarRemoved() + 1;
            }
        }
    }

    // generationDescendants : DFamilyTree int -> ListOfPersonRecord
    public ListOfPersonRecord generationDescendants ( int genNum ) {
        // genNum
        // this.info
        // this.children
        // this.children.generationDescendants( ... genNum ... )

        if ( genNum != 0 ) {
            // Example 1:
            // System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
            //        jayT.generationDescendants(17),
            //        new EmptyLOPR() );
            // genNum = 17
            // this.info = jay
            // this.children = !
            // this.children.generationDescendants( 0 ) = !
            // this.children.generationDescendants( 17 ) = !
            // this.children.generationDescendants( 10 ) = !
            // this.children.generationDescendants( 1024 ) = !
            // this.children.generationDescendants( ... genNum ... ) = !
            // return new EmptyLOPR();

            // Example 3:
            // System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
            //        jimT.generationDescendants(1),
            //        new ConsLOPR( jay, new ConsLOPR( kama, new EmptyLOPR() ) ) );
            // genNum = 1
            // this.info = jim
            // this.children = jayT : kamaT : !
            // this.children.generationDescendants( 0 ) = jay : kama : !
            // this.children.generationDescendants( 1 ) = !
            // this.children.generationDescendants( 2 ) = !
            // return this.children.generationDescendants( 0 );

            // Generalize 1 & 3:
            //return this.children.generationDescendants( 0 );

            // Example 4:
            // System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
            //        johnT.generationDescendants(3),
            //        new ConsLOPR( jay, new ConsLOPR( kama, new EmptyLOPR() ) ) );
            // genNum = 3
            // this.info = john
            // this.children = joT : !
            // this.children.generationDescendants( 0 ) = jo : !
            // this.children.generationDescendants( 1 ) = jim : pat : !
            // this.children.generationDescendants( 2 ) = jay : kama : !
            // this.children.generationDescendants( 3 or higher ) = !
            //return this.children.generationDescendants( 2 ) ;

            // Generalize (1 & 3) & 4
            return this.children.generationDescendants( genNum - 1 ) ;

        } else {
            // Example 2:
            // System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
            //        jayT.generationDescendants(0),
            //        new ConsLOPR( jay, new EmptyLOPR() ) );
            // genNum = 0
            // this.info = jay
            // this.children = !
            // this.children.generationDescendants( 17 ) = !
            // this.children.generationDescendants( 10 ) = !
            // this.children.generationDescendants( 1024 ) = !
            // this.children.generationDescendants( ... genNum ... ) = !
            return new ConsLOPR( this.info, new EmptyLOPR() ) ;
        }
    }
}

// A ListOfDFT is either...
//  EmptyLDFT
//  ConsLDFT
interface ListOfDFT {
    // countGenerations : ListOfDFT -> int
    public int countGenerations ( );
    // howFarRemoved : ListOfDFT -> int
    public int howFarRemoved ( );
    // generationDescendants : ListOfDFT int -> ListOfPersonRecord
    public ListOfPersonRecord generationDescendants ( int genNum );
}

// A EmptyLDFT is a...
//  new EmptyLDFT ()
// where
class EmptyLDFT implements ListOfDFT {

    public EmptyLDFT () {
    }

    public String toString () {
        return String.format("!");
    }


    // countGenerations : ListOfDFT -> int
    public int countGenerations ( ) {
        // ... ...

        // Example 1:
        //  System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
        //        new EmptyLDFT().countGenerations(),
        //        0 );
        return 0;
    }

    // howFarRemoved : ListOfDFT -> int
    public int howFarRemoved ( ) {
        // ... ...

        // Example 1:
        //  System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
        //        new EmptyLDFT().howFarRemoved(),
        //        -1 );
        return -1;
    }

    // generationDescendants : ListOfDFT int -> ListOfPersonRecord
    public ListOfPersonRecord generationDescendants ( int genNum ) {
        // ... genNum ...

        // Example 1:
        // System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
        //        new EmptyLDFT().generationDescendants(1),
        //        new EmptyLOPR() );
        return new EmptyLOPR() ;
    }

}

// A ConsLDFT is a...
//  new ConsLDFT( first, rest )
// where
//  first is DFamilyTree
//  rest is ListOfDFT
class ConsLDFT implements ListOfDFT {
    public DFamilyTree first;
    public ListOfDFT rest;

    public ConsLDFT ( DFamilyTree first0, ListOfDFT rest0 ) {
        first = first0;
        rest = rest0;
    }
    public String toString ( ) {
        return String.format("%s:%s", this.first, this.rest );
    }

    // countGenerations : ListOfDFT -> int
    public int countGenerations ( ) {
        // this.first
        // this.rest
        // this.first.countGenerations()
        // this.rest.countGenerations()

        // Example 1:
        //  System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
        //        new ConsLDFT( kamaT, new EmptyLDFT () ).countGenerations(),
        //        1 );
        // this.first = kamaT
        // this.rest = !
        // this.first.countGenerations() = 1
        // this.rest.countGenerations() = 0
        //return 1;

        // Example 2:
        //          System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
        //        new ConsLDFT( jimT, new ConsLDFT( patT, new EmptyLDFT() ) ).countGenerations(),
        //        2 );
        // this.first = jimT
        // this.rest = new ConsLDFT( patT, new EmptyLDFT() )
        // this.first.countGenerations() = 2
        // this.rest.countGenerations() = 1
        //return 2;

        // Generalize 1 & 2:
        //return this.first.countGenerations() ;

        // Example 3:
        // System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
        //        new ConsLDFT( patT, new ConsLDFT( jimT, new EmptyLDFT() ) ).countGenerations(),
        //        2 );
        // this.first = patT
        // this.rest = new ConsLDFT( jimT, new EmptyLDFT() )
        // this.first.countGenerations() = 1
        // this.rest.countGenerations() = 2
        //return 2;
        //return this.rest.countGenerations();

        // Generalize (1 & 2) & 3:
        return Math.max( this.first.countGenerations(), this.rest.countGenerations() );


    }

    // howFarRemoved : ListOfDFT -> int
    public int howFarRemoved ( ) {
        // this.first
        // this.rest
        // this.first.howFarRemoved()
        // this.rest.howFarRemoved()

        // Example 1:
        // System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
        //        new ConsLDFT( kamaT, new EmptyLDFT () ).howFarRemoved(),
        //        -1 );
        // this.first = kamaT
        // this.rest = !
        // this.first.howFarRemoved() = -1
        // this.rest.howFarRemoved() = -1
        //return -1;

        // Example 2:
        // System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
        //        new ConsLDFT( jayT, new ConsLDFT( kamaT, new EmptyLDFT () ) ).howFarRemoved(),
        //        0 );
        // this.first = jayT
        // this.rest = kama : !
        // this.first.howFarRemoved() = 0
        // this.rest.howFarRemoved() = -1
        //return 0;

        // Generalize 1 & 2;
        //return this.first.howFarRemoved() ;

        // Example 3:
        // System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
        //        new ConsLDFT( kamaT, new ConsLDFT( jayT, new EmptyLDFT () ) ).howFarRemoved(),
        //        0 );
        // this.first = kamaT
        // this.rest = jayT : !
        // this.first.howFarRemoved() = -1
        // this.rest.howFarRemoved() = 0
        // return 0;
        //return this.rest.howFarRemoved();

        // Generalize (1 & 2) & 3:
        return smallerOfOnesNotNegativeOne( this.first.howFarRemoved(), this.rest.howFarRemoved() );

    }

    // smallerOfOnesNotNegativeOne : int int -> int
    // Purpose: to return the number that is smaller, between the two, but don't count -1, as smaller.
    // Examples:
    // smallerOfOnesNotNegativeOne( -1, 31 ) = 31
    // smallerOfOnesNotNegativeOne( 4, 5 ) = 4
    // smallerOfOnesNotNegativeOne( 7, 1 ) = 1
    // smallerOfOnesNotNegativeOne( 0, -1 ) = 0
    // smallerOfOnesNotNegativeOne( -1, 0 ) = 0
    public static int smallerOfOnesNotNegativeOne ( int lhs, int rhs ) {
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

    // generationDescendants : ListOfDFT int -> ListOfPersonRecord
    public ListOfPersonRecord generationDescendants ( int genNum ) {
        // genNum
        // this.first
        // this.rest
        // this.first.generationDescendants( ... genNum ... )
        // this.rest.generationDescendants( ... genNum ... )

        // Example 1:
        //
        // System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
        //        new ConsLDFT( jayT, new ConsLDFT( kamaT, new EmptyLDFT () ) ).generationDescendants(0),
        //        new ConsLOPR( jay, new ConsLOPR( kama, new EmptyLOPR() ) ) );
        // genNum = 0
        // this.first = jayT
        // this.rest = kamaT : !
        // this.first.generationDescendants( genNum ) = jay : !
        // this.rest.generationDescendants( genNum ) = kama : !
        // output : jay : kama : !
        // return new ConsLOPR( jay, new ConsLOPR( kama, new EmptyLOPR() ) ) ;
        return (this.first.generationDescendants( genNum )).append( this.rest.generationDescendants( genNum ) );
    }

}

class scratch {
    public static void main ( String[] args ) {
        System.out.format("The answer is%n   %s%n", "Spaghetti");

        //PersonRecord joe = new PersonRecord( "Joe McCarthy", "brown" );
        PersonRecord jo = new PersonRecord( "Jo McCarthy", "brown" ); // <-- changed her eyes to brown
        PersonRecord jim = new PersonRecord( "Jim McCarthy", "brown" );
        //PersonRecord pam = new PersonRecord( "Pam McCarthy", "brown" );
        PersonRecord jay = new PersonRecord( "Jay McCarthy", "blue" );
        PersonRecord kama = new PersonRecord( "Kama McCarthy", "chartreuse" );
        PersonRecord pat = new PersonRecord( "Pat Thistle", "brown" );

        DFamilyTree jayT = new DFamilyTree( jay, new EmptyLDFT () );
        DFamilyTree kamaT = new DFamilyTree( kama, new EmptyLDFT () );
        DFamilyTree patT = new DFamilyTree( pat, new EmptyLDFT () );
        ListOfDFT jimsKids = new ConsLDFT( jayT, new ConsLDFT( kamaT, new EmptyLDFT () ) );
        DFamilyTree jimT = new DFamilyTree( jim, jimsKids );
        ListOfDFT josKids = new ConsLDFT( jimT, new ConsLDFT( patT, new EmptyLDFT() ) );
        DFamilyTree joT = new DFamilyTree( jo, josKids );
        DFamilyTree johnT = new DFamilyTree( new PersonRecord ( "John LaBonte", "brown" ), new ConsLDFT( joT, new EmptyLDFT () ) );

        System.out.format("The answer is%n   %s%n", "countGenerations");

        System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
                          johnT.countGenerations(),
                          4 );
        System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
                          joT.countGenerations(),
                          3 );
        System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
                          new EmptyLDFT().countGenerations(),
                          0 );
        System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
                          new ConsLDFT( kamaT, new EmptyLDFT () ).countGenerations(),
                          1 );
        System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
                          new ConsLDFT( jimT, new ConsLDFT( patT, new EmptyLDFT() ) ).countGenerations(),
                          2 );
        System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
                          new ConsLDFT( patT, new ConsLDFT( jimT, new EmptyLDFT() ) ).countGenerations(),
                          2 );

        System.out.format("The answer is%n   %s%n", "howFarRemoved");

        System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
                          jayT.howFarRemoved(),
                          0 );
        System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
                          kamaT.howFarRemoved(),
                          -1 );
        System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
                          jimT.howFarRemoved(),
                          1 );
        System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
                          new EmptyLDFT().howFarRemoved(),
                          -1 );
        System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
                          new ConsLDFT( kamaT, new EmptyLDFT () ).howFarRemoved(),
                          -1 );
        System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
                          new ConsLDFT( jayT, new ConsLDFT( kamaT, new EmptyLDFT () ) ).howFarRemoved(),
                          0 );
        System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
                          new ConsLDFT( kamaT, new ConsLDFT( jayT, new EmptyLDFT () ) ).howFarRemoved(),
                          0 );


        System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
                          ConsLDFT.smallerOfOnesNotNegativeOne( -1, 31 ), 31 );
        System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
                          ConsLDFT.smallerOfOnesNotNegativeOne( 4, 5 ), 4 );
        System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
                          ConsLDFT.smallerOfOnesNotNegativeOne( 7, 1 ), 1 );
        System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
                          ConsLDFT.smallerOfOnesNotNegativeOne( 0, -1 ), 0 );
        System.out.format("The answer is%n   %d%nbut should be%n   %d%n",
                          ConsLDFT.smallerOfOnesNotNegativeOne( -1, 0 ), 0 );

        System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
                          joT.howFarRemoved(),
                          2 );
        System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
                          johnT.howFarRemoved(),
                          3 );

        System.out.format("The answer is%n   %s%n", "generationDescendants");

        System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
                          jayT.generationDescendants(17),
                          new EmptyLOPR() );
        System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
                          jayT.generationDescendants(0),
                          new ConsLOPR( jay, new EmptyLOPR() ) );
        System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
                          jimT.generationDescendants(0),
                          new ConsLOPR( jim, new EmptyLOPR() ) );
        System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
                          jimT.generationDescendants(1),
                          new ConsLOPR( jay, new ConsLOPR( kama, new EmptyLOPR() ) ) );
        System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
                          new EmptyLDFT().generationDescendants(1),
                          new EmptyLOPR() );
        System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
                          new ConsLDFT( jayT, new ConsLDFT( kamaT, new EmptyLDFT () ) ).generationDescendants(0),
                          new ConsLOPR( jay, new ConsLOPR( kama, new EmptyLOPR() ) ) );

        System.out.format("The answer is%n   %s%nbut should be%n   %s%n",
                          johnT.generationDescendants(3),
                          new ConsLOPR( jay, new ConsLOPR( kama, new EmptyLOPR() ) ) );

        System.out.format("The answer is%n   %s%n", johnT);
    }
}
