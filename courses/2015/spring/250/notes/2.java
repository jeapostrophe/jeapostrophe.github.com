interface Unary {
    Unary add ( Unary right );
}

class NoFingers implements Unary {
    NoFingers() {}

    Unary add ( Unary right ) {
        return right;
    }
}

class OneFinger implements Unary {
    Unary rest;
    
    OneFinger( Unary rest ) {
        this.rest = rest;
    }

    Unary add ( Unary right ) {
        return new OneFinger( rest.add(right) );
    }
}

// Binary = List(boolean)

class Binary {
    static Binary add ( Binary left, Binary right ) {
        int width = max(left.length(), right.length());
        Binary el = left.extend(width);
        Binary er = right.extend(width);
        return el.add_with_carry(er, false);
    }

    // add_with_carry ( x, y, carry )
    // [] [] false = []
    // [] [] true = 1 :: []
    // [ 0 :: x ] [ 0 :: y ] carry = carry :: add_with_carry( x, y, false )
    // [ 0 :: x ] [ 1 :: y ]     0 =     1 :: add_with_carry( x, y, false )
    // [ 0 :: x ] [ 1 :: y ]     1 =     0 :: add_with_carry( x, y, true )
    // [ 1 :: x ] [ 0 :: y ]     0  =    1 :: add_with_carry( x, y, false )
    // [ 1 :: x ] [ 0 :: y ]     1  =    0 :: add_with_carry( x, y, true )
    // [ 1 :: x ] [ 1 :: y ] carry = carry :: add_with_carry( x, y, true )
}

class 32ary {
    // [ 77 :: x ] [ 99 :: y ] carry
    //   ^           ^          ^------ boolean
    //   |           |
    //   +-----------+--- any number between 0 and 2^32 - 1
}

// 32ary is called "BigInt"

