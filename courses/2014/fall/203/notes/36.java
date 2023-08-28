class Cons {
    int first;
    Cons rest;

    Cons() {};
}

class C36 {
    public static void main (String args[]) {
          while ( true ) {
              Cons left = new Cons();    
              Cons right = new Cons();
              left.rest = right;
              right.rest = left;
          }
    }
}
