class DFA {
    String start;
    Set accepts;
    HashMap trans;

    boolean interp( String input ) {
        String current = start;
        for ( int i = 0; i < input.length; i++ ) {
            current = trans.lookup(pair(current,input[i]));
        }
        return accepts.member(current);
    }
    
    public static void main() {
        HashMap<Pair<String,Char>, String> trans = new HashMap();
        trans.add(pair("Even", 0), "Odd");
        DFA evens =
            new DFA( "Even",
                     new Set("Even"),
                     trans );
        evens.interp("0011");
        evens.interp("");
    }
}
