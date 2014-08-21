// A BaseballPlayer is a...
//   new BaseballPlayer ( String name, double avg, int homeruns, int RBI, String position )
class BaseballPlayer {
    public String name;
    public double avg;
    public int homeruns;
    public int RBI;
    public String position;

    public BaseballPlayer ( String name0, double avg0, int homeruns0, int RBI0, String position0 ) {
	name = name0;
	avg = avg0;
	homeruns = homeruns0;
	RBI = RBI0;
	position = position0;
    }
}

class e0 {
    static boolean worthAMillion ( BaseballPlayer bp ) {
	return (bp.homeruns > 20) && ( bp.RBI > 100) && (bp.avg > .280);
    }

    public static void main (String[] args) {
	BaseballPlayer jay = new BaseballPlayer ( "Jay", .3, 15, 191, "Pitcher" );
	BaseballPlayer tim = new BaseballPlayer ( "Tim", .3, 21, 201, "Pitcher" );

	System.out.format("The answer is %b, the answer should be %b%n",
			  worthAMillion(jay),
			  false);
	System.out.format("The answer is %b, the answer should be %b%n",
			  worthAMillion(tim),
			  true);
    }
}