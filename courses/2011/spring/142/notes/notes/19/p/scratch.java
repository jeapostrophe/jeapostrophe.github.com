import java.io.InputStreamReader;
import java.io.IOException;

// A traffic light color is a String

class scratch {
    // next_direct : String -> String
    // Simulates a traffic light in direct style
    public static String next_direct(String color) {
        if (color.equals("Green")) {
            return "Yellow";
        } else if (color.equals("Yellow")) {
            return "Red";
        } else if (color.equals("Red")) {
            return "Green";
        } else {
            return "You Idiot";
        }
    }

    public static String color = "Green";

    // next : -> void
    // Simulates a traffic light in mutator style
    public static void next() {
        if (color.equals("Green")) {
            // store: color = "Green"
            color = "Yellow";
            // store: color = "Yellow"
        } else if (color.equals("Yellow")) {
            color = "Red";
        } else if (color.equals("Red")) {
            color = "Green";
        }
    }
    
    public static String colorAtUniversity = "Green";

    // nextAtUniversity : -> void
    // Simulates a traffic light in mutator style
    public static void nextAtUniversity() {
        if (colorAtUniversity.equals("Green")) {
            // store: color = "Green"
            colorAtUniversity = "Yellow";
            // store: color = "Yellow"
        } else if (colorAtUniversity.equals("Yellow")) {
            colorAtUniversity = "Red";
        } else if (colorAtUniversity.equals("Red")) {
            colorAtUniversity = "Green";
        }
    }

    public static String colorAtState = "Green";

    // nextAtState : -> void
    // Simulates a traffic light in mutator style
    public static void nextAtState() {
        if (colorAtState.equals("Green")) {
            // store: color = "Green"
            colorAtState = "Yellow";
            // store: color = "Yellow"
        } else if (colorAtState.equals("Yellow")) {
            colorAtState = "Red";
        } else if (colorAtState.equals("Red")) {
            colorAtState = "Green";
        }
    }

    public static void main(String[] args) {
        System.out.format("Answer is %s, should be %s%n", next_direct("Green"), "Yellow");
        System.out.format("Answer is %s, should be %s%n", next_direct("Yellow"), "Red");
        System.out.format("Answer is %s, should be %s%n", next_direct("Red"), "Green");      

        // Original traffic light
        
        // store: color = "Green"
        next();
        // store: color = "Yellow"
        System.out.format("Answer is %s, should be %s%n", color, "Yellow");
        next();
        // store: color = "Red"
        System.out.format("Answer is %s, should be %s%n", color, "Red");
        next();
        // store: color = "Green"
        System.out.format("Answer is %s, should be %s%n", color, "Green");

        // Ensure a precondition:
        color = "Green";
        // Do the test:
        next();
        // Check the postcondition:
        System.out.format("Answer is %s, should be %s%n", color, "Yellow");
        
        // Ensure a precondition:
        color = "Red";
        // Do the test:
        next();
        // store: color = "Green"
        // Check the postcondition:
        System.out.format("Answer is %s, should be %s%n", color, "Green");

        // Ensure a precondition:
        color = "Yellow";
        // store: color = "Yellow"
        // Do the test:
        next();
        // store: color = "Red"
        // Check the postcondition:
        System.out.format("Answer is %s, should be %s%n", color, "Red");

        // Traffic light at university

        // Ensure a precondition:
        colorAtUniversity = "Green";
        // Do the test:
        nextAtUniversity();
        // Check the postcondition:
        System.out.format("Answer is %s, should be %s%n", colorAtUniversity, "Yellow");
        
        // Ensure a precondition:
        colorAtUniversity = "Red";
        // Do the test:
        nextAtUniversity();
        // store: color = "Green"
        // Check the postcondition:
        System.out.format("Answer is %s, should be %s%n", colorAtUniversity, "Green");

        // Ensure a precondition:
        colorAtUniversity = "Yellow";
        // store: color = "Yellow"
        // Do the test:
        nextAtUniversity();
        // store: color = "Red"
        // Check the postcondition:
        System.out.format("Answer is %s, should be %s%n", colorAtUniversity, "Red");

        // Traffic light at state

        // Ensure a precondition:
        colorAtState = "Green";
        // Do the test:
        nextAtState();
        // Check the postcondition:
        System.out.format("Answer is %s, should be %s%n", colorAtState, "Yellow");
        
        // Ensure a precondition:
        colorAtState = "Red";
        // Do the test:
        nextAtState();
        // store: color = "Green"
        // Check the postcondition:
        System.out.format("Answer is %s, should be %s%n", colorAtState, "Green");

        // Ensure a precondition:
        colorAtState = "Yellow";
        // store: color = "Yellow"
        // Do the test:
        nextAtState();
        // store: color = "Red"
        // Check the postcondition:
        System.out.format("Answer is %s, should be %s%n", colorAtState, "Red");
    }
}

// A traffic light color is a String

class TrafficLight {
    // Creates problems:
    // public String color;

    private String color;

    public String getColor() {
        return this.color;
    }

    public TrafficLight() {
        this.color = "Green";
    }

    // next : TrafficLight -> void
    // Simulates a traffic light in mutator style
    public void next() {
        if (this.color.equals("Green")) {
            // store: color = "Green"
            this.color = "Yellow";
            // store: color = "Yellow"
        } else if (this.color.equals("Yellow")) {
            this.color = "Red";
        } else if (this.color.equals("Red")) {
            this.color = "Green";
        }
    }
}

class scratch2 {
    // next_direct : String -> String
    // Simulates a traffic light in direct style
    public static String next_direct(String color) {
        if (color.equals("Green")) {
            return "Yellow";
        } else if (color.equals("Yellow")) {
            return "Red";
        } else if (color.equals("Red")) {
            return "Green";
        } else {
            return "You Idiot";
        }
    }
    
    public static void main(String[] args) {
        System.out.format("Answer is %s, should be %s%n", next_direct("Green"), "Yellow");
        System.out.format("Answer is %s, should be %s%n", next_direct("Yellow"), "Red");
        System.out.format("Answer is %s, should be %s%n", next_direct("Red"), "Green");      

        // Traffic light at university

        TrafficLight lightAtUniversity = new TrafficLight();

        // Ensure a precondition:
        // lightAtUniversity.color = "Green";
        // Do the test:
        lightAtUniversity.next();
        // Check the postcondition:
        System.out.format("Answer is %s, should be %s%n", lightAtUniversity.getColor(), "Yellow");
        
        // Ensure a precondition:
        // lightAtUniversity.color = "Yellow";
        // Do the test:
        lightAtUniversity.next();
        // Check the postcondition:
        System.out.format("Answer is %s, should be %s%n", lightAtUniversity.getColor(), "Red");

        // Ensure a precondition:
        // lightAtUniversity.color = "Red";
        // Do the test:
        lightAtUniversity.next();
        // Check the postcondition:
        System.out.format("Answer is %s, should be %s%n", lightAtUniversity.getColor(), "Green");

        TrafficLight lightAtState = new TrafficLight();
        // store: lightAtUniversity.color = "Green", lightAtState.color = "Green"
        System.out.format("Answer is %s, should be %s%n", lightAtState.getColor(), "Green");
        lightAtState.next();
        // store: lightAtUniversity.color = "Green", lightAtState.color = "Yellow"
        System.out.format("Answer is %s, should be %s%n", lightAtUniversity.getColor(), "Green");
        System.out.format("Answer is %s, should be %s%n", lightAtState.getColor(), "Yellow");

        TrafficLight lightAtBulldog = new TrafficLight();
        evilFunctionInADifferentFileThatIAlsoNeedToUse(lightAtBulldog);
        lightAtBulldog.next();
        System.out.format("Answer is %s, should be %s%n", lightAtBulldog.getColor(), "Yellow");
        
        // store: lightAtBulldog.color = "Yellow"

        String bulldogColor = lightAtBulldog.getColor();
        // store: lightAtBulldog.color = "Yellow", bulldogColor = "Yellow"
        bulldogColor = "Purple";
        // store: lightAtBulldog.color = "Yellow", bulldogColor = "Purple"
        System.out.format("Answer is %s, should be %s%n", lightAtBulldog.getColor(), "Yellow");
    }

    public static void evilFunctionInADifferentFileThatIAlsoNeedToUse(TrafficLight light) {
        // perfectly innocent stuff...

        // more perfectly innocent stuff...

        // FOILED! OWNED! PWNT!
        // light.color = "Purple";
    }
}

class scratch3 {
    public static void main(String[] args) throws IOException {
        InputStreamReader user = new InputStreamReader(System.in);
        // store: user = <some list of characters that we don't know>
        System.out.format("Enter a character and press ENTER%n");
        int c = user.read();
        // store: user = <some list of characters that we don't know>
        System.out.format("You entered '%c'%n", c);
        int shouldBeReturn = user.read();
        System.out.format("You entered '%c'%n", shouldBeReturn);
    }
}
