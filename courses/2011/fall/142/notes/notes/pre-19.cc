#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

bool streq (const char* a, const char* b) {
  if (strcmp(a,b) == 0) {
    return true;
  } else {
    return false;
  }
}

// A traffic light color is a String

// next_direct : String -> String
// Simulates a traffic light in direct style
const char* next_direct1(const char* color) {
  if (streq(color,"Green")) {
    return "Yellow";
  } else if (streq(color,"Yellow")) {
    return "Red";
  } else if (streq(color,"Red")) {
    return "Green";
  } else {
    return "You Idiot";
  }
}

const char* color = "Green";

// next : -> void
// Simulates a traffic light in mutator style
void next() {
  if (streq(color,"Green")) {
    // store: color = "Green"
    color = "Yellow";
    // store: color = "Yellow"
  } else if (streq(color,"Yellow")) {
    color = "Red";
  } else if (streq(color,"Red")) {
    color = "Green";
  }
}
    
const char* colorAtUniversity = "Green";

// nextAtUniversity : -> void
// Simulates a traffic light in mutator style
void nextAtUniversity() {
  if (streq(colorAtUniversity,"Green")) {
    // store: color = "Green"
    colorAtUniversity = "Yellow";
    // store: color = "Yellow"
  } else if (streq(colorAtUniversity,"Yellow")) {
    colorAtUniversity = "Red";
  } else if (streq(colorAtUniversity,"Red")) {
    colorAtUniversity = "Green";
  }
}

const char* colorAtState = "Green";

// nextAtState : -> void
// Simulates a traffic light in mutator style
void nextAtState() {
  if (streq(colorAtState,"Green")) {
    // store: color = "Green"
    colorAtState = "Yellow";
    // store: color = "Yellow"
  } else if (streq(colorAtState,"Yellow")) {
    colorAtState = "Red";
  } else if (streq(colorAtState,"Red")) {
    colorAtState = "Green";
  }
}

// A traffic light color is a String

class TrafficLight {
public:
  // Creates problems:
  // const char* color;
private:
  const char* color;
public:
  const char* getColor() {
    return this->color;
  }

  TrafficLight() {
    this->color = "Green";
  }

  // next : TrafficLight -> void
  // Simulates a traffic light in mutator style
  void next() {
    if (streq(this->color,"Green")) {
      // store: color = "Green"
      this->color = "Yellow";
      // store: color = "Yellow"
    } else if (streq(this->color,"Yellow")) {
      this->color = "Red";
    } else if (streq(this->color,"Red")) {
      this->color = "Green";
    }
  }
};

// next_direct : String -> String
// Simulates a traffic light in direct style
const char* next_direct(const char* color) {
  if (streq(color,"Green")) {
    return "Yellow";
  } else if (streq(color,"Yellow")) {
    return "Red";
  } else if (streq(color,"Red")) {
    return "Green";
  } else {
    return "You Idiot";
  }
}
    
void evilFunctionInADifferentFileThatIAlsoNeedToUse(TrafficLight* light) {
  // perfectly innocent stuff...

  // more perfectly innocent stuff...

  // FOILED! OWNED! PWNT!
  // light.color = "Purple";
}

int main() {
  {
    printf("Answer is %s, should be %s\n", next_direct1("Green"), "Yellow");
    printf("Answer is %s, should be %s\n", next_direct1("Yellow"), "Red");
    printf("Answer is %s, should be %s\n", next_direct1("Red"), "Green");      

    // Original traffic light
        
    // store: color = "Green"
    next();
    // store: color = "Yellow"
    printf("Answer is %s, should be %s\n", color, "Yellow");
    next();
    // store: color = "Red"
    printf("Answer is %s, should be %s\n", color, "Red");
    next();
    // store: color = "Green"
    printf("Answer is %s, should be %s\n", color, "Green");

    // Ensure a precondition:
    color = "Green";
    // Do the test:
    next();
    // Check the postcondition:
    printf("Answer is %s, should be %s\n", color, "Yellow");
        
    // Ensure a precondition:
    color = "Red";
    // Do the test:
    next();
    // store: color = "Green"
    // Check the postcondition:
    printf("Answer is %s, should be %s\n", color, "Green");

    // Ensure a precondition:
    color = "Yellow";
    // store: color = "Yellow"
    // Do the test:
    next();
    // store: color = "Red"
    // Check the postcondition:
    printf("Answer is %s, should be %s\n", color, "Red");

    // Traffic light at university

    // Ensure a precondition:
    colorAtUniversity = "Green";
    // Do the test:
    nextAtUniversity();
    // Check the postcondition:
    printf("Answer is %s, should be %s\n", colorAtUniversity, "Yellow");
        
    // Ensure a precondition:
    colorAtUniversity = "Red";
    // Do the test:
    nextAtUniversity();
    // store: color = "Green"
    // Check the postcondition:
    printf("Answer is %s, should be %s\n", colorAtUniversity, "Green");

    // Ensure a precondition:
    colorAtUniversity = "Yellow";
    // store: color = "Yellow"
    // Do the test:
    nextAtUniversity();
    // store: color = "Red"
    // Check the postcondition:
    printf("Answer is %s, should be %s\n", colorAtUniversity, "Red");

    // Traffic light at state

    // Ensure a precondition:
    colorAtState = "Green";
    // Do the test:
    nextAtState();
    // Check the postcondition:
    printf("Answer is %s, should be %s\n", colorAtState, "Yellow");
        
    // Ensure a precondition:
    colorAtState = "Red";
    // Do the test:
    nextAtState();
    // store: color = "Green"
    // Check the postcondition:
    printf("Answer is %s, should be %s\n", colorAtState, "Green");

    // Ensure a precondition:
    colorAtState = "Yellow";
    // store: color = "Yellow"
    // Do the test:
    nextAtState();
    // store: color = "Red"
    // Check the postcondition:
    printf("Answer is %s, should be %s\n", colorAtState, "Red");
  }

  {
    printf("Answer is %s, should be %s\n", next_direct("Green"), "Yellow");
    printf("Answer is %s, should be %s\n", next_direct("Yellow"), "Red");
    printf("Answer is %s, should be %s\n", next_direct("Red"), "Green");      

    // Traffic light at university

    TrafficLight* lightAtUniversity = new TrafficLight();

    // Ensure a precondition:
    // lightAtUniversity.color = "Green";
    // Do the test:
    lightAtUniversity->next();
    // Check the postcondition:
    printf("Answer is %s, should be %s\n", lightAtUniversity->getColor(), "Yellow");
        
    // Ensure a precondition:
    // lightAtUniversity.color = "Yellow";
    // Do the test:
    lightAtUniversity->next();
    // Check the postcondition:
    printf("Answer is %s, should be %s\n", lightAtUniversity->getColor(), "Red");

    // Ensure a precondition:
    // lightAtUniversity.color = "Red";
    // Do the test:
    lightAtUniversity->next();
    // Check the postcondition:
    printf("Answer is %s, should be %s\n", lightAtUniversity->getColor(), "Green");

    TrafficLight* lightAtState = new TrafficLight();
    // store: lightAtUniversity.color = "Green", lightAtState.color = "Green"
    printf("Answer is %s, should be %s\n", lightAtState->getColor(), "Green");
    lightAtState->next();
    // store: lightAtUniversity.color = "Green", lightAtState.color = "Yellow"
    printf("Answer is %s, should be %s\n", lightAtUniversity->getColor(), "Green");
    printf("Answer is %s, should be %s\n", lightAtState->getColor(), "Yellow");

    TrafficLight* lightAtBulldog = new TrafficLight();
    evilFunctionInADifferentFileThatIAlsoNeedToUse(lightAtBulldog);
    lightAtBulldog->next();
    printf("Answer is %s, should be %s\n", lightAtBulldog->getColor(), "Yellow");
        
    // store: lightAtBulldog.color = "Yellow"

    const char* bulldogColor = lightAtBulldog->getColor();
    // store: lightAtBulldog.color = "Yellow", bulldogColor = "Yellow"
    bulldogColor = "Purple";
    // store: lightAtBulldog.color = "Yellow", bulldogColor = "Purple"
    printf("Answer is %s, should be %s\n", lightAtBulldog->getColor(), "Yellow");
  }

  {
    // store: user = <some list of characters that we don't know>
    printf("Enter a character and press ENTER\n");
    int c = getc(stdin);
    // store: user = <some list of characters that we don't know>
    printf("You entered '%c'\n", c);
    int shouldBeReturn = getc(stdin);
    printf("You entered '%c'\n", shouldBeReturn);
  }
}
