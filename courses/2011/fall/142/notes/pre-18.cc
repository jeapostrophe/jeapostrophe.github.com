/* XXX mention short circuiting */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

// A natural number is
//   0             "base case"
//   n + 1         "inductive case"
// where n is a natural number

// factorial_accum : int int -> int
// Computes factorial IN ACCUMULATOR STYLE!!!!!111
int factorial_accum(int n, int answer) {
  if (n != 0) {
    return factorial_accum(n - 1, n * answer);
  } else {
    return answer;
  }
}

int factorial(int n) {
  return factorial_accum(n, 1);
}

// factorial_mutant : int -> int
// Computes factorial IN MUTATOR STYLE!!!!!11qqq11
int factorial_mutant(int n) {
  // 0. Initialize the answer
  int answer = 1;
  // 1. Write a while loop, using the inductive case condition
  while (n != 0) {
    // 2. Turn argument-passing into MUTATION!!!1q11q
    int next_n = n - 1;
    int next_answer = n * answer;
    n = next_n;
    answer = next_answer;
  }
  // 3. Return the answer
  return answer;
}

// A phone book Entry is
//   new Entry(name, number)
// where name and number are strings

class Entry {
public:
  const char* name;
  const char* number;
    
  Entry(const char* name, const char* number) {
    this->name = name;
    this->number = number;
  }

  int print () {
    return printf("(%s, %s)", this->name, this->number);
  }
};

// A PhoneBook is
//   new EmptyLOE()
//   new ConsLOE(e, b)
// where e is an Entry and b is a PhoneBook
class PhoneBook {
public:
  // lookup : ListOfEntry name -> number
  // Returns the number of the given person, or "Not Found" if the person
  // isn't in the PhoneBook
  virtual const char* lookup(const char* name) = 0;
  virtual int print () = 0;
};

class EmptyLOE : public PhoneBook {
public:
  const char* lookup(const char* name) {
    // printf("Answer is %s, should be %s\n", empty->lookup("Bob"), "Not Found");
    return "Not Found";
  }

  int print() {
    return printf("!");
  }
};

class ConsLOE : public PhoneBook {
public:
  Entry* first;
  PhoneBook* rest;

  ConsLOE(Entry* first, PhoneBook* rest) {
    this->first = first;
    this->rest = rest;
  }

  int print() {
    this->first->print();
    printf(" : ");
    return this->rest->print();
  }

  const char* lookup(const char* name) {
    // printf("Answer is %s, should be %s\n", l1->lookup("Bob"), "111-1111");
    // printf("Answer is %s, should be %s\n", l2->lookup("Joe"), "222-2222");
    // If the first person is who we're looking for...
    if (strcmp(this->first->name, name) == 0) {
      // Return that person's number
      return this->first->number;
    } else {
      // printf("Answer is %s, should be %s\n", l2->lookup("Bob"), "111-1111");
      // Otherwise keep looking
      return this->rest->lookup(name);
    }
  }
};

// addEntry : String String PhoneBook -> PhoneBook
// Returns a new PhoneBook with a new entry for `name' in the front
PhoneBook* addEntry(const char* name, const char* number, PhoneBook* book) {
  return new ConsLOE(new Entry(name, number), book);
}
    
// nonMutationTests : -> void
// Prints results of tests, returns nothing
bool nonMutationTests() {
  PhoneBook* empty = new EmptyLOE();
  PhoneBook* l1 = new ConsLOE(new Entry("Bob", "111-1111"), empty);
  PhoneBook* l2 = new ConsLOE(new Entry("Joe", "222-2222"), l1);
  // Bob is not in the empty book:
  printf("Answer is %s, should be %s\n", empty->lookup("Bob"), "Not Found");
  printf("Answer is %s, should be %s\n", l1->lookup("Bob"), "111-1111");
  printf("Answer is %s, should be %s\n", l2->lookup("Joe"), "222-2222");
  printf("Answer is %s, should be %s\n", l2->lookup("Bob"), "111-1111");

  PhoneBook* l3 = addEntry("Tim", "333-3333", l2);
  printf("Answer is %s, should be %s\n", l3->lookup("Tim"), "333-3333");
  printf("Answer is %s, should be %s\n", l3->lookup("Bob"), "111-1111");
  return true;
}

// -------------------------------------------------------------------------

// The ONE AND ONLY PHONE BOOK IN TEH WORLD (starts empty)
PhoneBook* theBook = new EmptyLOE();
// This PhoneBook is permanently part of the store
    
// addEntry_mutant : String String -> void
// Adds a new entry to THE ONE AND ONLY PHONE BOOK IN TEH WORLD
// Demonstrates EXTERNAL mutation. Dangerous. Do not taunt.
void addEntry_mutant(const char* name, const char* number) {
  // Mutate theBook to add an entry:
  theBook = addEntry(name, number, theBook);
}

bool mutationTests() {
  // store: theBook = !
  printf("Answer is %s, should be %s\n", theBook->lookup("Bob"), "Not Found");

  addEntry_mutant("Bob", "111-1111");
  // store: theBook = ("Bob", "111-1111") : !
  printf("Answer is %d, should be %d\n", theBook->print(),
         (new ConsLOE(new Entry("Bob", "111-1111"), new EmptyLOE()))->print());
  printf("Answer is %s, should be %s\n", theBook->lookup("Bob"), "111-1111");

  addEntry_mutant("Joe", "222-2222");
  // store: theBook = ("Joe", "222-2222") : ("Bob", "111-1111") : !
  printf("Answer is %s, should be %s\n", theBook->lookup("Joe"), "222-2222");

  // What if the above addEntry_mutant lines were in another file? A file
  // in another directory? What if another file REMOVED lines from theBook?
  // We'd have to track the store through those files, too!

  addEntry_mutant("Tim", "333-3333");
  // store: theBook = ("Tim", "333-3333") : ("Joe", "222-2222") : ("Bob", "111-1111") : !
  printf("Answer is %s, should be %s\n", theBook->lookup("Tim"), "333-3333");
  printf("Answer is %s, should be %s\n", theBook->lookup("Bob"), "111-1111");

  // This is dangerous, EXTERNAL mutation: users of addEntry_mutant can't
  // reason just by substitution - they also have to track any part of the
  // store that addEntry_mutant affects (in this case, theBook)

  // We also have to reason about the whole program instead of just the
  // parts that we can see
  return true;
}

// -------------------------------------------------------------------------

// isPrime_accum : int int -> bool
// Determines whether n is prime, IN ACCUMULATOR STYLE
// Assumes n >= 2
bool isPrime_accum(int n, int current) {
  if (n <= current) {
    return true;  // didn't find a divisor
  } else {
    if (n % current == 0) {
      return false;  // found a divisor
    } else {
      return isPrime_accum(n, current + 1);  // look for more divisors
    }
  }
}

// isPrime : int -> bool
// Returns true if and only if n is prime; wrapper for isPrime_accum
bool isPrime(int n) {
  if (n < 2) {
    // printf("Answer is %s, should be %s\n", isPrime(0), false);
    // printf("Answer is %s, should be %s\n", isPrime(1), false);
    return false;
  } else {
    return isPrime_accum(n, 2);
  }
}

int main() {
  {
    /*
      int i = 1;
      printf("Answer is %d, should be %d\n", i, 1);
      int j = i;
      printf("Answer is %d, should be %d\n", j, 1);
      i = i + 1;
      printf("Answer is %d, should be %d\n", i, 2);
      // This is wrong because we substituted from the text of the program
      // When we use mutation (like i = i + 1) we have to substitute from
      // the "store"
      printf("Answer is %d, should be %d\n", j, 2);
    */
        
    int i = 1;
    // store: i = 1
    printf("Answer is %d, should be %d\n", i, 1);
    int j = i;
    // store: i = 1, j = 1
    printf("Answer is %d, should be %d\n", j, 1);
    i = i + 1;
    // store: i = 2, j = 1
    printf("Answer is %d, should be %d\n", i, 2);
    printf("Answer is %d, should be %d\n", j, 1);
  }

  {
    printf("Answer is %d, should be %d\n", factorial(0), 1);
    printf("Answer is %d, should be %d\n", factorial(1), 1);
    printf("Answer is %d, should be %d\n", factorial(2), 2);
    printf("Answer is %d, should be %d\n", factorial(3), 6);
    printf("Answer is %d, should be %d\n", factorial(6), 720);

    printf("Answer is %d, should be %d\n", factorial_mutant(0), 1);
    printf("Answer is %d, should be %d\n", factorial_mutant(1), 1);
    printf("Answer is %d, should be %d\n", factorial_mutant(2), 2);
    printf("Answer is %d, should be %d\n", factorial_mutant(3), 6);
    printf("Answer is %d, should be %d\n", factorial_mutant(6), 720);

    // Reasoning about the old, broken factorial_mutant
    /*
      int n = 3;
      int answer = 1;
      // store: n = 3, answer = 1
        
      if (3 != 0) {
      n = 3 - 1;
      // store: n = 2, answer = 1
      answer = 2 * 1;
      // store: n = 2, answer = 2
      }

      if (2 != 0) {
      n = 2 - 1;
      // store: n = 1, answer = 2
      answer = 1 * 2;
      // store: n = 1, answer = 2
      }

      if (1 != 0) {
      n = 1 - 1;
      // store: n = 0, answer = 2
      answer = 0 * 2;
      // store: n = 0, answer = 0
      }

      printf("Answer is %d\n", answer);
    */

    // Reasoning about the new, working factorial_mutant:
    int n = 3;
    int answer = 1;
    // store: n = 3, answer = 1

    if (3 != 0) {
      int next_n = 3 - 1;
      // store: n = 3, answer = 1, next_n = 2
      int next_answer = 3 * 1;
      // store: n = 3, answer = 1, next_n = 2, next_answer = 3
      n = next_n;
      // store: n = 2, answer = 1, next_n = 2, next_answer = 3
      answer = next_answer;
      // store: n = 2, answer = 3, next_n = 2, next_answer = 3
    }
    // store: n = 2, answer = 3

    if (2 != 0) {
      int next_n = 2 - 1;
      // store: n = 2, answer = 3, next_n = 1
      int next_answer = 2 * 3;
      // store: n = 2, answer = 3, next_n = 1, next_answer = 6
      n = next_n;
      // store: n = 1, answer = 3, next_n = 1, next_answer = 6
      answer = next_answer;
      // store: n = 1, answer = 6, next_n = 1, next_answer = 6
    }
    // store: n = 1, answer = 6

    if (1 != 0) {
      int next_n = 1 - 1;
      // store: n = 1, answer = 6, next_n = 0
      int next_answer = 1 * 6;
      // store: n = 1, answer = 6, next_n = 0, next_answer = 6
      n = next_n;
      // store: n = 0, answer = 6, next_n = 0, next_answer = 6
      answer = next_answer;
      // store: n = 0, answer = 6, next_n = 0, next_answer = 6
    }
    // store: n = 0, answer = 6
        
    /*
      while (0 != 0) {
      int next_n = n - 1;
      int next_answer = n * answer;
      n = next_n;
      answer = next_answer;
      }
    */

    printf("Answer is %d\n", answer);

    // These are the same because factorial_mutant uses INTERNAL mutation
    // i.e. its mutation is not visible to any user of it, so we can replace
    // its application factorial_mutant(3) with the answer 6
    // INTERNAL mutation lets you reason by substitution (on the outside)
    printf("Answer is %d, should be %d\n", factorial_mutant(factorial_mutant(3)), 720);
    printf("Answer is %d, should be %d\n", factorial_mutant(6), 720);
  }

  {
    // http://download.oracle.com/javase/7/docs/api/java/io/FileReader.html
    FILE* f = fopen("pre-18.cc", "r");
    // store: f = file "scratch3.java" at position 0
    printf("Answer is '%c', should be '%c'\n", getc(f), '#');
    // store: f = file "scratch3.java" at position 1
    printf("Answer is '%c', should be '%c'\n", getc(f), 'i');
    // store: f = file "scratch3.java" at position 2

    int c = getc(f);
    // store: f = file "scratch3.java" at position 3, c = 'p'
    if ('p' != -1) {
      printf("%c", c);
      c = getc(f);
      // store: f = file "scratch3.java" at position 4, c = 'o'
    }

    if ('o' != -1) {
      printf("%c", c);
      c = getc(f);
      // store: f = file "scratch3.java" at position 5, c = 'r'
    }

    while (c != -1) {
      printf("%c", c);
      c = getc(f);
    }
  }

  {
    nonMutationTests();
    mutationTests();
  }

  {
    printf("Answer is %d, should be %d\n", isPrime(0), false);
    printf("Answer is %d, should be %d\n", isPrime(1), false);
    printf("Answer is %d, should be %d\n", isPrime(2), true);
    printf("Answer is %d, should be %d\n", isPrime(3), true);
    printf("Answer is %d, should be %d\n", isPrime(4), false);
    printf("Answer is %d, should be %d\n", isPrime(5), true);
    printf("Answer is %d, should be %d\n", isPrime(6), false);
    printf("Answer is %d, should be %d\n", isPrime(7), true);
  }
}
