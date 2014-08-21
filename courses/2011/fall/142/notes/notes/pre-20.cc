#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

// sumArray_accum : int[] int int -> int
int sumArray_accum(int ns[], int ns_len, int i, int answer) {
  // ... ns ... i ... answer ...
  // ... ns.length ... ns[i] (only if i < ns.length) ...
  // ... sumArray_accum ...
        
  //printf("Answer is %d, should be %d\n", sumArray_accum(nums0, 0, 0), 0);
  // ns = new int[] {}
  // i = 0
  // ns.length = 0
  if (i == ns_len) {
    return answer;
  } else {
    //printf("Answer is %d, should be %d\n", sumArray_accum(nums1, 0, 0), 4);
    // ns = new int[] {4}
    // i = 0
    // ns.length = 1
    // answer = 0
    return sumArray_accum(ns, ns_len, i + 1, answer + ns[i]);
  }
}

// sumArray_while : int[] -> int
// Returns the sum of the elements of the array
int sumArray_while(int ns[], int ns_len) {
  // 0. define accumulators
  int answer = 0;
  int i = 0;
  // 1. while loop with inductive case condition
  while (i != ns_len) {
    // 2. update the accumulators
    answer = answer + ns[i];
    i = i + 1;
  }
  // 3. return the base-case answer
  return answer;
}

// sumArray_for : int[] -> int
// Returns the sum of the elements of the array, using FOR!!!1
int sumArray_for(int ns[], int ns_len) {
  int answer = 0;
  for (int i = 0; i != ns_len; i = i + 1) {
    answer = answer + ns[i];
  }
  // don't care about the value of i
  return answer;
}

int Arrays_print(int ns[], int ns_len) {
  for (int i = 0; i != ns_len; i = i + 1) {
    printf("%d, ", ns[i]);
  }
  return 0;
}

class Posn {
public:
  int x;
  int y;

  Posn(int x, int y) {
    this->x = x;
    this->y = y;
  }

  int print() {
    return printf("(%d, %d)", this->x, this->y);
  }

  // reflect : Posn -> Posn
  Posn* reflect() {
    return new Posn(this->y, this->x);
  }

  // reflect_this : Posn -> void
  void reflect_this() {
    // Doesn't work:
    /*
    // store: this->x = a, this->y = b
    this->x = this->y;
    // store: this->x = b, this->y = b
    this->y = this->x;
    // store: this->x = b, this->y = b
    */

    // store: this->x = a, this->y = b
    int temp = this->x;
    // store: this->x = a, this->y = b, temp = a
    this->x = this->y;
    // store: this->x = b, this->y = b, temp = a
    this->y = temp;
    // store: this->x = b, this->y = a, temp = a
  }
};

// reflectAll : Posn[] -> void
// Reflects all positions in the array, by mutating the array
void reflectAll(Posn* ps[], int ps_len) {
  for (int i = 0; i < ps_len; i = i + 1) {
    ps[i] = ps[i]->reflect();
  }
}

// reflectAllNoNew : Posn[] -> void
// Reflects all positions in the array, by mutating the positions
void reflectAllNoNew(Posn* ps[], int ps_len) {
  for (int i = 0; i < ps_len; i++) {
    ps[i]->reflect_this();
  }
}

// Unrolling for loops

// sum4 : -> void
// Computes 0 + (0 + 1 + 2 + 3), the sum of the first four natural numbers,
// and prints it.
void sum4() {
  int answer = 0;
  for (int i = 0; i < 4; i = i + 1) {
    answer = answer + i;
  }
  // Prints "Answer is 6\n"
  printf("Answer is %d\n", answer);
}
    
// sum4_unrolled : -> void
// This is equivalent to sum4 above, but the for loop is unrolled.
// Fortunately, the for loop is simple enough that we can unroll it without
// turning it into a while loop first, by copying the statement inside and
// replacing i with the values it will have.
// We could do store-tracking on this version of sum4.
void sum4_unrolled() {
  int answer = 0;
  answer = answer + 0;
  answer = answer + 1;
  answer = answer + 2;
  answer = answer + 3;
  // Prints "Answer is 6\n"
  printf("Answer is %d\n", answer);
}
    
// ------------------------------------------------------------------------
    
// We can't always unroll a for loop without turning it into a while loop
// first. The loop in badSum4 is one example:

// badSum4 : -> void
// Apparently tries to compute 0 + (0 + 1 + 2 + 3) but actually never
// returns.
void badSum4() {
  int answer = 0;
  for (int i = 0; i < 4; i = i + 1) {
    answer = answer + i;
    i = i - 1;  // WTH? This changes i, which we're looping over...
  }
  // The loop never stops, so Java never prints an answer. Bummer!
        
  printf("Answer is %d\n", answer);
}
    
// If we tried to unroll the for loop in badSum4, we would never stop
// unrolling. But we can unroll a while loop one iteration at a time.
// First, we change the for loop into a while loop:

// badSum4_while : -> void
// Equivalent to badSum4, but the for loop is changed to a while loop. Take
// some time to figure out where, in sum4, the statements were copied from.
void badSum4_while() {
  int answer = 0;
  int i = 0;
  while (i < 4) {
    answer = answer + i;
    i = i - 1;            
    i = i + 1;
  }
  printf("Answer is %d\n", answer);
}
    
// Then we unroll one iteration, by copying the loop and changing "while"
// to "if" in the upper copy.
    
// badSum4_while_unrolled : -> void
// Equivalent to badSum4_while, but with the while loop unrolled one
// iteration. Also has store-tracking comments.
void badSum4_while_unrolled() {
  int answer = 0;
  int i = 0;
  // store: answer = 0, i = 0
        
  if (i < 4) {
    answer = answer + i;
    // store: answer = 0, i = 0
    i = i - 1;
    // store: answer = 0, i = -1
    i = i + 1;
    // store: answer = 0, i = 0
  }
        
  // Ah. That's the problem: i = 0 in the store after every iteration.
  // Because 0 < 4, the while condition will always be true.
        
  while (i < 4) {
    answer = answer + i;
    i = i - 1;
    i = i + 1;
  }
  printf("Answer is %d\n", answer);
}
    
// ------------------------------------------------------------------------
    
// In the case of badSum4_while, we don't actually have to unroll the loop
// to figure out why it loops forever. We can do store-tracking in the loop
// by using names for unknown values, as in reflect_this in scratch3.java.

// badSum4_while2 : -> void
// Equivalent to badSum4_while. Has store-tracking comments within the
// while loop.
void badSum4_while2() {
  int answer = 0;
  int i = 0;
  while (i < 4) {
    // store: answer = a, i = b
    answer = answer + i;
    // store: answer = a + b, i = b
    i = i - 1;
    // store: answer = a + b, i = b - 1
    i = i + 1;
    // store: answer = a + b, i = b - 1 + 1 = b
            
    // If i < 4 in any iteration, then i < 4 in the next iteration.
    // Therefore, the loop will never stop.
  }
  printf("Answer is %d\n", answer);
}

int Arrays_printString(const char* ns[], int ns_len) {
  for (int i = 0; i != ns_len; i = i + 1) {
    printf("%s, ", ns[i]);
  }
  return 0;
}

int Arrays_printPosn(Posn* ns[], int ns_len) {
  for (int i = 0; i != ns_len; i = i + 1) {
    printf("%d, ", ns[i]->print());
  }
  return 0;
}

//

int main( int argv, const char* argc[]) {
  {
    int nums4[] = {4, 5, 6, 7};
    int nums4_len = 4;
    printf("The element at index 0 is %d\n", nums4[0]);
    printf("The element at index 1 is %d\n", nums4[1]);
    printf("The element at index 2 is %d\n", nums4[2]);
    printf("The element at index 3 is %d\n", nums4[3]);
    printf("The length is %d\n", nums4_len);

    // Doesn't work: get ArrayIndexOutOfBoundsException
    // printf("The element at index 4 is %d\n", nums4[4]);
    // nums4[4] = 8;
    // Can't change the length of an array
    // nums4.length = 5;

    nums4[0] = 40;
    printf("The element at index 0 is %d\n", nums4[0]);
    printf("The array is %d\n", Arrays_print(nums4, nums4_len));
        
    int nums0[] = {}; int nums0_len = 0;
    int nums1[] = {4}; int nums1_len = 1;
    int nums2[] = {4, 5}; int nums2_len = 2;
    printf("Answer is %d, should be %d\n", sumArray_accum(nums0, nums0_len, 0, 0), 0);
    printf("Answer is %d, should be %d\n", sumArray_accum(nums1, nums1_len, 0, 0), 4);
    printf("Answer is %d, should be %d\n", sumArray_accum(nums2, nums2_len, 0, 0), 9);
    printf("Answer is %d, should be %d\n",
           sumArray_accum(nums2, nums2_len, 0 + 1, 0 + nums2[0]), 9);
    printf("Answer is %d, should be %d\n",
           sumArray_accum(nums2, nums2_len, 1, 4), 9);
    printf("Answer is %d, should be %d\n",
           sumArray_accum(nums2, nums2_len, 1 + 1, 4 + nums2[1]), 9);
    printf("Answer is %d, should be %d\n",
           sumArray_accum(nums2, nums2_len, 2, 4 + 5), 9);
    printf("Answer is %d, should be %d\n",
           sumArray_accum(nums2, nums2_len, 2, 9), 9);
    printf("Answer is %d, should be %d\n", 9, 9);

    printf("sumArray_while\n");
    printf("Answer is %d, should be %d\n", sumArray_while(nums0, nums0_len), 0);
    printf("Answer is %d, should be %d\n", sumArray_while(nums1, nums1_len), 4);
    printf("Answer is %d, should be %d\n", sumArray_while(nums2, nums2_len), 9);

    printf("sumArray_for\n");
    printf("Answer is %d, should be %d\n", sumArray_for(nums0, nums0_len), 0);
    printf("Answer is %d, should be %d\n", sumArray_for(nums1, nums1_len), 4);
    printf("Answer is %d, should be %d\n", sumArray_for(nums2, nums2_len), 9);
  }
  {
    printf("The command-line arguments are %d\n",
           Arrays_printString(argc, argv));
  }
  {
    Posn* posns[] = {new Posn(1, 2), new Posn(10, 20)};
    Posn* after_posns[] = {new Posn(2, 1), new Posn(20, 10)};
    int posns_len = 2;
    reflectAll(posns, posns_len);
    printf("Answer is %d, should be %d\n",
           Arrays_printPosn(posns, posns_len),
           Arrays_printPosn(after_posns, 2));
  }
  {
    sum4();
    sum4_unrolled();
        
    // Don't call any versions of badSum4, because they loop forever
    //badSum4();
  }
}
