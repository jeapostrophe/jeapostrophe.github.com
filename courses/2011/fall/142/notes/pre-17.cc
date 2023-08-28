#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

// A natural number is
//   0
//   n + 1
// where n is a natural number

// A Tree is either a Node or Empty
class Tree {
public:
  // numberOfNodes : Tree -> int
  // Returns the number of nodes in a tree
  virtual int numberOfNodes() = 0;

  // numberOfNodes_accum : Tree int -> int
  // Returns the number of nodes in a tree IN ACCUMULATOR STYLE
  virtual int numberOfNodes_accum(int answer) = 0;
};

// An Empty is
//   Empty
class Empty : public Tree {
public:
  Empty() {
  }

  int numberOfNodes() {
    // printf("The answer is\n   %d\nbut should be\n   %d\n", new Empty().numberOfNodes(), 0);
    return 0;
  }

  int numberOfNodes_accum(int answer) {
    return answer;
  }
};

// A Node is
//   Node(name, left, right)
// where name is a String and left and right are Trees
class Node : public Tree {
public:
  const char* name;
  Tree* left;
  Tree* right;

  Node(const char* name, Tree* left, Tree* right) {
    this->name = name;
    this->left = left;
    this->right = right;
  }

  int numberOfNodes() {
    // printf("The answer is\n   %d\nbut should be\n   %d\n", A->numberOfNodes(), 1);
    // return 1;

    // ... Tree C ... C.left ... C.right ...
    // ... C.left->numberOfNodes ... C.right->numberOfNodes ...
    // printf("The answer is\n   %d\nbut should be\n   %d\n", C->numberOfNodes(), 3);
    return 1 + this->left->numberOfNodes() + this->right->numberOfNodes();
  }

  int numberOfNodes_accum(int answer) {
    // ... A ... answer ... A.left ... A.right ...
    // ... A.left->numberOfNodes_accum ... A.right->numberOfNodes_accum ...

    // answer = 0
    // A.left->numberOfNodes_accum(1 + answer) = 1
    // A.right->numberOfNodes_accum(1 + answer) = 1
    //printf("The answer is\n   %d\nbut should be\n   %d\n", A->numberOfNodes_accum(0), 1);
        
    int left_answer = this->left->numberOfNodes_accum(answer);
    int right_answer = this->right->numberOfNodes_accum(left_answer);
    int my_answer = 1 + right_answer;
    return my_answer;
  }
};


// factorial : int -> int
// factorial(n) = 1 * 2 * ... * (n-1) * n
// factorial(n) = factorial(n-1) * n, n > 0
int factorial(int n) {
  // ... n ... factorial ...
  if (n == 0) {
    return 1;
  } else {
    // n = 2
    // printf("The answer is\n   %d\nbut should be\n   %d\n", factorial(1), 1);
    return n * factorial(n - 1);
  }
}

// factoral_accum : int int -> int
// Computes factorial, in accumulator style
int factorial_accum(int n, int answer) {
  // ... n ... answer ... factorial_accum ...

  if (n == 0) {
    // n = 0
    // answer = 1
    // printf("The answer is\n   %d\nbut should be\n   %d\n", factorial_accum(0, 1), 1);
    return answer;
  } else {
    // n = 3
    // answer = 1
    // printf("The answer is\n   %d\nbut should be\n   %d\n", factorial_accum(3, 1), 6);
    return factorial_accum(n - 1, n * answer);
  }

}

int main() {
  printf("The answer is\n   %d\nbut should be\n   %d\n", factorial(0), 1);
  printf("The answer is\n   %d\nbut should be\n   %d\n", factorial(1), 1);
  printf("The answer is\n   %d\nbut should be\n   %d\n", factorial(2), 2);
  printf("The answer is\n   %d\nbut should be\n   %d\n", factorial(3), 6);
  printf("The answer is\n   %d\nbut should be\n   %d\n", factorial(6), 720);
  printf("The answer is %d\n", factorial(6));
  printf("The answer is %d\n", 6 * factorial(6 - 1));
  printf("The answer is %d\n", 6 * factorial(5));
  printf("The answer is %d\n", 6 * (5 * factorial(4)));
  printf("The answer is %d\n", 6 * (5 * (4 * factorial(3))));
  printf("The answer is %d\n", 6 * (5 * (4 * (3 * (2 * (1 * 1))))));
  //
  printf("The answer is\n   %d\nbut should be\n   %d\n", factorial_accum(0, 1), 1);
  printf("The answer is\n   %d\nbut should be\n   %d\n", factorial_accum(1, 1), 1);
  printf("The answer is\n   %d\nbut should be\n   %d\n", factorial_accum(2, 1), 2);
  printf("The answer is\n   %d\nbut should be\n   %d\n", factorial_accum(3, 1), 6);
  printf("The answer is %d\n", factorial_accum(4, 1));
  printf("The answer is %d\n", factorial_accum(4 - 1, 4 * 1));
  printf("The answer is %d\n", factorial_accum(3, 4));
  printf("The answer is %d\n", factorial_accum(3 - 1, 3 * (4 * 1)));
  printf("The answer is %d\n", factorial_accum(2, 12));
  printf("The answer is %d\n", factorial_accum(2 - 1, 2 * (3 * (4 * 1))));
  printf("The answer is %d\n", factorial_accum(1, 24));
  printf("The answer is %d\n", factorial_accum(1 - 1, 1 * (2 * (3 * (4 * 1)))));
  printf("The answer is %d\n", factorial_accum(0, 24));
  printf("The answer is %d\n", 24);

  Tree* A = new Node("A", new Empty(), new Empty());
  Tree* B = new Node("B", new Empty(), new Empty());
  Tree* C = new Node("C", A, B);

  printf("The answer is\n   %d\nbut should be\n   %d\n", (new Empty())->numberOfNodes(), 0);
  printf("The answer is\n   %d\nbut should be\n   %d\n", A->numberOfNodes(), 1);
  printf("The answer is\n   %d\nbut should be\n   %d\n", B->numberOfNodes(), 1);
  printf("The answer is\n   %d\nbut should be\n   %d\n", C->numberOfNodes(), 3);
  //
  printf("The answer is\n   %d\nbut should be\n   %d\n", (new Empty())->numberOfNodes_accum(0), 0);
  printf("The answer is\n   %d\nbut should be\n   %d\n", A->numberOfNodes_accum(0), 1);
  printf("The answer is\n   %d\nbut should be\n   %d\n", C->numberOfNodes_accum(0), 3);
}

