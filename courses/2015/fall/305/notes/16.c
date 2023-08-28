#include <stdio.h>

#define M 20000
#define N 40000

int arr[M][N];

long sum_ij() {
  long sum = 0;

  for (int i = 0; i < M; i++) {
    for (int j = 0; j < N; j++) {
      sum += arr[i][j];
    }
  }

  return sum;
}

long sum_ji() {
  long sum = 0;

  for (int j = 0; j < N; j++) {
    for (int i = 0; i < M; i++) {
      sum += arr[i][j];
    }
  }

  return sum;
}

long sum() {
  if (0) {
    return sum_ij();
  } else {
    return sum_ji();
  }
}

int main(int argc, char **argv) {
  printf("sum is %ld\n", sum());

  return 0;
}

void f() {};
void g() {};
void h() {};

void example() {
  /* for (int i = 0; i < 1000; i++) { */
  /*   f(); */
  /*   g(); */
  /*   h(); */
  /* } */
  for (int i = 0; i < 1000; i++) {
    f();
  }
  for (int i = 0; i < 1000; i++) {
    g();
  }
  for (int i = 0; i < 1000; i++) {
    h();
  }
}
