#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>

int partition(int[], int, int);
void insertion_sort(int[], int, int);

void quicksort(int A[], int p, int r) {
    if (p < r - 1) {
        int q = partition(A, p, r);
        quicksort(A, p, q);
        quicksort(A, q + 1, r);
    }
}

#define K 32
void limited_quicksort(int A[], int p, int r) {
    if (r - p > K) {
        int q = partition(A, p, r);
        limited_quicksort(A, p, q);
        limited_quicksort(A, q + 1, r);
    }
}

void insertion_sort(int A[], int p, int r) {
    int i, j, key;

    for (j = p + 1; j < r; j++) {
        key = A[j];
        for (i = j - 1; i >= p && A[i] > key; i--) {
            A[i + 1] = A[i];
        }
        A[i + 1] = key;
    }
}

void new_quicksort(int A[], int p, int r) {
  limited_quicksort(A, p, r);
  insertion_sort(A, p, r);
}

int partition(int A[], int p, int r) {
    int x, i, j, tmp;

    x = A[r - 1];
    i = p;

    for (j = p; j < r - 1; j++) {
        if (A[j] <= x) {
            tmp = A[i];
            A[i] = A[j];
            A[j] = tmp;
            i++;
        }
    }

    tmp = A[i];
    A[i] = A[r - 1];
    A[r - 1] = tmp;

    return i;
}

void run_and_time( void (*f)(int[], int, int),
                   int A[], int p, int r ) {
  clock_t before = clock();
  f(A, p, r);
  clock_t after = clock();
  printf("took %lu\n", after-before);
}

int main () {

  int N = 500000;
  int *A = calloc(N, sizeof(int));
  for (int i = 0; i < N; i++) {
    A[i] = random()%N;
  }
  int *B = calloc(N, sizeof(int));
  memcpy( B, A, sizeof(int)*N );

  run_and_time( quicksort, A, 0, N );
  run_and_time( new_quicksort, B, 0, N );
  
  return 0;
}
