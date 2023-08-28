#lang planet jaymccarthy/c

#include <stdio.h>
#include <stdlib.h>

int f(int x) {
 printf("f: x init %d\n", x);
 x = x + 1;
 printf("f: x after 1 %d\n", x);
 return x + 2;
}

int g(int *x) {
 printf("g: x init %d\n", *x);
 *x = *x + 1;
 printf("g: x after 1 %d\n", *x);
 return *x + 2;
}

int h(int &x) {
 printf("h: x init %d\n", x);
 x = x + 1;
 printf("h: x after 1 %d\n", x);
 return x + 2;
}

void swap(int &x, int &y) {
 int tmp = x;
 x = y;
 y = tmp;
 return;
}

int main() {
 int x = 0;
 printf("x init %d\n", x);
 x = 1;
 printf("x after 1 %d\n", x);
 
 int *xb = (int *)malloc(sizeof(int));
 *xb = 0;
 printf("xb init %d\n", *xb);
 *xb = 1;
 printf("xb after 1 %d\n", *xb);

 // x is not a pointer aka a box
 // *x = 1;

 // lvalue????
 // Watson says "left value"?
 // 4 + 4 = 5;
 // ids are okay: x = 1
 // .s are okay: x.field = 2;
 int a[3] = {0, 1, 2};
 printf("a[1] before: %d\n", a[1]);
 *(a + 1) = 2;
 printf("a[1] after: %d\n", a[1]);
 // lvalue = anything with an address
 
 // undeclared:
 // foo = 1;
 
 printf("value of x = 2 %d\n", x = 2);
 
 x = 5;
 f(x);
 printf("value of x after f %d\n", x);
 
 x = 5;
 g(&x); printf("value of x after g %d\n", x);

 x = 5;
 h(x);
 printf("value of x after h %d\n", x);

 int u = 0; int v = 1;
 printf("%d %d\n", u, v);
 swap(u, v);
 printf("%d %d\n", u, v);
 
 // isn't it so much better?
 // swap(&u, &v);
 
 
 return 0;
}