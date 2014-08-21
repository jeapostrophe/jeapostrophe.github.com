#lang planet jaymccarthy/c

#include <stdio.h>

void f ( int x ) {
 printf("f before %d\n", x);
 x = 1;
 printf("f after %d\n", x);
 return;
}

void g ( int *x ) {
 printf("g before %d\n", *x);
 *x = 1;
 printf("g after %d\n", *x);
 return;
}

void h ( int &x ) {
 printf("h before %d\n", x);
 x = 1;
 printf("h after %d\n", x);
 return;
}

void swap ( int &x, int &y ) {
 int tmp = x;
 x = y;
 y = tmp;
 return;
}

int main() {
 int x = 0;
 printf("main init %d\n", x);
 x = 1;
 printf("main after 1 %d\n", x);
 printf("main after 2 %d\n", x = 2);
 
 f(x);
 printf("main after f %d\n", x);
 
 g(&x);
 printf("main after g %d\n", x);
 
 x = 2;
 h(x);
 printf("main after h %d\n", x);
 
 /*
 struct { int contents } box = { contents: 0 };
 printf("%d\n", box.contents);
 box.contents = 1;
 printf("%d\n", box.contents);
 */
 
 f(0);
 
 int a = 0;
 int b = 1;
 printf("%d %d\n", a, b);
 swap(a, b);
 printf("%d %d\n", a, b);
 
 // lvalue???!??!?
 // items * price = total
 /*
 int items = 5000;
 int price = 6000;
 int total = 0;
 *(int *)(items * price) = total;
 */
 
 return 0;
}