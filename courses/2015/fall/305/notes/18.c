#include <stdio.h>
int main(int argc, char **argv) {
  int x;
 start:
  scanf("%d", &x);
  scanf("%d", &x);
  x = x * 2;
  if ( x > 60 ) goto start;
  return x;
}
