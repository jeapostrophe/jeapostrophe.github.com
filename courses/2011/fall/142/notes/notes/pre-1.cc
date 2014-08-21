#include <stdio.h>
#include <math.h>
#include <stdlib.h>

int main () {
  // x / y = z
  // => y * z = x
  // 91 / 2 = 45
  // => 45 * 2 = 91
  // %d => no decimals, "integer"
  printf("The answer is: %d\n", 91 / 2);
  // %f => decimals, "floating point"
  printf("The answer is: %f\n", 91.0 / 2);
  printf("The answer is: %f\n", sqrt(9));
  printf("The answer is: %f\n", sqrt(11));
  printf("The answer is: %f\n", sqrt(36481));
  printf("The answer is: %f\n", sqrt(-1));

  printf("The answer is: %d\n", abs(-2));
  printf("The answer is: %d\n", abs(2));

  printf("The answer is: %f\n", log(146.3));

  printf("The answer is: %f\n", pow( 23, 1067 ));
  printf("The answer is: %f\n", pow( 23, 6 ));

  printf("The answer is: %d\n", 2 + 3 * 6);
  printf("The answer is: %d\n", (2 + 3) * 6);
  printf("The answer is: %d\n", ((2 + 3) * 6));

  printf("The answer is: %d\n", (((2 + 3) * (6))));
	
  printf("The answer is: %f\n", pow( (20+3), 6 ));
  printf("The answer is: %f\n", pow( (20+3), (2+2+2) ));
  printf("The answer is: %f\n", pow( (20+3), sqrt(36) ));

  return 0;
}
