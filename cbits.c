// gcc -fPIC -shared cbits.c -o cbits.so
#include <stdio.h>

double putchard(double x) {
  printf("%f\n", x);
  return 0;
}