// gcc -fPIC -shared cbits.c -o cbits.so
#include <stdio.h>

double putchard(double x) {
  putchar((char)x);
  return 0;
}

double putd(double x) {
  printf("%f\n", x);
  return 0;
}