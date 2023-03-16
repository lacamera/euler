#include <stdio.h>

int fibonacci(int n) {
  if (n < 2) return n;
  return (fibonacci(n-1)+fibonacci(n-2));
}

int main() {
 int fib = 0;
 int sum = 0;
 int n = 2;

 do {
  fib = fibonacci(n);
  if (fib%2 == 0)
    sum += fib;
  n++;
 } while (fib <= 4000000);

 printf("%d\n", sum);
 return 0;
}
