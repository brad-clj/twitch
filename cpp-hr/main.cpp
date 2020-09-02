// https://www.hackerrank.com/challenges/ctci-fibonacci-numbers/problem

#include <cstddef>
#include <cstdint>

#include <iostream>
using std::cin;
using std::cout;

int32_t solve(int32_t n) {
  if (n == 0) {
    return 0;
  }
  if (n == 1) {
    return 1;
  }
  int32_t a = 0, b = 1, tmp;
  for (int32_t i=2; i <= n; ++i) {
    tmp = a + b;
    a = b;
    b = tmp;
  }
  return b;
}

int main()
{
  int32_t n;
  cin >> n;
  cout << solve(n) << '\n';
}
