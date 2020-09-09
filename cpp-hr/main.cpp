#include <cstddef>
#include <cstdint>

#include <iostream>
using std::cin;
using std::cout;

#include <vector>
using std::vector;

#include <algorithm>
using std::sort;
using std::max;

#include <functional>
using std::greater;

uint8_t get_high_bit(int32_t num)
{
  int32_t bit = 0;
  while (num) {
    num &= ~(1 << bit);
    ++bit;
  }
  return bit;
}

int32_t fill_bits(uint8_t bits)
{
  int32_t ret = 0;
  while (bits) {
    --bits;
    ret |= 1 << bits;
  }
  return ret;
}

int32_t solve(const vector<int32_t> &arr, int32_t q)
{
  auto bit = max(get_high_bit(q), get_high_bit(arr[0]));
  return 0;
}

int main()
{
  int32_t n;
  cin >> n;

  vector<int32_t> arr(n);
  for (int32_t i = 0; i < n; ++i) {
    cin >> arr[i];
  }
  
  std::sort(arr.begin(), arr.end(), greater<int32_t>());

  int32_t m;
  cin >> m;

  for (int32_t i = 0; i < m; ++i) {
    int32_t q;
    cin >> q;
    cout << solve(arr, q) << "\n";
  }
}
