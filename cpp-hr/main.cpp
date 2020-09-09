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
using std::lower_bound;
using std::upper_bound;

#include <functional>
using std::greater;

// https://www.hackerrank.com/challenges/maximum-xor

bool bit_test(int32_t num, uint8_t bit)
{
  return num & (1 << bit);
}

int32_t bit_set(int32_t num, uint8_t bit)
{
  return num | (1 << bit);
}

int32_t bit_clear(int32_t num, uint8_t bit)
{
  return num & ~(1 << bit);
}

uint8_t get_high_bit(int32_t num)
{
  int32_t bit = 0;
  while (num) {
    num = bit_clear(num, bit);
    ++bit;
  }
  return bit;
}

int32_t fill_bits(uint8_t bits)
{
  int32_t ret = 0;
  while (bits) {
    --bits;
    ret = bit_set(ret, bits);
  }
  return ret;
}

int32_t solve(const vector<int32_t> &arr, int32_t q)
{
  auto bit = max(get_high_bit(q), get_high_bit(arr[0]));
  auto upper = fill_bits(bit);
  int32_t lower = 0;
  while (true) {
    const auto is_bit_set = bit_test(q, bit);
    if (is_bit_set) {
      upper = bit_clear(upper, bit);
      lower = bit_clear(lower, bit);
    } else {
      upper = bit_set(upper, bit);
      lower = bit_set(lower, bit);
    }
    const auto upper_i = lower_bound(arr.begin(), arr.end(), upper, greater<int32_t>());
    const auto lower_i = upper_bound(arr.begin(), arr.end(), lower, greater<int32_t>());
    if (upper_i == lower_i) {
      if (is_bit_set) {
        upper = bit_set(upper, bit);
        lower = bit_set(lower, bit);
      } else {
        upper = bit_clear(upper, bit);
        lower = bit_clear(lower, bit);
      }
    } else if (lower_i - upper_i <= 150) {
      int32_t ret = 0;
      for (auto i = upper_i; i != lower_i; ++i) {
        ret = max(ret, *i ^ q);
      }
      return ret;
    }
    --bit;
  }
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
