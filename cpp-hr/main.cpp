#include <cstddef>
#include <cstdint>

#include <algorithm>
using std::min;
#include <iostream>
using std::cin;
using std::cout;
#include <tuple>
using std::make_tuple;
using std::tie;
using std::tuple;
#include <unordered_map>
using std::unordered_map;
#include <vector>
using std::vector;

struct Node {
  int64_t sum;
  int32_t tim_start;
  int32_t tim_end;
};

bool operator==(const Node& lhs, const Node& rhs)
{
  return lhs.tim_start == rhs.tim_start;
}

bool operator!=(const Node& lhs, const Node& rhs)
{
  return !(lhs == rhs);
}

bool operator<(const Node& lhs, const Node& rhs)
{
  return lhs.tim_start < rhs.tim_start;
}

bool operator>(const Node& lhs, const Node& rhs)
{
  return rhs < lhs;
}

bool is_related(const Node& ancestor, const Node& descendant) {
  return (descendant.tim_start > ancestor.tim_start &&
          descendant.tim_start <= ancestor.tim_end);
}

struct Solver {
  int64_t mini = INT64_MAX;
  int64_t sum = 0;
  int32_t n;
  vector<Node> nodes;
  vector<int64_t> c;
  vector<vector<int32_t>> graph;
  unordered_map<int64_t, Node> sums_min_node;
  unordered_map<int64_t, Node> sums_max_node;

  Solver(int32_t n)
    :n{n},
     nodes(size_t(n)),
     c(size_t(n)),
     graph(size_t(n))
  {}

  void dfs()
  {
    int32_t tim = 0;
    vector<tuple<size_t, int32_t, bool>> stack {make_tuple(0, 0, false)};
    while (stack.size() > 0) {
      size_t x;
      int32_t prev;
      bool done;
      tie(x, prev, done) = stack.back();
      stack.pop_back();
      auto& node = nodes[x];
      if (!done) {
        ++tim;
        node.tim_start = tim;
        node.sum = c[x];
        stack.push_back(make_tuple(x, prev, true));
        for (const auto y : graph[x]) {
          if (y != prev) {
            stack.push_back(make_tuple(y, x, false));
          }
        }
      } else {
        for (const auto y : graph[x]) {
          if (y != prev) {
            node.sum += nodes[y].sum;
          }
        }
        node.tim_end = tim;
      }
    }
  }

  void generate_sums()
  {
    for (const auto& node : nodes) {
      if (sums_min_node.count(node.sum) == 0) {
        sums_min_node[node.sum] = node;
        sums_max_node[node.sum] = node;
      } else {
        if (node < sums_min_node[node.sum]) {
          sums_min_node[node.sum] = node;
        }
        if (node > sums_max_node[node.sum]) {
          sums_max_node[node.sum] = node;
        }
      }
    }
  }

  // this gets called when the node is bigger than a third of the sum but less than half of the sum. so we are looking for
  // another node of equal size and the remaindr is the add value.
  void do_it(const Node& node)
  {
    const int64_t add = 3 * node.sum - sum;
    if (// this we have 2 nodes that are different with the same sum as the node that is being passed in
        (sums_min_node[node.sum] != sums_max_node[node.sum]) ||
        // checks if we have a node which isn't related so could be used to remove part of the remainder sum (sum - node.sum)
        // to get a subtree that is equal to node.sum. subtree being ancestor nodes but with some descendants removed
        (sums_min_node.count(node.sum - add) > 0 &&
         sums_min_node[node.sum - add] != node &&
         ! is_related(node, sums_min_node[node.sum - add])) ||
        // ditto..
        (sums_max_node.count(node.sum - add) > 0 &&
         sums_max_node[node.sum - add] != node &&
         ! is_related(node, sums_max_node[node.sum - add])) ||
        // i guess if we have a node with a sum of either of these 2 values it just works???
        (sums_min_node.count(2 * node.sum - add) > 0) ||
        (sums_min_node.count(2 * node.sum) > 0)) {
      mini = min(mini, add);
    }
  }

  // doesn't do anything if the remainder sum is odd, since we jump in to this only when the nodes is smaller than a third of
  // the total sum. if we did anything when the remainder is odd we wouldn't be able to have a split on the remainder that
  // would make two equal parts.
  void uradi(const Node& node)
  {
    if ((sum - node.sum) % 2 == 0) {
      const int64_t p = (sum - node.sum) / 2;
      if (// the node with the sum of p is not my ancestor, thus can be used as one of the bigger graph segements, and my
          // ancestors minus that p node will be the other half.
          (sums_min_node.count(p) > 0 &&
           sums_min_node[p] != node &&
           ! is_related(sums_min_node[p], node)) ||
          // dito, but max node...
          (sums_max_node.count(p) > 0 &&
           sums_max_node[p] != node &&
           ! is_related(sums_max_node[p], node)) ||
          // nodes exists that is ancestor that would match the size we need if we remove our node.sum from theirs
          (sums_min_node.count(p + node.sum) > 0 &&
           is_related(sums_min_node[p + node.sum], node)) ||
          // dito, but max node...
          (sums_max_node.count(p + node.sum) > 0 &&
           is_related(sums_max_node[p + node.sum], node))) {
        mini = min(mini, p - node.sum);
      }
    }
  }

  void solve()
  {
    for (int32_t i = 0; i < n; i++) {
      cin >> c[i];
      sum += c[i];
    }
    for (int32_t i = 0; i < n-1; i++) {
      int32_t x, y;
      cin >> x >> y;
      graph[x-1].push_back(y-1);
      graph[y-1].push_back(x-1);
    }
    dfs();
    generate_sums();
    if (sum % 2 == 0 &&
        sums_min_node.count(sum / 2) > 0) {
      mini = sum / 2;
    }
    for (const auto& node : nodes) {
      if (3 * node.sum <= sum) {
        uradi(node);
      } else if (2 * node.sum < sum) {
        do_it(node);
      }
    }
    if (mini == INT64_MAX) {
      cout << "-1\n";
    } else {
      cout << mini << "\n";
    }
  }
};

int main()
{
  int32_t q;
  cin >> q;
  for (int32_t i = 0; i < q; ++i) {
    int32_t n;
    cin >> n;
    Solver s(n);
    s.solve();
  }
}
