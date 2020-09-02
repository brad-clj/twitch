// https://www.hackerrank.com/challenges/ctci-is-binary-search-tree/problem

#include <cstddef>
#include <cstdint>

#include <iostream>
using std::cin;
using std::cout;

#include <vector>
using std::vector;

struct Node {
  int data;
  Node* left;
  Node* right;
};

struct Bounds {
  Node* node;
  int left;
  int right;
};

bool checkBST(Node* root)
{
  vector<Bounds> stack {{root, -1, -1}};
  while (stack.size()) {
    auto bound = stack.back();
    stack.pop_back();
    if (bound.node != nullptr) {
      if ((bound.left < 0 or bound.node->data > bound.left) and
          (bound.right < 0 or bound.node->data < bound.right)) {
        stack.push_back({bound.node->left, bound.left, bound.node->data});
        stack.push_back({bound.node->right, bound.node->data, bound.right});
      } else {
        return false;
      }
    }
  }
  return true;
}

int main()
{
  auto n = new Node{1};
  n->left = new Node{2};
  //n->right = new Node{2};
  cout << checkBST(n) << '\n';
}
