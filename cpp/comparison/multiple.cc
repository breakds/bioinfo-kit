#include <cstdio>
#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>

using std::vector;
using std::string;
using std::unordered_map;

typedef vector<vector<vector<int> > > Cube;


class CubeTable {
public:
  CubeTable(int m, int n, int p) :
    cube_(m), m_(m), n_(n), p_(p) {
    for (int i = 0; i < m; ++i) {
      cube_[i].resize(n);
      for (int j = 0; j < n; ++j) {
        cube_[i][j].resize(p, 0);
      }
    }
  }
  
  int operator()(int i, int j, int k) {
    if (0 <= i && i < m_ &&
        0 <= j && j < n_ &&
        0 <= k && k < p_) {
      return cube_[i][j][k];
    }
    return 0;
  }
  
  void Set(int i, int j, int k, int value) {
    cube_[i][j][k] = value;
  }
private:
  Cube cube_;
  int m_, n_, p_;
};

int Match(char a, char b, char c) {
  return (a == b && b == c) ? 1 : 0;
}

void GLA(const string &a, const string &b, const string &c) {

  CubeTable score(a.size(), b.size(), c.size());
  CubeTable choice(a.size(), b.size(), c.size());
  // Choice:
  // 0: a b c
  // 1: - b c
  // 2: a - c
  // 3: a b -
  // 4: - - c
  // 5: - b -
  // 6: a - -

  for (int i = 0; i < a.size(); ++i) {
    for (int j = 0; j < b.size(); ++j) {
      for (int k = 0; k < c.size(); ++k) {
        // Case 0:
        choice.Set(i, j, k, 0);
        score.Set(i, j, k, Match(a[i], b[j], c[k]) + score(i - 1, j - 1, k - 1));
                  
        // Case 1:
        if (score(i, j - 1, k - 1) > score(i, j, k)) {
          choice.Set(i, j, k, 1);
          score.Set(i, j, k, score(i, j - 1, k - 1));
        }

        // Case 2:
        if (score(i - 1, j, k - 1) > score(i, j, k)) {
          choice.Set(i, j, k, 2);
          score.Set(i, j, k, score(i - 1, j, k - 1));
        }

        // Case 3:
        if (score(i - 1, j - 1, k) > score(i, j, k)) {
          choice.Set(i, j, k, 3);
          score.Set(i, j, k, score(i - 1, j - 1, k));
        }

        // Case 4:
        if (score(i, j, k - 1) > score(i, j, k)) {
          choice.Set(i, j, k, 4);
          score.Set(i, j, k, score(i, j, k - 1));
        }

        // Case 5:
        if (score(i, j - 1, k) > score(i, j, k)) {
          choice.Set(i, j, k, 5);
          score.Set(i, j, k, score(i, j - 1, k));
        }

        // Case 6:
        if (score(i - 1, j, k) > score(i, j, k)) {
          choice.Set(i, j, k, 6);
          score.Set(i, j, k, score(i - 1, j, k));
        }
      }
    }
  }
  
  printf("%d\n", score(a.size() - 1, b.size() - 1, c.size() - 1));
  
  // Trace back the solution
  string result_a = "";
  string result_b = "";
  string result_c = "";
  {
    int i = a.size() - 1;
    int j = b.size() - 1;
    int k = c.size() - 1;
    vector<int> delta_a {-1, 0, -1, -1, 0, 0, -1};
    vector<int> delta_b {-1, -1, 0, -1, 0, -1, 0};
    vector<int> delta_c {-1, -1, -1, 0, -1, 0, 0}; 
    while (0 <= i || 0 <= j || 0 <= k) {
      int key = choice(i, j, k);
      if (0 <= i && -1 == delta_a[key]) {
          result_a = a[i--] + result_a;
      } else {
        result_a = "-" + result_a;
      }
      if (0 <= j && -1 == delta_b[key]) {
          result_b = b[j--] + result_b;
      } else {
        result_b = "-" + result_b;
      }
      if (0 <= k && -1 == delta_c[key]) {
          result_c = c[k--] + result_c;
      } else {
        result_c = "-" + result_c;
      }
    }
  }
  printf("%s\n", result_a.c_str());
  printf("%s\n", result_b.c_str());
  printf("%s\n", result_c.c_str());
}

int main(int argc, char **argv) {
  string a;
  string b;
  string c;
  std::cin >> a;
  std::cin >> b;
  std::cin >> c;
  GLA(a, b, c);
  return 0;
}


