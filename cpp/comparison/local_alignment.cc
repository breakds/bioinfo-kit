#include <cstdio>
#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>

using std::vector;
using std::string;
using std::unordered_map;


int PAM250(char a, char b) {
  static vector<vector<int> > table {
    {2, -2, 0, 0, -3, 1, -1, -1, -1, -2, -1, 0, 1, 0, -2, 1, 1, 0, -6, -3},
    {-2, 12, -5, -5, -4, -3, -3, -2, -5, -6, -5, -4, -3, -5, -4, 0, -2, -2, -8, 0},
    {0, -5, 4, 3, -6, 1, 1, -2, 0, -4, -3, 2, -1, 2, -1, 0, 0, -2, -7, -4},
    {0, -5, 3, 4, -5, 0, 1, -2, 0, -3, -2, 1, -1, 2, -1, 0, 0, -2, -7, -4},
    {-3, -4, -6, -5, 9, -5, -2, 1, -5, 2, 0, -3, -5, -5, -4, -3, -3, -1, 0, 7},
    {1, -3, 1, 0, -5, 5, -2, -3, -2, -4, -3, 0, 0, -1, -3, 1, 0, -1, -7, -5},
    {-1, -3, 1, 1, -2, -2, 6, -2, 0, -2, -2, 2, 0, 3, 2, -1, -1, -2, -3, 0},
    {-1, -2, -2, -2, 1, -3, -2, 5, -2, 2, 2, -2, -2, -2, -2, -1, 0, 4, -5, -1},
    {-1, -5, 0, 0, -5, -2, 0, -2, 5, -3, 0, 1, -1, 1, 3, 0, 0, -2, -3, -4},
    {-2, -6, -4, -3, 2, -4, -2, 2, -3, 6, 4, -3, -3, -2, -3, -3, -2, 2, -2, -1},
    {-1, -5, -3, -2, 0, -3, -2, 2, 0, 4, 6, -2, -2, -1, 0, -2, -1, 2, -4, -2},
    {0, -4, 2, 1, -3, 0, 2, -2, 1, -3, -2, 2, 0, 1, 0, 1, 0, -2, -4, -2},
    {1, -3, -1, -1, -5, 0, 0, -2, -1, -3, -2, 0, 6, 0, 0, 1, 0, -1, -6, -5},
    {0, -5, 2, 2, -5, -1, 3, -2, 1, -2, -1, 1, 0, 4, 1, -1, -1, -2, -5, -4},
    {-2, -4, -1, -1, -4, -3, 2, -2, 3, -3, 0, 0, 0, 1, 6, 0, -1, -2, 2, -4},
    {1, 0, 0, 0, -3, 1, -1, -1, 0, -3, -2, 1, 1, -1, 0, 2, 1, -1, -2, -3},
    {1, -2, 0, 0, -3, 0, -1, 0, 0, -2, -1, 0, 0, -1, -1, 1, 3, 0, -5, -3},
    {0, -2, -2, -2, -1, -1, -2, 4, -2, 2, 2, -2, -1, -2, -2, -1, 0, 4, -6, -2},
    {-6, -8, -7, -7, 0, -7, -3, -5, -3, -2, -4, -4, -6, -5, 2, -2, -5, -6, 17, 0},
    {-3, 0, -4, -4, 7, -5, 0, -1, -4, -1, -2, -2, -5, -4, -4, -3, -3, -2, 0, 10}
  };
  static unordered_map<char, int> translate {{'A', 0}, {'C', 1}, {'D', 2}, {'E', 3}, {'F', 4}, {'G', 5}, {'H', 6}, {'I', 7}, {'K', 8}, {'L', 9}, {'M', 10}, {'N', 11}, {'P', 12}, {'Q', 13}, {'R', 14}, {'S', 15}, {'T', 16}, {'V', 17}, {'W', 18}, {'Y', 19}};
  return table[translate[a]][translate[b]];
}

void GLA(const string &a, const string &b) {
  FILE *f = fopen("/home/breakds/tmp/produce.txt", "w");
  
  const int sigma = 5;
  if (b.empty()) {
    printf("%ld\n", -sigma * a.size());
    printf("%s\n", a.c_str());
    for (int i = 0; i < a.size(); ++i) {
      printf("-");
    }
    printf("\n");
  }
    
  vector<int> current(b.size());
  vector<int> previous(b.size());
  for (int j = 0; j < b.size(); ++j) {
    current[j] = -sigma * (1 + j);
  }
  
  vector<vector<int> > choice(a.size());
  for (int i = 0; i < a.size(); ++i) {
    choice[i].resize(b.size(), 0);
  }
  
  int max_i = 0;
  int max_j = 0;
  int max_score = 0;
  for (int i = 0; i < a.size(); ++i) {
    previous.swap(current);
    for (int j = 0; j < b.size(); ++j) {
      choice[i][j] = 0; // a[i] matches/mismatches b[j]
      if (0 == j) {
        current[j] = PAM250(a[i], b[j]) - sigma * i;
      } else {
        current[j] = previous[j - 1] + PAM250(a[i], b[j]);
      }

      if (0 > current[j]) {
        choice[i][j] = -2; // throw away before (including) a[i], b[j]
        current[j] = 0;
      }

      if (previous[j] - sigma > current[j]) {
        choice[i][j] = 1; // 1 = a[i - 1] matches b[j]
        current[j] = previous[j] - sigma;
      }

      if (0 < j && current[j - 1] - sigma > current[j]) {
        choice[i][j] = -1; // -1 = a[i] match b[j-1]
        current[j] = current[j - 1] - sigma;
      }
      
      if (current[j] > max_score) {
        max_score = current[j];
        max_i = i;
        max_j = j;
      }
    }
  }

  printf("%d\n", max_score);
    
  // Trace back the solution
  string result_a = "";
  string result_b = "";
  {
    int i = max_i;
    int j = max_j;
    while (0 <= i || 0 <= j) {
      if (0 <= i && 0 <= j) {
        int key = choice[i][j];
        if (0 == key) {
          result_a = a[i--] + result_a;
          result_b = b[j--] + result_b;
        }
        if (1 == key) { 
          result_a = a[i--] + result_a;
          result_b = "-" + result_b;
        }
        if (-1 == key) { 
          result_a = "-" + result_a;
          result_b = b[j--] + result_b;
        } else if (-2 == key) {
          break;
        }
      } else if (0 <= i) {
        result_a = a[i--] + result_a;
        result_b = "-" + result_b;
      } else {
        result_a = "-" + result_a;
        result_b = b[j--] + result_b;
      }
    }
  }
  printf("%s\n", result_a.c_str());
  printf("%s\n", result_b.c_str());
}

int main(int argc, char **argv) {
  string a;
  string b;
  std::cin >> a;
  std::cin >> b;
  GLA(a, b);
  return 0;
}


