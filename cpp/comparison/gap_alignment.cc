#include <cstdio>
#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>
#include <algorithm>

using std::vector;
using std::string;
using std::unordered_map;

const int sigma = 11;
const int mu = 1;

int Blosum62(char a, char b) {
  static vector<vector<int> > table {
    {4,  0, -2, -1, -2,  0, -2, -1, -1, -1, -1, -2, -1, -1, -1,  1,  0,  0, -3, -2},
    {0,  9, -3, -4, -2, -3, -3, -1, -3, -1, -1, -3, -3, -3, -3, -1, -1, -1, -2, -2},
    {-2, -3,  6,  2, -3, -1, -1, -3, -1, -4, -3,  1, -1,  0, -2,  0, -1, -3, -4, -3},
    {-1, -4,  2,  5, -3, -2,  0, -3,  1, -3, -2,  0, -1,  2,  0,  0, -1, -2, -3, -2},
    {-2, -2, -3, -3,  6, -3, -1,  0, -3,  0,  0, -3, -4, -3, -3, -2, -2, -1,  1,  3},
    {0, -3, -1, -2, -3,  6, -2, -4, -2, -4, -3,  0, -2, -2, -2,  0, -2, -3, -2, -3},
    {-2, -3, -1,  0, -1, -2,  8, -3, -1, -3, -2,  1, -2,  0,  0, -1, -2, -3, -2,  2},
    {-1, -1, -3, -3,  0, -4, -3,  4, -3,  2,  1, -3, -3, -3, -3, -2, -1,  3, -3, -1},
    {-1, -3, -1,  1, -3, -2, -1, -3,  5, -2, -1,  0, -1,  1,  2,  0, -1, -2, -3, -2},
    {-1, -1, -4, -3,  0, -4, -3,  2, -2,  4,  2, -3, -3, -2, -2, -2, -1,  1, -2, -1},
    {-1, -1, -3, -2,  0, -3, -2,  1, -1,  2,  5, -2, -2,  0, -1, -1, -1,  1, -1, -1},
    {-2, -3,  1,  0, -3,  0,  1, -3,  0, -3, -2,  6, -2,  0,  0,  1,  0, -3, -4, -2},
    {-1, -3, -1, -1, -4, -2, -2, -3, -1, -3, -2, -2,  7, -1, -2, -1, -1, -2, -4, -3},
    {-1, -3,  0,  2, -3, -2,  0, -3,  1, -2,  0,  0, -1,  5,  1,  0, -1, -2, -2, -1},
    {-1, -3, -2,  0, -3, -2,  0, -3,  2, -2, -1,  0, -2,  1,  5, -1, -1, -3, -3, -2},
    {1, -1,  0,  0, -2,  0, -1, -2,  0, -2, -1,  1, -1,  0, -1,  4,  1, -2, -3, -2},
    {0, -1, -1, -1, -2, -2, -2, -1, -1, -1, -1,  0, -1, -1, -1,  1,  5,  0, -2, -2},
    {0, -1, -3, -2, -1, -3, -3,  3, -2,  1,  1, -3, -2, -2, -3, -2,  0,  4, -3, -1},
    {-3, -2, -4, -3,  1, -2, -2, -3, -3, -2, -1, -4, -4, -2, -3, -3, -2, -3, 11,  2},
    {-2, -2, -3, -2,  3, -3,  2, -1, -2, -1, -1, -2, -3, -1, -2, -2, -2, -1,  2,  7}
  };
  static unordered_map<char, int> translate {{'A', 0}, {'C', 1}, {'D', 2}, {'E', 3}, {'F', 4}, {'G', 5}, {'H', 6}, {'I', 7}, {'K', 8}, {'L', 9}, {'M', 10}, {'N', 11}, {'P', 12}, {'Q', 13}, {'R', 14}, {'S', 15}, {'T', 16}, {'V', 17}, {'W', 18}, {'Y', 19}};
  return table[translate[a]][translate[b]];
}

void StateTransfer(const vector<int> &s, int choice, 
                   int match,
                   int *from, int *score) {
  if (0 == choice) {
    *from = static_cast<int>(std::max_element(s.begin(), s.end()) - s.begin());
    *score = s[*from] + match;
  } else {
    *from = 0;
    *score = s[0] - sigma;
    if (s[choice] - mu > *score) {
      *from = choice;
      *score = s[choice] - mu;
    }
    if (s[3 - choice] - sigma > *score) {
      *from = 3 - choice;
      *score = s[3 - choice] - sigma;
    }
  }
}

void GLA(const string &a, const string &b) {
  FILE *f = fopen("/home/breakds/tmp/produce.txt", "w");
  
  if (b.empty()) {
    printf("%ld\n", -sigma * a.size());
    printf("%s\n", a.c_str());
    for (int i = 0; i < a.size(); ++i) {
      printf("-");
    }
    printf("\n");
  }
    
  vector<vector<int> > current(b.size());
  vector<vector<int> > previous(b.size());
  for (int j = 0; j < b.size(); ++j) {
    current[j].resize(3);
    previous[j].resize(3);
  }
  
  vector<vector<vector<int> > > from(a.size());
  for (int i = 0; i < a.size(); ++i) {
    from[i].resize(b.size());
    for (int j = 0; j < b.size(); j++) {
      from[i][j].resize(3, 0);
    }
  }

  // i = 0
  for (int j = 0; j < b.size(); ++j) {
    current[j][0] = Blosum62(a[0], b[j]);
    current[j][1] = -2 * sigma;
    current[j][2] = -2 * sigma;
    from[0][j][0] = 0;
    from[0][j][1] = 0;
    from[0][j][2] = 0;
  }
  
  for (int i = 1; i < a.size(); ++i) {
    previous.swap(current);
    for (int j = 0; j < b.size(); ++j) {
      // case 0: a[i] matches/mismatches b[j]
      if (0 == j) {
        current[0][0] = -sigma - mu * (i - 1) + Blosum62(a[i], b[0]);
        from[i][0][0] = 0;
      } else {
        StateTransfer(previous[j - 1], 0, Blosum62(a[i], b[j]), 
                      &from[i][j][0], &current[j][0]);
      }
      
      // case 1: a[i - 1] matches b[j]
      StateTransfer(previous[j], 1, -1, &from[i][j][1], &current[j][1]);
      
      // case 2: a[i] matches b[j - 1]
      if (0 == j) {
        current[0][2] = - sigma * 2 - mu * (i - 1);
        from[i][0][0] = 0;
      } else {
        StateTransfer(current[j - 1], 2, -1, &from[i][j][2], &current[j][2]);
      }
    }
  }

  auto end_choice = std::max_element(current[b.size() - 1].begin(), 
                                     current[b.size() - 1].end());
                                     
  printf("%d\n", *end_choice);
    
  // Trace back the solution
  string result_a = "";
  string result_b = "";
  {
    int i = a.size() - 1;
    int j = b.size() - 1;
    int key = end_choice - current[b.size() - 1].begin();

    while (0 <= i || 0 <= j) {
      int new_key = from[i][j][key];
      if (0 <= i && 0 <= j) {
        if (0 == key) {
          result_a = a[i--] + result_a;
          result_b = b[j--] + result_b;
        }
        if (1 == key) { 
          result_a = a[i--] + result_a;
          result_b = "-" + result_b;
        }
        if (2 == key) { 
          result_a = "-" + result_a;
          result_b = b[j--] + result_b;
        }
      } else if (0 <= i) {
        result_a = a[i--] + result_a;
        result_b = "-" + result_b;
      } else {
        result_a = "-" + result_a;
        result_b = b[j--] + result_b;
      }
      key = new_key;
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


