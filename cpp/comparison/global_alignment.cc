#include <cstdio>
#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>

using std::vector;
using std::string;
using std::unordered_map;



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
  
  for (int i = 0; i < a.size(); ++i) {
    previous.swap(current);
    for (int j = 0; j < b.size(); ++j) {
      choice[i][j] = 0; // a[i] matches/mismatches b[j]
      if (0 == j) {
        current[j] = Blosum62(a[i], b[j]) - sigma * i;
      } else {
        current[j] = previous[j - 1] + Blosum62(a[i], b[j]);
      }
      
      if (previous[j] - sigma > current[j]) {
        choice[i][j] = 1; // 1 = a[i - 1] matches b[j]
        current[j] = previous[j] - sigma;
      }

      if (0 < j && current[j - 1] - sigma > current[j]) {
        choice[i][j] = -1; // -1 = a[i] match b[j-1]
        current[j] = current[j - 1] - sigma;
      }
    }
  }
  for (int j = 0; j < b.size(); ++j) {
    printf("%d ", current[j]);
  }
  printf("\n");
  
  printf("%d\n", current[b.size() - 1]);
    
  // Trace back the solution
  string result_a = "";
  string result_b = "";
  {
    int i = a.size() - 1;
    int j = b.size() - 1;
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

  // Verification
  {
    string new_a = "";
    for (const char &ch : result_a) {
      if ('-' != ch) {
        new_a += ch;
      }
    }
    if (a == new_a) {
      printf("String 1 verified!\n");
    } else {
      printf("String 1 verification failed!\n");
    }
  }

  {
    string new_b = "";
    for (const char &ch : result_b) {
      if ('-' != ch) {
        new_b += ch;
      }
    }
    if (b == new_b) {
      printf("String 2 verified!\n");
    } else {
      printf("String 2 verification failed!\n");
    }
  }

  {
    int score = 0;
    for (int i = 0; i < result_a.size(); ++i) {
      if ('-' == result_a[i] || '-' == result_b[i]) {
        score -= sigma;
      } else {
        score += Blosum62(result_a[i], result_b[i]);
      }
    }
    printf("Score: %d\n", score);
  }
}

int main(int argc, char **argv) {
  string a;
  string b;
  std::cin >> a;
  std::cin >> b;
  GLA(a, b);
  return 0;
}


