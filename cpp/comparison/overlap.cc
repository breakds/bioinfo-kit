#include <cstdio>
#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>

using std::vector;
using std::string;
using std::unordered_map;


int match(char a, char b) {
  return a == b ? 1 : -2;
}

void GLA(const string &a, const string &b) {
  FILE *f = fopen("/home/breakds/tmp/produce.txt", "w");
  
  const int sigma = 2;
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
        current[j] = match(a[i], b[j]);
      } else {
        current[j] = previous[j - 1] + match(a[i], b[j]);
      }
      
      if (!(0 == i && 0 == j)) {
        if (previous[j] - sigma > current[j]) {
          choice[i][j] = 1; // 1 = a[i - 1] matches b[j]
          current[j] = previous[j] - sigma;
        }
      }

      if (0 < j && current[j - 1] - sigma > current[j]) {
        choice[i][j] = -1; // -1 = a[i] match b[j-1]
        current[j] = current[j - 1] - sigma;
      }
    }
  }

  int max_j = 0;
  int max_score = -1000000;
  for (int j = 0; j < b.size(); ++j) {
    if (current[j] > max_score) {
      max_score = current[j];
      max_j = j;
    }
  }
  
  printf("%d\n", max_score);
    
  // Trace back the solution
  string result_a = "";
  string result_b = "";
  {
    int i = a.size() - 1;
    int j = max_j;
    while (0 <= j) {
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
}

int main(int argc, char **argv) {
  string a;
  string b;
  std::cin >> a;
  std::cin >> b;
  GLA(a, b);
  return 0;
}


