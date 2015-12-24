#include <cstdio>
#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>

using std::vector;
using std::string;
using std::unordered_map;


void EditDistance(const string &a, const string &b) {
  FILE *f = fopen("/home/breakds/tmp/produce.txt", "w");
  
  const int sigma = 1;
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
        current[j] = (a[i] == b[j] ? 0 : -1) - sigma * i;
      } else {
        current[j] = previous[j - 1] + (a[i] == b[j] ? 0 : -1);
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

  printf("%d\n", -current[b.size() - 1]);
}

int main(int argc, char **argv) {
  string a;
  string b;
  std::cin >> a;
  std::cin >> b;
  EditDistance(a, b);
  return 0;
}


