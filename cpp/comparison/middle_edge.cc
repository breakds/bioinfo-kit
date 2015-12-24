#include <cstdio>
#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>

using std::vector;
using std::string;
using std::unordered_map;

const int sigma = 5;

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

vector<int> GLA(const string &a, const string &b) {
  printf("A: %s\nB: %s\n", a.c_str(), b.c_str());
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

  return current;
}

string reverse(const string &input) {
  string result = "";
  for (const char &ch : input) result = ch + result;
  return result;
}

void reverse_vector(vector<int> &input) {
  for (int i = 0; i <= (input.size() >> 1); ++i) {
    int tmp = input[i];
    input[i] = input[input.size() - i - 1];
    input[input.size() - i - 1] = tmp;
  }
}

void MiddleEdge(const string &a, const string &b) {
  int mid = (a.size() >> 1) - 1;
  vector<int> first = GLA(a.substr(0, mid), b);
  vector<int> second = GLA(reverse(a.substr(mid + 1)), reverse(b));
  second.pop_back();
  reverse_vector(second);
  second.push_back(-sigma * a.substr(mid + 1).size());
  for (int j = 0; j < b.size(); ++j) {
    printf("%d ", first[j]);
  }
  printf("\n");

  for (int j = 0; j < b.size(); ++j) {
    printf("%d ", second[j]);
  }
  printf("\n");
  int delta = 0;
  int best_j = 0;
  int best_score = -10000000;
  for (int j = 0; j < b.size(); ++j) {
    if (first[j] + second[j] - sigma > best_score) {
      best_score = first[j] + second[j] - sigma;
      best_j = j;
      delta = 0;
    }
    if (j < b.size() - 1) {
      if (first[j] + second[j + 1] + Blosum62(a[mid], b[j]) > best_score) {
        best_score = first[j] + second[j + 1] + Blosum62(a[mid], b[j]);
        best_j = j;
        delta = 1;
      }
    }
  }
  printf("best_score: %d\n", best_score);
  if (0 == delta) {
    printf("%d + %d vs %d\n", first[best_j], second[best_j], -sigma);
  } else {
    printf("%d + %d : %c vs %c : %d\n", first[best_j], second[best_j + delta], 
           a[mid], b[best_j], Blosum62(a[mid], b[best_j]));
  }
  printf("best_j: %d, mid: %d\n", best_j, mid);
  printf("(%d, %d) (%d, %d)\n", best_j + 1, mid + 1, best_j + delta + 1, mid + 2);
}

int main(int argc, char **argv) {
  string a;
  string b;
  std::cin >> a;
  std::cin >> b;
  MiddleEdge(b, a);
  return 0;
}


