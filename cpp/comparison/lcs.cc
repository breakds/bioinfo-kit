#include <iostream>
#include <string>
#include <vector>

using std::vector;
using std::string;

string LCS(const string &a, const string &b) {
  if (a.empty() || b.empty()) return "";
  
  vector<int> current(b.size());
  std::fill(current.begin(), current.end(), 0);
  vector<int> previous(b.size());
  std::fill(previous.begin(), previous.end(), 0);
  
  vector<vector<int> > choice(a.size());
  for (int i = 0; i < a.size(); ++i) {
    choice[i].resize(b.size(), 0);
  }
  
  for (int i = 0; i < a.size(); ++i) {
    previous.swap(current);
    if (a[i] == b[0]) {
      choice[i][0] = 0;
      current[0] = 1;
    } else {
      choice[i][0] = -1;
      current[0] = 0;
    }
    for (int j = 1; j < b.size(); ++j) {
      if (a[i] == b[j]) {
        choice[i][j] = 0;
        current[j] = previous[j - 1] + 1;
      } else {
        if (previous[j] < current[j - 1]) {
          choice[i][j] = -1; // -1 = a[i] match b[j-1]
          current[j] = current[j - 1];
        } else {
          choice[i][j] = 1; // 1 = a[i - 1] matches b[j]
          current[j] = previous[j];
        }
      }
    }
  }

  // Trace back the solution
  string result = "";
  {
    int i = a.size() - 1;
    int j = b.size() - 1;
    while (0 <= i && 0 <= j) {
      int key = choice[i][j];
      if (0 == key) result = a[i] + result;
      if (0 <= key) i--;
      if (0 >= key) j--;
    }
  }
  
  return result;
}

int main(int argc, char **argv) {
  string a;
  string b;
  std::cin >> a;
  std::cin >> b;
  std::cout << LCS(a, b) << "\n";
  return 0;
}

