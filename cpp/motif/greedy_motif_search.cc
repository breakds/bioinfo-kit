#include <string>
#include <vector>
#include <iostream>
#include "motifs.h"

using std::string;
using std::vector;
using std::cin;
using std::cout;
using std::cerr;

using namespace motif;

vector<DNA::SubSeq> GreedyMotifSearch(const vector<DNA> &dnas,
                                      const int pattern_size) {
  vector<DNA::SubSeq> answer;
  double best = (dnas.size() + 2) * pattern_size;
  for (int i = 0; i < dnas[0].size(); ++i) {
    vector<DNA::SubSeq> current({dnas[0].GetSubSeq(i, pattern_size)});
    if (current[0].Empty()) break;
    
    StatsMatrix profile(current);
    for (int j = 1; j < dnas.size(); ++j) {
      current.push_back(profile.GreedyMatch(dnas[j]));
      profile.Update(current.back());
    }
    int score = profile.Score();
    if (score < best) {
      best = score;
      answer = current;
    }
  }
  return answer;
}

int main() {
  int pattern_size = 0;
  int dnas_size = 0;
  cin >> pattern_size;
  cin >> dnas_size;

  string dna_string;
  getline(cin, dna_string);
  vector<DNA> dnas;
  for (int i = 0; i < dnas_size; ++i) {
    getline(cin, dna_string);
    dnas.emplace_back(dna_string);
  }

  vector<DNA::SubSeq> result = GreedyMotifSearch(dnas, pattern_size);

  for (const DNA::SubSeq &motif : result) {
    motif.Print();
  }
  
  return 0;
}
