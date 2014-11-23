#include <string>
#include <vector>
#include "motifs.h"

using namespace motif;

using std::vector;
using std::string;


vector<DNA::SubSeq> RandomMotifSearch(const vector<DNA> &dnas,
				      const int pattern_size) {
  // Initialization:
  vector<DNA::SubSeq> current;
  current.reserve(dnas.size());

  for (const DNA &dna : dnas) {
    current.push_back(dna.RandomSubSeq(pattern_size));
  }

  StatsMatrix profile(current);
  int best = profile.Score();
  bool updated = false;
  do {
    updated = false;
    
    vector<DNA::SubSeq> candidates;
    candidates.reserve(dnas.size());
    for (const DNA &dna : dnas) {
      candidates.push_back(profile.GreedyMatch(dna));
    }

    StatsMatrix new_profile(candidates);
    int new_score = new_profile.Score();
    if (new_score < best) {
      best = new_score;
      profile = std::move(new_profile);
      current.swap(candidates);
      updated = true;
    }
  } while (updated);
  return current;
}

vector<DNA::SubSeq> RandomMotifConsensus(const vector<DNA> &dnas,
                                        const int pattern_size,
                                        const int repetition) {
  vector<DNA::SubSeq> current;
  int best = (dnas.size() + 2) * pattern_size;
  for (int i = 0; i < repetition; ++i) {
    vector<DNA::SubSeq> result = 
      RandomMotifSearch(dnas, pattern_size);
    StatsMatrix profile(result);
    double result_score = profile.Score();
    if (result_score < best) {
      best = result_score;
      current.swap(result);
    }
  }
  return current;
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

  vector<DNA::SubSeq> result = 
    RandomMotifConsensus(dnas, pattern_size, 1000);
  
  for (const DNA::SubSeq &motif : result) {
    motif.Print();
  }
  
  return 0;
}
