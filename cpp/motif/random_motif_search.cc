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
    current.push_back(dna.RandomSubSeq());
  }

  StatsMatrix profile(current);
  double best = profile.Score();
  do {
    
    
  } while 
  

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

  for (const DNA &dna : dnas) {
    dna.RandomSubSeq(pattern_size).Print();
  }
  
  return 0;
}
