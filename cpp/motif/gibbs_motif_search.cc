#include <string>
#include <vector>
#include <random>
#include <chrono>
#include "motifs.h"

using namespace motif;

using std::vector;
using std::string;


vector<DNA::SubSeq> GibbsMotifSearch(const vector<DNA> &dnas,
                                     const int pattern_size,
                                     const int repetition) {
  // Initialization of Random Generators;
  std::mt19937 
    generator(std::chrono::system_clock::now().time_since_epoch().count());
  std::uniform_int_distribution<int> dna_selector(0, dnas.size() - 1);

  // Initialization:
  vector<DNA::SubSeq> current;
  current.reserve(dnas.size());

  for (const DNA &dna : dnas) {
    current.push_back(dna.RandomSubSeq(pattern_size));
  }


  StatsMatrix profile(current);
  int best = profile.Score();

  for (int i = 0; i < repetition; ++i) {
    int j = dna_selector(generator);
    profile -= current[j];
    vector<double> distribution;
    distribution.reserve(dnas[j].size() - pattern_size + 1);
    for (int k = 0; k <= (dnas[j].size() - pattern_size); ++k) {
      DNA::SubSeq seq = dnas[j].GetSubSeq(k, pattern_size);
      distribution.push_back(profile.SubSeqPsuedoScore(seq));
    }
    // Construct Discrete Distribution Random Generator
    std::discrete_distribution<int> sampler(distribution.begin(),
                                            distribution.end());
    DNA::SubSeq candidate = dnas[j].GetSubSeq(sampler(generator), pattern_size);
    profile += candidate;
    int new_score = profile.Score();
    if (new_score < best) {
      best = new_score;
      current[j] = candidate;
    } else {
      profile -= candidate;
      profile += current[j];
    }
  }
  
  return current;
}

vector<DNA::SubSeq> GibbsMotifConsensus(const vector<DNA> &dnas,
                                        const int pattern_size,
                                        const int gibbs_repetition,
                                        const int repetition) {
  vector<DNA::SubSeq> current;
  int best = (dnas.size() + 2) * pattern_size;
  for (int i = 0; i < repetition; ++i) {
    vector<DNA::SubSeq> result = 
      GibbsMotifSearch(dnas, pattern_size, gibbs_repetition);
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
  int sampling_iterations = 0;
  cin >> pattern_size;
  cin >> dnas_size;
  cin >> sampling_iterations;

  string dna_string;
  getline(cin, dna_string);
  vector<DNA> dnas;
  for (int i = 0; i < dnas_size; ++i) {
    getline(cin, dna_string);
    dnas.emplace_back(dna_string);
  }

  vector<DNA::SubSeq> result = GibbsMotifConsensus(dnas, pattern_size, 
                                                   sampling_iterations,
                                                   20);
  
  for (const DNA::SubSeq &motif : result) {
    motif.Print();
  }
  
  return 0;
}
