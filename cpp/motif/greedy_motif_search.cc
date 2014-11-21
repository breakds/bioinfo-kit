#include <string>
#include <vector>
#include <iostream>
#include <algorithm>

using std::string;
using std::vector;
using std::cin;
using std::cout;
using std::cerr;

class DNA {
public:
  class SubSeq {
  public:
    SubSeq(const DNA *dna, int start, int length) 
      : parent_(dna), start_(start), 
        length_(start + length > dna->dna_.size() ? 0 : length) {}

    const int &operator[](int index) const {
      return parent_->dna_[start_ + index];
    }
    
    int size() const {
      return length_;
    }

    bool Empty() const {
      return (0 == length_);
    }

    void Print() const {
      for (int i = 0; i < length_; ++i) {
        cout << DNA::IdToNucleobase((*this)[start_ + i]);
      }
      cout << "\n";
    }

  private:
    const DNA *parent_;
    int start_;
    int length_;
  };

  explicit DNA(const string &dna_string) {
    dna_.reserve(dna_string.size());
    for (char nucleobase : dna_string) {
      dna_.push_back(NucleobaseToId(nucleobase));
    }
  }

  DNA(DNA &&other) {
    dna_.swap(other.dna_);
  }

  void Print() const {
    for (int x : dna_) {
      cout << IdToNucleobase(x);
    }
    cout << "\n";
  }

  SubSeq GetSubSeq(int start, int length) const {
    return SubSeq(this, start, length);
  }

  int size() const {
    return dna_.size();
  }

private:

  static int NucleobaseToId(char nucleobase) {
    switch (nucleobase) {
    case 'A': return 0;
    case 'C': return 1;
    case 'G': return 2;
    case 'T': return 3;
    }
    return -1;
  }
  
  static char IdToNucleobase(int id) {
    switch (id) {
    case 0 : return 'A';
    case 1 : return 'C';
    case 2 : return 'G';
    case 3 : return 'T';
    }
    return '*';
  }

  vector<int> dna_;
};

class StatsMatrix {
public:
  StatsMatrix(const vector<DNA::SubSeq> &seqs)
    : matrix_(seqs[0].size() << 2, 0) {
    for (const DNA::SubSeq &seq : seqs) {
      Update(seq);
    }
  }

  StatsMatrix(const StatsMatrix &other) {
    matrix_ = other.matrix_;
  }

  inline void Update(const DNA::SubSeq &seq) {
    for (int i = 0; i < seq.size(); ++i) {
      ++matrix_[(i << 2) + seq[i]];
    }
  }

  inline int operator()(int index, int nucleobase_id) const {
    return matrix_[(index << 2) + nucleobase_id];
  }

  inline int Psuedo(int index, int nucleobase_id) const {
    return matrix_[(index << 2) + nucleobase_id] + 1;
  }

  inline int size() const {
    return matrix_.size() >> 2;
  }

  int Score() const {
    int score = std::accumulate(matrix_.begin(), 
                               matrix_.end(), 0);
    auto iter = matrix_.begin();
    while (matrix_.end() > iter) {
      score -= *std::max_element(iter, iter + 4);
    }
    return score;
  }

  double SubSeqPsuedoScore(const DNA::SubSeq &seq) const {
    double score = 1.0;
    double total = Psuedo(0, 0) + Psuedo(0, 1) + 
      Psuedo(0, 2) + Psuedo(0, 3);
    for (int i = 0; i < seq.size(); ++i) {
      score *= Psuedo(i, seq[i]) / total;
    }
    return score;
  }

  DNA::SubSeq GreedyMatch(const DNA &dna) const {
    int pattern_size = size();
    int pos = 0;
    double best_score = -1.0;
    for (int i = 0; i < dna.size(); ++i) {
      DNA::SubSeq candidate = dna.GetSubSeq(i, pattern_size);
      if (candidate.Empty()) break;
      double score = SubSeqPsuedoScore(candidate);
      if (score > best_score) {
        best_score = score;
        pos = i;
      }
    }
    return dna.GetSubSeq(pos, pattern_size);
  }

private:
  vector<int> matrix_;
};



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

  vector<DNA> dnas;
  for (int i = 0; i < dnas_size; ++i) {
    string dna_string;
    getline(cin, dna_string);
    dnas.emplace_back(dna_string);
  }
  
  vector<DNA::SubSeq> result = GreedyMotifSearch(dnas, pattern_size);

  for (const DNA::SubSeq &motif : result) {
    motif.Print();
  }
  
  return 0;
}
