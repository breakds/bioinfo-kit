#include <iostream>
#include <string>
#include <utility>
#include <vector>

#include "assembly.h"

using namespace assembly;
using std::string;
using std::pair;
using std::vector;

int main(int argc, char **argv) {
  int read_size;
  int interval;
  std::cin >> read_size;
  std::cin >> interval;

  vector<pair<string, string> > raw_pairs;
  string raw_pair;
  do {
    raw_pair = "";
    std::cin >> raw_pair;
    if (!raw_pair.empty()) {
      raw_pairs.push_back(std::make_pair(raw_pair.substr(0, read_size),
                                         raw_pair.substr(read_size + 1, 
                                                         read_size)));
    }
  } while (!raw_pair.empty());

  ReadPairGraph graph(raw_pairs, interval);
  string result = std::move(DNAAssembler::Run(graph));
  std::cout << result << "\n";
  return 0;
}
