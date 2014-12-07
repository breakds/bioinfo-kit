#include <cstdio>
#include <cstdlib>

#include <algorithm>
#include <unordered_map>
#include <string>
#include <utility>
#include <vector>
#include <set>


namespace assembly {

  using std::string;
  using std::pair;
  using std::vector;
  using std::unordered_map;
  using std::set;

  int NucleobaseToId(char nucleobase) {
    switch (nucleobase) {
    case 'A': return 0;
    case 'C': return 1;
    case 'G': return 2;
    case 'T': return 3;
    }
    return -1;
  }
  
  char IdToNucleobase(int id) {
    switch (id) {
    case 0 : return 'A';
    case 1 : return 'C';
    case 2 : return 'G';
    case 3 : return 'T';
    }
    return '*';
  }

  class ReadPairGraph : public vector<vector<int>> {
  public:
    ReadPairGraph(const vector<pair<string, string>> &raw_pairs, int interval) 
      : vector<vector<int> >(), read_size_(raw_pairs[0].first.size()), 
        offset_(interval + read_size_) {
      unordered_map<string, int> inverse_dict;
      vector<string> prelim_dict;
      vector<vector<int>> prelim_graph;
      size_t node_counter = 0;
      // Construct Prelim Graph
      int node_id[2];
      for (const pair<string, string> &read_pair : raw_pairs) {
        for (int i = 0; i < 2; ++i) {
          string node_string = (0 == i) ? GetPairPrefix(read_pair)
            : GetPairPostfix(read_pair);
          auto dict_iter = inverse_dict.find(node_string);          
          if (inverse_dict.end() == dict_iter) {
            node_id[i] = node_counter;
            inverse_dict.emplace(node_string, node_counter++);
            prelim_dict.push_back(node_string);
            prelim_graph.push_back({});
          } else {
            node_id[i] = dict_iter->second;
          }
        }
        prelim_graph[node_id[0]].push_back(node_id[1]);
      }

      // graph_ = prelim_graph;
      // for (size_t i = 0; i < prelim_graph.size(); ++i) {
      //   prefix_.push_back(prelim_dict[i].substr(0, read_size_ - 1));
      //   postfix_.push_back(prelim_dict[i].substr(read_size_, read_size_ - 1));
      // }
      
      // Calculate in degree.
      vector<int> in_degrees(prelim_graph.size());
      for (size_t i = 0; i < prelim_graph.size(); ++i) {
        in_degrees[i] = static_cast<int>(prelim_graph[i].size());
      }

      // Merge Nodes
      for (size_t i = 0; i < prelim_graph.size(); ++i) {
        if (prelim_dict[i].empty()) continue;
        while (1 == prelim_graph[i].size() && 
               1 >= in_degrees[prelim_graph[i][0]]) {
          int j = prelim_graph[i][0];
          prelim_dict[i] = MergeNodeString(prelim_dict[i], 
                                           prelim_dict[j]);
          prelim_graph[i].swap(prelim_graph[j]);
          prelim_dict[j] = "";
        }
      }

      // Reorganize Dictionary and Graph
      vector<int> old_ids;
      int new_nodes = 0;
      for (size_t i = 0; i < prelim_graph.size(); ++i) {
        if (!prelim_dict[i].empty()) {
          old_ids.push_back(i);
        }
      }
      
      for (size_t i = 0; i < old_ids.size(); ++i) {
        int old_id = old_ids[i];
        int node_size = prelim_dict[old_id].size() >> 1;
        prefix_.push_back(prelim_dict[old_id].substr(0, node_size));
        postfix_.push_back(prelim_dict[old_id].substr(node_size + 1, 
                                                      node_size));
        this->emplace_back(std::move(prelim_graph[old_id]));
      }
    }

    vector<int> InDegrees() const {
      vector<int> result(this->size(), 0);
      for (const auto& ends : *this) {
        for (const int& end : ends) {
          ++result[end];
        }
      }
      return result;
    }

    int EdgeSize() const {
      return std::accumulate(this->begin(), this->end(), 0,
                             [](int x, vector<int> y) {
                               return x + static_cast<int>(y.size());
                             });
    }

    int read_size() const {
      return read_size_;
    }

    int overlap_size() const {
      return read_size_ - 2;
    }
    
    int offset() const {
      return offset_;
    }

    const string &Prefix(int id) const {
      return prefix_[id];
    }

    const string &Postfix(int id) const {
      return postfix_[id];
    }

    string DebugNodeString(int i) const {
      return prefix_[i] + "|" + postfix_[i];
    }
  
    void DebugPrint() const {
      for (size_t i = 0; i < this->size(); ++i) {
        printf("%s -> ", DebugNodeString(i).c_str());
        for (const int &end : (*this)[i]) {
          printf("%s  ", DebugNodeString(end).c_str());
        }
        printf("\n");
      }
      for (size_t i = 0; i < this->size(); ++i) {
        printf("%zd -> ", i);
        for (const int &end : (*this)[i]) {
          printf("%d  ", end);
        }
        printf("\n");
      }
    }

  private:
    string GetPairPrefix(const pair<string, string> &read_pair) {
      return read_pair.first.substr(0, read_size_ - 1) + "|" 
        + read_pair.second.substr(0, read_size_ - 1);
    }

    string GetPairPostfix(const pair<string, string> &read_pair) {
      return read_pair.first.substr(1) + "|" 
        + read_pair.second.substr(1);
    }

    string MergeNodeString(const string &a, const string &b) {
      size_t size_a = a.size() >> 1;
      size_t size_b = b.size() >> 1;
      int last_new_prefix = size_a + (size_b - read_size_ + 1);
      if (last_new_prefix >= offset_) {
        int abs_overlap_end = std::min(last_new_prefix, 
                                       offset_+ static_cast<int>(size_a) -1);
        int abs_overlap_begin = std::max(static_cast<int>(size_a), offset_);
        int overlap_size = abs_overlap_end - abs_overlap_begin + 1;
        string overlap_a = a.substr(abs_overlap_begin - offset_ + size_a + 1, 
                                    overlap_size);
        string overlap_b = b.substr(abs_overlap_begin - size_a + read_size_ - 2, 
                                    overlap_size);
        if (overlap_a != overlap_b) {
          printf("Invalid Merge %s + %s\n",
                 a.c_str(), b.c_str());
          exit(1);
        }
      }
      int append_size = last_new_prefix - size_a + 1;
      return a.substr(0, size_a) + b.substr(size_b - append_size, append_size)
        + a.substr(size_a) + b.substr(b.size() - append_size);
    }
    
    size_t read_size_;
    int offset_;
    std::vector<string> prefix_;
    std::vector<string> postfix_;
  };

  class DNAAssembler {
  public:
    static string Run(const ReadPairGraph &input) {
      int target_edges = input.EdgeSize();
      
      vector<vector<bool> > edge_mask = CreateEdgeMask(input);
      vector<StackElement> stack;
      stack.emplace_back(FindStart(input));
      vector<char> postfix;
      {
        const string& tmp = input.Postfix(stack[0].node);
        for (size_t i = 0; i < tmp.size(); ++i) {
          postfix.push_back(tmp[i]);
        }
      }
      
      while (stack.size() != target_edges + 1) {
        int node = stack.back().node;
        int j = stack.back().edge_pos + 1;
        int hit = -1;
        for (; j < input[node].size(); ++j) {
          if (!edge_mask[node][j]) {
            bool match = true;
            const string &prefix = input.Prefix(input[node][j]);
            for (int match_pos = postfix.size() - input.offset(), 
                   k = input.overlap_size();
                 match_pos < postfix.size();
                 ++match_pos, ++k) {
              if (0 <= match_pos && prefix[k] != postfix[match_pos]) {
                match = false;
                break;
              }
            }
            if (match) {
              hit = j;
              break;
            }
          }
        }
        
        if (-1 == hit) {
          int postfix_length = input.Postfix(node).size() 
            - input.overlap_size();
          for (int i = 0; i < postfix_length; ++i) {
            postfix.pop_back();
          }
          stack.pop_back();
          if (0 == stack.size()) {
            printf("Early termination error!\n");
            exit(-1);
          }
          edge_mask[stack.back().node][stack.back().edge_pos] = false;
        } else {
          edge_mask[node][hit] = true;
          stack.back().edge_pos = hit;
          stack.emplace_back(input[node][hit]);
          const string &new_postfix = input.Postfix(input[node][hit]);
          for (int i = input.overlap_size(); i < new_postfix.size(); ++i) {
            postfix.push_back(new_postfix[i]);
          }
        }
      }

      // Construct result string
      printf("Constructing result ...\n");
      string result = input.Prefix(stack[0].node);
      for (size_t i = 1; i < stack.size(); ++i) {
        const string &prefix = input.Prefix(stack[i].node);
        result += prefix.substr(input.overlap_size());
        if (result.size() >= input.offset()) {
          break;
        }
      }
      result.resize(input.offset() + postfix.size());
      for (size_t i = 0; i < postfix.size(); ++i) {
        result[i + input.offset()] = postfix[i];
      }
      return result;
    }
  
  private:
    struct StackElement {
      StackElement(int node_) : node(node_), edge_pos(-1) {}
      int node;
      int edge_pos;
    };

    static int FindStart(const ReadPairGraph &input) {
      vector<int> in_degrees = std::move(input.InDegrees());
      int start = -1;
      int end = -1;
      for (size_t i = 0; i < input.size(); ++i) {
        switch (in_degrees[i] - static_cast<int>(input[i].size())) {
        case 1:
          if (-1 == end) {
            end = i;
          } else {
            printf("More than one unbalanced ends.\n");
            exit(-1);
          }
          break;
        case -1:
          if (-1 == start) {
            start = i;
          } else {
            printf("More than one unbalanced starts.\n");
            exit(-1);
          }
          break;
        }
      }
      start = (-1 == start) ? 0 : start;
      return start;
    }

    static vector<vector<bool> > CreateEdgeMask(const ReadPairGraph &input) {
      vector<vector<bool> > result;
      result.reserve(input.size());
      for (const auto& ends : input) {
        result.emplace_back(ends.size(), false);
      }
      return result;
    }
  };
}

