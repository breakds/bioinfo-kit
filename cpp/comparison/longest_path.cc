#include <map>
#include <vector>
#include <iostream>
#include <string>
#include <set>

using std::vector;
using std::map;
using std::string;
using std::set;

struct Edge {
  Edge(int node_, int value_) : node(node_), value(value_) {}
  int node;
  int value;
};

class Graph {
 public:
  Graph() : edge_list_(), nodes_() {
    string buffer;
    while (getline(std::cin, buffer)) {
      int arrow_pos = buffer.find("->");
      if (-1 == arrow_pos) continue;
      int colon_pos = buffer.find(":");
      
      // Parse elements;
      int end = std::stoi(buffer.substr(0, arrow_pos));
      int start = std::stoi(buffer.substr(arrow_pos + 2, colon_pos - arrow_pos - 2));
      nodes_.insert(end);
      nodes_.insert(start);
      int value = std::stoi(buffer.substr(colon_pos + 1));
      auto ptr = edge_list_.find(start);
      if (edge_list_.end() == ptr) {
        edge_list_[start] = vector<Edge>({Edge(end, value)});
      } else {
        edge_list_[start].emplace_back(end, value);
      }
    }
  }

  vector<int> Topological() const {
    vector<int> result;
    set<int> visited;
    for (const int &node : nodes_) {
      if (0 == visited.count(node)) {
        DFS(node, result, visited);
      }
    }
    return result;
  }
  
  void DebugPrint() {
    for (auto& edges : edge_list_) {
      printf("%d:", edges.first);
      for (auto& edge : edges.second) {
        printf(" %d(%d)", edge.node, edge.value);
      }
      printf("\n");
    }
  }

  void Longest(int source, int sink) const {
    set<int> invalid;
    map<int, int> table;
    map<int, int> from;
    for (int &node : Topological()) {
      auto ptr = edge_list_.find(node);
      if (source == node) {
        table[node] = 0;
        from[node] = -1;
      } else if (edge_list_.end() != ptr) {
        int max_value = 0;
        int max_from_node = -1;
        for (const Edge &edge : ptr->second) {
          if (0 == invalid.count(edge.node)) {
            int new_value = table[edge.node] + edge.value;
            if (new_value > max_value) { 
              max_value = new_value;
              max_from_node = edge.node;
            }
          }
        }
        if (-1 == max_from_node) {
          invalid.insert(node);
        } else {
          table[node] = max_value;
          from[node] = max_from_node;
        }
      } else {
        invalid.insert(node);
      }
    }
    printf("%d\n", table[sink]);

    std::vector<int> path;
    int j = sink;
    while (j != -1) {
      path.push_back(j);
      j = from[j];
    }
    
    printf("%d", path.back());
    path.pop_back();
    while (!path.empty()) {
      printf("->%d", path.back());
      path.pop_back();
    }
    printf("\n");
  }

 private:
  void DFS(int node, vector<int> &result, set<int> &visited) const {
    visited.insert(node);
    auto ptr = edge_list_.find(node);
    if (edge_list_.end() != ptr) {
      const vector<Edge> &edges = ptr->second;
      for (const Edge& edge : edges) {
        if (0 == visited.count(edge.node)) {
          DFS(edge.node, result, visited);
        }
      }
    }
    result.push_back(node);
  }

  map<int, vector<Edge> > edge_list_;
  set<int> nodes_;
};




int main(int argc, char **argv) {
  int source = 0;
  int sink = 0;
  std::cin >> source;
  std::cin >> sink;
  Graph graph;
  graph.Longest(source, sink);
  return 0;
}
