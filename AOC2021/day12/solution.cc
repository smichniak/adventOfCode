#include <iostream>
#include <unordered_map>
#include <sstream>
#include <unordered_set>
#include "../includes/utils.h"

using std::unordered_map, std::stringstream, std::make_pair, std::unordered_multiset;

int const DAY = 12;
int const START = 1;
int const END = 0;

class Day12 : public Day<int> {
public:
    explicit Day12(int day_number) : Day(day_number) {
        for (string const& line : input_lines) {
            stringstream string_stream(line);
            string first_node, second_node;

            getline(string_stream, first_node, '-');
            getline(string_stream, second_node);

            int first_node_id = get_id(first_node), second_node_id = get_id(second_node);

            graph[first_node_id].emplace_back(second_node_id);
            graph[second_node_id].emplace_back(first_node_id);
        }
    }

    static int get_id(string const& node) {
        if (node == "start") {
            return START;
        } else if (node == "end") {
            return END;
        } else if (node[0] > 'a') {
            return (node[0] - 'a') * 30 + node[1] - 'a' + 2;
        } else {
            return -(node[0] - 'A') * 30 - node[1] + 'A' - 2;
        }
    }

    void dfs(int current_node, int k, int duplicate) {
        if (current_node == END) {
            paths[k]++;
        } else {
            path_nodes.insert(current_node);

            for (int neighbour : graph[current_node]) {
                if (neighbour <= 0) {
                    dfs(neighbour, k, duplicate);
                } else if (neighbour > 1) {
                    int count = path_nodes.count(neighbour);

                    if (count == 0) {
                        dfs(neighbour, k, duplicate);
                    } else if (count <= k && duplicate == 0) {
                        dfs(neighbour, k, neighbour);
                    }

                }
            }

            path_nodes.erase(path_nodes.find(current_node));
        }
    }

    int part1() override;

    int part2() override;


private:
    int paths[2] = {0, 0};
    unordered_multiset<int> path_nodes;
    unordered_map<int, vector<int>> graph;
};

int Day12::part1() {
    dfs(START, 0, 0);
    return paths[0];
}

int Day12::part2() {
    dfs(START, 1, 0);
    return paths[1];
}


int main() {
    Day12 day = Day12(DAY);

    cout << day.part1() << '\n';
    cout << day.part2() << '\n';

    return 0;
}
