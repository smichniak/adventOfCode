#include <iostream>
#include <sstream>
#include <numeric>
#include <algorithm>
#include "../includes/utils.h"

using std::vector, std::stringstream, std::prev, std::iota, std::max;

int const DAY = 9;
int const DX[4] = {-1, 1, 0, 0};
int const DY[4] = {0, 0, -1, 1};

class Find_union {
public:
    Find_union(int size) {
        roots = vector<int>(size);
        iota(roots.begin(), roots.end(), 0);
        weights = vector<int>(size, 1);
    }

    int find(int node) {
        int next_node = roots[node];
        if (next_node == node) {
            return next_node;
        }

        int root_node = find(next_node);
        roots[node] = root_node;
        return root_node;
    }

    void union_nodes(int node1, int node2) {
        int root1 = find(node1);
        int root2 = find(node2);
        if (root1 != root2) {
            if (weights[root1] > weights[root2]) {
                roots[root2] = root1;
                weights[root1] += weights[root2];
            } else {
                roots[root1] = root2;
                weights[root2] += weights[root1];
            }
        }
    }

    vector<int> const& get_weights() const {
        return weights;
    }

private:
    vector<int> roots;
    vector<int> weights;
};

class Day9 : public Day<int> {
public:
    explicit Day9(int day_number) : Day(day_number) {
        height = input_lines.size();
        width = input_lines[0].size();
        for (string const& line : input_lines) {
            levels.emplace_back();
            for (char digit : line) {
                prev(levels.end())->push_back(digit - '0');
            }
        }

    }

    bool has_lower_neighbour(int x, int y) {
        int level = levels[y][x];
        for (int d = 0; d < 4; ++d) {
            int n_x = x + DX[d];
            int n_y = y + DY[d];
            if (n_x < 0 || n_x >= width || n_y < 0 || n_y >= height) {
                continue;
            }

            if (levels[n_y][n_x] <= level) {
                return true;
            }
        }
        return false;
    }

    int get_index(int x, int y) const {
        return y * width + x;
    }

    int part1() override;

    int part2() override;


private:
    int height;
    int width;
    vector<vector<int>> levels;
};

int Day9::part1() {
    int risk_level = 0;
    for (int x = 0; x < width; ++x) {
        for (int y = 0; y < height; ++y) {
            if (!has_lower_neighbour(x, y)) {
                risk_level += levels[y][x] + 1;
            }
        }
    }
    return risk_level;
}

int Day9::part2() {
    Find_union basins(height * width);

    for (int x = 0; x < width; ++x) {
        for (int y = 0; y < height; ++y) {
            if (levels[y][x] != 9) {
                for (int d = 0; d < 4; ++d) {
                    int n_x = x + DX[d];
                    int n_y = y + DY[d];
                    if (n_x >= 0 && n_x < width && n_y >= 0 && n_y < height && levels[n_y][n_x] != 9) {
                        basins.union_nodes(get_index(x, y), get_index(n_x, n_y));
                    }
                }
            }
        }
    }

    int max1 = 0, max2 = 0, max3 = 0;
    for (int weight : basins.get_weights()) {
        if (weight > max1) {
            max3 = max2;
            max2 = max1;
            max1 = weight;
        } else if (weight > max2) {
            max3 = max2;
            max2 = weight;
        } else {
            max3 = max(max3, weight);
        }
    }

    return max1 * max2 * max3;
}


int main() {
    Day9 day = Day9(DAY);

    cout << day.part1() << '\n';
    cout << day.part2() << '\n';

    return 0;
}
