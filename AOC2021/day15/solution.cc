#include <iostream>
#include <unordered_map>
#include <queue>
#include <unordered_set>
#include "../includes/utils.h"

using std::pair, std::unordered_map, std::priority_queue, std::greater, std::tuple, std::make_tuple, std::unordered_set;

using point_queue = priority_queue<tuple<int, int, int>, vector<tuple<int, int, int>>, greater<>>;


int const DAY = 15;
int const MAP_SCALE = 5;
int const RISK_MODULO = 10;

template<typename T>
class scaled_vector {
private:
    int scale_factor;
    vector<T> vec;
public:
    scaled_vector() {
        scale_factor = 1;
    }

    explicit scaled_vector(int scale) {
        scale_factor = scale;
    }

    int size() {
        return scale_factor * vec.size();
    }

    int vec_size() {
        return vec.size();
    }

    T& operator[](int i) {
        return vec[i % vec.size()];
    }

    void emplace_back(T element) {
        vec.emplace_back(element);
    }

};

using scaled_array = scaled_vector<scaled_vector<int>>;
using array = vector<vector<int>>;


class Day15 : public Day<int> {
public:
    explicit Day15(int day_number, int scale) : Day(day_number) {
        risk_levels = scaled_vector<scaled_vector<int>>(scale);

        for (int i = 0; i < input_lines.size(); ++i) {
            risk_levels.emplace_back(scaled_vector<int>(scale));
            for (int j = 0; j < input_lines[0].size(); ++j) {
                risk_levels[i].emplace_back(input_lines[i][j] - '0');
            }
        }
    }

    void add_neighbours(array& path_weights, point_queue& path_weight_queue, int path_weight, int y, int x) {
        for (int d = 0; d < 4; ++d) {
            int n_y = y + DY[d];
            int n_x = x + DX[d];
            if (n_y >= 0 && n_y < risk_levels.size() && n_x >= 0 && n_x < risk_levels[0].size()) {
                int neighbour_level =
                        risk_levels[n_y][n_x] + n_y / risk_levels.vec_size() + n_x / risk_levels[0].vec_size();
                neighbour_level -= neighbour_level / RISK_MODULO * (RISK_MODULO - 1);

                int new_weight = path_weight + neighbour_level;
                if (new_weight < path_weights[n_y][n_x]) {
                    path_weights[n_y][n_x] = new_weight;

                    path_weight_queue.push(make_tuple(path_weights[n_y][n_x], n_y, n_x));
                }
            }
        }
    }

    void dijkstra(array& path_weights) {
        for (int i = 0; i < risk_levels.size(); ++i) {
            path_weights.emplace_back(vector<int>());
            for (int j = 0; j < risk_levels[0].size(); ++j) {
                path_weights[i].emplace_back(INT_MAX);
            }
        }
        path_weights[0][0] = 0;

        // Path weight, y, x
        point_queue path_weight_queue;
        path_weight_queue.push(make_tuple(0, 0, 0));
        while (!path_weight_queue.empty()) {
            auto[weight, y, x] = path_weight_queue.top();
            path_weight_queue.pop();
            add_neighbours(path_weights, path_weight_queue, weight, y, x);
        }

    }

    int part1() override;

    int part2() override;


private:
    scaled_array risk_levels;
};

int Day15::part1() {
    array path_weights;
    dijkstra(path_weights);
    return path_weights[risk_levels.size() - 1][risk_levels[0].size() - 1];

}

int Day15::part2() {
    array path_weights;
    dijkstra(path_weights);
    return path_weights[risk_levels.size() - 1][risk_levels[0].size() - 1];
}


int main() {
    Day15 day = Day15(DAY, 1);
    cout << day.part1() << '\n';

    day = Day15(DAY, MAP_SCALE);
    cout << day.part2() << '\n';

    return 0;
}

