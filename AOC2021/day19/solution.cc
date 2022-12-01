#include <iostream>
#include <set>
#include <unordered_set>
#include <algorithm>
#include <sstream>
#include "../includes/utils.h"

using std::set, std::set_intersection, std::tuple, std::make_tuple, std::stringstream, std::stoi, std::copy, std::get;
using std::initializer_list, std::multiset, std::abs, std::set_intersection, std::back_inserter, std::pair;
using std::sort;

using triple = tuple<int, int, int>;

int const DAY = 19;
int const MINIMUM_COMMON_BEACONS = 12;
int const BEACON_PAIRS = MINIMUM_COMMON_BEACONS * (MINIMUM_COMMON_BEACONS - 1) / 2;

class Matrix3x3 {
public:
    explicit Matrix3x3(initializer_list<int> l) {
        int i = 0;
        for (int item : l) {
            elements[i / 3][i % 3] = item;
            ++i;
        }
    }

    explicit Matrix3x3(int matrix_elements[3][3]) {
        copy(&matrix_elements[0][0], &matrix_elements[0][0] + 9, &elements[0][0]);
    }

    Matrix3x3 operator*(Matrix3x3 const& other) const {
        int new_matrix[3][3] = {};
        for (int i = 0; i < 3; ++i) {
            for (int j = 0; j < 3; ++j) {
                for (int k = 0; k < 3; ++k) {
                    new_matrix[i][j] += elements[i][k] * other.elements[k][j];
                }
            }
        }
        return Matrix3x3(new_matrix);
    }

    triple operator*(triple const& vector) const {
        int old_vector[3] = {get<0>(vector), get<1>(vector), get<2>(vector)};
        int new_vector[3] = {};
        for (int i = 0; i < 3; ++i) {
            for (int j = 0; j < 3; ++j) {
                new_vector[i] += elements[i][j] * old_vector[j];
            }
        }
        return make_tuple(new_vector[0], new_vector[1], new_vector[2]);
    }

    int operator[](int i) const {
        return elements[i / 3][i % 3];
    }

    int operator<(Matrix3x3 const& other) const {
        for (int i = 0; i < 9; ++i) {
            if ((*this)[i] < other[i]) {
                return -1;
            } else if ((*this)[i] > other[i]) {
                return 1;
            }
        }
        return 0;
    }

    void print() const {
        for (int i = 0; i < 3; ++i) {
            for (int j = 0; j < 3; ++j) {
                cout << elements[i][j] << ' ';
            }
            cout << '\n';
        }
    }


private:
    int elements[3][3];
};

set<Matrix3x3> generate_rotations() {
    Matrix3x3 const X = Matrix3x3({1, 0, 0, 0, 0, -1, 0, 1, 0});
    Matrix3x3 const Y = Matrix3x3({0, 0, 1, 0, 1, 0, -1, 0, 0});
    Matrix3x3 const Z = Matrix3x3({0, -1, 0, 1, 0, 0, 0, 0, 1});

    set<Matrix3x3> possible_rotations;
    for (int x = 0; x < 4; ++x) {
        for (int y = 0; y < 4; ++y) {
            for (int z = 0; z < 4; ++z) {
                Matrix3x3 current_rotation = Matrix3x3({1, 0, 0, 0, 1, 0, 0, 0, 1});
                for (int k = 0; k < x; ++k) {
                    current_rotation = current_rotation * X;
                }
                for (int k = 0; k < y; ++k) {
                    current_rotation = current_rotation * Y;
                }
                for (int k = 0; k < z; ++k) {
                    current_rotation = current_rotation * Z;
                }
                possible_rotations.insert(current_rotation);
            }
        }
    }
    return possible_rotations;
}

class Day19 : public Day<int> {
public:
    explicit Day19(int day_number) : Day(day_number) {
        string coordinate;
        for (string const& line : input_lines) {
            if (line[1] == '-') {
                scanners.emplace_back();
            } else if (!line.empty()) {
                stringstream string_stream(line);
                getline(string_stream, coordinate, ',');
                int x = stoi(coordinate);
                getline(string_stream, coordinate, ',');
                int y = stoi(coordinate);
                getline(string_stream, coordinate);
                int z = stoi(coordinate);
                scanners.back().emplace_back(x, y, z);
            }
        }

        for (vector<triple> const& beacons : scanners) {
            differences.emplace_back(get_differences(beacons));
        }
        rotation_graph = vector<vector<pair<int, Matrix3x3>>>(scanners.size());
        shift_graph = vector<vector<pair<int, triple>>>(scanners.size());
        get_intersecting();
        Matrix3x3 identity({1, 0, 0, 0, 1, 0, 0, 0, 1});
        rotate_beacons(0, identity);
        find_shifts(0);
    }

    static multiset<triple> get_differences(vector<triple> const& beacons) {
        multiset<triple> result;
        for (int i = 0; i < beacons.size(); ++i) {
            for (int j = i + 1; j < beacons.size(); ++j) {
                int diff_x = abs(get<0>(beacons[i]) - get<0>(beacons[j]));
                int diff_y = abs(get<1>(beacons[i]) - get<1>(beacons[j]));
                int diff_z = abs(get<2>(beacons[i]) - get<2>(beacons[j]));
                result.emplace(diff_x, diff_y, diff_z);
            }
        }
        return result;
    }

    static vector<triple> rotate_vector(vector<triple> const& old_vector, Matrix3x3 const& rotation_matrix) {
        vector<triple> new_set;
        for (triple coordinates : old_vector) {
            new_set.emplace_back(rotation_matrix * coordinates);
        }
        return new_set;
    }

    void get_intersecting() {
        for (int first_scanner = 0; first_scanner < scanners.size(); ++first_scanner) {
            for (int second_scanner = first_scanner + 1; second_scanner < scanners.size(); ++second_scanner) {
                for (Matrix3x3 const& rotation : rotations) {
                    vector<triple> rotated_beacons = rotate_vector(scanners[second_scanner], rotation);
                    multiset<triple> rotated_differences = get_differences(rotated_beacons);
                    vector<triple> intersection;

                    set_intersection(differences[first_scanner].begin(), differences[first_scanner].end(),
                                     rotated_differences.begin(), rotated_differences.end(),
                                     back_inserter(intersection));
                    if (intersection.size() >= BEACON_PAIRS) {
                        rotation_graph[first_scanner].emplace_back(second_scanner, rotation);

                        cout << first_scanner << ' ' << second_scanner << ' ' << intersection.size() << '\n';
                        rotation.print();
                        break;
                    }

                }


            }
        }

    }

    void rotate_beacons(int i, Matrix3x3 const& rotation) {
        scanners[i] = rotate_vector(scanners[i], rotation);
        for (auto&[neighbour, next_rotation] : rotation_graph[i]) {
            rotate_beacons(neighbour, rotation * next_rotation);
        }
    }

    void find_shifts(int scanner) {
        for (auto&[neighbour, _] : rotation_graph[scanner]) {
            for (int i = 0; i < scanners[scanner].size(); ++i) {
                for (int j = 0; j < scanners[scanner].size(); ++j) {
                    if (i != j) {
                        triple first_difference = scanners[scanner][i] - scanners[scanner][j];
                        for (int k = 0; k < scanners[neighbour].size(); ++k) {
                            for (int l = 0; l < scanners[neighbour].size(); ++l) {
                                if (k != l) {
                                    triple second_difference = scanners[neighbour][k] - scanners[neighbour][l];

                                    if (first_difference == second_difference) {
                                        shift_graph[scanner].emplace_back(neighbour, scanners[scanner][i] -
                                                                                     scanners[neighbour][k]);
                                        i = scanners[scanner].size();
                                    }
                                }

                            }
                        }
                    }
                }
            }
            find_shifts(neighbour);
        }
    }

    int part1() override;

    int part2() override;


private:
    static inline set<Matrix3x3> const rotations = generate_rotations();

    vector<vector<triple>> scanners;
    vector<multiset<triple>> differences;
    vector<vector<pair<int, Matrix3x3>>> rotation_graph;
    vector<vector<pair<int, triple>>> shift_graph;

};


int Day19::part1() {


}

int Day19::part2() {

}


int main() {
    Day19 day = Day19(DAY);

    cout << day.part1() << '\n';
    cout << day.part2() << '\n';

    return 0;
}

