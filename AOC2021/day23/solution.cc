#include <iostream>
#include <map>
#include <array>
#include <queue>
#include <utility>
#include "../includes/utils.h"

using std::array, std::map, std::pair, std::min, std::max, std::abs, std::priority_queue, std::greater;

using pair_vector = vector<pair<int, int>>;

int const DAY = 23;

enum AmphipodType {
    A = 0, B = 1, C = 2, D = 3, Empty = -1
};

map<AmphipodType, int> move_cost{{A, 1},
                                 {B, 10},
                                 {C, 100},
                                 {D, 1000}};

vector<int> const board_locations{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 21, 40, 41, 60, 61, 80, 81};


class BoardState {
public:
    BoardState() = default;

    explicit BoardState(map<int, AmphipodType> occupied_locations) {
        this->occupied_locations = std::move(occupied_locations);
    }

    vector<pair<BoardState, int>> reachable_states(int source_location) {
        vector<pair<BoardState, int>> result;
        AmphipodType amphipod_on_source = occupied_locations[source_location];
        if (amphipod_on_source == Empty) {
            return result;
        }
        pair_vector reachable = reachable_locations(source_location, amphipod_on_source);
        for (auto[target, cost] : reachable) {
            map<int, AmphipodType> new_occupied = occupied_locations;
            new_occupied[source_location] = Empty;
            new_occupied[target] = amphipod_on_source;
            result.emplace_back(new_occupied, cost);
        }

        return result;
    }

    bool operator==(BoardState const& other) const {
        return occupied_locations == other.occupied_locations;
    }

    bool operator<(BoardState const& other) const {
        return occupied_locations < other.occupied_locations;
    }

    char get_location_char(int location) const {
        AmphipodType type_on_location = occupied_locations.find(location)->second;
        if (type_on_location == Empty) {
            return '.';
        } else {
            return 'A' + type_on_location;
        }
    }

    void print() const {
        cout << "#############\n";
        cout << '#';
        for (int i = 0; i <= 10; ++i) {
            cout << get_location_char(i);
        }
        cout << "#\n";
        for (int i = 0; i <= 10; ++i) {
            if (i == 3 || i == 5 || i == 7 || i == 9) {
                cout << get_location_char((i - 1) * 10);
            } else {
                cout << '#';
            }
        }
        cout << "##\n";
        for (int i = 0; i <= 10; ++i) {
            if (i == 3 || i == 5 || i == 7 || i == 9) {
                cout << get_location_char((i - 1) * 10 + 1);
            } else {
                cout << '#';
            }
        }
        cout << "##\n\n";
    }

private:
    static inline bool legal_top_location(int location) {
        return location == 0 || location == 10 || location % 2 != 0;
    }

    pair_vector top_to_room(int location, AmphipodType amphipod_type) {
        vector<pair<int, int>> result;
        int target = (amphipod_type + 1) * 2;

        for (int i = min(location, target); i <= max(location, target); ++i) {
            if (i != location && occupied_locations[i] != Empty) {
                return result;
            }
        }

        int move_to_room_cost = (abs(target - location) + 1) * move_cost[amphipod_type];
        target *= 10; // Change target from top row to location of the room

        if (occupied_locations[target + 1] == Empty) {
            result.emplace_back(target + 1, move_to_room_cost + move_cost[amphipod_type]);
        } else if (occupied_locations[target] == Empty && occupied_locations[target + 1] == amphipod_type) {
            result.emplace_back(target, move_to_room_cost);
        }

        return result;
    }

    pair_vector room_to_top(int location, AmphipodType amphipod_type) {
        vector<pair<int, int>> result;
        bool deep_room = location % 10 == 1;
        if (deep_room && occupied_locations[location - 1] != Empty) {
            return result;
        }
        location /= 10;
        for (int i = 1; location - i >= 0 && occupied_locations[location - i] == Empty; ++i) {
            if (legal_top_location(location - i)) {
                int move_to_top_cost = (i + 1 + deep_room) * move_cost[amphipod_type];
                result.emplace_back(location - i, move_to_top_cost);
            }
        }

        for (int i = 1; location + i <= 10 && occupied_locations[location + i] == Empty; ++i) {
            if (legal_top_location(location + i)) {
                int move_to_top_cost = (i + 1 + deep_room) * move_cost[amphipod_type];
                result.emplace_back(location + i, move_to_top_cost);
            }
        }
        return result;
    }

    // Returns vector pairs in form <target_location, cost>
    pair_vector reachable_locations(int location, AmphipodType amphipod_type) {
        if (location <= 10) {
            return top_to_room(location, amphipod_type);
        } else {
            return room_to_top(location, amphipod_type);
        }
    }

    map<int, AmphipodType> occupied_locations;
};

using board_cost_map = map<BoardState, int>;
using cost_board = pair<int, BoardState>;

auto cmp_costs = [](cost_board const& a, cost_board const& b) { return a.first > b.first; };
using board_queue = priority_queue<cost_board, vector<cost_board>, decltype(cmp_costs)>;

class Day23 : public Day<int> {
public:
    explicit Day23(int day_number) : Day(day_number) {
        map<int, AmphipodType> occupied_locations;
        map<int, AmphipodType> target_occupied_locations;

        for (int location : board_locations) {
            if (location <= 10) {
                occupied_locations[location] = Empty;
                target_occupied_locations[location] = Empty;
            } else {
                string line = input_lines[2 + location % 10];
                char amphipod = line[1 + location / 10];
                occupied_locations[location] = static_cast<AmphipodType>(amphipod - 'A');

                target_occupied_locations[location] = static_cast<AmphipodType>(location / 20 - 1);
            }
        }


        starting_state = BoardState(occupied_locations);
        target_state = BoardState(target_occupied_locations);
    }

    int part1() override;

    int part2() override;


private:
    BoardState starting_state;
    BoardState target_state;

    void add_neighbours(board_cost_map& path_costs, board_queue& path_cost_queue, int cost, BoardState& board) {
        for (int source_location : board_locations) {
            for (auto const&[new_board, cost_to_move] : board.reachable_states(source_location)) {
                int new_cost = cost + cost_to_move;
                if (new_cost < path_costs[target_state]) {
                    if (!path_costs.contains(new_board) || new_cost < path_costs[new_board]) {
                        path_costs[new_board] = new_cost;
                        path_cost_queue.emplace(new_cost, new_board);
                    }
                }

            }
        }
    }


    void dijkstra(board_cost_map& board_costs) {
        board_costs[starting_state] = 0;
        board_costs[target_state] = INT_MAX;

        board_queue path_cost_queue; // Path cost, board

        path_cost_queue.emplace(0, starting_state);

        while (!path_cost_queue.empty()) {
            auto[cost, board] = path_cost_queue.top();
            path_cost_queue.pop();

            if (board == target_state) {
                cout << board_costs.size() << " size\n";
                break;
            }
            add_neighbours(board_costs, path_cost_queue, cost, board);
        }
    }

};


int Day23::part1() {
    board_cost_map board_costs;
    dijkstra(board_costs);
    return board_costs[target_state];
}

int Day23::part2() {

}


int main() {
    Day23 day = Day23(DAY);

    cout << day.part1() << '\n';
    cout << day.part2() << '\n';
    return 0;
}

