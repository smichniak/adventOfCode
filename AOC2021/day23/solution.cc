#include <iostream>
#include <map>
#include <array>
#include <queue>
#include <utility>
#include "../includes/utils.h"

using std::array, std::map, std::pair, std::min, std::max, std::abs, std::priority_queue;

using pair_vector = vector<pair<int, int>>;

int const DAY = 23;

enum AmphipodType {
    A = 0, B = 1, C = 2, D = 3, Empty = -1
};

map<AmphipodType, int> move_cost{{A, 1},
                                 {B, 10},
                                 {C, 100},
                                 {D, 1000}};

class BoardState {
public:
    BoardState() = default;

    explicit BoardState(map<int, AmphipodType> occupied_locations, int depth) {
        this->occupied_locations = std::move(occupied_locations);
        this->depth = depth;
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
            result.emplace_back(BoardState(new_occupied, depth), cost);
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
        for (int d = 0; d < depth; ++d) {
            for (int i = 0; i <= 10; ++i) {
                if (i == 3 || i == 5 || i == 7 || i == 9) {
                    cout << get_location_char((i - 1) * 10 + d);
                } else {
                    cout << '#';
                }
            }
            cout << "##\n";
        }

        cout << '\n';
    }

private:
    map<int, AmphipodType> occupied_locations;
    int depth{};

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

        int move_above_room_cost = abs(target - location) * move_cost[amphipod_type];
        target *= 10; // Change target from top row to location of the room

        if (occupied_locations[target + depth - 1] == Empty) {
            // Check if the the deepest location in a room is empty
            result.emplace_back(target + depth - 1, move_above_room_cost + move_cost[amphipod_type] * depth);
            return result;
        }

        for (int current_depth = depth - 2; current_depth >= 0; --current_depth) {
            // Find deepest, not filled location in a room
            if (occupied_locations[target + current_depth] == Empty &&
                occupied_locations[target + current_depth + 1] == amphipod_type) {
                result.emplace_back(target + current_depth,
                                    move_above_room_cost + move_cost[amphipod_type] * (current_depth + 1));
                break;
            }
        }

        return result;
    }

    pair_vector room_to_top(int location, AmphipodType amphipod_type) {
        vector<pair<int, int>> result;
        bool deep_location = location % 10 != 0;
        if (deep_location && occupied_locations[location - 1] != Empty) {
            return result;
        }
        int move_out_of_room_cost = (location % 10 + 1) * move_cost[amphipod_type];

        location /= 10;
        for (int i = 1; location - i >= 0 && occupied_locations[location - i] == Empty; ++i) {
            if (legal_top_location(location - i)) {
                int total_move_cost = i * move_cost[amphipod_type] + move_out_of_room_cost;
                result.emplace_back(location - i, total_move_cost);
            }
        }

        for (int i = 1; location + i <= 10 && occupied_locations[location + i] == Empty; ++i) {
            if (legal_top_location(location + i)) {
                int total_move_cost = i * move_cost[amphipod_type] + move_out_of_room_cost;
                result.emplace_back(location + i, total_move_cost);
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
};

using board_cost_map = map<BoardState, int>;
using cost_board = pair<int, BoardState>;

auto cmp_costs = [](cost_board const& a, cost_board const& b) { return a.first > b.first; };
using board_queue = priority_queue<cost_board, vector<cost_board>, decltype(cmp_costs)>;

class Day23 : public Day<int> {
public:
    explicit Day23(int day_number) : Day(day_number) {
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

    }

    int part1() override;

    int part2() override;


private:
    map<int, AmphipodType> occupied_locations;
    map<int, AmphipodType> target_occupied_locations;
    vector<int> board_locations{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 21, 40, 41, 60, 61, 80, 81};

    void add_neighbours(board_cost_map& path_costs, board_queue& path_cost_queue, int cost, BoardState& board,
                        BoardState& target_state) {
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


    void dijkstra(board_cost_map& board_costs, BoardState& starting_state, BoardState& target_state) {
        board_costs[starting_state] = 0;
        board_costs[target_state] = INT_MAX;

        board_queue path_cost_queue; // Path cost, board

        path_cost_queue.emplace(0, starting_state);

        while (!path_cost_queue.empty()) {
            auto[cost, board] = path_cost_queue.top();
            path_cost_queue.pop();

            if (board == target_state) {
                break;
            }
            add_neighbours(board_costs, path_cost_queue, cost, board, target_state);
        }
    }

    void add_folded_locations() {
        for (int location : board_locations) {
            if (location > 10 && location % 10 == 0) {
                board_locations.emplace_back(location + 2);
                board_locations.emplace_back(location + 3);
                target_occupied_locations[location + 2] = static_cast<AmphipodType>(location / 20 - 1);
                target_occupied_locations[location + 3] = static_cast<AmphipodType>(location / 20 - 1);
                occupied_locations[location + 3] = occupied_locations[location + 1];
            }
        }

        occupied_locations[21] = D;
        occupied_locations[22] = D;
        occupied_locations[41] = C;
        occupied_locations[42] = B;
        occupied_locations[61] = B;
        occupied_locations[62] = A;
        occupied_locations[81] = A;
        occupied_locations[82] = C;
    }
};


int Day23::part1() {
    int depth = 2;
    BoardState starting_state = BoardState(occupied_locations, depth);
    BoardState target_state = BoardState(target_occupied_locations, depth);

    board_cost_map board_costs;
    dijkstra(board_costs, starting_state, target_state);
    return board_costs[target_state];
}

int Day23::part2() {
    int depth = 4;
    add_folded_locations();

    BoardState starting_state = BoardState(occupied_locations, depth);
    BoardState target_state = BoardState(target_occupied_locations, depth);

    board_cost_map board_costs;
    dijkstra(board_costs, starting_state, target_state);
    return board_costs[target_state];
}


int main() {
    Day23 day = Day23(DAY);

    cout << day.part1() << '\n';
    cout << day.part2() << '\n';
    return 0;
}
