#ifndef AOC2021_UTILS_H
#define AOC2021_UTILS_H

#include <vector>
#include <fstream>
#include <string>

using std::cout, std::vector, std::string;

int const DX[4] = {-1, 1, 0, 0};
int const DY[4] = {0, 0, -1, 1};

template<typename T>
class Day {
public:
    explicit Day(int day_number) {
        string input_file_name = "day" + std::to_string(day_number) + "/input.in";
        std::ifstream file_stream(input_file_name);

        string line;
        while (std::getline(file_stream, line, '\n')) {
            input_lines.push_back(line);
        }

    }

    virtual T part1() = 0;

    virtual T part2() = 0;

protected:
    vector<string> input_lines;
};

namespace std {
    template<typename T, typename G>
    struct hash<std::pair<T, G>> {
        inline size_t operator()(const std::pair<T, G>& v) const {
            std::hash<T> T_hasher;
            std::hash<G> G_hasher;
            return T_hasher(v.first) ^ G_hasher(v.second);
        }
    };

//    template<>
//    struct hash<std::pair<char, char>> {
//        inline size_t operator()(const std::pair<int, int>& v) const {
//            std::hash<char> char_hasher;
//            return char_hasher(v.first) ^ char_hasher(v.second);
//        }
//    };
}

template <typename ... Ts, std::size_t ... Is>
std::tuple<Ts...> sumT (std::tuple<Ts...> const & t1,
                        std::tuple<Ts...> const & t2,
                        std::index_sequence<Is...> const &)
{ return { (std::get<Is>(t1) + std::get<Is>(t2))... }; }

template <typename ... Ts, std::size_t ... Is>
std::tuple<Ts...> diffT (std::tuple<Ts...> const & t1,
                         std::tuple<Ts...> const & t2,
                         std::index_sequence<Is...> const &)
{ return { (std::get<Is>(t1) - std::get<Is>(t2))... }; }

template <typename ... Ts>
std::tuple<Ts...> operator+ (std::tuple<Ts...> const & t1,
                             std::tuple<Ts...> const & t2)
{ return sumT(t1, t2, std::make_index_sequence<sizeof...(Ts)>{}); }

template <typename ... Ts>
std::tuple<Ts...> operator- (std::tuple<Ts...> const & t1,
                             std::tuple<Ts...> const & t2)
{ return diffT(t1, t2, std::make_index_sequence<sizeof...(Ts)>{}); }

#endif //AOC2021_UTILS_H
