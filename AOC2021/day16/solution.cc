#include <iostream>
#include "../includes/utils.h"

int const DAY = 16;

string hex_char_to_bin(char c) {
    switch (toupper(c)) {
        case '0':
            return "0000";
        case '1':
            return "0001";
        case '2':
            return "0010";
        case '3':
            return "0011";
        case '4':
            return "0100";
        case '5':
            return "0101";
        case '6':
            return "0110";
        case '7':
            return "0111";
        case '8':
            return "1000";
        case '9':
            return "1001";
        case 'A':
            return "1010";
        case 'B':
            return "1011";
        case 'C':
            return "1100";
        case 'D':
            return "1101";
        case 'E':
            return "1110";
        case 'F':
            return "1111";
    }
}

int read_version(int& i, vector<char> const& bits) {
    int version = bits[i] * 4 + bits[i + 1] * 2 + bits[i + 2];
    i += 3;
    return version;
}

int read_type(int& i, vector<char> const& bits) {
    int type = bits[i] * 4 + bits[i + 1] * 2 + bits[i + 2];
    i += 3;
    return type;
}

class Packet {
public:
    Packet(int version, int type) {
        this->version = version;
        this->type = type;
    }

    int get_version() const {
        return version;
    }

private:
    int type;
    int version;

};

class LiteralValue : public Packet {
public:
    LiteralValue(int& i, vector<char> const& bits) : Packet(read_version(i, bits), read_type(i, bits)) {

    }

private:
    int value;
};

class Operator : public Packet {
public:
    Operator(int& i, vector<char> const& bits) : Packet(read_version(i, bits), read_type(i, bits)) {


    }

private:
    vector<LiteralValue> sub_values;
    vector<Operator> sub_operators;
};

class Day16 : public Day<int> {
public:
    explicit Day16(int day_number) : Day(day_number) {
        vector<char> bits;
        for (char c : input_lines[0]) {
            for (char bit : hex_char_to_bin(c)) {
                bits.emplace_back(bit);
            }
        }
        int i = 0;
        root_packet = Operator(i, bits);
    }

    int part1() override;

    int part2() override;


private:
    Operator root_packet;
};

int Day16::part1() {


}

int Day16::part2() {

}


int main() {
    Day16 day = Day16(DAY);

    cout << day.part1() << '\n';
    cout << day.part2() << '\n';

    return 0;
}

