#include <iostream>
#include "../includes/utils.h"

int const DAY = 18;

class SnailfishNumber {
public:
    SnailfishNumber(int value, int nested_level = 0) {
        this->value = value;
        this->nested_level = nested_level;
        left_child = nullptr;
        right_child = nullptr;
        father = nullptr;
    }

    SnailfishNumber(SnailfishNumber* left, SnailfishNumber* right, SnailfishNumber* father, int level) {
        left_child = left;
        right_child = right;
        nested_level = level;
        this->father = father;
    }

    int magnitude() const {
        if (left_child == nullptr && right_child == nullptr) {
            return value;
        } else {
            return 3 * left_child->magnitude() + 2 * right_child->magnitude();
        }
    }

    void increase_nested() {
        nested_level++;
        if (left_child != nullptr && right_child != nullptr) {
            left_child->increase_nested();
            right_child->increase_nested();
        }
    }

    SnailfishNumber& operator+(SnailfishNumber& other) {
        auto* result = new SnailfishNumber(this, &other, nullptr, nested_level);
        this->father = result;
        other.father = result;
        increase_nested();
        other.increase_nested();

        return *result;
    }

    void reduce() {
        while (true) {
            SnailfishNumber* number_to_explode = can_explode();
            if (number_to_explode != nullptr) {
                number_to_explode->explode();
                continue;
            }

            SnailfishNumber* number_to_split = can_split();
            if (number_to_split != nullptr) {
                number_to_split->split();
                continue;
            }
            break;
        }

    }

private:
    int value, nested_level;
    SnailfishNumber* left_child;
    SnailfishNumber* right_child;
    SnailfishNumber* father;

    SnailfishNumber* number_to_left() {
        SnailfishNumber* current_number = nullptr;
        SnailfishNumber* current_father = this;

        do {
            current_number = current_father;
            current_father = current_father->father;
            if (current_father == nullptr) {
                return nullptr;
            }
        } while (current_father->left_child == current_number);

        current_number = current_father->left_child;
        while (current_number->right_child != nullptr) {
            current_number = current_number->right_child;
        }
        return current_number;
    }

    SnailfishNumber* number_to_right() {
        SnailfishNumber* current_number = nullptr;
        SnailfishNumber* current_father = this;

        do {
            current_number = current_father;
            current_father = current_father->father;
            if (current_father == nullptr) {
                return nullptr;
            }
        } while (current_father->right_child == current_number);

        current_number = current_father->right_child;
        while (current_number->left_child != nullptr) {
            current_number = current_number->left_child;
        }
        return current_number;
    }

    void explode() {
        SnailfishNumber* left_neighbour = number_to_left();
        left_neighbour->value += left_child->value;

        SnailfishNumber* right_neighbour = number_to_right();
        right_neighbour->value += right_child->value;

        auto* new_num = new SnailfishNumber(0, nested_level);
        new_num->father = father;
        if (this == father->right_child) {
            father->right_child = new_num;
        } else {
            father->left_child = new_num;
        }

        delete this;
    }

    void split() {
        int new_left_val = value / 2;
        int new_right_val = value / 2 + (value % 2 == 0);

        auto* new_left = new SnailfishNumber(new_left_val, nested_level);
        auto* new_right = new SnailfishNumber(new_right_val, nested_level);
        auto new_pair = *new_left + *new_right;

        if (this == father->right_child) {
            father->right_child = &new_pair;
        } else {
            father->left_child = &new_pair;
        }

        delete this;
    }

    SnailfishNumber* can_explode() {
        if (left_child == nullptr && right_child == nullptr) {
            return nullptr;
        }
        if (nested_level >= 4) {
            return this;
        }

        SnailfishNumber* left_explode = left_child->can_explode();
        if (left_explode == nullptr) {
            return right_child->can_explode();
        } else {
            return left_explode;
        }

    }

    SnailfishNumber* can_split() {
        if (left_child == nullptr && right_child == nullptr) {
            if (value >= 10) {
                return this;
            } else {
                return nullptr;
            }
        }
        SnailfishNumber* left_split = left_child->can_explode();
        if (left_split == nullptr) {
            return right_child->can_explode();
        } else {
            return left_split;
        }
    }
};

class Day18 : public Day<int> {
public:
    explicit Day18(int day_number) : Day(day_number) {
        for (string const& line : input_lines) {
            numbers.emplace_back(create_number(line, 0, line.size() - 1));
        }
    }

    SnailfishNumber create_number(string const& str, int i, int j) {
        if (str[i] != '[') {
            int num = str[i] - '0';
            return SnailfishNumber(num);
        }

        int open = 1;
        for (int index = i + 1; index < j; ++index) {
            if (open == 1 && str[index] == ',') {
                SnailfishNumber left = create_number(str, i + 1, index - 1);
                SnailfishNumber right = create_number(str, index + 1, j - 1);
                return left + right;
            }
            open += (str[index] == '[') - (str[index] == ']');
        }

    }

    int part1() override;

    int part2() override;


private:
    vector<SnailfishNumber> numbers;
};

int Day18::part1() {
    SnailfishNumber sum = numbers[0];
    for (int i = 1; i < numbers.size(); ++i) {
        sum = sum + numbers[i];
        sum.reduce();
    }

    return sum.magnitude();
}

int Day18::part2() {

}


int main() {
    Day18 day = Day18(DAY);

    cout << day.part1() << '\n';
    cout << day.part2() << '\n';

    return 0;
}

