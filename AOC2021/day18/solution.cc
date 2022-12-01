#include <iostream>
#include <memory>
#include "../includes/utils.h"

using std::shared_ptr, std::make_shared, std::move, std::enable_shared_from_this;

int const DAY = 18;

class SnailfishNumber : public enable_shared_from_this<SnailfishNumber> {
public:
    explicit SnailfishNumber(int value, int nested_level = 0) {
        this->value = value;
        this->nested_level = nested_level;
        left_child = nullptr;
        right_child = nullptr;
        father = nullptr;
    }

    SnailfishNumber(shared_ptr<SnailfishNumber>& left, shared_ptr<SnailfishNumber>& right, int level) {
        left_child = left;
        right_child = right;
        nested_level = level;
        father = nullptr;
    }

    [[nodiscard]] int magnitude() const {
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
        shared_ptr<SnailfishNumber> this_ptr = shared_from_this();
        shared_ptr<SnailfishNumber> other_ptr = other.shared_from_this();

        shared_ptr<SnailfishNumber> result_ptr = make_shared<SnailfishNumber>(this_ptr, other_ptr,
                                                                              nested_level);

        result_ptr->left_child->father = result_ptr.get();
        result_ptr->right_child->father = result_ptr.get();
        result_ptr->left_child->increase_nested();
        result_ptr->right_child->increase_nested();

        return *result_ptr;
    }

    void print() {
        if (left_child == nullptr && right_child == nullptr) {
            cout << value;
            return;
        }

        cout << '[';
        if (left_child != nullptr) {
            left_child->print();
        }
        cout << ',';
        if (right_child != nullptr) {
            right_child->print();
        }
        cout << "]";

    }

    void reduce() {
        while (true) {
            shared_ptr<SnailfishNumber> number_to_explode = can_explode();
            if (number_to_explode != nullptr) {
                number_to_explode->explode();

//                print();
//                cout << "explode \n" << std::flush;

                continue;
            }

            shared_ptr<SnailfishNumber> number_to_split = can_split();
            if (number_to_split != nullptr) {
                number_to_split->split();

//                print();
//                cout << "reduce \n" << std::flush;

                continue;
            }
            break;
        }

    }

private:
    int value = -1, nested_level;
    shared_ptr<SnailfishNumber> left_child;
    shared_ptr<SnailfishNumber> right_child;
    SnailfishNumber* father;

    shared_ptr<SnailfishNumber> find_left_child() {
        shared_ptr<SnailfishNumber> node = shared_from_this();
        while (node->left_child != nullptr) {
            node = node->left_child;
        }
        return node;
    }

    shared_ptr<SnailfishNumber> find_right_child() {
        shared_ptr<SnailfishNumber> node = shared_from_this();
        while (node->right_child != nullptr) {
            node = node->right_child;
        }
        return node;
    }

    shared_ptr<SnailfishNumber> number_to_left() {
        shared_ptr<SnailfishNumber> node = shared_from_this();

        while (node->father->left_child == node) {
            node = node->father->shared_from_this();
            if (node->father == nullptr) {
                return nullptr;
            }
        }
        return node->father->left_child->find_right_child();
    }

    shared_ptr<SnailfishNumber> number_to_right() {
        shared_ptr<SnailfishNumber> node = shared_from_this();

        while (node->father->right_child == node) {
            node = node->father->shared_from_this();
            if (node->father == nullptr) {
                return nullptr;
            }
        }
        return node->father->right_child->find_left_child();
    }

    void explode() {
        shared_ptr<SnailfishNumber> left_neighbour = number_to_left();
        if (left_neighbour != nullptr) {
            bool left_neighbour_right = left_neighbour->father->right_child == left_neighbour;
            left_neighbour->value += left_child->value;
            if (left_neighbour_right) {
                left_neighbour->father->right_child = left_neighbour;
            } else {
                left_neighbour->father->left_child = left_neighbour;
            }
        }

        shared_ptr<SnailfishNumber> right_neighbour = number_to_right();
        if (right_neighbour != nullptr) {
            bool right_neighbour_right = right_neighbour->father->right_child == right_neighbour;
            right_neighbour->value += right_child->value;

            if (right_neighbour_right) {
                right_neighbour->father->right_child = right_neighbour;
            } else {
                right_neighbour->father->left_child = right_neighbour;
            }
        }

        shared_ptr<SnailfishNumber> new_num = make_shared<SnailfishNumber>(0, nested_level);

        new_num->father = father;
        if (shared_from_this() == father->right_child) {
            father->right_child = new_num;
        } else {
            father->left_child = new_num;
        }
    }

    void split() {
        int new_left_val = value / 2;
        int new_right_val = value / 2 + (value % 2 == 0);

        shared_ptr<SnailfishNumber> new_left = make_shared<SnailfishNumber>(new_left_val, nested_level);
        shared_ptr<SnailfishNumber> new_right = make_shared<SnailfishNumber>(new_right_val, nested_level);

        auto new_pair = make_shared<SnailfishNumber>(*new_left + *new_right); // TODO Get shared pointer insead of calling constructor
//        auto new_pair = (*new_left + *new_right).shared_from_this();

        if (shared_from_this() == father->right_child) {
            father->right_child = new_pair->shared_from_this();
        } else {
            father->left_child = new_pair->shared_from_this();
        }
    }

    shared_ptr<SnailfishNumber> can_explode() {
        if (left_child == nullptr && right_child == nullptr) {
            return nullptr;
        }
        if (nested_level >= 4) {
            return shared_from_this();
        }

        shared_ptr<SnailfishNumber> left_explode = left_child->can_explode();
        if (left_explode == nullptr) {
            return right_child->can_explode();
        } else {
            return left_explode;
        }

    }

    shared_ptr<SnailfishNumber> can_split() {
        if (left_child == nullptr && right_child == nullptr) {
            if (value >= 10) {
                return shared_from_this();
            } else {
                return nullptr;
            }
        }
        shared_ptr<SnailfishNumber> left_split = left_child->can_split();
        if (left_split == nullptr) {
            return right_child->can_split();
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

    shared_ptr<SnailfishNumber> create_number(string const& str, int i, int j) {
        if (str[i] != '[') {
            int num = str[i] - '0';
            return make_shared<SnailfishNumber>(num);
        }

        int open = 1;
        for (int index = i + 1; index < j; ++index) {
            if (open == 1 && str[index] == ',') {
                shared_ptr<SnailfishNumber> left = create_number(str, i + 1, index - 1);
                shared_ptr<SnailfishNumber> right = create_number(str, index + 1, j - 1);
                return make_shared<SnailfishNumber>(*left + *right);
//                return (*left + *right).shared_from_this();
            }
            open += (str[index] == '[') - (str[index] == ']');
        }

    }

    int part1() override;

    int part2() override;


private:
    vector<shared_ptr<SnailfishNumber>> numbers;
};

int Day18::part1() {
    shared_ptr<SnailfishNumber> sum = numbers[0];
    for (int i = 1; i < numbers.size(); ++i) {
        sum = make_shared<SnailfishNumber>(*sum + *numbers[i]);
        sum->reduce();
    }
    sum->print();
    cout << '\n' << std::flush;

    return sum->magnitude();
}

int Day18::part2() {

}


int main() {
    Day18 day = Day18(DAY);

    cout << day.part1() << '\n';
    cout << day.part2() << '\n';

    return 0;
}

