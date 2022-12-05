// Template
#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <tuple>

using namespace std;

// Useful functions
// requires compiler flag -fconcepts
void print1(auto val) {
    cout << "Part 1: " << val << endl;
}

void print2(auto val) {
    cout << "Part 2: " << val << endl;
}

vector<string> all_lines() {
    vector<string> lines;
    string current;
    while(!cin.eof()) {
        getline(cin, current);
        lines.push_back(current);
    }
    return lines;
}

// Actual solution

typedef tuple<int, int> coords;

int main()
{
    // Part 1
    string line;
    cin >> line;
    coords current;
    map<coords, int> counts;
    counts[current]++;
    for (char c : line) {
        switch (c) {
            case '<':
                current = {get<0>(current) - 1, get<1>(current)};
                break;
            case '>':
                current = {get<0>(current) + 1, get<1>(current)};
                break;
            case '^':
                current = {get<0>(current), get<1>(current) + 1};
                break;
            case 'v':
                current = {get<0>(current), get<1>(current) - 1};
                break;
        }
        counts[current]++;
    }
    print1(counts.size());

    // Part 2
    coords santa1, santa2;
    counts.clear();
    counts[santa1]++;
    bool which = false;

    constexpr coords up {0, 1}, down {0, -1}, left {-1, 0}, right {1, 0};

    for (char c : line) {
        switch (c) {
            case '<':
                current = left;
                break;
            case '>':
                current = right;
                break;
            case '^':
                current = up;
                break;
            case 'v':
                current = down;
                break;
        }

        coords& changed = which ? santa1 : santa2;
        changed = { get<0>(changed) + get<0>(current), get<1>(changed) + get<1>(current) };
        counts[changed]++;
        which = !which;
    }
    print2(counts.size());
}