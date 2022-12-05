// Template
#include <iostream>
#include <string>
#include <vector>

using namespace std;

// Useful functions

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

int main()
{
    // Part 1
    vector<string> lines = all_lines();
    int total = 0;
    for (const string& line : lines) {
        size_t firstx = line.find("x"), lastx = line.rfind("x");
        int first = stoi(line.substr(0, firstx));
        int last = stoi(line.substr(lastx + 1));
        int mid = stoi(line.substr(firstx + 1, lastx - firstx - 1));
        int areaA = first * last, areaB = last * mid, areaC = mid * first;
        int local = 2 * (areaA + areaB + areaC) + min({areaA, areaB, areaC});
        total += local;
    }
    print1(total);

    // Part 2
    total = 0;
    for (const string& line : lines) {
        size_t firstx = line.find("x"), lastx = line.rfind("x");
        int first = stoi(line.substr(0, firstx));
        int last = stoi(line.substr(lastx + 1));
        int mid = stoi(line.substr(firstx + 1, lastx - firstx - 1));
        int perimeter = 2 * (first + last + mid - max({first, last, mid}));
        int volume = first * last * mid;
        total += perimeter + volume;
    }
    print2(total);
}
