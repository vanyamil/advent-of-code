#include <iostream>
#include <string>

using namespace std;

void print1(auto val) {
    cout << "Part 1: " << val << endl;
}

void print2(auto val) {
    cout << "Part 2: " << val << endl;
}

int main()
{
    // Part 1
    string line;
    cin >> line;
    int count = 0;
    for (char i : line) {
        if (i == '(')
            count++;
        else
            count--;
    }
    print1(count);
    
    // Part 2
    count = 0;
    int idx = 1;
    for (char i : line) {
        if (i == '(')
            count++;
        else {
            count--;
            if (count < 0) {
                print2(idx);
                break;
            }
        }
        idx ++;
    }
}
