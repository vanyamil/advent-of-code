#include <iostream>

using namespace std;

void one() {
    int x = 0, y = 0;
    
    string dir;
    int amount;
    
    while(!cin.eof()) {
        cin >> dir >> amount;
        switch(dir[0]) {
            case 'f':  // forward
                x += amount;
                break;
            case 'd': // down
                y += amount;
                break;
            case 'u': // up
                y = max(0, y - amount); // safety against going too high
                break;
        }
    }
    
    cout << "Part 1: " << x * y;
}

void two() {
    int x = 0, y = 0, aim = 0;
    
    string dir;
    int amount;
    
    while(!cin.eof()) {
        cin >> dir >> amount;
        switch(dir[0]) {
            case 'f':  // forward
                x += amount;
                y = max(0, y + aim * amount);
                break;
            case 'd': // down
                aim += amount;
                break;
            case 'u': // up
                aim -= amount; 
                break;
        }
    }
    
    cout << "Part 2: " << x * y;
}

int main()
{
    // one();
    two();
}
