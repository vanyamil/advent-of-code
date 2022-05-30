#include <iostream>

using namespace std;

void one() {
    int last, curr, count_ups = 0;
    cin >> curr;
    
    while(!cin.eof()) 
    {
        last = curr;
        cin >> curr;
        if(curr > last)
            count_ups++;
    }

    cout << "Part 1: " << count_ups;
}

void two() {
    int lastWindow, three, two, one, newWindow, count_ups = 0;
    cin >> three >> two >> one;
    newWindow = three + two + one;
    
    while(!cin.eof())
    {
        lastWindow = newWindow;
        three = two;
        two = one;
        cin >> one;
        newWindow = three + two + one;
        if(lastWindow < newWindow)
            count_ups++;
    }

    cout << "Part 2: " << count_ups;
}

int main()
{
    // one();
    two();
}
