#include <iostream>
#include <string>
#include <vector>
#include <algorithm>

using namespace std;

void one() {
    string line;
    vector<int> counts;
    int numLines = 0;
    
    // For each line
    while(!cin.eof()) {
        cin >> line;
        
        // On first line, we figure out the length of the strings
        if(counts.empty()) 
            counts.resize(line.size(), 0);
            
        // Then, we count the number of ones
        for(int i = 0; i < line.size(); i++)
            if(line[i] == '1')
                counts[i]++;
        
        // And the total - 0s is total minus 1s
        numLines++;
    }
    
    string gamma, epsilon;
    
    // In order, for each bit
    for(int ones : counts) {
        bool more_ones = (2 * ones) > numLines;
        // Build the appropriate bit on the result strings
        gamma += more_ones ? '1' : '0';
        epsilon += more_ones ? '0' : '1';
    }
    
    // Debug
    cout << numLines << endl;
    cout << counts[0] << endl;
    cout << gamma << endl;
    cout << epsilon << endl;
    
    // Convert to decimal number
    int result = stoi(gamma, nullptr, 2) * stoi(epsilon, nullptr, 2);
    cout << "Part 1: " << result;
}

void two() {
    string line;
    vector<string> first_lines, second_lines;
    int length = 0;
    
    while(!cin.eof()) {
        cin >> line;
        if(length == 0)
            length = line.size();
        first_lines.push_back(line);
        second_lines.push_back(line);
    }
    
    // Let's use algorithm library for this.
    for(int i = 0; i < length; i++) {
        if(first_lines.size() > 1) {
            auto count_1 = count_if(first_lines.begin(), first_lines.end(), [i](string x) { 
                return x[i] == '1';
            });
            char keep = count_1 * 2 >= first_lines.size() ? '1' : '0';
            auto end_1 = remove_if(first_lines.begin(), first_lines.end(), [=](string x) {
                return x[i] != keep;
            });
            first_lines.resize(end_1 - first_lines.begin());
        }
        
        if(second_lines.size() > 1) {
            auto count_2 = count_if(second_lines.begin(), second_lines.end(), [i](string x) { 
                return x[i] == '1';
            });
            char keep = count_2 * 2 >= second_lines.size() ? '0' : '1';
            auto end_2 = remove_if(second_lines.begin(), second_lines.end(), [=](string x) {
                return x[i] != keep;
            });
            second_lines.resize(end_2 - second_lines.begin());
        }
    }
    
    cout << "first lines" << first_lines.size() << endl;
    cout << "second lines" << second_lines.size() << endl;
    
    // Convert to decimal number
    int result = stoi(first_lines[0], nullptr, 2) * stoi(second_lines[0], nullptr, 2);
    cout << "Part 2: " << result;
}

int main()
{
    // one();
    two();
}
