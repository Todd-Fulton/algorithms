#include <algo/search.hpp>

#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

int main()
{
    try {

        vector<int> example{{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}};
        cout << "Searching for number: 4\n";
        cout << "... in vector: {";
        ranges::for_each(ranges::take_view(example, 9),
                         [](auto const& x) { cout << x << ", "; });
        cout << example.back() << "}\n";

        auto itr = algo::linear_search(example, 4);
        if (itr != std::end(example)) {

            cout << "Found number " << *itr
                 << " at index: " << std::distance(example.begin(), itr)
                 << ".\n";
        }
        else {
            cout << "Was not able to find number 4!\n";
        }
    }
    catch (...) {
        cout << "An exception occured.\n";
    }

    return 0;
}
