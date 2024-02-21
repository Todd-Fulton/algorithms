/**
 * Copyright (C) 2023  Todd W. Fulton
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program.  If not, see <https://www.gnu.org/licenses/>.
 **/

#include <algo/search.hpp>

#include <iostream>
#include <range/v3/algorithm.hpp>
#include <range/v3/view/take.hpp>
#include <vector>

using std::cout;
using std::vector;

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
                 << " at index: " << std::distance(example.begin(), itr) << ".\n";
        }
        else {
            cout << "Was not able to find number 4!\n";
        }
    }
    catch (...) {
        cout << "An exception occurred.\n";
    }

    return 0;
}
