/**
 * Copyright (C) 2023  Todd W. Fulton
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as
 *published by the Free Software Foundation, either version 3 of the
 *License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *License along with this program.  If not, see
 *<https://www.gnu.org/licenses/>.
 **/

#include <gtest/gtest.h>

#include <iostream>
#include <random>

constinit unsigned long long seed = 0;       // NOLINT
constinit unsigned long long range_size = 0; // NOLINT

int main(int argc, char** argv)
{
    testing::InitGoogleTest(&argc, argv);

    static constexpr std::string_view shuffle_seed_arg = "--seed=";
    static constexpr std::string_view range_size_arg = "--range-size=";

    for (int i = 0; i < argc; i++) {
        std::string_view arg{argv[i]}; // NOLINT
        if (arg.starts_with(shuffle_seed_arg)) {
            seed = std::stoull(arg.begin() + shuffle_seed_arg.length());
        }
        else if (arg.starts_with(range_size_arg)) {
            range_size =
                std::stoull(arg.begin() + range_size_arg.length());
        }
    }
    std::random_device rand{};
    if (seed == 0) {
        // initialize seed
        seed = rand();
    }
    if (range_size == 0) {
        range_size = 20 + (rand() % 200);
    }

    std::cout << "Shuffle Seed: " << seed << "\n";
    std::cout << "Range Size: " << range_size << "\n";
    return RUN_ALL_TESTS();
}
