#include <gtest/gtest.h>

#include <iostream>
#include <random>

constinit unsigned long long seed = 0; // NOLINT

int main(int argc, char** argv)
{
    testing::InitGoogleTest(&argc, argv);

    static constexpr std::string_view shuffle_seed_arg = "--seed=";

    for (int i = 0; i < argc; i++) {
        std::string_view arg{argv[i]}; // NOLINT
        if (arg.starts_with(shuffle_seed_arg)) {
            seed = std::stoull(arg.begin() + shuffle_seed_arg.length());
        }
    }
    if (seed == 0) {
        // initialize seed
        std::random_device rand{};
        seed = rand();
    }

    std::cout << "Shuffle Seed: " << seed << "\n";
    return RUN_ALL_TESTS();
}
