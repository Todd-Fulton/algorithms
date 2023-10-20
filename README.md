## C++ algorithms and data structures study
Just a project for me to study and practice algorithms and datastructures in C++.


## C++ project management
This project also functions as a sandbox to study and practice the management of C++ projects.

## Building

Clone the repository, create and cd into the build directory:
```
git clone https://Todd-Fulton/algorithms.git
mkdir algorithms/build
cd algorithms/build
```

Configure and build:
```
cmake -DCMAKE_TOOLCHAIN_FILE=<vcpkg_root>/scripts/buildsystems/vcpkg.cmake ..
cmake --build .
```

To build and run the tests:
```
cmake -DCMAKE_TOOLCHAIN_FILE=<vcpkg_root/scripts/buildsystems/vcpkg.cmake \
      -DALGORITHMS_TESTING=ON ..
cmake --build .

ctest --test-dir algo/tests
```

To build the examples:
```
cmake -DCMAKE_TOOLCHAIN_FILE=<vcpkg_root/scripts/buildsystems/vcpkg.cmake \
      -DALGORITHMS_EXAMPLES=ON ..
cmake --build .
```

### TODO: 
- Other build options
- using custom toolchain/c++ std library


