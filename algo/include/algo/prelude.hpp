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

#include <utility>

#define FWD(x) std::forward<decltype(x)>(x)
#define RNG_VALUE_T(range) ranges::range_value_t<decltype(range)>
#define RNG_ITR_T(range) ranges::iterator_t<decltype(range)>
#define ITR_VALUE_T(itr) std::iter_value_t<decltype(itr)>
#define REMOVE_CVREF_T(var) std::remove_cvref_t<decltype(var)>


#if defined(__clang__)
#define ASSUME(expr) __builtin_assume(expr)
#elif defined(__GNUC__) && !defined(__ICC)
#define ASSUME(expr)                                                                \
    if (expr) {}                                                                    \
    else {                                                                          \
        __builtin_unreachable();                                                    \
    }
#elif defined(_MSC_VER) || defined(__ICC)
#define ASSUME(expr) __assume(expr)
#endif

// Only one or the other, but not both
#if (defined(DEBUG) || defined(_DEBUG)) && (defined(NDEBUG) || defined(_NDEBUG))
#error Both DEBUG and NDEBUG are defined.
#endif

// The only time we switch to debug is when asked.
// NDEBUG or {nothing} results
// in release build (fewer surprises at runtime).
#if defined(DEBUG) || defined(_DEBUG)
#define ALGO_BUILD_DEBUG 1
#else
#define ALGO_BUILD_RELEASE 1
#endif

