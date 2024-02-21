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

#pragma once

#include <concepts>
#include <functional>
namespace algo
{

struct ordering
{
    struct descending
    {};
    struct ascending
    {};
};

/**
 * @brief  `predicate_for` is a struct that contains a function object for comparing two
 * elements of the same type, depending on the order of the elements. The function object
 * can be used to compare elements in both ascending and descending orders.
 *
 * @tparam Ordering Typically `algo::ordering`, can also be std::less<>, std::greater<>,
 * etc or a custom function object.
 * @tparam T the type the predicate operates on
 */
template <class Ordering, class T = void>
struct predicate_for;

template <class Ordering>
requires(!(std::same_as<Ordering, ordering::ascending> or
           std::same_as<Ordering, ordering::descending>))
struct predicate_for<Ordering, void>
{
    using type = Ordering;
};

template <template <class> class Cmp, class T>
requires(std::invocable<Cmp<T>, T>)
struct predicate_for<Cmp<T>, T>
{
    using type = Cmp<T>;
};

/** Rebinds Cmp<void> to Cmp<T> */
template <template <class> class Cmp, class T>
// requires(std::invocable<Cmp<void>, T>)
struct predicate_for<Cmp<void>, T>
{
    using type = Cmp<T>;
};

/** specifically for ordering::ascending */
template <class T>
struct predicate_for<ordering::ascending, T>
{
    using type = std::less<T>;
};

/** specifically for ordering::descending */
template <class T>
struct predicate_for<ordering::descending, T>
{
    using type = std::greater<T>;
};

/** using declaration for pred<Ordering, T>::type */
template <class Ordering, class T = void>
using predicate_for_t = typename predicate_for<std::remove_cvref_t<Ordering>, T>::type;
} // namespace algo
