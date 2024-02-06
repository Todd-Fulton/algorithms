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

#include <range/v3/iterator/concepts.hpp>
#include <range/v3/range_concepts.hpp>

#include <algo/prelude.hpp>

// TODO:
// [ ] Add concept constrains to functions and member functions

namespace algo
{

namespace _lomuto_partition
{

constexpr auto algorithm(auto range,
                         auto&& predicate,
                         auto&& projection = std::identity{})
{
    auto low = ranges::begin(range);

    for (auto& elem : range) {
        if (predicate(projection(elem))) {
            using std::swap;
            swap(*low, elem);
            ++low;
        }
    }

    return std::make_pair(std::move(low), std::move(range));
}

template <class Predicate, class Projection>
struct _adapter
{
    struct type;
};

template <class Predicate, class Projection>
using adapter =
    _adapter<std::decay_t<Predicate>, std::decay_t<Projection>>::type;

namespace _cpo
{

struct _fn
{
    template <ranges::forward_range Range,
              class Predicate,
              class Projection = ranges::identity>
    static constexpr auto operator()(Range&& range,
                                     Predicate&& predicate,
                                     Projection&& projection = {})
    {
        return tag_invoke(
            _fn{}, FWD(range), FWD(predicate), FWD(projection));
    }

    template <class Predicate, class Projection = std::identity>
    requires(!ranges::range<Predicate>)
    static constexpr auto operator()(Predicate&& predicate,
                                     Projection&& projection = {})
    {
        return adapter<Predicate, Projection>{FWD(predicate),
                                              FWD(projection)};
    }

private:
    friend constexpr auto tag_invoke(_fn const& /*f*/,
                                     ranges::forward_range auto&& range,
                                     auto&& predicate,
                                     auto&& projection = std::identity{})
    {
        return algorithm(FWD(range), FWD(predicate), FWD(projection));
    }
};
} // namespace _cpo
} // namespace _lomuto_partition

constexpr auto lomuto_partition = _lomuto_partition::_cpo::_fn{};

namespace _lomuto_partition
{

template <class Predicate, class Projection>
struct _adapter<Predicate, Projection>::type
{
    Predicate pred_;
    Projection proj_;

    template <class Adapter, ranges::forward_range Range>
    requires std::same_as<std::remove_cvref_t<Adapter>, type>
    constexpr friend auto operator|(Range&& range, Adapter&& self)
    {
        return tag_invoke(lomuto_partition,
                          FWD(range),
                          FWD(self).pred_,
                          FWD(self).proj_);
    }
};

} // namespace _lomuto_partition

// Branchless Lomuto partition
// https://orlp.net/blog/branchless-lomuto-partitioning/
namespace _branchless_lomuto_partition
{

constexpr auto algorithm(auto range, auto&& predicate, auto&& projection)
{
    if (ranges::empty(range)) {
        return std::make_pair(ranges::iterator_t<decltype(range)>{},
                              std::move(range));
    }

    auto i = ranges::begin(range);
    auto j = ranges::begin(range);
    auto temp = ranges::iter_move(i);

    const auto last = i + ranges::range_difference_t<decltype(range)>(
                              ranges::size(range) - 1);

    while (j != last) {
        *j = ranges::iter_move(i);
        ++j;
        *i = ranges::iter_move(j);
        i += predicate(projection(*i));
    }

    *j = ranges::iter_move(i);
    *i = std::move(temp);
    i += predicate(projection(*i));

    return std::make_pair(i, std::move(range));
}

template <class Predicate, class Projection>
struct _adapter
{
    struct type;
};

template <class Predicate, class Projection>
using adapter =
    _adapter<std::decay_t<Predicate>, std::decay_t<Projection>>::type;

namespace _cpo
{

struct _fn
{
    template <ranges::random_access_range Range,
              class Predicate,
              class Projection = ranges::identity>
    static constexpr auto operator()(Range&& range,
                                     Predicate&& predicate,
                                     Projection&& projection = {})
    {
        return tag_invoke(
            _fn{}, FWD(range), FWD(predicate), FWD(projection));
    }

    template <class Predicate, class Projection = std::identity>
    requires(!ranges::range<Predicate>)
    static constexpr auto operator()(Predicate&& predicate,
                                     Projection&& projection = {})
    {
        return adapter<Predicate, Projection>{FWD(predicate),
                                              FWD(projection)};
    }

private:
    friend constexpr auto tag_invoke(
        _fn const& /*f*/,
        ranges::random_access_range auto&& range,
        auto&& predicate,
        auto&& projection = std::identity{})
    {
        return algorithm(FWD(range), FWD(predicate), FWD(projection));
    }
};
} // namespace _cpo
} // namespace _branchless_lomuto_partition

constexpr auto branchless_lomuto_partition =
    _branchless_lomuto_partition::_cpo::_fn{};

namespace _branchless_lomuto_partition
{

template <class Predicate, class Projection>
struct _adapter<Predicate, Projection>::type
{
    Predicate pred_;
    Projection proj_;

    template <class Adapter, ranges::random_access_range Range>
    requires std::same_as<std::remove_cvref_t<Adapter>, type>
    constexpr friend auto operator|(Range&& range, Adapter&& self)
    {
        return tag_invoke(branchless_lomuto_partition,
                          FWD(range),
                          FWD(self).pred_,
                          FWD(self).proj_);
    }
};

} // namespace _branchless_lomuto_partition
} // namespace algo

#include <algo/prologue.hpp>
