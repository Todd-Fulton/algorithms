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


#include <range/v3/algorithm/find_if.hpp>
#include <range/v3/algorithm/find_if_not.hpp>
#include <range/v3/algorithm/reverse.hpp>
#include <range/v3/iterator/concepts.hpp>
#include <range/v3/range_concepts.hpp>

#include <cassert>

#include <algo/prelude.hpp>

namespace algo
{
namespace _hoare_partition
{
constexpr auto
algorithm(auto range, auto&& predicate, auto&& projection) noexcept(
    std::is_nothrow_move_assignable_v<RNG_VALUE_T(range)> and
    std::is_nothrow_invocable_v<decltype(projection),
                                RNG_VALUE_T(range)> and
    std::is_nothrow_invocable_v<
        decltype(predicate),
        std::invoke_result_t<decltype(projection), RNG_VALUE_T(range)>>)
    -> std::pair<RNG_ITR_T(range), std::remove_cvref_t<decltype(range)>>
{
    auto low = begin(range);
    auto high = prev(end(range));

    while (true) {
        while (predicate(projection(*low)) and low < high) {
            ++low;
        }
        while (!predicate(projection(*high)) and low < high) {
            --high;
        }
        if (low < high) {
            ranges::iter_swap(low, high);
            ++low;
            --high;
        }
        else {
            if (predicate(projection(*low))) {
                ++low;
            }
            ranges::iter_swap(low, prev(end(range)));
            return std::make_pair(std::move(low), std::move(range));
        }
    }
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
        return algorithm(FWD(range), FWD(predicate), FWD(projection));
    }

    template <class Predicate, class Projection = std::identity>
    static constexpr auto operator()(Predicate&& predicate,
                                     Projection&& projection = {})
    {
        return adapter<Predicate, Projection>{FWD(predicate),
                                              FWD(projection)};
    }

private:
    friend constexpr auto tag_invoke(
        _fn const& f,
        ranges::random_access_range auto&& range,
        auto&& predicate,
        auto&& projection = std::identity{})
    {
        return f(FWD(range), FWD(predicate), FWD(projection));
    }
};
} // namespace _cpo
} // namespace _hoare_partition

constexpr auto hoare_partition = _hoare_partition::_cpo::_fn{};

namespace _hoare_partition
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
        return tag_invoke(
            hoare_partition, FWD(range), FWD(self).pred_, FWD(self).proj_);
    }
};
} // namespace _hoare_partition

namespace _alexandrescu_partition
{

// TODO: Can be used with bidirectional_iterator
constexpr auto
algorithm(auto range, auto&& predicate, auto&& projection) noexcept(
    std::is_nothrow_move_assignable_v<RNG_VALUE_T(range)> and
    std::is_nothrow_invocable_v<decltype(projection),
                                RNG_VALUE_T(range)> and
    std::is_nothrow_invocable_v<
        decltype(predicate),
        std::invoke_result_t<decltype(projection), RNG_VALUE_T(range)>>)
    -> std::pair<RNG_ITR_T(range), std::remove_cvref_t<decltype(range)>>
{
    auto predecessor = ranges::find_if_not(range, predicate, projection);
    auto successor = prev(end(range));
    if (predecessor < successor) {

        auto original_succ = *predecessor; // copy

        // ensure while loops terminate with predicate
        ranges::iter_swap(predecessor, successor);

        while (true) {

            // find next successor value
            do { // NOLINT *do*
                ++predecessor;
            } while (predicate(projection(*predecessor)));

            // overwrite the old terminal node with new successor value
            *successor = ranges::iter_move(predecessor);

            // find next predecessor value
            // NOTE: this may evaluate a moved-from value located at
            // `predecessor`
            do { // NOLINT *do*
                --successor;
            } while (!predicate(projection(*successor)));

            // NOTE: we may have moved passed the moved from value
            if (predecessor >= successor) {
                break;
            }
            // overwrite the old reverse terminal node with new predecessor
            // value
            *predecessor = ranges::iter_move(successor);
        }
        assert(successor >= ranges::begin(range));
        assert(ranges::distance(predecessor, successor) < 2);
        assert(predicate(projection(*successor)));
        if (predecessor == successor + 2) {
            assert(!predicate(projection(*(successor + 1))));
            *predecessor = ranges::iter_move(successor + 1);
            --predecessor;
        }

        *predecessor = std::move(original_succ);
    }
    return std::make_pair(std::move(predecessor, std::move(range)));
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
        return algorithm(FWD(range), FWD(predicate), FWD(projection));
    }

    template <class Predicate, class Projection = std::identity>
    static constexpr auto operator()(Predicate&& predicate,
                                     Projection&& projection = {})
    {
        return adapter<Predicate, Projection>{FWD(predicate),
                                              FWD(projection)};
    }

private:
    friend constexpr auto tag_invoke(
        _fn const& f,
        ranges::random_access_range auto&& range,
        auto&& predicate,
        auto&& projection = std::identity{})
    {
        return f(FWD(range), FWD(predicate), FWD(projection));
    }
};
} // namespace _cpo
} // namespace _alexandrescu_partition

constexpr auto alexandrescu_partition =
    _alexandrescu_partition::_cpo::_fn{};

namespace _alexandrescu_partition
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
        return tag_invoke(
            hoare_partition, FWD(range), FWD(self).pred_, FWD(self).proj_);
    }
};
} // namespace _alexandrescu_partition

} // namespace algo

#include <algo/prologue.hpp>

