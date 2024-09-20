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

#pragma once

#include <range/v3/iterator/concepts.hpp>
#include <range/v3/iterator/operations.hpp>
#include <unifex/tag_invoke.hpp>

namespace algo
{
namespace _find_mid
{

template <class Itr, class Sentinel>
requires ranges::forward_iterator<Itr> and ranges::sentinel_for<Sentinel, Itr>
constexpr auto algorithm(Itr first, Sentinel last) -> Itr
{
    auto mid = first;
    while (first != last and ++first != last) {
        ++first;
        ++mid;
    }
    return mid;
}
} // namespace _find_mid

inline constexpr struct find_mid_fn
{
    template <class Itr, class Sentinel>
    requires unifex::tag_invocable<find_mid_fn, Itr, Sentinel>
    static constexpr auto operator()(Itr&& itr, Sentinel&& sent)
        noexcept(unifex::is_nothrow_tag_invocable_v<find_mid_fn, Itr, Sentinel>)
            -> unifex::tag_invoke_result_t<find_mid_fn, Itr, Sentinel>
    {
        return unifex::tag_invoke(
            find_mid_fn{}, std::forward<Itr>(itr), std::forward<Sentinel>(sent));
    }

    template <class Range>
    requires unifex::tag_invocable<find_mid_fn, Range>
    static constexpr auto operator()(Range&& rng)
        noexcept(unifex::is_nothrow_tag_invocable_v<find_mid_fn, Range>)
            -> unifex::tag_invoke_result_t<find_mid_fn, Range>
    {
        return unifex::tag_invoke(find_mid_fn{}, std::forward<Range>(rng));
    }

} find_mid;

} // namespace algo
