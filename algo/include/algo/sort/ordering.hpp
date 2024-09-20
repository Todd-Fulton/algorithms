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
#include <unifex/tag_invoke.hpp>

namespace algo::ordering
{
static constexpr struct ascending_fn
{
    template <class LHS, class RHS>
    requires unifex::tag_invocable<ascending_fn, LHS, RHS>
    constexpr bool operator()(LHS&& lhs, RHS&& rhs) const
        noexcept(unifex::is_nothrow_tag_invocable_v<ascending_fn, LHS, RHS>)
    {
        return tag_invoke(*this, std::forward<LHS>(lhs), std::forward<RHS>(rhs));
    }

    template <class LHS, class RHS>
    requires(not unifex::tag_invocable<ascending_fn, LHS, RHS> and
             requires(LHS lhs, RHS rhs) {
                 { lhs < rhs } -> std::convertible_to<bool>;
             })
    constexpr bool operator()(LHS&& lhs, RHS&& rhs) const
        noexcept(noexcept(std::forward<LHS>(lhs) < std::forward<RHS>(rhs)))
    {
        return std::forward<LHS>(lhs) < std::forward<RHS>(rhs);
    }

} ascending;

static inline constexpr struct descending_fn
{
    template <class LHS, class RHS>
    requires unifex::tag_invocable<descending_fn, LHS, RHS>
    constexpr bool operator()(LHS&& lhs, RHS&& rhs) const
        noexcept(unifex::is_nothrow_tag_invocable_v<descending_fn, LHS, RHS>)
    {
        return tag_invoke(descending_fn{}, std::forward<LHS>(lhs), std::forward<RHS>(rhs));
    }

    template <class LHS, class RHS>
    requires(not unifex::tag_invocable<ascending_fn, LHS, RHS> and
             requires(LHS lhs, RHS rhs) {
                 { lhs > rhs } -> std::convertible_to<bool>;
             })
    constexpr bool operator()(LHS&& lhs, RHS&& rhs) const
        noexcept(noexcept(std::forward<LHS>(lhs) > std::forward<RHS>(rhs)))
    {
        return std::forward<LHS>(lhs) > std::forward<RHS>(rhs);
    }
} descending;

} // namespace algo::ordering
