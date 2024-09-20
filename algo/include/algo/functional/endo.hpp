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

#include <functional>
#include <unifex/tag_invoke.hpp>
#include <utility>

namespace algo
{
inline constexpr struct compose_fn
{
    template <class F, class G>
    requires(unifex::tag_invocable<compose_fn, F, G>)
    static constexpr auto operator()(F&& f, G&& g)
        noexcept(unifex::is_nothrow_tag_invocable_v<compose_fn, F, G>)
            -> unifex::tag_invoke_result_t<compose_fn, F, G>
    {
        return unifex::tag_invoke(compose_fn{}, std::forward<F>(f), std::forward<G>(g));
    }
} compose;

template <class A>
struct Endo
{
    std::vector<std::function<A(A)>> funcs;

    template <class Fa_a>
    requires(std::invocable<Fa_a, A> and
             not std::same_as<std::remove_cvref_t<Fa_a>, Endo>)
    constexpr Endo& operator*=(Fa_a&& fn)
    {
        funcs.emplace_back(std::forward<Fa_a>(fn));
        return *this;
    }

    template <class Fa_a>
    requires(std::invocable<Fa_a, A> and
             not std::same_as<std::remove_cvref_t<Fa_a>, Endo>)
    constexpr Endo operator*(Fa_a&& fn) const
    {
        Endo result{*this};
        result *= std::forward<Fa_a>(fn);
        return result;
    }

    template <class Ea>
    requires(std::same_as<std::remove_cvref_t<Ea>, Endo>)
    constexpr Endo& operator*=(Ea&& ea)
    {
        funcs.append_range(std::forward_like<Ea>(ea.funcs));
        return *this;
    }

    template <class Ea>
    requires(std::same_as<std::remove_cvref_t<Ea>, Endo>)
    constexpr Endo operator*(Ea&& ea) const
    {
        Endo result{*this};
        result *= std::forward<Ea>(ea);
        return result;
    }

    template <class A_>
    constexpr A operator()(A_&& a) const
    {
        A result{std::forward<A_>(a)};
        for (auto const& f : funcs) {
            if constexpr (std::movable<A>) {
                result = f(std::move(result));
            }
            else {
                result = f(result);
            }
        }
        return result;
    }
};

} // namespace algo
