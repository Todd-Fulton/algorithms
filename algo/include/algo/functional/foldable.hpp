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

#include <algo/functional/endo.hpp>
#include <algo/functional/functor.hpp>
#include <algo/functional/monoid.hpp>
#include <algo/sequence.hpp>

#include <functional>
#include <unifex/tag_invoke.hpp>

namespace algo
{
struct foldr_fn;

inline constexpr struct fold_map_fn
{
    // foldMap :: Monoid m => (a -> m) -> t a -> m
    template <MonoidClass M, class Fn, class Fa>
    requires(unifex::tag_invocable<fold_map_fn, M, Fn, Fa> and
             std::invocable<Fn, element_t<Fa>> and
             MonoidFor<M, std::invoke_result_t<Fn, element_t<Fa>>>)
    static constexpr auto operator()(M&& mc, Fn&& fn, Fa&& foldable)
        noexcept(unifex::is_nothrow_tag_invocable_v<fold_map_fn, M, Fn, Fa>)
            -> unifex::tag_invoke_result_t<fold_map_fn, M, Fn, Fa>
    {
        return unifex::tag_invoke(fold_map_fn{},
                                  std::forward<M>(mc),
                                  std::forward<Fn>(fn),
                                  std::forward<Fa>(foldable));
    }

private:
    // TODO: better name
    template <class M, class Fn>
    struct capture
    {
        std::reference_wrapper<std::remove_cvref_t<M>> mc;
        std::reference_wrapper<std::remove_cvref_t<Fn>> fn;
        constexpr auto operator()(auto&& a, auto&& b)
            noexcept(noexcept(mc.append(fn(std::forward<decltype(a)>(a)),
                                        std::forward<decltype(b)>(b))))
        {
            return mc.append(fn(std::forward<decltype(a)>(a)),
                             std::forward<decltype(b)>(b));
        }
    };

public:
    // Implemented in terms of foldr
    // foldMap :: Monoid m => (a -> m) -> t a -> m
    // foldMap f = foldr (mappend . f) mempty
    template <MonoidClass M, class Fn, class Fa>
    requires(not unifex::tag_invocable<fold_map_fn, M, Fn, Fa> and
             std::invocable<foldr_fn, capture<M, Fn>, monoid_type_t<M>, Fa> and
             std::invocable<Fn, element_t<Fa>> and
             MonoidFor<M, std::invoke_result_t<Fn, element_t<Fa>>>)
    static constexpr auto operator()(M&& mc, Fn&& fn, Fa&& foldable) noexcept(
        std::is_nothrow_invocable_v<foldr_fn, capture<M, Fn>, monoid_type_t<M>, Fa>)
        -> std::invoke_result_t<foldr_fn, capture<M, Fn>, monoid_type_t<M>, Fa>;
} fold_map;

inline constexpr struct foldr_fn
{
    // foldr :: (a -> b -> b) -> b -> t a -> b
    template <class Fn, class B, class Fa>
    requires(unifex::tag_invocable<foldr_fn, Fn, B, Fa> and
             std::invocable<Fn, element_t<Fa>, B> and
             std::same_as<
                 std::remove_cvref_t<B>,
                 std::remove_cvref_t<unifex::tag_invoke_result_t<foldr_fn, Fn, B, Fa>>>)
    static constexpr auto operator()(Fn&& fn, B&& init, Fa&& foldable)
        noexcept(unifex::is_nothrow_tag_invocable_v<foldr_fn, Fn, B, Fa>)
            -> unifex::tag_invoke_result_t<foldr_fn, Fn, B, Fa>
    {
        return unifex::tag_invoke(foldr_fn{},
                                  std::forward<Fn>(fn),
                                  std::forward<B>(init),
                                  std::forward<Fa>(foldable));
    }

    // Implemented in terms of foldMap
    // foldr :: (a -> b -> b) -> b -> t a -> b
    template <class Fn, class B, class Fa>
    requires(not unifex::tag_invocable<foldr_fn, Fn, B, Fa> and
             std::invocable<Fn, element_t<Fa>, B> and
             // If a and b are the same, then we can construct a monoid instance from fn
             // and use a more efficient method.
             not std::same_as<std::remove_cvref_t<B>, value_t<Fa>> and
             // If Fa is a bidirectional_sequence then we can simply loop over the
             // elements and use a more efficient method.
             not bidirectional_sequence<Fa> and std::invocable<Fn, element_t<Fa>, B> and
             // All else fails, if we can execute fold_map, then we can construct a
             // monoid from Endo<B> under composure.
             unifex::tag_invocable<fold_map_fn,
                                   Monoid<compose_fn, Endo<std::remove_cvref_t<B>>, Fa>>)
    static constexpr auto operator()(Fn&& fn, B&& init, Fa&& foldable)
        noexcept(unifex::is_nothrow_tag_invocable_v<foldr_fn, Fn, B, Fa>)
            -> unifex::tag_invoke_result_t<foldr_fn, Fn, B, Fa>
    {
        using MonT = Monoid<compose_fn, Endo<std::remove_cvref_t<B>>, Fa>;

        // FIXME: Don't use lambdas in headers
        auto make_monoid = [&](const_element_t<Fa> a) {
            return MonT{
                [&a, &fn](auto&& b) { return fn(a, std::forward<decltype(b)>(b)); }};
        };

        return static_cast<Endo<std::remove_cvref_t<B>>>(fold_map(
            std::move(make_monoid), std::forward<Fa>(foldable)))(std::forward<B>(init));
    }

    template <class Fn, class B, class Fa>
    requires(not unifex::tag_invocable<foldr_fn, Fn, B, Fa> and
             std::invocable<Fn, element_t<Fa>, B> and bidirectional_sequence<Fa>)
    static constexpr auto operator()(Fn&& fn, B&& init, Fa&& foldable)
        noexcept(unifex::is_nothrow_tag_invocable_v<foldr_fn, Fn, B, Fa>)
            -> unifex::tag_invoke_result_t<foldr_fn, Fn, B, Fa>
    {
        auto result = std::forward<B>(init);
        auto cur = algo::terminal(foldable);
        auto const term = first(foldable);
        while (cur != term) {
            algo::dec(foldable, cur);
            if constexpr (std::movable<std::remove_cvref_t<B>> and
                          std::invocable<Fn, const_element_t<Fa>, rvalue_t<B>>) {
                result = std::invoke(fn, algo::read_at(foldable, cur), std::move(result));
            }
            else {
                result = std::invoke(fn, algo::read_at(foldable, cur), result);
            }
        }
        return result;
    }
} foldr;

// Implemented in terms of foldr
// foldMap :: Monoid m => (a -> m) -> t a -> m
// foldMap f = foldr (mappend . f) mempty
template <MonoidClass M, class Fn, class Fa>
requires(not unifex::tag_invocable<fold_map_fn, M, Fn, Fa> and
         std::invocable<foldr_fn, fold_map_fn::capture<M, Fn>, monoid_type_t<M>, Fa> and
         std::invocable<Fn, element_t<Fa>> and
         MonoidFor<M, std::invoke_result_t<Fn, element_t<Fa>>>)
constexpr auto fold_map_fn::operator()(M&& mc, Fn&& fn, Fa&& foldable) noexcept(
    std::is_nothrow_invocable_v<foldr_fn,
                                fold_map_fn::capture<M, Fn>,
                                monoid_type_t<M>,
                                Fa>)
    -> std::invoke_result_t<foldr_fn, fold_map_fn::capture<M, Fn>, monoid_type_t<M>, Fa>
{
    return algo::foldr(
        fold_map_fn::capture{mc, fn}, mc.identity(), std::forward<Fa>(foldable));
}

} // namespace algo
