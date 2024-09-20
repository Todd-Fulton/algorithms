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

#include <algo/sequence.hpp>

#include <concepts>
#include <utility>

namespace algo
{

template <class M, class BinOp, class EmptyOp>
concept Monoidal =
    std::invocable<BinOp, M, M> and std::same_as<std::invoke_result_t<BinOp, M, M>, M> and
    std::same_as<std::remove_cvref_t<M>, std::invoke_result_t<EmptyOp>>;

template <class BinOp, class EmptyOp, Monoidal<BinOp, EmptyOp> M>
struct Monoid
{
    using binary_op_type = std::remove_cvref_t<BinOp>;
    using empty_op_type = std::remove_cvref_t<EmptyOp>;

    using type = std::remove_cvref_t<M>;

    [[no_unique_address]] binary_op_type op_;
    [[no_unique_address]] empty_op_type emp_;

    template <std::constructible_from<binary_op_type> BFn,
              std::constructible_from<empty_op_type> EFn>
    constexpr explicit Monoid(BFn&& binop, EFn&& empop)
        noexcept(std::is_nothrow_constructible_v<binary_op_type, BFn> and
                 std::is_nothrow_constructible_v<empty_op_type, EFn>)
        : op_{std::forward<BFn>(binop)}
        , emp_{std::forward<EFn>(empop)}
    {
    }

    template <class A, class B>
    constexpr auto append(this auto&& self, A&& a, B&& b) noexcept(
        std::is_nothrow_invocable_v<binary_op_type, A, B> and
        std::is_nothrow_constructible_v<type, std::invoke_result_t<binary_op_type, A, B>>)
        -> type
    requires(std::same_as<type, std::invoke_result_t<binary_op_type, A, B>>)
    {
        return std::invoke(std::forward_like<decltype(self)>(self.op_),
                           std::forward<A>(a),
                           std::forward<B>(b));
    }

    template <class A, class B>
    constexpr auto append(this auto&& self, A&& a, B&& b)
        noexcept(std::is_nothrow_invocable_v<binary_op_type, type&, B> and
                 std::is_nothrow_constructible_v<type, A>) -> type
    requires(std::same_as<type&, std::invoke_result_t<binary_op_type, type&, B>> or
             std::same_as<void, std::invoke_result_t<binary_op_type, type&, B>>)
    {
        type tmp{std::forward<A>(a)};
        std::invoke(std::forward_like<decltype(self)>(self.op_), tmp, std::forward<B>(b));
        return tmp;
    }

    template <class A, class B>
    constexpr void inplace_append(this auto&& self, A& a, B&& b) noexcept(
        std::is_nothrow_invocable_v<binary_op_type, A const&, B> and
        std::is_nothrow_assignable_v<type,
                                     std::invoke_result_t<binary_op_type, A const&, B>>)
    requires(std::is_assignable_v<type, type> and
             std::same_as<type, std::invoke_result_t<binary_op_type, A const&, B>>)
    {
        a = std::invoke(
            std::forward_like<decltype(self)>(self.op_), a, std::forward<B>(b));
    }

    template <class A, class B>
    constexpr void inplace_append(this auto&& self, A& a, B&& b)
        noexcept(std::is_nothrow_invocable_v<binary_op_type, A&, B> and
                 std::is_nothrow_constructible_v<type, A>)
    requires(std::same_as<type&, std::invoke_result_t<binary_op_type, A&, B>> or
             std::same_as<void, std::invoke_result_t<binary_op_type, A&, B>>)
    {
        std::invoke(std::forward_like<decltype(self)>(self.op_), a, std::forward<B>(b));
    }

    constexpr M identity(this auto&& self)
    {
        return std::invoke(std::forward_like<decltype(self)>(self.emp_));
    }

    template <sequence Seq>
    constexpr auto concat(this auto&& self, Seq&& seq) -> value_t<Seq>
    {
        auto result = self.identity();
        for (auto cur = algo::first(seq); cur != terminal(seq); inc(seq, cur)) {
            self.inplace_append(result,
                                std::forward_like<decltype(seq)>(read_at(seq, cur)));
        }
        return result;
    }
};

template <class T>
inline constexpr bool is_monoid_v = false;

template <class BinOp, class EmptyOp, class M>
inline constexpr bool is_monoid_v<Monoid<BinOp, EmptyOp, M>> = true;

template <class T>
requires(std::is_reference_v<T>)
inline constexpr bool is_monoid_v<T> = is_monoid_v<std::remove_cvref_t<T>>;

template <class T>
requires(std::is_const_v<T>)
inline constexpr bool is_monoid_v<T> = is_monoid_v<std::remove_cvref_t<T>>;

template <class T>
concept MonoidClass = is_monoid_v<T>;

template <MonoidClass M>
using monoid_type = std::remove_cvref_t<M>;

template <MonoidClass M>
using monoid_binary_op_t = monoid_type<M>::binary_op_type;

template <MonoidClass M>
using monoid_type_t = typename monoid_type<M>::type;

template <class M, class T>
concept MonoidFor =
    MonoidClass<M> and std::same_as<monoid_type_t<M>, std::remove_cvref_t<T>>;

} // namespace algo
