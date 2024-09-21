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


#include <unifex/tag_invoke.hpp>

#include <algorithm>
#include <concepts>
#include <memory>

namespace algo
{

template <class T>
concept relocatable = std::movable<T> and std::destructible<T>;

template <class T>
constexpr inline bool is_relocatable_v = relocatable<T>;

template <class T>
struct is_relocatable : std::bool_constant<relocatable<T>>
{};

template <class T>
constexpr inline bool is_trivially_relocatable_v =
    std::is_trivially_move_assignable_v<T> and
    std::is_trivially_move_constructible_v<T> and std::is_trivially_destructible_v<T>;

template <class T>
requires requires {
    { T::trivially_relocatable } -> std::convertible_to<bool>;
}
constexpr inline bool is_trivially_relocatable_v<T> = T::trivially_relocatable;

template <class T>
struct is_trivially_relocatable : std::bool_constant<is_trivially_relocatable_v<T>>
{};

template <class T>
concept trivially_relocatable = is_trivially_relocatable_v<T>;

template <class T>
concept nothrow_relocatable =
    trivially_relocatable<T> or
    (std::is_nothrow_constructible_v<T> and std::is_nothrow_move_assignable_v<T> and
     std::is_nothrow_destructible_v<T>);

template <class T>
constexpr inline bool is_nothrow_relocatable_v = nothrow_relocatable<T>;

template <class T>
struct is_nothrow_relocatable : std::bool_constant<nothrow_relocatable<T>>
{};

template <class T>
requires(relocatable<T> and not trivially_relocatable<T>)
constexpr T* relocate_at(T* source, T* dest) noexcept(nothrow_relocatable<T>)
{
    struct guard
    {
        T* t;
        ~guard()
        {
            std::destroy_at(t);
        }
    } g{source};

    std::construct_at(dest, std::move(source));
    return dest;
}

template <class T>
requires(relocatable<T> and not trivially_relocatable<T>)
constexpr T* relocate_at_n(T* source, T* dest, size_t n) noexcept(nothrow_relocatable<T>)
{
    if (source > dest) {
        for (size_t i = n - 1; i != std::numeric_limits<size_t>::max(); --i) {
            relocate_at(source + i, dest + i);
        }
    }
    else {
        for (size_t i = 0; i < n; ++i) {
            relocate_at(source + i, dest + i);
        }
    }
    return dest;
}

template <class T>
requires(trivially_relocatable<T>)
constexpr T* relocate_at_n(T* source, T* dest, size_t n) noexcept
{
    if (source != dest) {
        __builtin_memmove(
            static_cast<void*>(dest), static_cast<void*>(source), sizeof(T) * n);
        if consteval {
            for (char* itr = static_cast<char*>(static_cast<void*>(source));
                 itr != static_cast<char*>(static_cast<void*>(source + n));
                 std::advance(itr, 1)) {
                *itr = 0;
            }
        }
        else {
            __builtin_memset(static_cast<void*>(source), 0, sizeof(T) * n);
        }
    }
    return dest;
}

template <class T>
requires(trivially_relocatable<T>)
constexpr T* relocate_at(T* source, T* dest) noexcept
{
    return relocate_at_n(source, dest, 1);
}

constexpr inline struct swap_fn
{
    template <class A, class B>
    requires unifex::tag_invocable<swap_fn, A&, B&>
    static constexpr void operator()(A& left, B& right)
        noexcept(unifex::is_nothrow_tag_invocable_v<swap_fn, A&, B&>)
    {
        return unifex::tag_invoke(swap_fn{}, left, right);
    }

    template <class A, class B>
    requires(not unifex::tag_invocable<swap_fn, A&, B&> and
             std::is_swappable_with_v<A, B>)
    static constexpr void operator()(A& left, B& right)
        noexcept(std::is_nothrow_swappable_with_v<A, B>)
    {
        using std::swap;
        swap(left, right);
    }
} swap;

} // namespace algo
