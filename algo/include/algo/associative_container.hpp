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

#include "container.hpp"

namespace algo
{
// associative containers
struct values_fn;
struct keys_fn;
struct value_relation_fn;
struct value_projection_fn;
struct key_relation_fn;
struct key_projection_fn;

// unordered associative containers
struct hash_function_fn;
struct key_predicate_fn;

namespace _traits
{
template <class Mapping>
requires(std::invocable<values_fn, Mapping>)
struct value_type<Mapping>
    : std::type_identity<value_t<std::invoke_result_t<values_fn, Mapping>>>
{};
} // namespace _traits

template <class M>
using key_t = value_t<std::invoke_result_t<keys_fn, M>>;

namespace _concepts
{
template <class C>
concept supports_associative_insertion =
    std::invocable<insert_fn, C, const_lvalue_t<key_t<C>>, const_lvalue_t<value_t<C>>> or
    std::invocable<insert_fn, C, rvalue_t<key_t<C>>, const_lvalue_t<value_t<C>>> or
    std::invocable<insert_fn, C, const_lvalue_t<key_t<C>>, rvalue_t<value_t<C>>> or
    std::invocable<insert_fn, C, rvalue_t<key_t<C>>, rvalue_t<value_t<C>>>;

template <class M>
concept supports_associative_removal =
    std::invocable<remove_fn, M, const_lvalue_t<key_t<M>>> or
    std::invocable<remove_fn, M, rvalue_t<key_t<M>>>;

} // namespace _concepts
template <class M>
concept associative_container =
    _concepts::container<M> and _concepts::supports_associative_insertion<M> and
    _concepts::supports_associative_removal<M> and std::invocable<keys_fn, M> and
    std::invocable<values_fn, M>;

template <class M>
concept unordered_associate_container =
    _concepts::container<M> and _concepts::supports_associative_insertion<M> and
    _concepts::supports_associative_removal<M> and std::invocable<keys_fn, M> and
    std::invocable<values_fn, M>;

} // namespace algo
