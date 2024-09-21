#pragma once

#include <algo/utility.hpp>

namespace algo {
template <class T>
using lvalue_t = std::add_lvalue_reference_t<std::remove_reference_t<T>>;

template <class T>
using rvalue_t = std::add_rvalue_reference_t<std::remove_reference_t<T>>;

template <class T>
using const_lvalue_t =
    std::add_lvalue_reference_t<std::add_const_t<std::remove_reference_t<T>>>;

template <class A>
concept swappable = std::invocable<swap_fn, lvalue_t<A>, lvalue_t<A>>;

}
