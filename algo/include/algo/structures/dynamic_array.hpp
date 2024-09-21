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
#include <algorithm>
#include <cassert>
#include <exception>
#include <iterator>
#include <memory>

#include <algo/container.hpp>
#include <algo/utility.hpp>
#include <utility>

namespace algo
{

template <class T, class Allocator = std::allocator<T>>
requires(!std::is_reference_v<T>)
class dynamic_array
{
public:
    using value_type = T;
    using allocator_type = Allocator;

    static constexpr bool trivially_relocatable = true;

    constexpr dynamic_array() = default;
    constexpr dynamic_array(dynamic_array const& arr)
        : alloc_{arr.alloc_}
    {
        using AllocT = std::allocator_traits<allocator_type>;
        assert(arr.begin_ <= arr.end_ and arr.end_ <= arr.cap_end_);
        size_t m;
        if (algo::size(arr) > 0) {
            assert(arr.begin_ != nullptr);
            m = algo::capacity(arr);
            begin_ = AllocT::allocate(alloc_, m);
            end_ = begin_;
            cap_end_ = begin_ + m;
            try {
                if constexpr (std::is_trivially_copyable_v<value_type>) {
                    std::memcpy(begin_, arr.begin_, algo::size(arr));
                }
                else {
                    end_ = std::uninitialized_copy(arr.begin_, arr.end_, end_);
                }
            }
            catch (...) {
                AllocT::deallocate(alloc_, begin_, m);
                begin_ = end_ = cap_end_ = nullptr;
                std::rethrow_exception(std::current_exception());
            }
        }
    }

    constexpr dynamic_array(dynamic_array&& arr) noexcept(algo::swappable<dynamic_array>)
        : dynamic_array()
    {
        algo::swap(*this, arr);
    }

private:
    /**
     * @brief Always allocate new memory for a copy. Handles cases where
     * copy throws.
     *
     * @param other The array to copy.
     * @param new_alloc The allocator to use for the new memory, may be this->alloc_
     * if allocators do not propagate_on_container_copy_assignment.
     */
    constexpr void new_memory_copy_(dynamic_array const& other, allocator_type& new_alloc)
    {
        using AllocT = std::allocator_traits<allocator_type>;
        auto const other_size = algo::size(other);
        auto new_space_ = AllocT::allocate(new_alloc, other_size);
        if constexpr (std::is_nothrow_copy_assignable_v<value_type>) {
            std::uninitialized_copy(other.begin_, other.end_, new_space_);
        }
        else {
            try {
                std::uninitialized_copy(other.begin_, other.end_, new_space_);
            }
            catch (...) {
                AllocT::deallocate(new_alloc, new_space_);
                throw;
            }
        }
        AllocT::deallocate(alloc_, begin_, algo::capacity(*this));
        begin_ = new_space_;
        end_ = std::next(begin_, std::ptrdiff_t(other_size));
        cap_end_ = end_;
    }

    /**
     * @brief An optimization for nothrow copy assignable types.
     * Possibly vvoids an allocation if this array has a large enough capacity.
     *
     * @param other The array to copy
     * @param new_alloc The allocator to use for new memory (if needed).
     */
    constexpr void copy_(dynamic_array const& other, allocator_type& new_alloc)
    requires(std::is_nothrow_copy_assignable_v<value_type>)
    {
        auto const other_size = algo::size(other);
        if (algo::capacity(*this) >= other_size) {
            auto const cnt = std::min(algo::size(*this), other_size);
            auto new_end_ = std::copy_n(other.begin_, cnt, begin_);
            if (algo::size(*this) >= other_size) {
                for (auto i = new_end_; i < end_; std::advance(i, 1)) {
                    std::destroy_at(i);
                }
                end_ = new_end_;
            }
            else {
                end_ = std::uninitialized_copy_n(
                    other.begin_ + std::ptrdiff_t(cnt), other_size - cnt, new_end_);
            }
        }
        else {
            new_memory_copy_(other, new_alloc);
        }
    }

public:
    constexpr dynamic_array& operator=(dynamic_array const& other)
    requires(not std::allocator_traits<
             allocator_type>::propagate_on_container_copy_assignment::value)
    {
        if (this != &other) {
            if constexpr (std::is_nothrow_copy_assignable_v<value_type>) {
                copy_(other, alloc_);
            }
            else {
                new_memory_copy_(other, alloc_);
            }
        }
        return *this;
    }

    constexpr dynamic_array& operator=(dynamic_array const& other)
    requires(std::allocator_traits<
             allocator_type>::propagate_on_container_copy_assignment::value)
    {

        if (this != &other) {
            if constexpr (std::is_nothrow_copy_assignable_v<value_type>) {
                if (alloc_ != other.alloc_) {
                    new_memory_copy_(other, other.alloc_);
                }
                else {
                    // same allocator, so we may be able to avoid allocation
                    copy_(other, other.alloc_);
                }
            }
            else {
                // value_type is not nothrow copy assignable, so we can't avoid allocation
                new_memory_copy_(other, other.alloc_);
            }
            alloc_ = other.alloc_;
        }
        return *this;
    }

    constexpr ~dynamic_array()
    {
        using AllocT = std::allocator_traits<allocator_type>;
        for (auto itr = this->begin_; itr != this->end_; std::advance(itr, 1)) {
            AllocT::destroy(alloc_, itr);
        }
        AllocT::deallocate(alloc_, begin_, usize(*this));
        begin_ = end_ = cap_end_ = nullptr;
    }

private:
    value_type* begin_{nullptr};
    value_type* end_{nullptr};
    value_type* cap_end_{nullptr};

    [[no_unique_address]] allocator_type alloc_;

    using cursor_type = value_type*;

    constexpr void bounds_check([[maybe_unused]] index_t const& cur) const noexcept
    {
        assert(cur >= 0);
        assert(cur < algo::terminal(*this));
    }

    constexpr friend void tag_invoke([[maybe_unused]] swap_fn _,
                                     dynamic_array& left,
                                     dynamic_array& right)
        noexcept(std::is_nothrow_swappable_v<value_type*> and
                 std::is_nothrow_swappable_v<distance_t> and
                 std::is_nothrow_swappable_v<allocator_type>)
    requires(std::is_nothrow_swappable_v<allocator_type>)
    {
        algo::swap(left.begin_, right.begin_);
        algo::swap(left.end_, right.end_);
        algo::swap(left.alloc_, right.alloc_);
    }

    constexpr friend auto tag_invoke([[maybe_unused]] first_fn tag,
                                     [[maybe_unused]] dynamic_array const& arr) noexcept
        -> index_t
    {
        return 0;
    }

    constexpr friend auto tag_invoke([[maybe_unused]] terminal_fn tag,
                                     dynamic_array const& arr) noexcept -> index_t
    {
        return arr.end_ - arr.begin_;
    }

    constexpr friend auto tag_invoke([[maybe_unused]] inc_fn tag,
                                     dynamic_array const& arr,
                                     lvalue_t<index_t> cur) noexcept -> lvalue_t<index_t>
    {
        return algo::inc(arr, cur, 1);
    }

    constexpr friend auto tag_invoke([[maybe_unused]] inc_fn tag,
                                     [[maybe_unused]] dynamic_array const& arr,
                                     lvalue_t<index_t> cur,
                                     distance_t n) noexcept -> lvalue_t<index_t>
    {
        cur += n;
        assert(cur <= algo::size(arr));
        assert(cur >= 0);
        return cur;
    }

    constexpr friend auto tag_invoke([[maybe_unused]] dec_fn tag,
                                     dynamic_array const& arr,
                                     lvalue_t<index_t> cur) noexcept -> lvalue_t<index_t>
    {
        return algo::dec(arr, cur, 1);
    }

    constexpr friend auto tag_invoke([[maybe_unused]] dec_fn tag,
                                     [[maybe_unused]] dynamic_array const& arr,
                                     lvalue_t<index_t> cur,
                                     distance_t n) noexcept -> lvalue_t<index_t>
    {
        cur -= n;
        assert(cur >= 0);
        return cur;
    }

    constexpr friend auto tag_invoke([[maybe_unused]] is_last_fn tag,
                                     dynamic_array const& arr,
                                     const_lvalue_t<index_t> cur) noexcept -> bool
    {
        return arr.end_ == cur;
    }

    constexpr friend auto tag_invoke([[maybe_unused]] distance_fn tag,
                                     [[maybe_unused]] dynamic_array const& arr,
                                     index_t a,
                                     index_t b) noexcept -> distance_t
    {
        // only compare cursors within the bounds of the internal buffer
        assert(a >= 0);
        assert(a <= algo::capacity(arr));
        assert(b >= 0);
        assert(b <= algo::capacity(arr));
        return b - a;
    }

    constexpr friend auto tag_invoke([[maybe_unused]] size_fn tag,
                                     dynamic_array const& arr) noexcept -> distance_t
    {
        assert(arr.end_ >= arr.begin_);
        return arr.end_ - arr.begin_;
    }

    constexpr friend auto tag_invoke([[maybe_unused]] usize_fn tag,
                                     dynamic_array const& arr) noexcept -> size_t
    {
        assert(arr.end_ >= arr.begin_);
        return size_t(arr.end_ - arr.begin_);
    }

    constexpr friend auto tag_invoke([[maybe_unused]] capacity_fn tag,
                                     dynamic_array const& arr) noexcept -> distance_t
    {
        assert(arr.cap_end_ >= arr.begin_);
        return arr.cap_end_ - arr.begin_;
    }

    constexpr friend auto tag_invoke([[maybe_unused]] allocator_fn tag,
                                     dynamic_array const& arr) noexcept -> allocator_type
    {
        return arr.alloc_;
    }

    constexpr friend auto tag_invoke([[maybe_unused]] empty_fn tag,
                                     dynamic_array const& arr) noexcept -> bool
    {
        assert(arr.cap_end_ >= arr.begin_);
        return arr.begin_ == arr.end_;
    }

    constexpr friend void tag_invoke([[maybe_unused]] reserve_fn tag,
                                     dynamic_array& arr,
                                     size_t n)
    {
        const auto old_cap = size_t(capacity(arr));
        if (n > old_cap) {
            const auto sz = size(arr);
            n = std::max(size_t(old_cap + (old_cap / 2)), size_t(old_cap + n));
            T* buff = std::allocator_traits<allocator_type>::allocate(arr.alloc_, n);
            relocate_at_n(arr.begin_, buff, size_t(sz));
            std::swap(arr.begin_, buff);
            arr.end_ = arr.begin_ + sz;
            arr.cap_end_ = arr.begin_ + distance_t(n);
            if (buff != nullptr) {
                std::allocator_traits<allocator_type>::deallocate(
                    arr.alloc_, buff, old_cap);
            }
        }
    }

    template <class... Args>
    requires(std::constructible_from<T, Args...> and sizeof...(Args) > 0)
    constexpr friend auto tag_invoke([[maybe_unused]] insert_fn tag,
                                     dynamic_array& arr,
                                     index_t idx,
                                     size_t cnt,
                                     Args&&... args) -> index_t
    {

        auto cap = capacity(arr);
        auto const sz = size(arr);
        T* dest = arr.begin_;

        assert(idx >= 0);
        assert(idx <= sz);
        [[assume(idx >= 0 and idx <= sz)]];

        if (cap == sz) {
            cap = std::max(distance_t(8), cap + std::max(cap / 2, distance_t(cnt)));
            dest =
                std::allocator_traits<allocator_type>::allocate(arr.alloc_, size_t(cap));
        }
        if constexpr (nothrow_relocatable<T>) {
            relocate_at_n(arr.begin_, dest, size_t(idx));
            relocate_at_n(
                arr.begin_ + idx, dest + idx + distance_t(cnt), size_t(sz - idx));
        }
        else {
            try {
                relocate_at_n(arr.begin_, dest, size_t(idx));
                relocate_at_n(
                    arr.begin_ + idx, dest + idx + distance_t(cnt), size_t(sz - idx));
            }
            catch (...) {
                if (arr.begin_ != dest) {
                    std::allocator_traits<allocator_type>::deallocate(
                        arr.alloc_, dest, size_t(cap));
                }
                std::rethrow_exception(std::current_exception());
            }
        }
        if (arr.begin_ != dest) {
            std::allocator_traits<allocator_type>::deallocate(
                arr.alloc_, arr.begin_, size_t(capacity(arr)));
        }
        arr.begin_ = dest;
        arr.end_ = arr.begin_ + sz + distance_t(cnt);
        arr.cap_end_ = arr.begin_ + cap;
        for (size_t i = 0; i < cnt; ++i) {
            std::allocator_traits<allocator_type>::construct(
                arr.alloc_, arr.begin_ + idx + i, std::forward<Args>(args)...);
        }
        return idx;
    }

    template <class... Args>
    requires(std::constructible_from<T, Args...> and sizeof...(Args) > 0)
    constexpr friend auto tag_invoke([[maybe_unused]] insert_fn tag,
                                     dynamic_array& arr,
                                     index_t idx,
                                     Args&&... args) -> index_t
    {
        return algo::insert(arr, idx, size_t(1), std::forward<Args>(args)...);
    }

    template <sized_sequence Seq>
    requires(std::constructible_from<T, element_t<Seq>>)
    static constexpr auto tag_invoke([[maybe_unused]] insert_fn tag,
                                     dynamic_array& arr,
                                     index_t idx,
                                     Seq&& seq) -> index_t
    {
        auto cap = capacity(arr);
        auto sz = size(arr);
        auto cnt = size(seq);
        T* dest = arr.begin_;

        assert(idx >= 0);
        assert(idx <= sz);
        [[assume(idx >= 0 and idx <= sz)]];

        if (cap - sz < cnt) {
            cap = std::max(distance_t(8), cap + std::max(cap / 2, distance_t(cnt)));
            dest =
                std::allocator_traits<allocator_type>::allocate(arr.alloc_, size_t(cap));
        }
        if constexpr (nothrow_relocatable<T>) {
            relocate_at_n(arr.begin_, dest, size_t(idx));
            relocate_at_n(arr.begin_ + idx, dest + idx + cnt, size_t(sz - idx));
        }
        else {
            try {
                relocate_at_n(arr.begin_, dest, size_t(idx));
                relocate_at_n(arr.begin_ + idx, dest + idx + cnt, size_t(sz - idx));
            }
            catch (...) {
                if (arr.begin_ != dest) {
                    std::allocator_traits<allocator_type>::deallocate(
                        arr.alloc_, dest, size_t(cap));
                }
                std::rethrow_exception(std::current_exception());
            }
        }
        if (arr.begin_ != dest) {
            std::allocator_traits<allocator_type>::deallocate(
                arr.alloc_, arr.begin_, capacity(arr));
        }
        arr.begin_ = dest;
        arr.end_ = arr.begin_ + sz + cnt;
        arr.cap_end_ = arr.begin_ + cap;
        for (auto c1 = idx, c2 = first(seq); c2 != cnt; inc(seq, c2), inc(arr, c1)) {
            std::allocator_traits<allocator_type>::construct(
                arr.alloc_, arr.begin_ + c1, std::forward_like<Seq>(read_at(seq, c2)));
        }
        return idx;
    }

    friend constexpr auto tag_invoke([[maybe_unused]] remove_fn tag,
                                     dynamic_array& arr,
                                     index_t fst,
                                     index_t lst) -> index_t
    {

        assert(fst <= lst);
        assert(fst >= 0);
        [[assume(fst <= lst)]];
        [[assume(fst >= 0)]];
        const auto term = terminal(arr);
        assert(lst <= term);
        [[assume(lst <= term)]];

        const auto cnt = distance(arr, fst, lst);
        const auto rem = distance(arr, lst, term);
        if (cnt > 0 or fst != term) {
            if constexpr (std::is_trivially_destructible_v<T> or
                          is_trivially_relocatable_v<T>) {
                __builtin_memset(
                    static_cast<char*>(arr.begin_ + fst), 0, sizeof(T) * cnt);
                relocate_at_n(arr.begin_ + lst, arr.begin_ + fst, rem);
            }
            else {
                T* src = arr.begin_ + lst;
                T* dst = arr.begin_ + fst;
                if (cnt < rem) {
                    for (; dst < lst; std::advance(dst, 1), std::advance(src, 1)) {
                        std::allocator_traits<allocator_type>::destroy(arr.alloc_, dst);
                        relocate_at(src, dst);
                    }
                    relocate_at_n(src, dst, rem - cnt);
                }
                else {
                    for (T const* const lim = dst + rem; dst < lim;
                         std::advance(dst, 1), std::advance(src, 1)) {
                        std::allocator_traits<allocator_type>::destroy(arr.alloc_, dst);
                        relocate_at(src, dst);
                    }
                    for (T const* const lim = arr.end_; dst < lim; std::advance(src, 1)) {
                        std::allocator_traits<allocator_type>::destroy(arr.alloc_, src);
                    }
                }
            }
            arr.end_ -= cnt;
        }
        return std::min(terminal(arr), lst);
    }

    friend constexpr auto tag_invoke([[maybe_unused]] remove_fn tag,
                                     dynamic_array& arr,
                                     index_t pos) -> index_t
    {
        if (pos != algo::terminal(arr)) {
            return algo::remove(arr, pos, algo::next(arr, pos));
        }
        return pos;
    }

    template <class Arr>
    requires(std::same_as<std::remove_cvref_t<Arr>, dynamic_array>)
    friend constexpr auto tag_invoke([[maybe_unused]] read_at_fn tag,
                                     Arr&& arr,
                                     index_t cur) -> decltype(arr.begin_[cur])
    {
        arr.bounds_check(cur);
        return arr.begin_[cur];
    }
};

} // namespace algo
