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

#include <algo/search.hpp>
#include <algo/sort/ordering.hpp>
#include <algo/sort/sorted.hpp>

#include <range/v3/iterator/concepts.hpp>
#include <range/v3/iterator/operations.hpp>
#include <unifex/tag_invoke.hpp>

#include <random>

namespace algo
{

struct remove_fn;
struct insert_fn;
struct is_sorted_fn;

template <class T,
          class Relation = ordering::ascending_fn,
          class Projection = ranges::identity,
          class Alloc = std::allocator<T>>
requires ranges::strict_weak_order<
    Relation,
    std::invoke_result_t<std::remove_cvref_t<Projection>, T>,
    std::invoke_result_t<std::remove_cvref_t<Projection>, T>>
class SkipList
{
public:
    using value_type = std::remove_reference_t<T>;
    using allocator_type = Alloc;
    using size_type = size_t;
    using difference_type = ptrdiff_t;
    using reference = value_type&;
    using const_reference = value_type const&;
    using pointer = std::allocator_traits<allocator_type>::pointer;
    using const_pointer = std::allocator_traits<allocator_type>::const_pointer;

private:
    class Node
    {
        Node* next_;
        T value_;

    public:
        template <class... Args>
        requires std::constructible_from<T, Args...>
        constexpr Node(uint level, Alloc& alloc, Args&&... args)
        try : next_{std::allocator_traits<Alloc>::allocate(alloc, level)},
            value_{std::forward<Args>(args)...} {
        }
        catch (...) {
            std::allocator_traits<Alloc>::deallocate(alloc, this->next_, level);
        }

        constexpr void deallocate_storage(Alloc& alloc, uint level)
        {
            std::allocator_traits<Alloc>::deallocate(alloc, next_, level);
        }

        constexpr auto&& value(this auto& self) noexcept
        {
            return std::forward_like<decltype(self)>(self.value_);
        }

        constexpr Node const* next(size_t idx) const& noexcept
        {
            return next_[idx]; // NOLINT
        }
    };

    template <class U>
    class Iterator
    {
        friend SkipList;

        Node* current_{nullptr};

    public:
        using difference_type = std::ptrdiff_t;
        using value_type = U;
        using pointer = U*;
        using reference = U&;
        using iterator_category = std::forward_iterator_tag;

        constexpr explicit Iterator(Node* node) noexcept
            : current_{node}
        {
        }
        constexpr Iterator() noexcept = default;
        constexpr ~Iterator() noexcept = default;

        constexpr Iterator(Iterator&& other) noexcept
            : current_{other.current_}
        {
            other.current_ = nullptr;
        }

        constexpr Iterator(Iterator const& other) noexcept = default;

        constexpr Iterator& operator=(Iterator&& other) noexcept
        {
            if (&other != this) {
                current_ = other.current_;
                other.current_ = nullptr;
            }
            return *this;
        }

        constexpr Iterator& operator=(Iterator const& other) noexcept = default;

        constexpr reference operator*() const
        {
            assert(current_ != nullptr);
            return current_->value();
        }

        constexpr pointer operator->() const
        {
            assert(current_ != nullptr);
            return &(current_->value());
        }

        constexpr Iterator operator++() noexcept
        {
            assert(current_ != nullptr);
            current_ = (*current_)[0];
        }

        constexpr Iterator& operator++(int) noexcept
        {
            Iterator tmp{*this};
            ++(*this);
            return tmp;
        }

        constexpr bool operator==(Iterator const& other) noexcept
        {
            return current_ == other.current_;
        }
    };

public:
    using iterator = Iterator<value_type>;
    using const_iterator = Iterator<const value_type>;

    using relation_type = std::remove_cvref_t<Relation>;
    using projection_type = std::remove_cvref_t<Projection>;

    static constexpr bool sorted_container_v = true;

    template <class Relation2, class Projection2>
    requires(std::constructible_from<relation_type, Relation2> and
             std::constructible_from<projection_type, Projection2>)
    constexpr explicit SkipList(Relation2&& rel = {},
                                Projection2&& proj = {},
                                allocator_type const& alloc = {})
        : rel_{std::forward<Relation2>(rel)}
        , proj_(std::forward<Projection2>(proj))
        , alloc_{alloc}
        , head_{nullptr, alloc_, 33}
        , rand_{std::random_device{}()} {};

    constexpr iterator begin() const&
    {
        return {&head_};
    }

    constexpr iterator end() const&
    {
        return {};
    }
    constexpr const_iterator cbegin() const&
    {
        return {&head_};
    }

    constexpr const_iterator cend() const&
    {
        return {};
    }

    [[nodiscard]] constexpr size_type size() const noexcept
    {
        return size_;
    }

private:
    [[no_unique_address]] ranges::semiregular_box_t<Relation> rel_;
    [[no_unique_address]] ranges::semiregular_box_t<Projection> proj_;
    [[no_unique_address]] allocator_type alloc_;
    Node* head_;
    std::mt19937 rand_;
    size_type size_ = 0;
    short levels_ = 1;

    friend constexpr auto tag_invoke([[maybe_unused]] search_fn /*unused*/,
                                     SkipList const& skiplist,
                                     auto const& value) -> const_iterator
    {
        Node* cur = skiplist.head_;
        difference_type i = skiplist.levels_ - 1;
        for (; i >= 0; i--) {
            while (
                (*cur)[i] != nullptr and
                !skiplist.rel_(skiplist.proj_(value), skiplist.proj_((*cur)[i]->key()))) {
                cur = (*cur)[i];
            }
        }
        return {(*cur)[i]};
    }

    friend constexpr void tag_invoke([[maybe_unused]] remove_fn const& /*unused*/,
                                     SkipList& skiplist,
                                     T const& value)
    requires(std::equality_comparable<T>)
    {
        Node* cur = skiplist.head_;
        Node* del{nullptr};
        uint level = 0;
        for (int i = skiplist.levels_ - 1; i >= 0; i--) {
            for (; cur->next(i) != nullptr; cur = cur->next(i)) {
                if (cur->next(i)->value() == value) {
                    del = cur->next(i);
                    level = uint(i);
                    cur->next(i) = cur->next(i)->next(i);
                    break;
                }

                if (cur->next(i)->value() > value) {
                    break;
                }
            }
        }
        if (del != nullptr) {
            del->deallocate_storage(skiplist.alloc_, level);
            std::allocator_traits<Alloc>::destroy(skiplist.alloc_, del);
            std::allocator_traits<Alloc>::deallocate(skiplist.alloc_, 1);
        }
    }

    friend constexpr void tag_invoke([[maybe_unused]] remove_fn const& /*unused*/,
                                     SkipList& skiplist,
                                     const_iterator itr)
    {
        Node* cur = skiplist.head_;
        for (difference_type i = skiplist.levels_ - 1; i >= 0; i--) {
            for (; cur->next(i) != nullptr; cur = cur->next(i)) {
                if (cur->next(i) == itr.current_) {
                    for (difference_type j = i; j >= 0; --j) {
                        cur->next(j) = cur->next(j)->next(j);
                    }
                    itr.current_->deallocate_storage(skiplist.alloc_, i);
                    std::allocator_traits<Alloc>::destroy(skiplist.alloc_, itr.current_);
                    std::allocator_traits<Alloc>::deallocate(skiplist.alloc_, 1);
                    return;
                }

                if (skiplist.rel_(skiplist.proj_(cur->next(i)->value()),
                                  skiplist.proj_(*itr))) {
                    break;
                }
            }
        }
    }

    template <class U>
    requires std::same_as<std::remove_cvref_t<U>, T>
    friend constexpr iterator tag_invoke(insert_fn const& /*unused*/,
                                         SkipList& skiplist,
                                         U&& val)
    {
        int level = 0;

        for (auto ri = skiplist.rand_(); (ri & 1U) == 1; ri >>= 1U) {
            ++level;
            if (level == skiplist.levels_) {
                ++skiplist.levels_;
                break;
            }
        }

        Node* current = skiplist.head_;

        using alloc_traits = std::allocator_traits<allocator_type>;
        auto newNode = alloc_traits::allocate(skiplist.alloc_, 1);
        alloc_traits::construct(
            skiplist.alloc_, newNode, level + 1, skiplist.alloc_, std::forward<U>(val));

        int i = skiplist.levels_ - 1;
        for (; i >= 0; --i) {
            assert(i < current->size());
            [[assume(i < current->size())]];
            for (; current->at(i) != nullptr and
                   not skiplist.rel_(skiplist.proj_(val),
                                     skiplist.proj_(*current->at(i)->key()));
                 current = current->at(i)) {}

            if (i <= level) {
                newNode.at(i) = current->at(i);
                current->at(i) = newNode;
            }
        }
        ++skiplist.size_;
        return iterator{current->at(i)};
    }

    friend constexpr bool tag_invoke(is_sorted_fn const& /*unused*/,
                                     SkipList const& /*unused*/) noexcept
    {
        return true;
    }
};

} // namespace algo
