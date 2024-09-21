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

#include "sequence.hpp"
#include <ranges>

namespace algo
{

// containers
struct insert_fn;
struct remove_fn;
struct max_size_fn;
struct empty_fn;

// optional
struct push_front_fn;
struct push_back_fn;
struct pop_front_fn;
struct pop_back_fn;
struct emplace_fn;
struct emplace_front_fn;
struct emplace_back_fn;

// dynamic
struct clear_fn;
struct allocator_fn;

// optional
struct resize_fn;
struct reserve_fn;
struct capacity_fn;
struct shrink_to_fit_fn;
struct contains_fn;

namespace _concepts
{
template <class C>
concept supports_sequence_insertion =
    std::invocable<insert_fn, C, const_lvalue_t<cursor_t<C>>, element_t<C>> or
    std::invocable<insert_fn, C, const_lvalue_t<cursor_t<C>>, rvalue_element_t<C>>;

template <class C>
concept supports_sequence_removal = std::invocable<remove_fn, C, cursor_t<C>>;

template <class C>
concept container =
    std::invocable<empty_fn, C> and std::invocable<size_fn, C> and
    std::invocable<max_size_fn, C> and std::invocable<empty_fn, C> and
    std::swappable<std::remove_cvref_t<C>> and std::equality_comparable<C> and
    std::movable<C> and std::copyable<C> and std::is_default_constructible_v<C>;

} // namespace _concepts

template <class C>
concept dynamic_container =
    std::invocable<clear_fn, C> and std::invocable<allocator_fn, C>;

template <class C>
concept sequence_container =
    sequence<C> and _concepts::supports_sequence_insertion<C> and
    _concepts::supports_sequence_removal<C> and _concepts::container<C>;

template <class C>
concept forward_container = sequence_container<C> and multipass_sequence<C>;

template <class C>
concept bidirectional_container = forward_container<C> and bidirectional_sequence<C>;

template <class C>
concept random_access_container =
    bidirectional_container<C> and random_access_sequence<C>;

template <class C>
concept contiguous_container = random_access_container<C> and contiguous_sequence<C>;

constexpr inline struct insert_fn
{
    /**
     * @brief Calls custom implimentation of insert
     *
     * @tparam Container A type satisfying container
     * @tparam Args Argument types for insert function
     * @param container a non-rvalue reference of type `Container`
     * @param args argumemts for insert function
     * @return cursor_t<Container>
     */
    template <class Container, class... Args>
    requires(not std::is_rvalue_reference_v<Container> and
             unifex::tag_invocable<insert_fn, Container, Args...>)
    static constexpr auto operator()(Container&& container, Args&&... args)
        noexcept(unifex::is_nothrow_tag_invocable_v<insert_fn, Container, Args...>)
            -> unifex::tag_invoke_result_t<insert_fn, Container, Args...>
    {
        return unifex::tag_invoke(insert_fn{}, container, std::forward<Args>(args)...);
    }

    /**
     * @brief Default implimentation of insert for types conforming to
     * `contiguous_range` concept. Calls the insert member function on
     * `container`
     */
    template <class Container, index_t, class U>
    requires(not std::is_rvalue_reference_v<Container> and
             not unifex::tag_invocable<insert_fn, Container, index_t, U> and
             std::ranges::contiguous_range<Container> and
             std::ranges::sized_range<Container> and
             requires(Container&& c, std::ranges::iterator_t<Container> itr, U u) {
                 { c.insert(itr, u) };
             })
    static constexpr auto operator()(Container&& container, index_t idx, U&& elem)
        noexcept(noexcept(std::forward<Container>(container).insert(
            std::declval<std::ranges::iterator_t<Container>>(),
            std::forward<U>(elem)))) -> index_t
    {
        auto itr = std::ranges::begin(container);
        std::advance(itr, idx);
        std::forward<Container>(container).insert(itr, std::forward<U>(elem));
        return idx;
    }

    /**
     * @brief Default implimentation of insert for types conforming to
     * `contiguous_range` concept. Calls the insert member function on
     * `container`. Inserts `n` copies of `elem` into the container at
     * position `idx`.
     */
    template <class Container, index_t, class U>
    requires(not std::is_rvalue_reference_v<Container> and
             not unifex::tag_invocable<insert_fn, Container, index_t, size_t, U> and
             std::ranges::contiguous_range<Container> and
             std::ranges::sized_range<Container> and
             requires(
                 Container&& c, std::ranges::iterator_t<Container> itr, size_t n, U u) {
                 { c.insert(itr, n, u) };
             })
    static constexpr auto operator()(Container&& container,
                                     index_t idx,
                                     size_t n,
                                     U&& elem)
        noexcept(noexcept(std::forward<Container>(container).insert(
            std::declval<std::ranges::iterator_t<Container>>(),
            n,
            std::forward<U>(elem)))) -> index_t
    {
        auto itr = std::ranges::begin(container);
        std::advance(itr, idx);
        std::forward<Container>(container).insert(itr, n, std::forward<U>(elem));
        return idx;
    }

    /**
     * @brief Default implimentation of `insert` for types conforming to
     * `contiguous_range` concept. Calls the emplace member function.
     */
    template <class Container, index_t, class... Args>
    requires(not std::is_rvalue_reference_v<Container> and
             not unifex::tag_invocable<insert_fn, Container, index_t, Args...> and
             std::ranges::contiguous_range<Container> and
             std::ranges::sized_range<Container> and
             requires(Container&& c,
                      std::ranges::iterator_t<Container> itr,
                      Args&&... args) {
                 { c.emplace(itr, std::forward<Args>(args)...) };
             })
    static constexpr auto operator()(Container&& container, index_t idx, Args&&... args)
        noexcept(noexcept(std::forward<Container>(container).emplace(
            std::declval<std::ranges::iterator_t<Container>>(),
            std::forward<Args>(args)...))) -> index_t
    {
        auto itr = std::ranges::begin(container);
        std::advance(itr, idx);
        std::forward<Container>(container).emplace(itr, std::forward<Args>(args)...);
        return idx;
    }

    template <class Container, class SourceContainer>
    requires(not std::is_rvalue_reference_v<Container> and
             not unifex::tag_invocable<insert_fn,
                                       Container,
                                       index_t,
                                       SourceContainer,
                                       const_lvalue_t<cursor_t<SourceContainer>>,
                                       const_lvalue_t<cursor_t<SourceContainer>>> and
             std::ranges::contiguous_range<Container> and
             std::ranges::sized_range<Container> and
             std::ranges::range<SourceContainer> and
             std::ranges::sized_range<SourceContainer> and
             requires(Container c,
                      std::ranges::iterator_t<Container> i,
                      std::ranges::iterator_t<SourceContainer> s,
                      std::ranges::iterator_t<SourceContainer> e) {
                 { c.insert(i, s, e) };
             })
    static constexpr auto operator()(Container&& container,
                                     index_t pos,
                                     SourceContainer&& src,
                                     const_lvalue_t<cursor_t<SourceContainer>> s,
                                     const_lvalue_t<cursor_t<SourceContainer>> e)
    {
        auto itr = std::ranges::begin(container);
        std::advance(itr, pos);
        auto sitr = std::ranges::begin(src);
        auto eitr = std::ranges::begin(src);
        std::advance(sitr, s);
        std::advance(eitr, e);
        container.insert(itr, sitr, eitr);
        return pos;
    }
} insert;

constexpr inline struct remove_fn
{
    template <class Container, class... Args>
    requires(unifex::tag_invocable<remove_fn, lvalue_t<Container>, Args...>)
    static constexpr void operator()(lvalue_t<Container> container, Args&&... args)
        noexcept(
            unifex::is_nothrow_tag_invocable_v<remove_fn, lvalue_t<Container>, Args...>)
    {
        unifex::tag_invoke(remove_fn{}, container, std::forward<Args>(args)...);
    }

    template <class Container>
    requires(not unifex::tag_invocable<remove_fn, lvalue_t<Container>, index_t> and
             std::ranges::contiguous_range<Container> and
             std::ranges::sized_range<Container> and
             requires(lvalue_t<Container> c, index_t idx) {
                 { c.erase(idx) };
             })
    static constexpr auto operator()(lvalue_t<Container> container, index_t idx)
        noexcept(noexcept(std::forward<Container>(container).erase(
            std::declval<std::ranges::iterator_t<Container>>()))) -> index_t
    {
        auto itr = std::ranges::begin(container);
        std::advance(itr, idx);
        return std::distance(std::ranges::begin(container), container.erase(itr));
    }

    template <class Container>
    requires(not unifex::
                 tag_invocable<remove_fn, lvalue_t<Container>, index_t, index_t> and
             std::ranges::contiguous_range<Container> and
             std::ranges::sized_range<Container> and
             requires(lvalue_t<Container> c, index_t idx) {
                 { c.erase(idx, idx) };
             })
    static constexpr auto operator()(lvalue_t<Container> container,
                                     index_t fst,
                                     index_t lst)
        noexcept(noexcept(std::forward<Container>(container).erase(
            std::declval<std::ranges::iterator_t<Container>>(),
            std::declval<std::ranges::iterator_t<Container>>()))) -> index_t
    {
        assert(fst <= lst);
        assert(fst <= std::ranges::size(container));
        assert(lst <= std::ranges::size(container));
        [[assume(fst <= lst)]];
        [[assume(fst <= std::ranges::size(container))]];
        [[assume(lst <= std::ranges::size(container))]];

        auto fitr = std::ranges::begin(container);
        auto litr = std::ranges::begin(container);
        std::advance(fitr, fst);
        std::advance(litr, lst);
        return std::distance(std::ranges::begin(container), container.erase(fitr, litr));
    }

} remove;

constexpr inline struct max_size_fn
{
    template <class Container>
    requires(not std::is_rvalue_reference_v<Container> and
             unifex::tag_invocable<max_size_fn, Container>)
    static constexpr auto operator()(Container&& container)
        noexcept(unifex::is_nothrow_tag_invocable_v<max_size_fn, Container>)
            -> unifex::tag_invoke_result_t<max_size_fn, Container>
    {
        return unifex::tag_invoke(max_size_fn{}, std::forward<Container>(container));
    }
} max_size;

constexpr inline struct empty_fn
{

    template <class Container>
    requires(not std::is_rvalue_reference_v<Container> and
             unifex::tag_invocable<empty_fn, Container> and
             std::convertible_to<unifex::tag_invoke_result_t<empty_fn, Container>, bool>)
    static constexpr auto operator()(Container&& container)
        noexcept(unifex::is_nothrow_tag_invocable_v<max_size_fn, Container>) -> bool
    {
        return unifex::tag_invoke(empty_fn{}, std::forward<Container>(container));
    }
} empty;

constexpr inline struct capacity_fn
{
    template <class Container>
    requires(not std::is_rvalue_reference_v<Container> and
             unifex::tag_invocable<capacity_fn, Container>)
    static constexpr auto operator()(Container&& container)
        noexcept(unifex::is_nothrow_tag_invocable_v<capacity_fn, Container>)
            -> unifex::tag_invoke_result_t<capacity_fn, Container>
    {
        return unifex::tag_invoke(capacity_fn{}, std::forward<Container>(container));
    }
} capacity;

constexpr inline struct allocator_fn
{
    template <class Container>
    requires(not std::is_rvalue_reference_v<Container> and
             unifex::tag_invocable<allocator_fn, Container>)
    static constexpr auto operator()(Container&& container)
        noexcept(unifex::is_nothrow_tag_invocable_v<allocator_fn, Container>)
            -> unifex::tag_invoke_result_t<allocator_fn, Container>
    {
        return unifex::tag_invoke(allocator_fn{}, std::forward<Container>(container));
    }
} allocator;

constexpr inline struct reserve_fn
{
    template <class Container>
    requires(unifex::tag_invocable<reserve_fn, Container&, size_t>)
    static constexpr void operator()(Container& container, size_t n)
        noexcept(unifex::is_nothrow_tag_invocable_v<reserve_fn, Container&, size_t>)
    {
        unifex::tag_invoke(reserve_fn{}, container, n);
    }
} reserve;

namespace _defaults
{
template <std::ranges::range Rng>
class range_cursor
{
public:
    using range_type = Rng;
    using iterator_type = std::ranges::iterator_t<Rng>;

    constexpr range_cursor() = default;
    constexpr range_cursor(range_cursor const& other) noexcept
        : rng_{other.rng_}
        , itr_{other.itr_}
    {
        assert(other.rng_ == this->rng_);
    }

    constexpr range_cursor(range_cursor&& other) = default;

    constexpr range_cursor& operator=(range_cursor const& other) noexcept
    {
        assert(std::addressof(other.rng_) == this->rng_);
        if (this != std::addressof(other)) {
            this->rng_ = other.rng_;
            this->itr_ = other.itr_;
        }
        return *this;
    }

    constexpr ~range_cursor() = default;

    constexpr auto operator==(range_cursor const& other) const noexcept -> bool
    {
        assert(other.rng_ == this->rng_);
        return itr_ == other.itr_;
    }

    constexpr auto operator<=>(range_cursor const& other) const noexcept
        -> std::compare_three_way_result_t<iterator_type>
    {
        assert(other.rng_ == this->rng_);
        return itr_ <=> other.itr_;
    }

private:
    range_type* rng_;
    iterator_type itr_;

    template <class Cursor>
    static constexpr bool is_common_cursor_v =
        std::same_as<range_cursor<std::remove_cvref_t<
                         typename std::remove_cvref_t<Cursor>::range_type>>,
                     range_cursor<std::remove_reference_t<range_type>>>;

    friend constexpr auto tag_invoke([[maybe_unused]] to_iterator_fn tag,
                                     [[maybe_unused]] range_type& rng,
                                     range_cursor const& cur) noexcept -> iterator_type
    {
        assert(std::addressof(rng) == cur.rng_);
        return cur.itr_;
    }

    template <class Itr>
    requires(std::constructible_from<iterator_type, Itr>)
    friend constexpr auto tag_invoke([[maybe_unused]] from_iterator_fn tag,
                                     range_type& rng,
                                     Itr&& itr)
        noexcept(std::is_nothrow_constructible_v<iterator_type, Itr>) -> range_cursor
    {
        return {rng, std::forward<Itr>(itr)};
    }

    friend constexpr auto tag_invoke([[maybe_unused]] first_fn tag, range_type& rng)
        noexcept(std::is_nothrow_invocable_v<from_iterator_fn,
                                             range_type&,
                                             iterator_type&&> and
                 noexcept(std::ranges::begin(rng)))
            -> decltype(from_iterator(rng, std::ranges::begin(rng)))
    {
        return from_iterator(rng, std::ranges::begin(rng));
    }

    friend constexpr auto tag_invoke([[maybe_unused]] terminal_fn tag, range_type& rng)
        noexcept(std::is_nothrow_invocable_v<from_iterator_fn,
                                             range_type&,
                                             iterator_type&&> and
                 noexcept(std::ranges::end(rng)))
            -> decltype(from_iterator(rng, std::ranges::end(rng)))
    requires(std::ranges::common_range<range_type>)
    {
        return from_iterator(rng, std::ranges::end(rng));
    }

    friend constexpr auto tag_invoke([[maybe_unused]] terminal_fn tag, range_type& rng)
        noexcept(noexcept(std::ranges::end(rng))) -> std::ranges::sentinel_t<range_type>
    requires(not std::ranges::common_range<range_type>)
    {
        return std::ranges::end(rng);
    }

    template <class Cursor>
    requires(is_common_cursor_v<Cursor>)
    friend constexpr auto tag_invoke([[maybe_unused]] is_last_fn tag,
                                     [[maybe_unused]] range_type& rng,
                                     Cursor const& cur)
        noexcept(noexcept(cur.itr_ == std::ranges::end(cur.rng_))) -> bool
    {
        assert(std::addressof(rng) == cur.rng_);
        return cur.itr_ == std::ranges::end(cur.rng_);
    }

    friend constexpr auto tag_invoke(
        [[maybe_unused]] is_last_fn tag,
        [[maybe_unused]] range_type& rng,
        [[maybe_unused]] std::ranges::sentinel_t<range_type> const& cur) noexcept -> bool
    requires(not std::ranges::common_range<range_type>)
    {
        return true;
    }

    template <class Cursor>
    requires(is_common_cursor_v<Cursor> and std::weakly_incrementable<iterator_type>)
    friend constexpr auto tag_invoke([[maybe_unused]] inc_fn tag,
                                     [[maybe_unused]] range_type& rng,
                                     Cursor& cur,
                                     distance_t offset)
        noexcept(noexcept(std::ranges::advance(cur.itr_, offset))) -> Cursor&
    {
        assert(std::addressof(rng) == cur.rng_);
        std::ranges::advance(cur.itr_, offset);
        return cur;
    }

    template <class Cursor>
    requires(is_common_cursor_v<Cursor> and std::weakly_incrementable<iterator_type>)
    friend constexpr auto tag_invoke([[maybe_unused]] inc_fn tag,
                                     [[maybe_unused]] range_type& rng,
                                     Cursor& cur)
        noexcept(noexcept(algo::inc(rng, cur, 1))) -> Cursor&
    {
        assert(std::addressof(rng) == cur.rng_);
        ++cur.itr_;
        return cur;
    }

    template <class Cursor>
    requires(is_common_cursor_v<Cursor> and std::bidirectional_iterator<iterator_type>)
    friend constexpr auto tag_invoke([[maybe_unused]] dec_fn tag,
                                     [[maybe_unused]] range_type& rng,
                                     Cursor& cur,
                                     distance_t offset)
        noexcept(noexcept(std::ranges::advance(cur.itr_, -offset))) -> Cursor&
    {
        assert(std::addressof(rng) == cur.rng_);
        std::ranges::advance(cur.itr_, -offset);
        return cur;
    }

    template <class Cursor>
    requires(is_common_cursor_v<Cursor> and std::bidirectional_iterator<iterator_type>)
    friend constexpr auto tag_invoke([[maybe_unused]] dec_fn tag,
                                     [[maybe_unused]] range_type& rng,
                                     Cursor& cur)
        noexcept(noexcept(algo::inc(rng, cur, 1))) -> Cursor&
    {
        assert(std::addressof(rng) == cur.rng_);
        --cur.itr_;
        return cur;
    }

    template <class Cursor>
    requires(is_common_cursor_v<Cursor>)
    friend constexpr auto tag_invoke([[maybe_unused]] read_at_fn tag,
                                     [[maybe_unused]] range_type& rng,
                                     Cursor const& cur)
        -> std::ranges::range_reference_t<range_type>
    {
        assert(std::addressof(rng) == cur.rng_);
        return *cur.itr_;
    }

    template <class Cursor>
    requires(is_common_cursor_v<Cursor>)
    friend constexpr auto tag_invoke([[maybe_unused]] read_at_unchecked_fn tag,
                                     [[maybe_unused]] range_type& rng,
                                     Cursor const& cur)
        -> std::ranges::range_reference_t<range_type>
    {
        return *cur.itr_;
    }

    template <class From, class To>
    requires(is_common_cursor_v<From> and is_common_cursor_v<To>)
    friend constexpr auto tag_invoke([[maybe_unused]] distance_fn tag,
                                     [[maybe_unused]] range_type& rng,
                                     From const& from,
                                     To const& to)
        noexcept(noexcept(std::ranges::distance(from.itr_, to.itr_))) -> distance_t
    {
        assert(from.rng_ == to.rng_ and to.rng_ == std::addressof(rng));
        return std::ranges::distance(from.itr_, to.itr_);
    }

    template <class From, class Sent>
    requires(is_common_cursor_v<From> and
             std::sentinel_for<Sent, typename From::iterator_type>)
    friend constexpr auto tag_invoke([[maybe_unused]] distance_fn tag,
                                     [[maybe_unused]] range_type& rng,
                                     From const& from,
                                     Sent const& sent)
        noexcept(noexcept(std::ranges::distance(from.itr_, sent))) -> distance_t
    {
        assert(from.rng_ == std::addressof(rng));
        return std::ranges::distance(from.itr_, sent);
    }

    friend constexpr auto tag_invoke([[maybe_unused]] data_fn tag, range_type& rng)
        noexcept(noexcept(std::ranges::data(rng)))
            -> std::add_pointer_t<std::remove_reference_t<element_t<range_type>>>
    requires(std::ranges::contiguous_range<range_type>)
    {
        return std::ranges::data(rng);
    }

    friend constexpr auto tag_invoke([[maybe_unused]] size_fn tag, range_type& rng)
        noexcept(noexcept(std::ranges::size(rng))) -> distance_t
    requires(std::ranges::sized_range<range_type>)
    {
        return distance_t(std::ranges::size(rng));
    }

    friend constexpr auto tag_invoke([[maybe_unused]] usize_fn tag, range_type& rng)
        noexcept(noexcept(std::ranges::size(rng))) -> size_t
    requires(std::ranges::sized_range<range_type>)
    {
        return size_t(std::ranges::size(rng));
    }

    template <class Cursor>
    requires(is_common_cursor_v<Cursor>)
    friend constexpr auto tag_invoke([[maybe_unused]] next_fn tag,
                                     range_type& rng,
                                     Cursor cur)
        noexcept(noexcept(algo::from_iterator(rng, std::ranges::next(cur.itr_))))
            -> range_cursor
    {
        assert(std::addressof(rng) == cur.rng_);
        return algo::from_iterator(rng, std::ranges::next(cur.itr_));
    }

    template <class Cursor>
    requires(is_common_cursor_v<Cursor>)
    friend constexpr auto tag_invoke([[maybe_unused]] next_fn tag,
                                     range_type& rng,
                                     Cursor&& cur,
                                     distance_t offset)
        noexcept(noexcept(algo::from_iterator(
            rng, std::ranges::next(std::forward_like<Cursor>(cur.itr_), offset))))
            -> range_cursor
    {
        assert(std::addressof(rng) == cur.rng_);
        return algo::from_iterator(
            rng, std::ranges::next(std::forward_like<Cursor>(cur.itr_), offset));
    }

    template <class Cursor, class Term>
    requires(is_common_cursor_v<Cursor> and is_common_cursor_v<Term> and
             std::ranges::common_range<range_type>)
    friend constexpr auto tag_invoke([[maybe_unused]] next_fn tag,
                                     range_type& rng,
                                     Cursor&& cur,
                                     Term&& term)
        noexcept(noexcept(algo::from_iterator(
            rng,
            std::ranges::next(std::forward_like<Cursor>(cur.itr_),
                              std::forward_like<Term>(term.itr_))))) -> range_cursor
    {
        assert(std::addressof(rng) == cur.rng_ and cur.rng_ == term.rng_);
        return algo::from_iterator(rng,
                                   std::ranges::next(std::forward_like<Cursor>(cur.itr_),
                                                     std::forward_like<Term>(term.itr_)));
    }

    template <class Cursor, class Sent>
    requires(is_common_cursor_v<Cursor> and not std::ranges::common_range<range_type> and
             std::sentinel_for<Sent, typename Cursor::iterator_type>)
    friend constexpr auto tag_invoke([[maybe_unused]] next_fn tag,
                                     range_type& rng,
                                     Cursor&& cur,
                                     Sent&& sent)
        noexcept(noexcept(algo::from_iterator(
            rng,
            std::ranges::next(std::forward_like<Cursor>(cur.itr_),
                              std::forward<Sent>(sent))))) -> range_cursor
    {
        assert(std::addressof(rng) == cur.rng_);
        return algo::from_iterator(rng,
                                   std::ranges::next(std::forward_like<Cursor>(cur.itr_),
                                                     std::forward<Sent>(sent)));
    }

    template <class Cursor, class Term>
    requires(is_common_cursor_v<Cursor> and is_common_cursor_v<Term> and
             std::ranges::common_range<range_type>)
    friend constexpr auto tag_invoke([[maybe_unused]] next_fn tag,
                                     range_type& rng,
                                     Cursor&& cur,
                                     distance_t offset,
                                     Term&& term)
        noexcept(noexcept(algo::from_iterator(
            rng,
            std::ranges::next(std::forward_like<Cursor>(cur.itr_),
                              offset,
                              std::forward_like<Term>(term.itr_))))) -> range_cursor
    {
        assert(std::addressof(rng) == cur.rng_ and cur.rng_ == term.rng_);
        return algo::from_iterator(rng,
                                   std::ranges::next(std::forward_like<Cursor>(cur.itr_),
                                                     offset,
                                                     std::forward_like<Term>(term.itr_)));
    }

    template <class Cursor, class Sent>
    requires(is_common_cursor_v<Cursor> and not std::ranges::common_range<range_type> and
             std::sentinel_for<Sent, typename Cursor::iterator_type>)
    friend constexpr auto tag_invoke([[maybe_unused]] next_fn tag,
                                     range_type& rng,
                                     Cursor&& cur,
                                     distance_t offset,
                                     Sent&& sent)
        noexcept(noexcept(algo::from_iterator(
            rng,
            std::ranges::next(std::forward_like<Cursor>(cur.itr_),
                              offset,
                              std::forward<Sent>(sent))))) -> range_cursor
    {
        assert(std::addressof(rng) == cur.rng_);
        return algo::from_iterator(rng,
                                   std::ranges::next(std::forward_like<Cursor>(cur.itr_),
                                                     offset,
                                                     std::forward<Sent>(sent)));
    }

    template <class Cursor>
    requires(is_common_cursor_v<Cursor> and std::bidirectional_iterator<iterator_type>)
    friend constexpr auto tag_invoke([[maybe_unused]] prev_fn tag,
                                     range_type& rng,
                                     Cursor cur)
        noexcept(noexcept(algo::from_iterator(rng, std::ranges::prev(cur.itr_))))
            -> range_cursor
    {
        assert(std::addressof(rng) == cur.rng_);
        return algo::from_iterator(rng, std::ranges::prev(cur.itr_));
    }

    template <class Cursor>
    requires(is_common_cursor_v<Cursor> and std::bidirectional_iterator<iterator_type>)
    friend constexpr auto tag_invoke([[maybe_unused]] prev_fn tag,
                                     range_type& rng,
                                     Cursor&& cur,
                                     distance_t offset)
        noexcept(noexcept(algo::from_iterator(
            rng, std::ranges::prev(std::forward_like<Cursor>(cur.itr_), offset))))
            -> range_cursor
    {
        assert(std::addressof(rng) == cur.rng_);
        return algo::from_iterator(
            rng, std::ranges::prev(std::forward_like<Cursor>(cur.itr_), offset));
    }

    template <class Cursor, class Term>
    requires(is_common_cursor_v<Cursor> and is_common_cursor_v<Term> and
             std::ranges::common_range<range_type> and
             std::bidirectional_iterator<iterator_type>)
    friend constexpr auto tag_invoke([[maybe_unused]] prev_fn tag,
                                     range_type& rng,
                                     Cursor&& cur,
                                     distance_t offset,
                                     Term&& term)
        noexcept(noexcept(algo::from_iterator(
            rng,
            std::ranges::prev(std::forward_like<Cursor>(cur.itr_),
                              offset,
                              std::forward_like<Term>(term.itr_))))) -> range_cursor
    {
        assert(std::addressof(rng) == cur.rng_ and cur.rng_ == term.rng_);
        return algo::from_iterator(rng,
                                   std::ranges::prev(std::forward_like<Cursor>(cur.itr_),
                                                     offset,
                                                     std::forward_like<Term>(term.itr_)));
    }

    friend constexpr auto tag_invoke([[maybe_unused]] is_empty_fn tag, range_type& rng)
        noexcept(noexcept(std::ranges::empty(rng))) -> decltype(std::ranges::empty(rng))
    {
        return std::ranges::empty(rng);
    }

    template <std::ranges::range Rng2>
    requires(std::swappable_with<value_t<Rng>, value_t<Rng2>> and
             not(std::is_const_v<Rng> or std::is_const_v<Rng2>))
    friend constexpr auto tag_invoke([[maybe_unused]] swap_with_fn tag,
                                     [[maybe_unused]] range_type& rng1,
                                     range_cursor const& cur1,
                                     [[maybe_unused]] Rng2& rng2,
                                     range_cursor<Rng2> const& cur2)
        noexcept(std::is_nothrow_swappable_with_v<value_t<Rng>, value_t<Rng2>>) -> void
    {
        assert(std::addressof(rng1) == cur1.rng_);
        assert(std::addressof(rng2) == cur2.rng_);
        std::ranges::iter_swap(cur1.itr_, cur2.itr_);
    }
};
} // namespace _defaults

template <std::ranges::range Rng>
using range_cursor_t = _defaults::range_cursor<std::remove_reference_t<Rng>>;
} // namespace algo
