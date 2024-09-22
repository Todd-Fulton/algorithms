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

#include <algo/traits.hpp>

#include <compare>
#include <concepts>
#include <optional>

#include <ranges>
#include <unifex/tag_invoke.hpp>

namespace algo
{
struct first_fn;
struct is_last_fn;
struct terminal_fn;
struct inc_fn;
struct dec_fn;
struct read_at_fn;
struct read_at_unchecked_fn;
struct move_at_fn;
struct move_at_unchecked_fn;
struct distance_fn;
struct data_fn;
struct size_fn;
struct usize_fn;
struct next_fn;
struct prev_fn;
struct is_empty_fn;
struct swap_with_fn;
struct swap_at_fn;
struct front_fn;
struct back_fn;
struct begin_fn;
struct end_fn;

template <class Seq>
struct sequence_traits;

/**
 * @brief The cursor type associated with the sequence type `Seq`
 * Cursors are used for iteration and element access.
 *
 * @tparam T A type for which _traits::cursor_type is specialized
 */
template <class T>
using cursor_t = std::invoke_result_t<
    first_fn,
    std::conditional_t<std::is_reference_v<T>, T, std::add_lvalue_reference_t<T>>>;

namespace _traits
{

template <class Seq>
requires(std::invocable<read_at_fn, lvalue_t<Seq>, const_lvalue_t<cursor_t<Seq>>>)
struct element_type
    : std::type_identity<
          std::invoke_result_t<read_at_fn, lvalue_t<Seq>, const_lvalue_t<cursor_t<Seq>>>>
{};
template <class T>
struct value_type;

template <class T>
requires requires { T::value_type; }
struct value_type<T>
{
    using type = typename T::value_type;
};

template <class Seq>
requires requires { typename element_type<Seq>::type; }
struct value_type<Seq>
{
    using type = std::remove_cvref_t<typename element_type<Seq>::type>;
};
} // namespace _traits

/**
 * @brief The element type associated with the sequence type `Seq`.
 *
 * @tparam Seq A type satisfying the `sequence` concept
 */
template <class T>
using element_t = typename _traits::element_type<T>::type;

using distance_t = std::ptrdiff_t;
using index_t = distance_t;

template <class Seq>
using rvalue_element_t = std::conditional_t<
    std::is_invocable_v<move_at_fn, lvalue_t<Seq>, const_lvalue_t<cursor_t<Seq>>>,
    std::invoke_result_t<move_at_fn, lvalue_t<Seq>, const_lvalue_t<cursor_t<Seq>>>,
    std::conditional_t<std::is_lvalue_reference_v<element_t<Seq>>,
                       rvalue_t<element_t<Seq>>,
                       element_t<Seq>>>;

template <class T>
using value_t = _traits::value_type<T>::type;

template <class Seq>
using const_element_t = std::common_reference_t<value_t<Seq> const&&, element_t<Seq>>;

template <class T>
concept can_reference = requires { typename lvalue_t<T>; };

template <class Cur>
concept cursor = std::movable<Cur>;

template <class Cur>
concept regular_cursor = cursor<Cur> and std::regular<Cur>;

template <class Cur>
concept ordered_cursor =
    regular_cursor<Cur> and std::three_way_comparable<Cur, std::strong_ordering>;

template <class Seq>
concept sequence =
    std::invocable<first_fn, Seq> and cursor<std::invoke_result_t<first_fn, Seq>> and
    std::invocable<is_last_fn, Seq, const_lvalue_t<cursor_t<Seq>>> and
    std::convertible_to<std::invoke_result_t<first_fn, Seq>, bool> and
    std::invocable<read_at_fn, Seq, const_lvalue_t<cursor_t<Seq>>> and
    can_reference<std::invoke_result_t<first_fn, Seq>> and
    std::invocable<inc_fn, Seq, lvalue_t<cursor_t<Seq>>> and
    std::convertible_to<std::invoke_result_t<inc_fn, Seq, lvalue_t<cursor_t<Seq>>>,
                        lvalue_t<cursor_t<Seq>>>;

template <class>
inline constexpr bool disable_multipass = false;

template <class T>
requires requires { T::disable_multipass; } &&
             std::convertible_to<decltype(T::multipass_sequence), bool>
inline constexpr bool disable_multipass<T> = T::disable_multipass;

template <class Seq>
concept multipass_sequence =
    sequence<Seq> and regular_cursor<cursor_t<Seq>> and !disable_multipass<Seq>;

template <class Seq>
concept bidirectional_sequence =
    multipass_sequence<Seq> and std::invocable<dec_fn, Seq, lvalue_t<cursor_t<Seq>>>;

template <class Seq>
concept random_access_sequence =
    bidirectional_sequence<Seq> and ordered_cursor<cursor_t<Seq>> and
    std::invocable<inc_fn, Seq, lvalue_t<cursor_t<Seq>>, distance_t> and
    std::invocable<distance_fn,
                   Seq,
                   const_lvalue_t<cursor_t<Seq>>,
                   const_lvalue_t<cursor_t<Seq>>> and
    std::convertible_to<std::invoke_result_t<distance_fn,
                                             Seq,
                                             const_lvalue_t<cursor_t<Seq>>,
                                             const_lvalue_t<cursor_t<Seq>>>,
                        distance_t>;

template <class Seq>
concept bounded_sequence =
    sequence<Seq> and std::invocable<terminal_fn, Seq> and
    std::same_as<std::invoke_result_t<terminal_fn, Seq>, cursor_t<Seq>>;

template <class Seq>
concept sized_sequence =
    sequence<Seq> and
    ((std::invocable<size_fn, Seq> and
      std::convertible_to<std::invoke_result_t<size_fn, Seq>, distance_t>) or
     (random_access_sequence<Seq> and bounded_sequence<Seq>));

template <class Seq>
concept contiguous_sequence =
    random_access_sequence<Seq> and bounded_sequence<Seq> and
    std::is_lvalue_reference_v<element_t<Seq>> and
    std::same_as<value_t<Seq>, std::remove_cvref_t<element_t<Seq>>> and
    std::invocable<data_fn, Seq> and
    std::same_as<std::invoke_result_t<data_fn, Seq>, std::add_pointer_t<element_t<Seq>>>;

template <class Seq>
inline constexpr bool is_infinite_seq = false;

template <class Seq>
requires requires { Seq::is_infinite; } and
             std::convertible_to<typename Seq::is_infinite, bool>
inline constexpr bool is_infinite_seq<Seq> = Seq::is_infinite;

template <class Seq>
concept infinite_sequence = sequence<Seq> and not bounded_sequence<Seq> and
                            not sized_sequence<Seq> and is_infinite_seq<Seq>;

template <class Seq>
concept read_only_sequence =
    sequence<Seq> and std::same_as<element_t<Seq>, const_element_t<Seq>>;

template <typename Seq>
concept const_iterable_sequence =
    // Seq and Seq const must both be sequences
    sequence<Seq> && sequence<Seq const> &&
    // Seq and Seq const must have the same cursor and value types
    std::same_as<cursor_t<Seq>, cursor_t<Seq const>> &&
    std::same_as<value_t<Seq>, value_t<Seq const>> &&
    // Seq and Seq const must have the same const_element type
    std::same_as<const_element_t<Seq>, const_element_t<Seq const>> &&
    // Seq and Seq const must model the same extended sequence concepts
    (multipass_sequence<Seq> == multipass_sequence<Seq const>) &&
    (bidirectional_sequence<Seq> == bidirectional_sequence<Seq const>) &&
    (random_access_sequence<Seq> == random_access_sequence<Seq const>) &&
    (contiguous_sequence<Seq> == contiguous_sequence<Seq const>) &&
    (bounded_sequence<Seq> == bounded_sequence<Seq const>) &&
    (sized_sequence<Seq> == sized_sequence<Seq const>) &&
    (infinite_sequence<Seq> == infinite_sequence<Seq const>) &&
    // If Seq is read-only, Seq const must be read-only as well
    (!read_only_sequence<Seq> || read_only_sequence<Seq const>);

template <class Seq, class T>
concept writeable_sequence_of =
    sequence<Seq> and requires(element_t<Seq> elem, T&& item) {
        {
            elem = std::forward<decltype(item)>(item)
        } -> std::same_as<lvalue_t<element_t<Seq>>>;
    };

constexpr inline struct to_iterator_fn
{
    template <std::ranges::range Rng, class Cur>
    requires(not std::is_rvalue_reference_v<Rng> and
             unifex::tag_invocable<to_iterator_fn, Rng, Cur>)
    static constexpr auto operator()(Rng&& rng, Cur&& cur) noexcept
        -> std::ranges::iterator_t<Rng>
    {
        return unifex::tag_invoke(
            to_iterator_fn{}, std::forward<Rng>(rng), std::forward<Cur>(cur));
    }
} to_iterator;

constexpr inline struct from_iterator_fn
{
    template <std::ranges::range Rng, class Itr>
    requires(
        not std::is_rvalue_reference_v<Rng> and
        unifex::tag_invocable<from_iterator_fn, Rng, std::ranges::iterator_t<Rng>> and
        std::same_as<std::remove_cvref_t<Itr>, std::ranges::iterator_t<Rng>>)
    static constexpr auto operator()(Rng&& rng, Itr&& itr) noexcept -> cursor_t<Rng>
    {
        return unifex::tag_invoke(
            from_iterator_fn{}, std::forward<Rng>(rng), std::forward<Itr>(itr));
    }
} from_iterator;

constexpr inline struct distance_fn
{
    template <class Seq>
    requires(
        unifex::tag_invocable<distance_fn,
                              Seq const&,
                              const_lvalue_t<cursor_t<Seq>>,
                              const_lvalue_t<cursor_t<Seq>>> and
        std::is_convertible_v<unifex::tag_invoke_result_t<distance_fn,
                                                          Seq const&,
                                                          const_lvalue_t<cursor_t<Seq>>,
                                                          const_lvalue_t<cursor_t<Seq>>>,
                              distance_t>)
    static constexpr auto operator()(Seq const& seq,
                                     const_lvalue_t<cursor_t<Seq>> from,
                                     const_lvalue_t<cursor_t<Seq>> to)
        noexcept(unifex::is_nothrow_tag_invocable_v<distance_fn,
                                                    Seq const&,
                                                    const_lvalue_t<cursor_t<Seq>>,
                                                    const_lvalue_t<cursor_t<Seq>>>)
            -> distance_t
    {
        return unifex::tag_invoke(distance_fn{}, seq, from, to);
    }
} distance;

constexpr inline struct is_empty_fn
{
    template <class Seq>
    requires(not std::is_rvalue_reference_v<Seq> and
             unifex::tag_invocable<is_empty_fn, Seq> and
             std::is_convertible_v<unifex::tag_invoke_result_t<is_empty_fn, Seq>, bool>)
    static constexpr auto operator()(Seq&& seq)
        noexcept(unifex::is_nothrow_tag_invocable_v<is_empty_fn, Seq>) -> bool
    {
        return unifex::tag_invoke(is_empty_fn{}, std::forward<Seq>(seq));
    }
} is_empty;

constexpr inline struct is_last_fn
{
    template <class Seq>
    requires(
        unifex::tag_invocable<is_last_fn, Seq> and
        std::is_convertible_v<
            unifex::tag_invoke_result_t<is_last_fn, Seq, const_lvalue_t<cursor_t<Seq>>>,
            bool>)
    static constexpr auto operator()(const_lvalue_t<Seq> rng,
                                     const_lvalue_t<cursor_t<Seq>> cur)
        noexcept(unifex::is_nothrow_tag_invocable_v<is_last_fn,
                                                    const_lvalue_t<Seq>,
                                                    const_lvalue_t<cursor_t<Seq>>>)
            -> bool
    {
        return unifex::tag_invoke(is_last_fn{}, rng, cur);
    }
} is_last;

constexpr inline struct size_fn
{
    /**
     * @brief Get the size of a sequence
     *
     * @tparam Seq Any type providing `tag_invoke(size_fn{}, self)`
     * @param seq
     * @return the size of seq
     */
    template <class Seq>
    requires(unifex::tag_invocable<size_fn, Seq> and
             std::convertible_to<unifex::tag_invoke_result_t<size_fn, Seq>, distance_t>)
    static constexpr auto operator()(Seq&& seq)
        noexcept(unifex::is_nothrow_tag_invocable_v<size_fn, Seq>) -> distance_t
    {
        return unifex::tag_invoke(size_fn{}, std::forward<Seq>(seq));
    }
} size;

constexpr inline struct usize_fn
{
    template <class Seq>
    requires(not std::is_rvalue_reference_v<Seq> and
             unifex::tag_invocable<usize_fn, Seq> and
             std::convertible_to<unifex::tag_invoke_result_t<usize_fn, Seq>, size_t>)
    static constexpr auto operator()(Seq&& seq)
        noexcept(unifex::is_nothrow_tag_invocable_v<usize_fn, Seq>) -> size_t
    {
        return unifex::tag_invoke(usize_fn{}, std::forward<Seq>(seq));
    }
} usize;

constexpr inline struct first_fn
{
    template <class Seq>
    requires(not std::is_rvalue_reference_v<Seq> and unifex::tag_invocable<first_fn, Seq>)
    static constexpr auto operator()(Seq&& seq)
        noexcept(unifex::is_nothrow_tag_invocable_v<first_fn, Seq>)
            -> unifex::tag_invoke_result_t<first_fn, Seq>
    {
        return unifex::tag_invoke(first_fn{}, std::forward<Seq>(seq));
    }

} first;

constexpr inline struct terminal_fn
{
    template <class Seq>
    requires(not std::is_rvalue_reference_v<Seq> and
             unifex::tag_invocable<terminal_fn, Seq>)
    static constexpr auto operator()(Seq&& rng)
        noexcept(unifex::is_nothrow_tag_invocable_v<terminal_fn, Seq>)
            -> unifex::tag_invoke_result_t<terminal_fn, Seq>
    {
        return unifex::tag_invoke(terminal_fn{}, std::forward<Seq>(rng));
    }
} terminal;

constexpr inline struct dec_fn
{
    template <class Seq>
    requires(not std::is_rvalue_reference_v<Seq> and
             unifex::tag_invocable<dec_fn, Seq> and
             std::is_convertible_v<unifex::tag_invoke_result_t<dec_fn, Seq>,
                                   lvalue_t<cursor_t<Seq>>>)
    static constexpr auto operator()(Seq&& seq, lvalue_t<cursor_t<Seq>> cur)
        noexcept(unifex::is_nothrow_tag_invocable_v<dec_fn, Seq, lvalue_t<cursor_t<Seq>>>)
            -> lvalue_t<cursor_t<Seq>>
    {
        return unifex::tag_invoke(dec_fn{}, seq, cur);
    }

    template <class Seq>
    requires(not std::is_rvalue_reference_v<Seq> and
             unifex::tag_invocable<dec_fn, Seq, lvalue_t<cursor_t<Seq>>, distance_t> and
             std::is_convertible_v<unifex::tag_invoke_result_t<dec_fn, Seq, distance_t>,
                                   lvalue_t<cursor_t<Seq>>>)
    static constexpr auto operator()(Seq&& seq,
                                     lvalue_t<cursor_t<Seq>> cur,
                                     distance_t offset) //
        noexcept(unifex::is_nothrow_tag_invocable_v<dec_fn,
                                                    Seq,
                                                    lvalue_t<cursor_t<Seq>>,
                                                    distance_t>)
            -> lvalue_t<cursor_t<Seq>>
    {
        return unifex::tag_invoke(dec_fn{}, std::forward<Seq>(seq), cur, offset);
    }

} dec;

constexpr inline struct inc_fn
{
    template <class Seq>
    requires(unifex::tag_invocable<inc_fn, Seq, cursor_t<Seq>&>)
    static constexpr auto operator()(Seq&& seq, cursor_t<Seq>& cur)
        noexcept(unifex::is_nothrow_tag_invocable_v<inc_fn, Seq&&, cursor_t<Seq>&>)
            -> lvalue_t<cursor_t<Seq>>
    {
        return unifex::tag_invoke(inc_fn{}, seq, cur);
    }

    template <class Seq>
    requires(not std::is_rvalue_reference_v<Seq> and
             unifex::tag_invocable<inc_fn, Seq, lvalue_t<cursor_t<Seq>>, distance_t>)
    static constexpr auto operator()(Seq&& seq,
                                     lvalue_t<cursor_t<Seq>> cur,
                                     distance_t offset) //
        noexcept(unifex::is_nothrow_tag_invocable_v<inc_fn,
                                                    Seq&&,
                                                    lvalue_t<cursor_t<Seq>>,
                                                    distance_t>)
            -> lvalue_t<cursor_t<Seq>>
    {
        return unifex::tag_invoke(inc_fn{}, std::forward<Seq>(seq), cur, offset);
    }

} inc;

constexpr inline struct next_fn
{
    /**
     * @brief customization point for next(seq, cur)
     *
     * Returns a new cursor pointing to the next element after `cur`
     *
     * @tparam Seq A `sequence` type
     * @param rng a lvalue sequence
     * @param cur the cursor pointing to the current element
     * @return a cursor pointing to the next element
     */
    template <class Seq>
    requires(!std::is_rvalue_reference_v<Seq> and unifex::tag_invocable<next_fn, Seq>)
    static constexpr auto operator()(Seq&& rng, cursor_t<Seq> cur)
        noexcept(unifex::is_nothrow_tag_invocable_v<next_fn, Seq, cursor_t<Seq>>)
            -> cursor_t<Seq>
    {
        return unifex::tag_invoke(next_fn{}, rng, cur);
    }

    /**
     * @brief default next implementation uses `inc()`
     *
     * Returns a new cursor pointing to the next element after `cur`
     *
     * @tparam Seq A `sequence` type
     * @param rng a lvalue sequence
     * @param cur the cursor pointing to the current element
     * @return a cursor pointing to the next element
     */
    template <class Seq>
    requires(not std::is_rvalue_reference_v<Seq> and
             not unifex::tag_invocable<next_fn, Seq> and std::invocable<inc_fn, Seq>)
    static constexpr auto operator()(Seq&& rng, cursor_t<Seq> cur)
        noexcept(std::is_nothrow_invocable_v<inc_fn, Seq, cursor_t<Seq>>) -> cursor_t<Seq>
    {
        return algo::inc(rng, cur);
    }

    template <class Seq>
    requires(not std::is_rvalue_reference_v<Seq> and
             unifex::tag_invocable<next_fn, Seq, distance_t>)
    static constexpr auto operator()(Seq&& rng, cursor_t<Seq> cur, distance_t offset)
        noexcept(
            unifex::is_nothrow_tag_invocable_v<next_fn, Seq, cursor_t<Seq>, distance_t>)
            -> cursor_t<Seq>
    {
        return unifex::tag_invoke(next_fn{}, rng, cur, offset);
    }

    template <class Seq>
    requires(unifex::tag_invocable<inc_fn, Seq> and
             not unifex::tag_invocable<next_fn, Seq>)
    static constexpr auto operator()(Seq& rng, cursor_t<Seq> cur, distance_t offset)
        noexcept(
            unifex::is_nothrow_tag_invocable_v<inc_fn, Seq, cursor_t<Seq>, distance_t>)
            -> cursor_t<Seq>
    {
        if constexpr (std::invocable<inc_fn, Seq, cursor_t<Seq>, distance_t>) {
            return unifex::tag_invoke(inc_fn{}, rng, cur, offset);
        }
        else if constexpr (std::invocable<inc_fn, Seq, cursor_t<Seq>> and
                           std::invocable<dec_fn, Seq, cursor_t<Seq>>) {
            if (offset > 0) {
                do { // NOLINT
                    algo::inc(rng, cur);
                } while (--offset > 0);
            }
            else {
                do { // NOLINT
                    algo::dec(rng, cur);
                } while (++offset < 0);
            }
            return cur;
        }
        else {
            while (offset-- > 0) {
                algo::inc(rng, cur);
            }
            return cur;
        }
    }
} next;

constexpr inline struct prev_fn
{
    /**
     * @brief customization point for prev(seq, cur)
     *
     * Returns a new cursor pointing to the previous element after `cur`
     *
     * @tparam Seq A `sequence` type
     * @param rng a lvalue sequence
     * @param cur the cursor pointing to the current element
     * @return a cursor pointing to the prev element
     */
    template <class Seq>
    requires(!std::is_rvalue_reference_v<Seq> and unifex::tag_invocable<prev_fn, Seq>)
    static constexpr auto operator()(Seq&& rng, cursor_t<Seq> cur)
        noexcept(unifex::is_nothrow_tag_invocable_v<prev_fn, Seq, cursor_t<Seq>>)
            -> cursor_t<Seq>
    {
        return unifex::tag_invoke(prev_fn{}, rng, cur);
    }

    /**
     * @brief default prev implementation uses `dec()`
     *
     * Returns a new cursor pointing to the previous element after `cur`
     *
     * @tparam Seq A `sequence` type
     * @param rng a lvalue sequence
     * @param cur the cursor pointing to the current element
     * @return a cursor pointing to the prev element
     */
    template <class Seq>
    requires(not std::is_rvalue_reference_v<Seq> and
             not unifex::tag_invocable<prev_fn, Seq> and std::invocable<dec_fn, Seq>)
    static constexpr auto operator()(Seq&& rng, cursor_t<Seq> cur)
        noexcept(std::is_nothrow_invocable_v<dec_fn, Seq, cursor_t<Seq>>) -> cursor_t<Seq>
    {
        return algo::dec(rng, cur);
    }

    template <class Seq>
    requires(not std::is_rvalue_reference_v<Seq> and
             unifex::tag_invocable<prev_fn, Seq, distance_t>)
    static constexpr auto operator()(Seq&& rng, cursor_t<Seq> cur, distance_t offset)
        noexcept(
            unifex::is_nothrow_tag_invocable_v<prev_fn, Seq, cursor_t<Seq>, distance_t>)
            -> cursor_t<Seq>
    {
        return unifex::tag_invoke(prev_fn{}, rng, cur, offset);
    }

    template <class Seq>
    requires(unifex::tag_invocable<inc_fn, Seq> and
             not unifex::tag_invocable<prev_fn, Seq>)
    static constexpr auto operator()(Seq& rng, cursor_t<Seq> cur, distance_t offset)
        // TODO: Fix noexcept for different cases
        noexcept(
            unifex::is_nothrow_tag_invocable_v<inc_fn, Seq, cursor_t<Seq>, distance_t>)
            -> cursor_t<Seq>
    {
        if constexpr (std::invocable<inc_fn, Seq, cursor_t<Seq>, distance_t>) {
            return algo::inc(rng, cur, offset);
        }
        else if constexpr (std::invocable<inc_fn, Seq, cursor_t<Seq>> and
                           std::invocable<dec_fn, Seq, cursor_t<Seq>>) {
            if (offset > 0) {
                do { // NOLINT
                    algo::dec(rng, cur);
                } while (--offset > 0);
            }
            else {
                do { // NOLINT
                    algo::inc(rng, cur);
                } while (++offset < 0);
            }
            return cur;
        }
        else {
            while (offset-- > 0) {
                algo::dec(rng, cur);
            }
            return cur;
        }
    }
} prev;

inline constexpr struct read_at_fn
{
    template <class Seq>
    requires(not std::is_rvalue_reference_v<Seq> and
             unifex::tag_invocable<read_at_fn, Seq, const_lvalue_t<cursor_t<Seq>>>)
    static constexpr auto operator()(Seq&& seq, const_lvalue_t<cursor_t<Seq>> cur)
        noexcept(unifex::is_nothrow_tag_invocable_v<read_at_fn,
                                                    Seq,
                                                    const_lvalue_t<cursor_t<Seq>>>)
            -> unifex::tag_invoke_result_t<read_at_fn, Seq, const_lvalue_t<cursor_t<Seq>>>
    {
        return unifex::tag_invoke(read_at_fn{}, std::forward<Seq>(seq), cur);
    }
} read_at;

inline constexpr struct move_at_fn
{
    template <class Seq>
    requires(not std::is_rvalue_reference_v<Seq> and
             unifex::tag_invocable<move_at_fn, Seq, const_lvalue_t<cursor_t<Seq>>>)
    static constexpr auto operator()(Seq&& seq, const_lvalue_t<cursor_t<Seq>> cur)
        noexcept(unifex::is_nothrow_tag_invocable_v<move_at_fn,
                                                    Seq,
                                                    const_lvalue_t<cursor_t<Seq>>>)
            -> unifex::tag_invoke_result_t<move_at_fn, Seq, const_lvalue_t<cursor_t<Seq>>>
    {
        return unifex::tag_invoke(move_at_fn{}, std::forward<Seq>(seq), cur);
    }

    template <class Seq>
    requires(not std::is_rvalue_reference_v<Seq> and
             not unifex::tag_invocable<move_at_fn, Seq, const_lvalue_t<cursor_t<Seq>>> and
             std::invocable<read_at_fn, Seq, const_lvalue_t<cursor_t<Seq>>> and
             std::is_lvalue_reference_v<element_t<Seq>>)
    static constexpr auto operator()(Seq&& seq, const_lvalue_t<cursor_t<Seq>> cur)
        noexcept(unifex::is_nothrow_tag_invocable_v<move_at_fn,
                                                    Seq,
                                                    const_lvalue_t<cursor_t<Seq>>>)
            -> rvalue_t<
                std::invoke_result_t<read_at_fn, Seq, const_lvalue_t<cursor_t<Seq>>>>
    {
        return std::move(std::invoke(read_at, std::forward<Seq>(seq), cur));
    }

    template <class Seq>
    requires(not std::is_rvalue_reference_v<Seq> and
             not unifex::tag_invocable<move_at_fn, Seq, const_lvalue_t<cursor_t<Seq>>> and
             std::invocable<read_at_fn, Seq, const_lvalue_t<cursor_t<Seq>>> and
             not std::is_lvalue_reference_v<element_t<Seq>>)
    static constexpr auto operator()(Seq&& seq, const_lvalue_t<cursor_t<Seq>> cur)
        noexcept(unifex::is_nothrow_tag_invocable_v<move_at_fn,
                                                    Seq,
                                                    const_lvalue_t<cursor_t<Seq>>>)
            -> std::invoke_result_t<read_at_fn, Seq, const_lvalue_t<cursor_t<Seq>>>
    {
        std::invoke(read_at, std::forward<Seq>(seq), cur);
    }
} move_at;

/**
 * @brief Access element at cursor without performing any safety checks.
 *
 * @param seq A `sequence` to read from
 * @param cur A cursor pointing to an element in the `sequence` seq
 * @return A lvalue reference of type `element_t<Seq>`
 */
inline constexpr struct read_at_unchecked_fn
{
    template <class Seq>
    requires(
        not std::is_rvalue_reference_v<Seq> and
        unifex::tag_invocable<read_at_unchecked_fn, Seq, const_lvalue_t<cursor_t<Seq>>>)
    static constexpr auto operator()(Seq&& seq, const_lvalue_t<cursor_t<Seq>> cur)
        noexcept(unifex::is_nothrow_tag_invocable_v<read_at_unchecked_fn,
                                                    Seq,
                                                    const_lvalue_t<cursor_t<Seq>>>)
            -> unifex::tag_invoke_result_t<read_at_unchecked_fn,
                                           Seq,
                                           const_lvalue_t<cursor_t<Seq>>>
    {
        return unifex::tag_invoke(read_at_unchecked_fn{}, std::forward<Seq>(seq), cur);
    }

    template <class Seq>
    requires(not std::is_rvalue_reference_v<Seq> and
             not unifex::
                 tag_invocable<read_at_unchecked_fn, Seq, const_lvalue_t<cursor_t<Seq>>>)
    static constexpr auto operator()(Seq&& seq, const_lvalue_t<cursor_t<Seq>> cur)
        noexcept(std::is_nothrow_move_assignable_v<value_t<Seq>> and
                 std::is_nothrow_move_constructible_v<value_t<Seq>>)
            -> decltype(std::invoke(algo::read_at, std::forward<Seq>(seq), cur))
    {
        return std::invoke(algo::read_at, std::forward<Seq>(seq), cur);
    }
} read_at_unchecked;

inline constexpr struct move_at_unchecked_fn
{
    template <class Seq>
    requires(
        not std::is_rvalue_reference_v<Seq> and
        unifex::tag_invocable<move_at_unchecked_fn, Seq, const_lvalue_t<cursor_t<Seq>>>)
    static constexpr auto operator()(Seq&& seq, const_lvalue_t<cursor_t<Seq>> cur)
        noexcept(unifex::is_nothrow_tag_invocable_v<move_at_unchecked_fn,
                                                    Seq,
                                                    const_lvalue_t<cursor_t<Seq>>>)
            -> unifex::tag_invoke_result_t<move_at_unchecked_fn,
                                           Seq,
                                           const_lvalue_t<cursor_t<Seq>>>
    {
        return unifex::tag_invoke(move_at_unchecked_fn{}, std::forward<Seq>(seq), cur);
    }

    template <class Seq>
    requires(not std::is_rvalue_reference_v<Seq> and
             not unifex::tag_invocable<move_at_unchecked_fn,
                                       Seq,
                                       const_lvalue_t<cursor_t<Seq>>> and
             std::invocable<move_at_fn, Seq, const_lvalue_t<cursor_t<Seq>>>)
    static constexpr auto operator()(Seq&& seq, const_lvalue_t<cursor_t<Seq>> cur)
        noexcept(
            std::is_nothrow_invocable_v<move_at_fn, Seq, const_lvalue_t<cursor_t<Seq>>>)
            -> std::invoke_result_t<move_at_fn, Seq, const_lvalue_t<cursor_t<Seq>>>
    {
        return std::invoke(move_at, std::forward<Seq>(seq), cur);
    }

    template <class Seq>
    requires(not std::is_rvalue_reference_v<Seq> and
             not unifex::tag_invocable<move_at_unchecked_fn,
                                       Seq,
                                       const_lvalue_t<cursor_t<Seq>>> and
             std::invocable<read_at_unchecked_fn, Seq, const_lvalue_t<cursor_t<Seq>>> and
             std::is_lvalue_reference_v<element_t<Seq>>)
    static constexpr auto operator()(Seq&& seq, const_lvalue_t<cursor_t<Seq>> cur)
        noexcept(std::is_nothrow_invocable_v<read_at_unchecked_fn,
                                             Seq,
                                             const_lvalue_t<cursor_t<Seq>>> and
                 std::is_nothrow_move_constructible_v<value_t<Seq>> and
                 std::is_nothrow_move_assignable_v<value_t<Seq>>)
            -> decltype(std::move(
                std::invoke(read_at_unchecked, std::forward<Seq>(seq), cur)))
    {
        return std::move(std::invoke(read_at_unchecked, std::forward<Seq>(seq), cur));
    }

    template <class Seq>
    requires(not std::is_rvalue_reference_v<Seq> and
             not unifex::tag_invocable<move_at_unchecked_fn,
                                       Seq,
                                       const_lvalue_t<cursor_t<Seq>>> and
             std::invocable<read_at_unchecked_fn, Seq, const_lvalue_t<cursor_t<Seq>>> and
             not std::is_lvalue_reference_v<element_t<Seq>>)
    static constexpr auto operator()(
        Seq&& seq,
        const_lvalue_t<cursor_t<Seq>>
            cur) noexcept(std::is_nothrow_invocable_v<read_at_unchecked_fn,
                                                      Seq,
                                                      const_lvalue_t<cursor_t<Seq>>>)
        -> std::invoke_result_t<read_at_unchecked_fn, Seq, const_lvalue_t<cursor_t<Seq>>>
    {
        return std::invoke(read_at_unchecked, std::forward<Seq>(seq), cur);
    }
} move_at_unchecked;

constexpr inline struct swap_with_fn
{
    template <class Seq1, class Seq2>
    requires(not(std::is_rvalue_reference_v<Seq1> or std::is_rvalue_reference_v<Seq2>) and
             std::swappable_with<lvalue_t<element_t<Seq1>>, lvalue_t<element_t<Seq2>>> and
             std::invocable<read_at_fn, Seq1, const_lvalue_t<cursor_t<Seq1>>> and
             std::invocable<read_at_fn, Seq2, const_lvalue_t<cursor_t<Seq2>>>)
    static constexpr void operator()(Seq1&& seq1,
                                     const_lvalue_t<cursor_t<Seq1>> cur1,
                                     Seq2&& seq2,
                                     const_lvalue_t<cursor_t<Seq2>> cur2)
        noexcept(std::is_nothrow_swappable_with_v<lvalue_t<element_t<Seq1>>,
                                                  lvalue_t<element_t<Seq2>>>)
    {
        using std::swap;
        swap(algo::read_at(seq1, cur1), algo::read_at(seq2, cur2));
    }
} swap_with;

constexpr inline struct swap_at_fn
{
} swap_at;

constexpr inline struct data_fn
{
private:
    template <class Seq>
    using pointer_result_t = std::add_pointer_t<std::remove_reference_t<element_t<Seq>>>;

public:
    template <class Seq>
    requires(not std::is_rvalue_reference_v<Seq> and
             unifex::tag_invocable<data_fn, Seq> and
             std::convertible_to<unifex::tag_invoke_result_t<data_fn, Seq>,
                                 pointer_result_t<Seq>>)
    static constexpr auto operator()(Seq&& seq)
        noexcept(unifex::is_nothrow_tag_invocable_v<data_fn, Seq>)
            -> pointer_result_t<Seq>
    {
        return unifex::tag_invoke(data_fn{}, std::forward<Seq>(seq));
    }
} data;

constexpr inline struct front_fn
{
    template <class Seq>
    requires(not std::is_rvalue_reference_v<Seq> and
             std::invocable<is_last_fn, Seq, cursor_t<Seq>> and
             std::invocable<read_at_fn, Seq, cursor_t<Seq>>)
    [[nodiscard]]
    static constexpr auto operator()(Seq&& seq)
        noexcept(std::is_nothrow_invocable_v<first_fn, Seq>)
            -> std::optional<element_t<Seq>>
    {
        std::optional<element_t<Seq>> ret;
        auto cur = algo::first(seq);
        if (!algo::is_last(seq, cur)) {
            ret.emplace(algo::read_at(seq, cur));
        }
        return ret;
    }
} front;

constexpr inline struct back_fn
{
    template <class Seq>
    requires(not std::is_rvalue_reference_v<Seq> and std::invocable<terminal_fn, Seq> and
             std::invocable<first_fn, Seq> and
             std::invocable<read_at_fn, Seq, cursor_t<Seq>> and
             std::invocable<dec_fn, Seq, cursor_t<Seq>>)
    static constexpr auto operator()(Seq&& seq)
        noexcept(std::is_nothrow_invocable_v<terminal_fn, Seq> and
                 std::is_nothrow_invocable_v<first_fn, Seq> and
                 std::is_nothrow_invocable_v<read_at_fn, Seq, cursor_t<Seq>> and
                 std::is_nothrow_invocable_v<dec_fn, Seq, cursor_t<Seq>>)

            -> std::optional<element_t<Seq>>
    {
        std::optional<element_t<Seq>> ret;
        auto cur = algo::terminal(seq);
        if (cur != algo::first(seq)) {
            ret.emplace(algo::read_at(seq, algo::dec(seq, cur)));
        }
        return ret;
    }
} back;

} // namespace algo
