/**
 * Copyright (C) 2023  Todd W. Fulton
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as
 *published by the Free Software Foundation, either version 3 of the
 *License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *License along with this program.  If not, see
 *<https://www.gnu.org/licenses/>.
 **/
#pragma once

#include <range/v3/iterator/concepts.hpp>
#include <range/v3/view/adaptor.hpp>

namespace algo
{

template <class Range, class Relation, class Projection>
requires(ranges::viewable_range<Range> and
         ranges::indirect_strict_weak_order<
             Relation,
             ranges::projected<ranges::iterator_t<Range>, Projection>,
             ranges::projected<ranges::iterator_t<Range>, Projection>>)
class sorted_view : public ranges::view_adaptor<sorted_view<Range, Relation, Projection>, Range>
{
    friend ranges::range_access;
    [[no_unique_address]] ranges::semiregular_box_t<Relation> relation_;
    [[no_unique_address]] ranges::semiregular_box_t<Projection> projection_;

    class adapter : public ranges::adaptor_base
    {
    public:
        constexpr adapter() = default;
    };

    constexpr adapter begin_adapter() const
    {
        return {};
    }
    constexpr adapter end_adapter() const
    {
        return {};
    }

public:
    constexpr sorted_view() = default;

    template <class Range2, class Relation2, class Projection2 = ranges::identity>
    requires(std::convertible_to<Range2, Range> and
             std::convertible_to<Relation2, Relation> and
             std::convertible_to<Projection2, Projection>)
    constexpr sorted_view(Range2&& rng,
                          Relation2&& relation,
                          Projection2&& projection = {})
        // TODO: noexcept
        : sorted_view<Range, Relation, Projection>::view_adaptor{std::forward<Range2>(
              rng)}
        , relation_{std::forward<Relation2>(relation)}
        , projection_{std::forward<Projection2>(projection)}
    {
    }

    template <class Itr,
              class Sentinel,
              class Relation2,
              class Projection2 = ranges::identity>
    requires(std::constructible_from<Range, Itr, Sentinel> and
             ranges::sentinel_for<std::remove_cvref_t<Sentinel>,
                                  std::remove_cvref_t<Itr>> and
             std::convertible_to<Relation2, Relation> and
             std::convertible_to<Projection2, Projection>)
    constexpr sorted_view(Itr&& first,
                          Sentinel&& last,
                          Relation2&& relation,
                          Projection2&& projection = {})
        // TODO: noexcept
        : sorted_view<Range, Relation, Projection>::view_adaptor{Range{
              std::forward<Itr>(first), std::forward<Sentinel>(last)}}
        , relation_{std::forward<Relation2>(relation)}
        , projection_{std::forward<Projection2>(projection)}
    {
    }

    constexpr auto&& relation(this auto&& self)
    {
        return std::forward_like<decltype(self)>(self.relation_);
    }

    constexpr auto&& projection(this auto&& self)
    {
        return std::forward_like<decltype(self)>(self.projection_);
    }

    constexpr auto operator==(auto const& other) const
        noexcept(noexcept(this->base() == other)) -> decltype(this->base() == other)
    {
        return this->base() == other;
    }

    template <class Other>
    requires requires(sorted_view self) {
        { self.base().base() } -> std::convertible_to<Range>;
    }
    constexpr auto operator==(Other const& other) const
        noexcept(noexcept(this->base().base() == other))
    {
        return this->base().base() == other;
    }

    constexpr auto operator==(auto const& other) const
        noexcept(noexcept(this->base() == other.base())) -> decltype(this->base() ==
                                                                     other.base())
    {
        return this->base() == other.base();
    }
};

template <class Range, class Relation, class Projection>
requires(not ranges::viewable_range<Range> and ranges::range<Range> and
         ranges::indirect_strict_weak_order<
             Relation,
             ranges::projected<ranges::iterator_t<Range>, Projection>,
             ranges::projected<ranges::iterator_t<Range>, Projection>>)
class sorted_range
{
    [[no_unique_address]] Range range_;
    [[no_unique_address]] ranges::semiregular_box_t<Relation> relation_;
    [[no_unique_address]] ranges::semiregular_box_t<Projection> projection_;

public:
    template <class Range2, class Relation2, class Projection2>
    requires(ranges::convertible_to<Range2, Range> and
             ranges::convertible_to<Relation2, Relation> and
             ranges::convertible_to<Projection2, Projection>)
    constexpr sorted_range(Range2&& range, Relation2&& relation, Projection2&& projection)
        : range_{std::forward<Range2>(range)}
        , relation_{std::forward<Relation2>(relation)}
        , projection_{std::forward<Projection2>(projection)}
    {
    }

    constexpr auto&& base(this auto&& self)
    {
        return std::forward_like<decltype(self)>(self.range_);
    }

    constexpr auto&& relation(this auto&& self)
    {
        return std::forward_like<decltype(self)>(self.relation_);
    }

    constexpr auto&& projection(this auto&& self)
    {
        return std::forward_like<decltype(self)>(self.projection_);
    }

    constexpr auto operator[](this auto&& self, auto&& idx) noexcept(noexcept(
        std::forward_like<decltype(self)>(self.range_)[std::forward<decltype(idx)>(idx)]))
        -> decltype(std::forward_like<decltype(self)>(
            self.range_)[std::forward<decltype(idx)>(idx)])
    {
        return std::forward_like<decltype(self)>(
            self.range_)[std::forward<decltype(idx)>(idx)];
    }

    constexpr auto begin(this auto&& self)
        noexcept(noexcept(std::forward_like<decltype(self)>(self.range_).begin()))
            -> decltype(std::forward_like<decltype(self)>(self.range_).begin())
    {
        return std::forward_like<decltype(self)>(self.range_).begin();
    }

    constexpr auto end(this auto&& self)
        noexcept(noexcept(std::forward_like<decltype(self)>(self.range_).end()))
            -> decltype(std::forward_like<decltype(self)>(self.range_).end())
    {
        return std::forward_like<decltype(self)>(self.range_).end();
    }

    constexpr auto size(this auto&& self)
        noexcept(noexcept(std::forward_like<decltype(self)>(self.range_).size()))
            -> decltype(std::forward_like<decltype(self)>(self.range_).size())
    {
        return std::forward_like<decltype(self)>(self.range_).size();
    }

    constexpr auto operator==(auto const& other) const
        noexcept(noexcept(this->range_ == other)) -> decltype(this->range_ == other)
    {
        return this->range_ == other;
    }

    constexpr auto operator==(auto const& other) const
        noexcept(noexcept(this->range_ == other.range_)) -> decltype(this->range_ ==
                                                                     other.range_)
    {
        return this->range_ == other.range_;
    }

private:
    friend constexpr auto begin(sorted_range& self) noexcept(
        noexcept(ranges::begin(self.range_))) -> decltype(ranges::begin(self.range_))
    {
        return ranges::begin(self.range_);
    }

    friend constexpr auto end(sorted_range& self)
        noexcept(noexcept(ranges::end(self.range_))) -> decltype(ranges::end(self.range_))
    {
        return ranges::end(self.range_);
    }

    friend constexpr auto begin(sorted_range const& self) noexcept(
        noexcept(ranges::begin(self.range_))) -> decltype(ranges::begin(self.range_))
    {
        return ranges::begin(self.range_);
    }

    friend constexpr auto end(sorted_range const& self)
        noexcept(noexcept(ranges::end(self.range_))) -> decltype(ranges::end(self.range_))
    {
        return ranges::end(self.range_);
    }

    friend constexpr auto size(sorted_range const& self)
        noexcept(noexcept(ranges::size(self.range_)))
            -> decltype(ranges::size(self.range_))
    requires(ranges::sized_range<Range>)
    {
        return ranges::size(self.range_);
    }
};

template <class... Args>
struct _sorted;

template <class Range, class Relation, class Projection>
requires(ranges::viewable_range<Range> and
         ranges::indirect_strict_weak_order<
             Relation,
             ranges::projected<ranges::iterator_t<Range>, Projection>,
             ranges::projected<ranges::iterator_t<Range>, Projection>>)
struct _sorted<Range, Relation, Projection>
{
    using type = sorted_view<Range, std::decay_t<Relation>, std::decay_t<Projection>>;
};

template <class Range, class Relation, class Projection>
requires(not ranges::viewable_range<Range> and ranges::range<Range> and
         ranges::indirect_strict_weak_order<
             Relation,
             ranges::projected<ranges::iterator_t<Range>, Projection>,
             ranges::projected<ranges::iterator_t<Range>, Projection>>)
struct _sorted<Range, Relation, Projection>
{
    using type = sorted_range<std::remove_cvref_t<Range>,
                              std::decay_t<Relation>,
                              std::decay_t<Projection>>;
};

template <class Itr, class Sentinel, class Relation, class Projection>
requires(ranges::sized_sentinel_for<std::remove_cvref_t<Sentinel>,
                                    std::remove_cvref_t<Itr>> and
         ranges::indirect_strict_weak_order<Relation,
                                            ranges::projected<Itr, Projection>,
                                            ranges::projected<Itr, Projection>>)
struct _sorted<Itr, Sentinel, Relation, Projection>
{
    using type = sorted_view<
        ranges::subrange<std::remove_cvref_t<Itr>, std::remove_cvref_t<Sentinel>>,
        std::decay_t<Relation>,
        std::decay_t<Projection>>;
};

template <class... Args>
using sorted = typename _sorted<Args...>::type;

template <class T>
static inline constexpr bool is_sorted_v = false;

template <class Range, class Relation, class Projection>
static inline constexpr bool is_sorted_v<sorted_view<Range, Relation, Projection>> = true;

template <class Range, class Relation, class Projection>
static inline constexpr bool is_sorted_v<sorted_range<Range, Relation, Projection>> =
    true;

template <class T>
concept Sorted = is_sorted_v<std::remove_cvref_t<T>>;

} // namespace algo
