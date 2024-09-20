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
// For some reason this prevents a crash in clang-p2996
namespace algo
{
}

#include <algo/meta/packs/take.hpp>
#include <algo/meta/traits/function_traits.hpp>

#include <functional>
#include <type_traits>

namespace algo
{
inline constexpr struct fix_fn
{
    template <Callable Gn, class... Args>
    requires(not unifex::tag_invocable<fix_fn, Gn, Args...> and NotOverloaded<Gn> and
             fixable_with_v<Gn, Args...>)
    static constexpr auto operator()(Gn&& gn, Args&&... args)
        noexcept((InvokableWith<Gn, Args...> and
                  std::is_nothrow_invocable_v<Gn, Args...>) or
                 (true and ... and
                  std::conditional_t<std::is_rvalue_reference_v<Args>,
                                     std::is_nothrow_move_assignable<Args>,
                                     std::is_nothrow_copy_assignable<Args>>::value))
    {
        if constexpr (InvokableWith<Gn, Args...>) {
            return std::invoke(std::forward<Gn>(gn), std::forward<Args>(args)...);
        }
        else {
            using narg_types =
                meta::drop_t<sizeof...(Args), typename function_traits<Gn>::param_types>;

            return []<class... NArgs>(
                       [[maybe_unused]] meta::TypeList<NArgs...> /*unused*/,
                       Gn&& gn,
                       Args&&... args) {
                if constexpr (function_traits<Gn>::is_const and
                              InvokableWith<Gn,
                                            std::remove_reference_t<Args> const&...,
                                            NArgs...>) {
                    return
                        [gn = std::forward<Gn>(gn), ... args = std::forward<Args>(args)](
                            NArgs... nargs) noexcept(noexcept(gn(std::
                                                                     declval<
                                                                         decltype(args)>()...,
                                                                 std::declval<
                                                                     decltype(nargs)>()...)))
                            -> typename function_traits<Gn>::return_type {
                            return gn(args..., std::forward<NArgs>(nargs)...);
                        };
                }
                else {
                    return
                        [gn = std::forward<Gn>(gn), ... args = std::forward<Args>(args)](
                            NArgs... nargs) mutable noexcept(noexcept(gn(std::
                                                                             declval<
                                                                                 decltype(args)>()...,
                                                                         std::declval<
                                                                             decltype(nargs)>()...)))
                            -> typename function_traits<Gn>::return_type {
                            return gn(args..., std::forward<NArgs>(nargs)...);
                        };
                }
            }(narg_types{}, std::forward<Gn>(gn), std::forward<Args>(args)...);
        }
    }

    // For generic and overloaded function objects
    template <Callable Gn, class... Args>
    requires(not unifex::tag_invocable<fix_fn, Gn, Args...> and Overloaded<Gn> and
             fixable_with_v<Gn, Args...>)
    static constexpr auto operator()(Gn&& gn, Args&&... args)
        noexcept((InvokableWith<Gn, Args...> and
                  std::is_nothrow_invocable_v<Gn, Args...>) or
                 (true and ... and
                  std::conditional_t<std::is_rvalue_reference_v<Args>,
                                     std::is_nothrow_move_assignable<Args>,
                                     std::is_nothrow_copy_assignable<Args>>::value))
    {
        if constexpr (InvokableWith<Gn, Args...>) {
            return std::invoke(std::forward<Gn>(gn), std::forward<Args>(args)...);
        }
        else {
            return [gn = std::forward<Gn>(gn), ... args = std::forward<Args>()](
                       auto&&... nargs) mutable noexcept(std::
                                                             is_nothrow_invocable_v<
                                                                 decltype(gn),
                                                                 decltype(args)...,
                                                                 decltype(nargs)...>)
                       -> std::invoke_result_t<decltype(gn),
                                               decltype(args)...,
                                               decltype(nargs)...> {
                return std::invoke(gn, args..., std::forward<decltype(nargs)>(nargs)...);
            };
        }
    }

    // Customization point
    template <class Fn, class... Args>
    requires(unifex::tag_invocable<fix_fn, Fn, Args...>)
    static constexpr auto operator()(Fn&& fn, Args&&... args)
        noexcept(unifex::is_nothrow_tag_invocable_v<fix_fn, Fn, Args...>)
            -> unifex::tag_invoke_result_t<fix_fn, Fn, Args...>
    {
        return unifex::tag_invoke(
            fix_fn{}, std::forward<Fn>(fn), std::forward<Args>(args)...);
    }
} fix;

template <class F, class... Args>
using fixed_type_t = callable_traits<std::invoke_result_t<fix_fn, F, Args...>>::type;

template <class F, class G>
concept Composable = Callable<F> and Callable<G> and requires(return_type_t<F> r, G g) {
    { fix(g, static_cast<return_type_t<F>>(r)) };
};

inline constexpr struct compose_fn
{
    template <Callable F, Callable G>
    requires(not unifex::tag_invocable<compose_fn, F, G> and NotOverloaded<F> and
             NotOverloaded<G> and Composable<F, G>)
    static constexpr auto operator()(F&& f, G&& g)
    {
        // TODO: Return typed Callable
        using f_traits = function_traits<F>;
        using g_traits = function_traits<G>;
        return []<class... FArgs, class... GArgs>(meta::TypeList<FArgs...> /*unused*/,
                                                  meta::TypeList<GArgs...> /*unused*/,
                                                  Callable auto&& f,
                                                  Callable auto&& g) {
            if constexpr (not(f_traits::is_const and g_traits::is_const)) {
                return [f = std::forward<F>(f),
                        g = std::forward<G>(g)](FArgs... fargs, GArgs... gargs) mutable //
                    noexcept(noexcept(
                        std::invoke(g,
                                    std::invoke(f, std::forward<FArgs>(fargs)...),
                                    std::forward<GArgs>(gargs)...))) {
                        return std::invoke(g,
                                           std::invoke(f, std::forward<FArgs>(fargs)...),
                                           std::forward<GArgs>(gargs)...);
                    };
            }
            else {
                return [f = std::forward<F>(f), g = std::forward<G>(g)](FArgs... fargs,
                                                                        GArgs... gargs) //
                    noexcept(noexcept(
                        std::invoke(g,
                                    std::invoke(f, std::forward<FArgs>(fargs)...),
                                    std::forward<GArgs>(gargs)...))) {
                        return std::invoke(g,
                                           std::invoke(f, std::forward<FArgs>(fargs)...),
                                           std::forward<GArgs>(gargs)...);
                    };
            }
        }(typename f_traits::param_types{},
               meta::drop_t<1, typename g_traits::param_types>{},
               std::forward<F>(f),
               std::forward<G>(g));
    }

    template <class F, class G>
    requires(unifex::tag_invocable<compose_fn, F, G>)
    static constexpr auto operator()(F&& f, G&& g)
        noexcept(unifex::is_nothrow_tag_invocable_v<compose_fn, F, G>)
            -> unifex::tag_invoke_result_t<compose_fn, F, G>
    {
        return unifex::tag_invoke(compose_fn{}, std::forward<F>(f), std::forward<G>(g));
    }

} compose;

template <class F, class G>
using composed_type_t = callable_traits<std::invoke_result_t<compose_fn, F, G>>::type;



template <meta::TypeClass R, class Args>
struct GenericFunctionHelper;

template <meta::TypeClass Ret, meta::TypeClass... Args>
struct GenericFunctionHelper<Ret, meta::TypeList<Args...>>
{
    struct traits
    {
    };
};

// Ret, Args are MetaTypes
template <Callable Mf>
struct MakeGenericFunction
{
    struct traits : GenericFunctionHelper<return_type_t<Mf>, param_types_t<Mf>>
    {
        static constexpr bool is_const = function_traits<Mf>::is_const;
        static constexpr bool is_volatile = function_traits<Mf>::is_volatile;
        static constexpr bool is_rvalue = function_traits<Mf>::is_rvalue;
        static constexpr bool is_lvalue = function_traits<Mf>::is_lvalue;
        static constexpr bool is_noexcept = function_traits<Mf>::is_noexcept;
        static constexpr bool is_member_function = false;
        using type = Mf;
    };

    template <Overloaded Fn>
    struct type : Fn
    {
        using Fn::Fn;

        template <class... VArgs>
        constexpr auto operator()(this auto&& self, VArgs&&... uargs)
            noexcept(std::is_nothrow_invocable_v<Fn, VArgs...>)
        requires(std::invocable<Fn, VArgs...> and
                 (not is_noexcept_v<Mf> or std::is_nothrow_invocable_v<Fn, VArgs...>) and
                 traits::template args_valid_v<VArgs...> and
                 traits::template ret_valid_v<std::invoke_result_t<Fn, VArgs...>>)
        {
            return std::invoke(static_cast<Fn>(self), std::forward<VArgs>(uargs)...);
        }

    private:
        constexpr friend auto tag_invoke(
            [[maybe_unused]] get_callable_traits_fn tag,
            [[maybe_unused]] std::type_identity<type> ty) noexcept -> traits
        {
            return {};
        }
    };
};

} // namespace algo

#ifdef ALGO_TESTING
namespace algo::_function::_tests
{
constexpr void test_func_void_int([[maybe_unused]] int x)
{
}

constexpr auto test_generic_lamda = []([[maybe_unused]] auto&&... args) {};

using test_func_ptr = void (*)(int);
using test_func_ptr_ptr = test_func_ptr*;

static_assert(Callable<decltype(test_func_void_int)>);
static_assert(Callable<test_func_ptr>);
static_assert(Callable<decltype([](auto) {})>);

static_assert(not Callable<test_func_ptr_ptr>);

static_assert(ConstCallable<test_func_ptr, int>);
static_assert(ConstCallable<decltype(test_func_void_int), int>);
static_assert(ConstCallable<void(int), int>);
static_assert(ConstCallable<decltype([](int) {}), int>);

static_assert(not ConstCallable<test_func_ptr_ptr, int>);
static_assert(not ConstCallable<decltype([x = 1](int) mutable {
                                    ++x;
                                    return x;
                                }),
                                int>);
} // namespace algo::_function::_tests
#endif
