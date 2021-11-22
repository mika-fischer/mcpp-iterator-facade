// Copyright Mika Fischer 2021.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

#pragma once

#include <type_traits>

namespace mcpp {
// Detector idiom /////////////////////////////////////////////////////////
// See https://en.cppreference.com/w/cpp/experimental/is_detected
namespace detail {
template <class Default, class AlwaysVoid, template <class...> class Op, class... Args>
struct detector {
    using value_t = std::false_type;
    using type = Default;
};

template <class Default, template <class...> class Op, class... Args>
struct detector<Default, std::void_t<Op<Args...>>, Op, Args...> {
    using value_t = std::true_type;
    using type = Op<Args...>;
};
} // namespace detail

struct nonesuch {
    ~nonesuch() = delete;
    nonesuch(nonesuch const &) = delete;
    void operator=(nonesuch const &) = delete;
    nonesuch(nonesuch &&) = delete;
    void operator=(nonesuch &&) = delete;
};

template <class Default, template <class...> class Op, class... Args>
using detected_or = detail::detector<Default, void, Op, Args...>;

template <template <class...> class Op, class... Args>
using is_detected = typename detected_or<nonesuch, Op, Args...>::value_t;

template <template <class...> class Op, class... Args>
using detected_t = typename detected_or<nonesuch, Op, Args...>::type;

// Additional utilities

template <template <class...> class Op, class... Args>
constexpr inline bool is_detected_v = is_detected<Op, Args...>::value;

template <class Default, template <class...> class Op, class... Args>
using detected_or_t = typename detected_or<Default, Op, Args...>::type;

template <class Expected, template <class...> class Op, class... Args>
using is_detected_exact = std::is_same<Expected, detected_t<Op, Args...>>;

template <class Expected, template <class...> class Op, class... Args>
constexpr inline bool is_detected_exact_v = is_detected_exact<Expected, Op, Args...>::value;

template <class To, template <class...> class Op, class... Args>
using is_detected_convertible = std::is_convertible<detected_t<Op, Args...>, To>;

template <class To, template <class...> class Op, class... Args>
constexpr inline bool is_detected_convertible_v = is_detected_convertible<To, Op, Args...>::value;

// End of detector idiom

template <typename Struct, typename... Args>
using is_direct_list_initializable_impl = decltype(Struct{std::declval<Args>()...});

template <typename Struct, typename... Args>
using is_direct_list_initializable = is_detected<is_direct_list_initializable_impl, Struct, Args...>;

template <typename Struct, typename... Args>
constexpr bool is_direct_list_initializable_v = is_direct_list_initializable<Struct, Args...>::value;

template <typename>
constexpr inline bool dependent_false_v = false;

} // namespace mcpp