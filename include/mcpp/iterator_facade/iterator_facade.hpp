// Copyright Mika Fischer 2021.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

#pragma once

#include "mcpp/iterator_facade/detail/type_traits.hpp"
#include <iterator>
#include <type_traits>

namespace mcpp::iterator_facade {
namespace detail {

template <typename T>
using dereference_t = decltype(std::declval<const T &>().dereference());
template <typename T>
using equal_to_t = decltype(std::declval<const T &>().equal_to(std::declval<const T &>()));
template <typename T>
using increment_t = decltype(std::declval<T &>().increment());
template <typename T>
using decrement_t = decltype(std::declval<T &>().decrement());
template <typename T>
using advance_t = decltype(std::declval<T &>().advance(std::declval<int>()));
template <typename T>
using distance_to_t = decltype(std::declval<const T &>().distance_to(std::declval<const T &>()));
template <typename T>
using is_at_end_t = decltype(std::declval<const T &>().is_at_end());
template <typename T>
using distance_to_end_t = decltype(std::declval<const T &>().distance_to_end());
template <typename T>
using value_type_t = typename T::value_type;
template <typename T>
using iterator_category_t = typename T::iterator_category;

template <typename T>
constexpr bool has_dereference = is_detected_v<dereference_t, T>;
template <typename T>
constexpr bool has_equal_to = is_detected_exact_v<bool, equal_to_t, T>;
template <typename T>
constexpr bool has_increment = is_detected_v<increment_t, T>;
template <typename T>
constexpr bool has_decrement = is_detected_v<decrement_t, T>;
template <typename T>
constexpr bool has_advance = is_detected_v<advance_t, T>;
template <typename T>
constexpr bool has_distance_to = is_detected_v<distance_to_t, T>;
template <typename T>
constexpr bool has_is_at_end = is_detected_v<is_at_end_t, T>;
template <typename T>
constexpr bool has_distance_to_end = is_detected_v<distance_to_end_t, T>;

template <typename T>
constexpr bool is_random_access = has_advance<T> &&has_distance_to<T>;
template <typename T>
constexpr bool is_bidirectional = is_random_access<T> || has_decrement<T>;

template <typename T>
constexpr auto iterator_category() {
    if constexpr (is_random_access<T>) {
        return std::random_access_iterator_tag{};
    } else if constexpr (is_bidirectional<T>) {
        return std::bidirectional_iterator_tag{};
    } else {
        return std::forward_iterator_tag{};
    }
}

template <class Reference>
struct arrow_proxy {
    Reference r;
    auto operator->() -> Reference * { return &r; }
};

template <typename State>
constexpr auto pointer_dereference(const State &state) {
    if constexpr (std::is_reference_v<decltype(state.dereference())>) {
        return std::addressof(state.dereference());
    } else {
        return detail::arrow_proxy<decltype(state.dereference())>{state.dereference()};
    }
}

template <typename Iter, typename State>
struct unsized_sentinel_interface {
    // TODO: Let State override sentinel_type
    struct sentinel_type {};
    static constexpr auto sentinel() noexcept -> sentinel_type { return {}; }

    friend auto operator==(const Iter &it, sentinel_type /*unused*/) -> bool {
        if constexpr (has_is_at_end<State>) {
            return it.state().is_at_end();
        } else {
            return it.state().distance_to_end() == 0;
        }
    }
    friend auto operator!=(const Iter &it, sentinel_type sentinel) -> bool { return !(it == sentinel); }
    friend auto operator==(sentinel_type sentinel, const Iter &it) -> bool { return it == sentinel; }
    friend auto operator!=(sentinel_type sentinel, const Iter &it) -> bool { return !(it == sentinel); }
};

template <typename Iter, typename State>
struct sized_sentinel_interface : unsized_sentinel_interface<Iter, State> {
    using sentinel_type = typename unsized_sentinel_interface<Iter, State>::sentinel_type;
    friend auto operator-(sentinel_type /*unused*/, const Iter &it) { return it.state().distance_to_end(); }
};

struct no_sentinel_interface {};

template <typename Iter, typename State, typename enable = void>
using sentinel_interface = std::conditional_t<
    has_distance_to_end<State>, sized_sentinel_interface<Iter, State>,
    std::conditional_t<has_is_at_end<State>, unsized_sentinel_interface<Iter, State>, no_sentinel_interface>>;

} // namespace detail

template <typename State>
class iterator_facade : public detail::sentinel_interface<iterator_facade<State>, State> {
  private:
    static_assert(detail::has_dereference<State>, "Need .dereference()");
    static_assert(detail::has_increment<State> || detail::has_advance<State>,
                  "Need .increment() or .advance(integral_type)");

    State state_;

  public:
    // TODO: Not so nice that this is public
    auto state() -> State & { return state_; }
    auto state() const -> const State & { return state_; }

    using reference = detail::dereference_t<State>;
    using value_type = detected_or_t<std::remove_cv_t<std::remove_reference_t<reference>>, detail::value_type_t, State>;
    using pointer = decltype(detail::pointer_dereference(std::declval<State>()));
    using difference_type = detected_or_t<std::ptrdiff_t, detail::distance_to_t, State>;
    using iterator_category =
        detected_or_t<decltype(detail::iterator_category<State>()), detail::iterator_category_t, State>;
    using iterator_concept = iterator_category;

    template <typename... Args,
              std::enable_if_t<std::is_constructible_v<State, Args...> && !std::is_aggregate_v<State>, int> = 0>
    explicit iterator_facade(Args &&...args) : state_(std::forward<Args>(args)...) {}

    template <typename... Args,
              std::enable_if_t<is_direct_list_initializable_v<State, Args...> && std::is_aggregate_v<State>, int> = 0>
    explicit iterator_facade(Args &&...args) : state_{std::forward<Args>(args)...} {}

    auto operator*() const -> reference { return state_.dereference(); }

    auto operator->() const -> pointer { return detail::pointer_dereference(state_); }

    template <typename T = State, std::enable_if_t<detail::has_equal_to<T> || detail::has_distance_to<T>, int> = 0>
    friend auto operator==(const iterator_facade &left, const iterator_facade &right) -> bool {
        if constexpr (detail::has_equal_to<State>) {
            return left.state_.equal_to(right.state_);
        } else {
            static_assert(detail::has_distance_to<State>, "Need .distance_to() or .equal_to()");
            return left.state_.distance_to(right.state_) == 0;
        }
    }

    template <typename T = State, std::enable_if_t<detail::has_equal_to<T> || detail::has_distance_to<T>, int> = 0>
    friend auto operator!=(const iterator_facade &left, const iterator_facade &right) -> bool {
        return !(left == right);
    }

    auto operator++() -> iterator_facade & {
        if constexpr (detail::has_increment<State>) {
            state_.increment();
        } else {
            static_assert(detail::has_advance<State>, "Need .advance() or .increment()");
            state_.advance(1);
        }
        return *this;
    }

    auto operator++(int)
        -> std::conditional_t<std::is_same_v<iterator_category, std::input_iterator_tag>, void, iterator_facade> {
        if constexpr (std::is_same_v<iterator_category, std::input_iterator_tag>) {
            return operator++();
        } else {
            auto copy = *this;
            operator++();
            return copy;
        }
    }

    template <typename T = State, typename = std::enable_if_t<detail::is_bidirectional<T>>>
    auto operator--() -> iterator_facade {
        if constexpr (detail::has_decrement<State>) {
            this->state().decrement();
        } else {
            static_assert(detail::has_advance<State>, "Need .advance() or .decrement()");
            this->state().advance(-1);
        }
        return *this;
    }

    template <typename T = State, typename = std::enable_if_t<detail::is_bidirectional<T>>>
    auto operator--(int)
        -> std::conditional_t<std::is_same_v<iterator_category, std::input_iterator_tag>, void, iterator_facade> {
        if constexpr (std::is_same_v<iterator_category, std::input_iterator_tag>) {
            return operator--();
        } else {
            auto copy = *this;
            operator--();
            return copy;
        }
    }

    template <typename T = State, std::enable_if_t<detail::is_random_access<T>, int> = 0>
    auto operator+=(difference_type n) -> iterator_facade & {
        state().advance(n);
        return *this;
    }

    template <typename T = State, std::enable_if_t<detail::is_random_access<T>, int> = 0>
    friend auto operator+(iterator_facade a, difference_type n) -> iterator_facade {
        return a += n;
    }

    template <typename T = State, std::enable_if_t<detail::is_random_access<T>, int> = 0>
    friend auto operator+(difference_type n, iterator_facade a) -> iterator_facade {
        return a += n;
    }

    template <typename T = State, std::enable_if_t<detail::is_random_access<T>, int> = 0>
    auto operator-=(difference_type n) -> iterator_facade & {
        state().advance(-n);
        return *this;
    }

    template <typename T = State, std::enable_if_t<detail::is_random_access<T>, int> = 0>
    friend auto operator-(iterator_facade i, difference_type n) -> iterator_facade {
        return i -= n;
    }

    template <typename T = State, std::enable_if_t<detail::is_random_access<T>, int> = 0>
    friend auto operator-(const iterator_facade &b, const iterator_facade &a) -> difference_type {
        return a.distance_to(b);
    }

    template <typename T = State, std::enable_if_t<detail::is_random_access<T>, int> = 0>
    auto operator[](difference_type n) -> reference {
        return *(*this + n);
    }

    template <typename T = State, std::enable_if_t<detail::is_random_access<T>, int> = 0>
    friend auto operator<(const iterator_facade &a, const iterator_facade &b) -> bool {
        return a.distance_to(b) > 0;
    }

    template <typename T = State, std::enable_if_t<detail::is_random_access<T>, int> = 0>
    friend auto operator>(const iterator_facade &a, const iterator_facade &b) -> bool {
        return b < a;
    }

    template <typename T = State, std::enable_if_t<detail::is_random_access<T>, int> = 0>
    friend auto operator>=(const iterator_facade &a, const iterator_facade &b) -> bool {
        return !(a < b);
    }

    template <typename T = State, std::enable_if_t<detail::is_random_access<T>, int> = 0>
    friend auto operator<=(const iterator_facade &a, const iterator_facade &b) -> bool {
        return !(a > b);
    }
};

} // namespace mcpp::iterator_facade