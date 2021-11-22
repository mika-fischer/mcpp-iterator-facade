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

template <typename State>
struct iterator_traits {
    static_assert(has_dereference<State>, "Iterator state needs 'auto dereference() const -> reference' member");

    using reference = dereference_t<State>;
    using value_type = detected_or_t<std::remove_cv_t<std::remove_reference_t<reference>>, value_type_t, State>;
    using pointer = decltype(pointer_dereference(std::declval<State>()));
    using difference_type = detected_or_t<std::ptrdiff_t, distance_to_t, State>;
    using iterator_category = detected_or_t<decltype(iterator_category<State>()), iterator_category_t, State>;
    using iterator_concept = iterator_category;
};

template <typename Iter, typename State>
struct iterator_state_access {
    static auto state(Iter &iter) -> State & { return iter.state_; }
    static auto state(const Iter &iter) -> const State & { return iter.state_; }
};

template <typename Iter, typename State, bool = has_is_at_end<State>, bool = has_distance_to_end<State>>
struct sentinel_interface {};

template <typename Iter, typename State>
struct sentinel_interface<Iter, State, true, false> {
    // TODO: Let State override sentinel_type
    struct sentinel_type {};
    static constexpr auto sentinel() noexcept -> sentinel_type { return {}; }
    static auto state(const Iter &iter) -> const State & { return iterator_state_access<Iter, State>::state(iter); }

    friend auto operator==(const Iter &it, sentinel_type /*unused*/) -> bool {
        if constexpr (has_is_at_end<State>) {
            return state(it).is_at_end();
        } else {
            return state(it).distance_to_end() == 0;
        }
    }
    friend auto operator!=(const Iter &it, sentinel_type sentinel) -> bool { return !(it == sentinel); }
    friend auto operator==(sentinel_type sentinel, const Iter &it) -> bool { return it == sentinel; }
    friend auto operator!=(sentinel_type sentinel, const Iter &it) -> bool { return !(it == sentinel); }
};

template <typename Iter, typename State, bool has_is_at_end>
struct sentinel_interface<Iter, State, has_is_at_end, true> : sentinel_interface<Iter, State, true, false> {
    using sentinel_type = typename sentinel_interface<Iter, State, true, false>::sentinel_type;
    using difference_type = typename detail::iterator_traits<State>::difference_type;

    friend auto operator-(sentinel_type /*unused*/, const Iter &it) -> difference_type {
        return it.state().distance_to_end();
    }
};

template <typename Iter, typename State, bool = has_equal_to<State> || has_distance_to<State>>
struct equality_interface {};

template <typename Iter, typename State>
struct equality_interface<Iter, State, true> {
    static auto state(const Iter &iter) -> const State & { return iterator_state_access<Iter, State>::state(iter); }
    friend auto operator==(const Iter &a, const Iter &b) -> bool {
        if constexpr (has_equal_to<State>) {
            return state(a).equal_to(state(b));
        } else {
            static_assert(has_distance_to<State>, "Need .distance_to() or .equal_to()");
            return state(a).distance_to(state(b)) == 0;
        }
    }
    friend auto operator!=(const Iter &left, const Iter &right) -> bool { return !(left == right); }
};

template <typename Iter, typename State>
struct base_iterator_interface : iterator_traits<State>,
                                 equality_interface<Iter, State>,
                                 sentinel_interface<Iter, State> {
    static_assert(has_increment<State> || has_advance<State>,
                  "Iterator state needs 'void increment()' or 'void advance(difference_type)'");

    auto iter() -> Iter & { return static_cast<Iter &>(*this); }
    auto iter() const -> const Iter & { return static_cast<const Iter &>(*this); }
    static auto state(Iter &iter) -> State & { return iterator_state_access<Iter, State>::state(iter); }
    static auto state(const Iter &iter) -> const State & { return iterator_state_access<Iter, State>::state(iter); }

    using reference = typename detail::iterator_traits<State>::reference;
    using pointer = typename detail::iterator_traits<State>::pointer;

    auto operator*() const -> reference { return state(iter()).dereference(); }
    auto operator->() const -> pointer { return detail::pointer_dereference(state(iter())); }
    auto operator++() -> Iter & {
        if constexpr (has_increment<State>) {
            state(iter()).increment();
        } else {
            state(iter()).advance(1);
        }
        return static_cast<Iter &>(*this);
    }
};

template <typename Iter, typename State, typename Category = typename iterator_traits<State>::iterator_category>
struct iterator_interface {
    static_assert(dependent_false_v<Iter>, "Invalid iterator category specified");
};

template <typename Iter, typename State>
struct iterator_interface<Iter, State, std::input_iterator_tag> : base_iterator_interface<Iter, State> {
    using base_iterator_interface<Iter, State>::operator++;
    auto operator++(int) -> void { operator++(); }
};

template <typename Iter, typename State>
struct iterator_interface<Iter, State, std::forward_iterator_tag> : base_iterator_interface<Iter, State> {
    using base_iterator_interface<Iter, State>::operator++;
    auto operator++(int) -> Iter {
        auto copy = *this;
        operator++();
        return copy;
    }
};

template <typename Iter, typename State>
struct iterator_interface<Iter, State, std::bidirectional_iterator_tag>
    : iterator_interface<Iter, State, std::forward_iterator_tag> {
    auto operator--() -> Iter & {
        if constexpr (detail::has_decrement<State>) {
            state(iter()).decrement();
        } else {
            static_assert(detail::has_advance<State>, "Need .advance() or .decrement()");
            state(iter()).advance(-1);
        }
        return *this;
    }
    auto operator--(int) -> Iter {
        auto copy = *this;
        operator--();
        return copy;
    }
};

template <typename Iter, typename State>
struct iterator_interface<Iter, State, std::random_access_iterator_tag>
    : iterator_interface<Iter, State, std::bidirectional_iterator_tag> {
    using reference = typename detail::iterator_traits<State>::reference;
    using difference_type = typename detail::iterator_traits<State>::difference_type;

    auto operator+=(difference_type n) -> Iter & {
        state(iter()).advance(n);
        return *this;
    }
    friend auto operator-(const Iter &b, const Iter &a) -> difference_type { return state(a).distance_to(state(b)); }

    friend auto operator+(Iter a, difference_type n) -> Iter { return a += n; }
    friend auto operator+(difference_type n, Iter a) -> Iter { return a += n; }
    auto operator-=(difference_type n) -> Iter & { return (*this) += -n; }
    friend auto operator-(Iter i, difference_type n) -> Iter { return i -= n; }
    auto operator[](difference_type n) -> reference { return *(*this + n); }
    friend auto operator<(const Iter &a, const Iter &b) -> bool { return a - b < 0; }
    friend auto operator>(const Iter &a, const Iter &b) -> bool { return b < a; }
    friend auto operator>=(const Iter &a, const Iter &b) -> bool { return !(a < b); }
    friend auto operator<=(const Iter &a, const Iter &b) -> bool { return !(a > b); }
};

} // namespace detail

template <typename State>
class iterator_facade : public detail::iterator_interface<iterator_facade<State>, State> {
  private:
    State state_;
    friend class detail::iterator_state_access<iterator_facade<State>, State>;

  public:
    template <typename... Args,
              std::enable_if_t<std::is_constructible_v<State, Args...> && !std::is_aggregate_v<State>, int> = 0>
    explicit iterator_facade(Args &&...args) : state_(std::forward<Args>(args)...) {}

    template <
        typename... Args,
        std::enable_if_t<mcpp::is_direct_list_initializable_v<State, Args...> && std::is_aggregate_v<State>, int> = 0>
    explicit iterator_facade(Args &&...args) : state_{std::forward<Args>(args)...} {}
};

} // namespace mcpp::iterator_facade