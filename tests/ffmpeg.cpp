// Copyright Mika Fischer 2021.
// Distributed under the Boost Software License, Version 1.0.
// (See accompanying file LICENSE.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

#include "mcpp/iterator_facade/iterator_facade.hpp"
#include <functional>
#include <iterator>
#include <string_view>

using namespace std::literals;

template <auto func, auto... args>
class ffmpeg_range_opaque {
  private:
    struct iter_state {
        using iterator_category = std::input_iterator_tag;

        void *state = nullptr;
        const char *val = nullptr;

        auto dereference() const -> std::string_view { return val; }
        auto equal_to(const iter_state &o) const -> bool { return val == nullptr && o.val == nullptr; }
        void increment() { val = std::invoke(func, &state, args...); }
    };

  public:
    using iterator = mcpp::iterator_facade::iterator_facade<iter_state>;
    auto begin() const { return ++iterator{}; }
    auto end() const { return iterator{}; }
};

template <auto func>
class ffmpeg_range_next {
  private:
    struct iter_state {
        decltype(func(nullptr)) val{nullptr};

        using iterator_category = std::input_iterator_tag;
        auto dereference() const -> decltype(auto) { return *val; }
        auto equal_to(const iter_state &o) const -> bool { return val == nullptr && o.val == nullptr; }
        void increment() { val = std::invoke(func, val); }
    };

  public:
    using iterator = mcpp::iterator_facade::iterator_facade<iter_state>;
    auto begin() const { return ++iterator{}; }
    auto end() const { return iterator{}; }
};

extern "C" {
struct AVCodecDescriptor;
auto avcodec_descriptor_next(const AVCodecDescriptor * /* prev */) -> const AVCodecDescriptor * {
    return nullptr;
}
auto avio_enum_protocols(void ** /*opaque*/, int /*output*/) -> const char * {
    return nullptr;
}
}

using codec_descriptors = ffmpeg_range_next<avcodec_descriptor_next>;
using cd_iter = codec_descriptors::iterator;
using cd_iter_traits = std::iterator_traits<cd_iter>;
static_assert(std::is_same_v<cd_iter_traits::iterator_category, std::input_iterator_tag>);
static_assert(std::is_same_v<cd_iter_traits::reference, const AVCodecDescriptor &>);
static_assert(std::is_same_v<cd_iter_traits::pointer, const AVCodecDescriptor *>);
static_assert(std::is_same_v<cd_iter_traits::value_type, AVCodecDescriptor>);
static_assert(std::is_same_v<cd_iter_traits::difference_type, std::ptrdiff_t>);

using avio_protocols = ffmpeg_range_opaque<avio_enum_protocols, 0>;
using ap_iter = avio_protocols::iterator;
using ap_iter_traits = std::iterator_traits<ap_iter>;
static_assert(std::is_same_v<ap_iter_traits::iterator_category, std::input_iterator_tag>);
static_assert(std::is_same_v<ap_iter_traits::reference, std::string_view>);
static_assert(std::is_same_v<ap_iter_traits::pointer, mcpp::iterator_facade::detail::arrow_proxy<std::string_view>>);
static_assert(std::is_same_v<ap_iter_traits::value_type, std::string_view>); // TODO: What *should* this be?
static_assert(std::is_same_v<ap_iter_traits::difference_type, std::ptrdiff_t>);

auto main() -> int {
    for (const auto &desc : codec_descriptors()) {
    }
    for (auto proto : avio_protocols()) {
    }
}