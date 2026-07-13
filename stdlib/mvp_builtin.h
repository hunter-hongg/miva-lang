//! MVP Programming Language Built-in Functions
#ifndef MVP_BUILTIN_H
#define MVP_BUILTIN_H

#include <climits>
#include <cstdarg>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cinttypes>
#include <charconv>
#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>
#include <format>
#include <memory>
#include <utility>
#include <future>
#include <thread>
#include <functional>

#include "mvp_copyable.h"

extern "C" {
typedef struct {
  char _;
} mvp_builtin_unit; // void type，为了避免return voidFunc()失效，采用这种写法
                    // 所有Builtin返回void时都应当返回mvp_builtin_void类型
typedef int64_t mvp_builtin_int;     // 默认int类型
typedef int32_t mvp_builtin_int32;   // 默认int32类型
typedef int16_t mvp_builtin_short;   // 默认short类型
typedef int8_t mvp_builtin_byte;     // 默认byte类型
typedef uint64_t mvp_builtin_uint;   // 默认unsigned int类型
typedef uint32_t mvp_builtin_uint32; // 默认unsigned int32类型
typedef uint16_t mvp_builtin_ushort; // 默认unsigned short类型
typedef uint8_t mvp_builtin_ubyte;   // 默认unsigned byte类型
typedef int8_t mvp_builtin_boolean;  // 默认boolean类型
typedef double mvp_builtin_float;    // 默认float类型
const mvp_builtin_unit mvp_builtin_void = {};
}
typedef std::string mvp_builtin_string; // 默认string类型

inline mvp_builtin_string mvp_to_string(mvp_builtin_boolean const &v) {
    return v ? "true" : "false";
}

inline mvp_builtin_string mvp_to_string(mvp_builtin_int const &v) {
    char buf[64];
    int len = std::snprintf(buf, sizeof(buf), "%" PRId64, v);
    return mvp_builtin_string(buf, static_cast<size_t>(len));
}

inline mvp_builtin_string mvp_to_string(mvp_builtin_float const &v) {
    char buf[64];
    auto res = std::to_chars(buf, buf + sizeof(buf), v);
    return mvp_builtin_string(buf, static_cast<size_t>(res.ptr - buf));
}

inline mvp_builtin_string mvp_to_string(mvp_builtin_string const &v) {
    return v;
}

inline mvp_builtin_unit mvp_panic(mvp_builtin_string const &str) {
  throw std::runtime_error(str);
}

inline mvp_builtin_unit mvp_print(mvp_builtin_string const &str) {
  printf("%s", str.c_str());
  return mvp_builtin_void;
}

template<typename... Args>
inline mvp_builtin_unit mvp_prints(Args&&... args) {
    // 使用折叠表达式拼接到一个 string
    mvp_builtin_string output;
    output.reserve(128); // 预分配，避免多次 realloc

    ((output += mvp_to_string(args)), ...);

    // ⚡ 关键：用 fwrite 直接写 stdout，绕过 iostream
    std::fwrite(output.data(), 1, output.size(), stdout);
    return mvp_builtin_void;
}

inline mvp_builtin_unit mvp_println(mvp_builtin_string const &str) {
  printf("%s\n", str.c_str());
  return mvp_builtin_void;
}

template<typename... Args>
inline mvp_builtin_unit mvp_printlns(Args&&... args) {
  mvp_prints(args...);
  std::fputc('\n', stdout);
  return mvp_builtin_void;
}

inline mvp_builtin_unit mvp_error(mvp_builtin_string const &str) {
  std::fwrite(str.c_str(), 1, str.size(), stderr);
  return mvp_builtin_void;
}

inline mvp_builtin_unit mvp_errorln(mvp_builtin_string const &str) {
  std::fwrite(str.c_str(), 1, str.size(), stderr);
  std::fputc('\n', stderr);
  return mvp_builtin_void;
}

template <typename... Args>
inline mvp_builtin_unit mvp_errors(Args const &... args) {
  mvp_builtin_string output;
  output.reserve(128); // 预分配，避免多次 realloc
  ((output += mvp_to_string(args)), ...);
  return mvp_error(output);
}

template <typename... Args>
inline mvp_builtin_unit mvp_errorlns(Args const &... args) {
  mvp_errors(args...);
  std::fputc('\n', stderr);
  return mvp_builtin_void;
}

inline mvp_builtin_unit mvp_exit(mvp_builtin_int code) {
  std::exit(code);
  return mvp_builtin_void; // never reach
}

inline mvp_builtin_unit mvp_abort() {
  std::abort();
  return mvp_builtin_void; // never reach
}

template <typename T>
struct mvp_builtin_box {
    std::unique_ptr<T> value;
    mvp_builtin_box() : value(std::make_unique<T>()) {}
    // 构造函数：接受一个值并移动构造到堆上
    explicit mvp_builtin_box(T val)
        : value(std::make_unique<T>(std::move(val))) {}

    // 拷贝构造函数：深拷贝（需要 T 支持拷贝）
    mvp_builtin_box(const mvp_builtin_box& other)
        : value(std::make_unique<T>(*other.value)) {}

    // 拷贝赋值运算符
    mvp_builtin_box& operator=(const mvp_builtin_box& other) {
        if (this != &other) {
            value = std::make_unique<T>(*other.value);
        }
        return *this;
    }

    // 移动构造函数：默认即可（unique_ptr 已支持移动）
    mvp_builtin_box(mvp_builtin_box&& other) noexcept = default;

    // 移动赋值运算符：默认即可
    mvp_builtin_box& operator=(mvp_builtin_box&& other) noexcept = default;

    // 析构函数：默认即可（unique_ptr 自动释放）
    ~mvp_builtin_box() = default;

    // 解引用操作符
    T& operator*() { return *value; }
    const T& operator*() const { return *value; }

    // 箭头操作符
    T* operator->() { return value.get(); }
    const T* operator->() const { return value.get(); }
};

template <typename T>
inline mvp_builtin_box<T> mvp_box_new(T value) {
  return mvp_builtin_box<T>(value);
}

template <typename T>
inline T mvp_box_deref(mvp_builtin_box<T> const &box) {
  return *box;
}

inline mvp_builtin_int mvp_string_parse(mvp_builtin_string const &str) {
  mvp_builtin_int result = 0;
  for (char c : str) {
    if (c < '0' || c > '9') {
      mvp_panic("Invalid number format");
    }
    result = result * 10 + (c - '0');
  }
  return result;
}

inline mvp_builtin_string mvp_string_concat(mvp_builtin_string const &a, mvp_builtin_string const &b) {
  return a + b;
}

inline mvp_builtin_int mvp_string_length(mvp_builtin_string const &str) {
  return str.size();
}

inline mvp_builtin_string mvp_string_make(mvp_builtin_string const &init, int size) {
  mvp_builtin_string res = init;
  res.reserve(size);
  return res;
}

inline std::vector<mvp_builtin_int> mvp_range(mvp_builtin_int start, mvp_builtin_int end) {
  std::vector<mvp_builtin_int> res;
  for (mvp_builtin_int i = start; i < end; ++i) {
    res.push_back(i);
  }
  return res;
}

inline std::vector<mvp_builtin_int> mvp_range(mvp_builtin_int end) {
  return mvp_range(0, end);
}

inline std::vector<mvp_builtin_int> mvp_range(mvp_builtin_int start, mvp_builtin_int end, mvp_builtin_int step) {
  std::vector<mvp_builtin_int> res;
  for (mvp_builtin_int i = start; i < end; i += step) {
    res.push_back(i);
  }
  return res;
}

typedef void* mvp_builtin_ptrany;
inline mvp_builtin_ptrany mvp_alloc(mvp_builtin_int size) {
  auto t =  malloc(size);
  if (t) {
    return t;
  } else {
    mvp_panic("malloc failed");
  }
}
inline void mvp_free(mvp_builtin_ptrany ptr) {
  free(ptr);
}
inline mvp_builtin_ptrany mvp_realloc(mvp_builtin_ptrany ptr, mvp_builtin_int size) {
  auto t = realloc(ptr, size);
  if (t) {
    return t;
  } else {
    mvp_panic("realloc failed");
  }
}
template<typename T>
inline void mvp_builtin_ptrset(T* ptr, T value) {
  *ptr = value;
}
// Move a raw pointer forward/back by `n` bytes (like C's `(char*)p + n`).
// Returns the resulting pointer. This is an unsafe byte offset; the caller
// is responsible for ensuring the resulting address is valid.
inline mvp_builtin_ptrany mvp_ptr_offset(mvp_builtin_ptrany p, mvp_builtin_int n) {
  return (mvp_builtin_ptrany)((char*)p + n);
}

// ── Async / future ────────────────────────────────────────────────────────

// A `future[T]` holds the result of a computation running on its own thread.
// The shared_ptr keeps the future copyable so both `let f = task(); f.await()`
// and `task().await()` work.
template <typename T>
struct mvp_future {
    std::shared_ptr<std::future<T>> handle;
};

// Spawn a task on a new OS thread and return a `future[T]` handle to it.
template <typename F>
inline auto mvp_async_spawn(F&& fn) -> mvp_future<decltype(fn())> {
    using T = decltype(fn());
    mvp_future<T> result;
    result.handle = std::make_shared<std::future<T>>(
        std::async(std::launch::async, std::forward<F>(fn)));
    return result;
}

// Block until the task finishes and return its result (the `.await()` method).
template <typename T>
inline T mvp_async_await(mvp_future<T> const& f) {
    return f.handle->get();
}

// ── JSON parsing & serialization ──────────────────────────────────────────
//
// A parsed JSON document is represented by a heap-allocated `MivaJson` tree.
// Miva code interacts with it through opaque `ptrany` handles returned by the
// `json_*` builtins below. Sub-value handles (from `json_array_get`,
// `json_object_get`, `json_object_find`) are *borrowed* views into the tree and
// must NOT be passed to `json_free` — only the root handle owns the memory.

struct MivaJson {
    enum class Kind { Null, Bool, Number, String, Array, Object } kind;
    bool b = false;
    double num = 0.0;
    std::string str;
    std::vector<std::unique_ptr<MivaJson>> arr;
    std::vector<std::pair<std::string, std::unique_ptr<MivaJson>>> obj;

    explicit MivaJson(Kind k = Kind::Null) : kind(k) {}
};

struct MivaJsonParser {
    mvp_builtin_string const& s;
    size_t i = 0;

    explicit MivaJsonParser(mvp_builtin_string const& src) : s(src) {}

    [[noreturn]] void fail(char const* msg) {
        mvp_panic(mvp_builtin_string("json: ") + msg +
                  " (at position " + std::to_string(i) + ")");
    }

    void ws() {
        while (i < s.size()) {
            char c = s[i];
            if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
                ++i;
            } else {
                break;
            }
        }
    }

    char peek() {
        if (i >= s.size()) fail("unexpected end of input");
        return s[i];
    }

    char get() {
        if (i >= s.size()) fail("unexpected end of input");
        return s[i++];
    }

    std::unique_ptr<MivaJson> parse_value() {
        ws();
        if (i >= s.size()) fail("unexpected end of input");
        char c = s[i];
        switch (c) {
            case 'n': return parse_lit("null", MivaJson::Kind::Null);
            case 't': return parse_lit("true", MivaJson::Kind::Bool, true);
            case 'f': return parse_lit("false", MivaJson::Kind::Bool, false);
            case '"': {
                auto v = std::make_unique<MivaJson>(MivaJson::Kind::String);
                v->str = parse_string();
                return v;
            }
            case '[': return parse_array();
            case '{': return parse_object();
            default:
                if (c == '-' || (c >= '0' && c <= '9')) return parse_number();
                fail("unexpected character");
        }
    }

    std::unique_ptr<MivaJson> parse_lit(char const* lit, MivaJson::Kind kind,
                                        bool val = false) {
        size_t n = std::strlen(lit);
        if (i + n > s.size() || s.compare(i, n, lit) != 0) fail("invalid literal");
        i += n;
        auto v = std::make_unique<MivaJson>(kind);
        if (kind == MivaJson::Kind::Bool) v->b = val;
        return v;
    }

    std::unique_ptr<MivaJson> parse_number() {
        size_t start = i;
        if (peek() == '-') ++i;
        if (i >= s.size()) fail("invalid number");
        if (s[i] == '0') {
            ++i;
        } else if (s[i] >= '1' && s[i] <= '9') {
            while (i < s.size() && s[i] >= '0' && s[i] <= '9') ++i;
        } else {
            fail("invalid number");
        }
        if (i < s.size() && s[i] == '.') {
            ++i;
            if (i >= s.size() || s[i] < '0' || s[i] > '9') fail("invalid number fraction");
            while (i < s.size() && s[i] >= '0' && s[i] <= '9') ++i;
        }
        if (i < s.size() && (s[i] == 'e' || s[i] == 'E')) {
            ++i;
            if (i < s.size() && (s[i] == '+' || s[i] == '-')) ++i;
            if (i >= s.size() || s[i] < '0' || s[i] > '9') fail("invalid number exponent");
            while (i < s.size() && s[i] >= '0' && s[i] <= '9') ++i;
        }
        auto v = std::make_unique<MivaJson>(MivaJson::Kind::Number);
        v->num = std::strtod(s.substr(start, i - start).c_str(), nullptr);
        return v;
    }

    mvp_builtin_string parse_string() {
        get(); // consume opening quote
        std::string out;
        out.reserve(16);
        while (true) {
            if (i >= s.size()) fail("unterminated string");
            char c = s[i++];
            if (c == '"') break;
            if (c == '\\') {
                if (i >= s.size()) fail("unterminated escape");
                char e = s[i++];
                switch (e) {
                    case '"': out.push_back('"'); break;
                    case '\\': out.push_back('\\'); break;
                    case '/': out.push_back('/'); break;
                    case 'b': out.push_back('\b'); break;
                    case 'f': out.push_back('\f'); break;
                    case 'n': out.push_back('\n'); break;
                    case 'r': out.push_back('\r'); break;
                    case 't': out.push_back('\t'); break;
                    case 'u': {
                        unsigned cp = parse_hex4();
                        if (cp >= 0xD800 && cp <= 0xDBFF) {
                            if (i + 2 > s.size() || s[i] != '\\' || s[i + 1] != 'u')
                                fail("invalid surrogate pair");
                            i += 2;
                            unsigned lo = parse_hex4();
                            if (lo < 0xDC00 || lo > 0xDFFF) fail("invalid low surrogate");
                            cp = 0x10000 + ((cp - 0xD800) << 10) + (lo - 0xDC00);
                        }
                        encode_utf8(cp, out);
                        break;
                    }
                    default: fail("invalid escape");
                }
            } else {
                out.push_back(c);
            }
        }
        return out;
    }

    unsigned parse_hex4() {
        if (i + 4 > s.size()) fail("invalid \\u escape");
        unsigned v = 0;
        for (int k = 0; k < 4; ++k) {
            char c = s[i++];
            v <<= 4;
            if (c >= '0' && c <= '9') v |= static_cast<unsigned>(c - '0');
            else if (c >= 'a' && c <= 'f') v |= static_cast<unsigned>(c - 'a' + 10);
            else if (c >= 'A' && c <= 'F') v |= static_cast<unsigned>(c - 'A' + 10);
            else fail("invalid hex digit");
        }
        return v;
    }

    static void encode_utf8(unsigned cp, std::string& out) {
        if (cp <= 0x7F) {
            out.push_back(static_cast<char>(cp));
        } else if (cp <= 0x7FF) {
            out.push_back(static_cast<char>(0xC0 | (cp >> 6)));
            out.push_back(static_cast<char>(0x80 | (cp & 0x3F)));
        } else if (cp <= 0xFFFF) {
            out.push_back(static_cast<char>(0xE0 | (cp >> 12)));
            out.push_back(static_cast<char>(0x80 | ((cp >> 6) & 0x3F)));
            out.push_back(static_cast<char>(0x80 | (cp & 0x3F)));
        } else {
            out.push_back(static_cast<char>(0xF0 | (cp >> 18)));
            out.push_back(static_cast<char>(0x80 | ((cp >> 12) & 0x3F)));
            out.push_back(static_cast<char>(0x80 | ((cp >> 6) & 0x3F)));
            out.push_back(static_cast<char>(0x80 | (cp & 0x3F)));
        }
    }

    std::unique_ptr<MivaJson> parse_array() {
        get(); // '['
        auto v = std::make_unique<MivaJson>(MivaJson::Kind::Array);
        ws();
        if (peek() == ']') { get(); return v; }
        while (true) {
            v->arr.push_back(parse_value());
            ws();
            char c = get();
            if (c == ',') { ws(); continue; }
            if (c == ']') break;
            fail("expected ',' or ']' in array");
        }
        return v;
    }

    std::unique_ptr<MivaJson> parse_object() {
        get(); // '{'
        auto v = std::make_unique<MivaJson>(MivaJson::Kind::Object);
        ws();
        if (peek() == '}') { get(); return v; }
        while (true) {
            ws();
            if (peek() != '"') fail("expected string key in object");
            std::string key = parse_string();
            ws();
            if (get() != ':') fail("expected ':' in object");
            auto val = parse_value();
            v->obj.emplace_back(std::move(key), std::move(val));
            ws();
            char c = get();
            if (c == ',') { ws(); continue; }
            if (c == '}') break;
            fail("expected ',' or '}' in object");
        }
        return v;
    }
};

inline mvp_builtin_ptrany mvp_json_parse(mvp_builtin_string const& src) {
    MivaJsonParser parser(src);
    auto v = parser.parse_value();
    parser.ws();
    if (parser.i != src.size()) parser.fail("trailing characters after value");
    return static_cast<mvp_builtin_ptrany>(v.release());
}

inline MivaJson* mvp_json_as_node(mvp_builtin_ptrany v) {
    if (!v) mvp_panic("json: null handle");
    return static_cast<MivaJson*>(v);
}

inline mvp_builtin_int mvp_json_kind(mvp_builtin_ptrany v) {
    if (!v) return -1;
    return static_cast<mvp_builtin_int>(static_cast<MivaJson*>(v)->kind);
}

inline mvp_builtin_boolean mvp_json_bool(mvp_builtin_ptrany v) {
    MivaJson* j = mvp_json_as_node(v);
    if (j->kind != MivaJson::Kind::Bool) mvp_panic("json: expected bool");
    return j->b;
}

inline mvp_builtin_float mvp_json_number(mvp_builtin_ptrany v) {
    MivaJson* j = mvp_json_as_node(v);
    if (j->kind != MivaJson::Kind::Number) mvp_panic("json: expected number");
    return j->num;
}

inline mvp_builtin_string mvp_json_string(mvp_builtin_ptrany v) {
    MivaJson* j = mvp_json_as_node(v);
    if (j->kind != MivaJson::Kind::String) mvp_panic("json: expected string");
    return j->str;
}

inline mvp_builtin_int mvp_json_array_len(mvp_builtin_ptrany v) {
    MivaJson* j = mvp_json_as_node(v);
    if (j->kind != MivaJson::Kind::Array) mvp_panic("json: expected array");
    return static_cast<mvp_builtin_int>(j->arr.size());
}

inline mvp_builtin_ptrany mvp_json_array_get(mvp_builtin_ptrany v, mvp_builtin_int idx) {
    MivaJson* j = mvp_json_as_node(v);
    if (j->kind != MivaJson::Kind::Array) mvp_panic("json: expected array");
    if (idx < 0 || idx >= static_cast<mvp_builtin_int>(j->arr.size()))
        mvp_panic("json: array index out of range");
    return static_cast<mvp_builtin_ptrany>(j->arr[idx].get());
}

inline mvp_builtin_int mvp_json_object_len(mvp_builtin_ptrany v) {
    MivaJson* j = mvp_json_as_node(v);
    if (j->kind != MivaJson::Kind::Object) mvp_panic("json: expected object");
    return static_cast<mvp_builtin_int>(j->obj.size());
}

inline mvp_builtin_string mvp_json_object_key(mvp_builtin_ptrany v, mvp_builtin_int idx) {
    MivaJson* j = mvp_json_as_node(v);
    if (j->kind != MivaJson::Kind::Object) mvp_panic("json: expected object");
    if (idx < 0 || idx >= static_cast<mvp_builtin_int>(j->obj.size()))
        mvp_panic("json: object index out of range");
    return j->obj[idx].first;
}

inline mvp_builtin_ptrany mvp_json_object_get(mvp_builtin_ptrany v, mvp_builtin_int idx) {
    MivaJson* j = mvp_json_as_node(v);
    if (j->kind != MivaJson::Kind::Object) mvp_panic("json: expected object");
    if (idx < 0 || idx >= static_cast<mvp_builtin_int>(j->obj.size()))
        mvp_panic("json: object index out of range");
    return static_cast<mvp_builtin_ptrany>(j->obj[idx].second.get());
}

inline mvp_builtin_ptrany mvp_json_object_find(mvp_builtin_ptrany v,
                                                mvp_builtin_string const& key) {
    MivaJson* j = mvp_json_as_node(v);
    if (j->kind != MivaJson::Kind::Object) mvp_panic("json: expected object");
    for (auto const& kv : j->obj) {
        if (kv.first == key) return static_cast<mvp_builtin_ptrany>(kv.second.get());
    }
    return nullptr;
}

inline void mvp_json_free(mvp_builtin_ptrany v) {
    delete static_cast<MivaJson*>(v);
}

inline void mvp_json_stringify_impl(MivaJson const* j, std::string& out) {
    switch (j->kind) {
        case MivaJson::Kind::Null:
            out += "null";
            break;
        case MivaJson::Kind::Bool:
            out += j->b ? "true" : "false";
            break;
        case MivaJson::Kind::Number: {
            std::string n = std::to_string(j->num);
            auto dot = n.find('.');
            if (dot != std::string::npos) {
                // Drop trailing zeros of the fractional part; keep the integer
                // part (including any internal zeros) intact.
                size_t last = n.find_last_not_of('0');
                if (last == std::string::npos || last <= dot) {
                    n.erase(dot); // fractional part was all zeros
                } else {
                    n.erase(last + 1);
                }
            }
            out += n;
            break;
        }
        case MivaJson::Kind::String: {
            out.push_back('"');
            for (char c : j->str) {
                switch (c) {
                    case '"': out += "\\\""; break;
                    case '\\': out += "\\\\"; break;
                    case '\b': out += "\\b"; break;
                    case '\f': out += "\\f"; break;
                    case '\n': out += "\\n"; break;
                    case '\r': out += "\\r"; break;
                    case '\t': out += "\\t"; break;
                    default:
                        if (static_cast<unsigned char>(c) < 0x20) {
                            char buf[8];
                            std::snprintf(buf, sizeof(buf), "\\u%04x",
                                          static_cast<unsigned char>(c));
                            out += buf;
                        } else {
                            out.push_back(c);
                        }
                }
            }
            out.push_back('"');
            break;
        }
        case MivaJson::Kind::Array: {
            out.push_back('[');
            for (size_t k = 0; k < j->arr.size(); ++k) {
                if (k) out.push_back(',');
                mvp_json_stringify_impl(j->arr[k].get(), out);
            }
            out.push_back(']');
            break;
        }
        case MivaJson::Kind::Object: {
            out.push_back('{');
            for (size_t k = 0; k < j->obj.size(); ++k) {
                if (k) out.push_back(',');
                out.push_back('"');
                for (char c : j->obj[k].first) {
                    if (c == '"') out += "\\\"";
                    else if (c == '\\') out += "\\\\";
                    else out.push_back(c);
                }
                out += "\":";
                mvp_json_stringify_impl(j->obj[k].second.get(), out);
            }
            out.push_back('}');
            break;
        }
    }
}

inline mvp_builtin_string mvp_json_stringify(mvp_builtin_ptrany v) {
    MivaJson* j = mvp_json_as_node(v);
    std::string out;
    out.reserve(64);
    mvp_json_stringify_impl(j, out);
    return out;
}

// ── XML parsing and serialization (mirrors std/json) ───────────────────────────
//
// A parsed XML document is represented by a heap-allocated `MivaXml` tree. Miva
// code interacts with it through opaque `ptrany` handles returned by the `xml_*`
// builtins below. Child handles (from `xml_child_get`) are *borrowed* views into
// the tree and must NOT be passed to `xml_free` — only the root handle owns the
// memory.

struct MivaXml {
    enum class Kind { Null, Element, Text, Comment, CData, PI, Document } kind;
    std::string tag;
    std::vector<std::pair<std::string, std::string>> attrs;
    std::vector<std::unique_ptr<MivaXml>> children;
    std::string text;       // text/comment/cdata content
    std::string pi_target;  // PI target
    std::string pi_data;    // PI data

    explicit MivaXml(Kind k = Kind::Null) : kind(k) {}
};

// Decode XML entities: &amp; &lt; &gt; &quot; &apos; and numeric &#NNN; / &#xHHH;.
static void mvp_xml_encode_utf8(unsigned cp, std::string& out) {
    if (cp <= 0x7F) {
        out.push_back(static_cast<char>(cp));
    } else if (cp <= 0x7FF) {
        out.push_back(static_cast<char>(0xC0 | (cp >> 6)));
        out.push_back(static_cast<char>(0x80 | (cp & 0x3F)));
    } else if (cp <= 0xFFFF) {
        out.push_back(static_cast<char>(0xE0 | (cp >> 12)));
        out.push_back(static_cast<char>(0x80 | ((cp >> 6) & 0x3F)));
        out.push_back(static_cast<char>(0x80 | (cp & 0x3F)));
    } else {
        out.push_back(static_cast<char>(0xF0 | (cp >> 18)));
        out.push_back(static_cast<char>(0x80 | ((cp >> 12) & 0x3F)));
        out.push_back(static_cast<char>(0x80 | ((cp >> 6) & 0x3F)));
        out.push_back(static_cast<char>(0x80 | (cp & 0x3F)));
    }
}

static std::string mvp_xml_decode_entities(std::string const& raw) {
    std::string out;
    out.reserve(raw.size());
    size_t i = 0;
    while (i < raw.size()) {
        char c = raw[i];
        if (c == '&') {
            size_t j = i + 1;
            while (j < raw.size() && raw[j] != ';') ++j;
            if (j < raw.size()) {
                std::string ent = raw.substr(i + 1, j - i - 1);
                char decoded = '&';
                if (!ent.empty() && ent[0] == '#') {
                    if (ent.size() > 1 && (ent[1] == 'x' || ent[1] == 'X')) {
                        unsigned cp = (unsigned)std::strtoul(ent.substr(2).c_str(), nullptr, 16);
                        mvp_xml_encode_utf8(cp, out);
                    } else {
                        unsigned cp = (unsigned)std::strtoul(ent.substr(1).c_str(), nullptr, 10);
                        mvp_xml_encode_utf8(cp, out);
                    }
                } else if (ent == "amp") {
                    decoded = '&';
                    out.push_back(decoded);
                } else if (ent == "lt") {
                    decoded = '<';
                    out.push_back(decoded);
                } else if (ent == "gt") {
                    decoded = '>';
                    out.push_back(decoded);
                } else if (ent == "quot") {
                    decoded = '"';
                    out.push_back(decoded);
                } else if (ent == "apos") {
                    decoded = '\'';
                    out.push_back(decoded);
                } else {
                    out.push_back('&');
                }
                i = j + 1;
            } else {
                out.push_back(c);
                ++i;
            }
        } else {
            out.push_back(c);
            ++i;
        }
    }
    return out;
}

struct MivaXmlParser {
    mvp_builtin_string const& s;
    size_t i = 0;

    explicit MivaXmlParser(mvp_builtin_string const& src) : s(src) {}

    [[noreturn]] void fail(char const* msg) {
        mvp_panic(mvp_builtin_string("xml: ") + msg +
                  " (at position " + std::to_string(i) + ")");
    }

    void skip_ws() {
        while (i < s.size()) {
            char c = s[i];
            if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
                ++i;
            } else {
                break;
            }
        }
    }

    bool starts_with(char const* lit) {
        size_t n = std::strlen(lit);
        if (i + n > s.size()) return false;
        return s.compare(i, n, lit) == 0;
    }

    char peek() {
        if (i >= s.size()) fail("unexpected end of input");
        return s[i];
    }

    char get() {
        if (i >= s.size()) fail("unexpected end of input");
        return s[i++];
    }

    bool eof() { return i >= s.size(); }

    std::string parse_name() {
        std::string name;
        while (i < s.size()) {
            char c = s[i];
            if (std::isalnum((unsigned char)c) || c == ':' || c == '-' || c == '_' || c == '.') {
                name.push_back(c);
                ++i;
            } else {
                break;
            }
        }
        if (name.empty()) fail("expected a name");
        return name;
    }

    std::vector<std::pair<std::string, std::string>> parse_attributes() {
        std::vector<std::pair<std::string, std::string>> attrs;
        for (;;) {
            while (i < s.size() && (s[i] == ' ' || s[i] == '\t' || s[i] == '\n' || s[i] == '\r')) ++i;
            if (i >= s.size() || s[i] == '>' || s[i] == '/') break;
            std::string name = parse_name();
            while (i < s.size() && (s[i] == ' ' || s[i] == '\t' || s[i] == '\n' || s[i] == '\r')) ++i;
            if (i >= s.size() || s[i] != '=') fail("expected '=' after attribute");
            ++i; // consume '='
            while (i < s.size() && (s[i] == ' ' || s[i] == '\t' || s[i] == '\n' || s[i] == '\r')) ++i;
            if (i >= s.size() || (s[i] != '"' && s[i] != '\'')) fail("expected attribute value quote");
            char q = s[i++];
            std::string val;
            while (i < s.size() && s[i] != q) {
                val.push_back(s[i]);
                ++i;
            }
            if (i >= s.size()) fail("unterminated attribute value");
            ++i; // consume closing quote
            attrs.emplace_back(std::move(name), mvp_xml_decode_entities(val));
        }
        return attrs;
    }

    std::unique_ptr<MivaXml> parse_pi() {
        get(); get(); // consume "<?"
        std::string target;
        while (i < s.size() && s[i] != '?' && !std::isspace((unsigned char)s[i])) {
            target.push_back(s[i]);
            ++i;
        }
        while (i < s.size() && std::isspace((unsigned char)s[i])) ++i;
        std::string data;
        while (!eof()) {
            if (starts_with("?>")) { get(); get(); break; }
            data.push_back(s[i]);
            ++i;
        }
        auto node = std::make_unique<MivaXml>(MivaXml::Kind::PI);
        node->pi_target = std::move(target);
        node->pi_data = std::move(data);
        return node;
    }

    std::unique_ptr<MivaXml> parse_comment() {
        for (int k = 0; k < 4; ++k) get(); // consume "<!--"
        std::string text;
        while (!eof()) {
            if (starts_with("-->")) { get(); get(); get(); break; }
            text.push_back(s[i]);
            ++i;
        }
        auto node = std::make_unique<MivaXml>(MivaXml::Kind::Comment);
        node->text = std::move(text);
        return node;
    }

    std::unique_ptr<MivaXml> parse_cdata() {
        for (int k = 0; k < 9; ++k) get(); // consume "<![CDATA["
        std::string text;
        while (!eof()) {
            if (starts_with("]]>")) { get(); get(); get(); break; }
            text.push_back(s[i]);
            ++i;
        }
        auto node = std::make_unique<MivaXml>(MivaXml::Kind::CData);
        node->text = std::move(text);
        return node;
    }

    void skip_declaration() {
        get(); get(); // consume "<!"
        int depth = 1;
        while (!eof()) {
            char c = s[i];
            if (c == '"' || c == '\'') {
                char q = c;
                ++i;
                while (!eof() && s[i] != q) ++i;
                if (!eof()) ++i;
            } else if (c == '<') {
                ++i;
                ++depth;
            } else if (c == '>') {
                ++i;
                --depth;
                if (depth == 0) break;
            } else {
                ++i;
            }
        }
    }

    std::unique_ptr<MivaXml> parse_element() {
        get(); // consume '<'
        std::string tag = parse_name();
        std::vector<std::pair<std::string, std::string>> attrs = parse_attributes();
        bool self_closing = false;
        if (starts_with("/>")) { get(); get(); self_closing = true; }
        else if (i < s.size() && s[i] == '>') { get(); }
        else fail("expected '>' or '/>' in element");

        auto node = std::make_unique<MivaXml>(MivaXml::Kind::Element);
        node->tag = std::move(tag);
        node->attrs = std::move(attrs);
        if (self_closing) return node;

        for (;;) {
            if (eof()) fail("unexpected EOF in element content");
            if (starts_with("</")) {
                get(); get(); // consume "</"
                std::string endtag = parse_name();
                while (i < s.size() && s[i] != '>') ++i;
                if (i < s.size()) get(); // consume '>'
                if (endtag != node->tag) {
                    fail("mismatched end tag");
                }
                break;
            } else if (starts_with("<!--")) {
                node->children.push_back(parse_comment());
            } else if (starts_with("<![CDATA[")) {
                node->children.push_back(parse_cdata());
            } else if (starts_with("<?")) {
                node->children.push_back(parse_pi());
            } else if (s[i] == '<') {
                node->children.push_back(parse_element());
            } else {
                std::string text;
                while (i < s.size() && s[i] != '<') {
                    text.push_back(s[i]);
                    ++i;
                }
                std::string decoded = mvp_xml_decode_entities(text);
                // Trim surrounding whitespace; emit a Text node only if non-empty.
                size_t b = 0, e = decoded.size();
                while (b < e && std::isspace((unsigned char)decoded[b])) ++b;
                while (e > b && std::isspace((unsigned char)decoded[e - 1])) --e;
                if (e > b) {
                    auto t = std::make_unique<MivaXml>(MivaXml::Kind::Text);
                    t->text = decoded.substr(b, e - b);
                    node->children.push_back(std::move(t));
                }
            }
        }
        return node;
    }

    std::unique_ptr<MivaXml> parse_document() {
        auto doc = std::make_unique<MivaXml>(MivaXml::Kind::Document);
        for (;;) {
            skip_ws();
            if (eof()) break;
            if (s[i] != '<') break;
            if (starts_with("<?")) {
                doc->children.push_back(parse_pi());
            } else if (starts_with("<!--")) {
                doc->children.push_back(parse_comment());
            } else if (starts_with("<![CDATA[")) {
                doc->children.push_back(parse_cdata());
            } else if (starts_with("<!")) {
                skip_declaration();
            } else {
                doc->children.push_back(parse_element());
            }
        }
        return doc;
    }
};

inline mvp_builtin_ptrany mvp_xml_parse(mvp_builtin_string const& src) {
    MivaXmlParser parser(src);
    auto doc = parser.parse_document();
    return static_cast<mvp_builtin_ptrany>(doc.release());
}

inline MivaXml* mvp_xml_as_node(mvp_builtin_ptrany v) {
    if (!v) mvp_panic("xml: null handle");
    return static_cast<MivaXml*>(v);
}

inline mvp_builtin_int mvp_xml_kind(mvp_builtin_ptrany v) {
    if (!v) return -1;
    return static_cast<mvp_builtin_int>(static_cast<MivaXml*>(v)->kind);
}

inline mvp_builtin_string mvp_xml_tag(mvp_builtin_ptrany v) {
    MivaXml* x = mvp_xml_as_node(v);
    if (x->kind != MivaXml::Kind::Element) mvp_panic("xml: expected element");
    return x->tag;
}

inline mvp_builtin_int mvp_xml_attr_count(mvp_builtin_ptrany v) {
    MivaXml* x = mvp_xml_as_node(v);
    if (x->kind != MivaXml::Kind::Element) return 0;
    return static_cast<mvp_builtin_int>(x->attrs.size());
}

inline mvp_builtin_string mvp_xml_attr_name(mvp_builtin_ptrany v, mvp_builtin_int idx) {
    MivaXml* x = mvp_xml_as_node(v);
    if (x->kind != MivaXml::Kind::Element) mvp_panic("xml: expected element");
    if (idx < 0 || idx >= static_cast<mvp_builtin_int>(x->attrs.size()))
        mvp_panic("xml: attribute index out of range");
    return x->attrs[idx].first;
}

inline mvp_builtin_string mvp_xml_attr_value(mvp_builtin_ptrany v, mvp_builtin_int idx) {
    MivaXml* x = mvp_xml_as_node(v);
    if (x->kind != MivaXml::Kind::Element) mvp_panic("xml: expected element");
    if (idx < 0 || idx >= static_cast<mvp_builtin_int>(x->attrs.size()))
        mvp_panic("xml: attribute index out of range");
    return x->attrs[idx].second;
}

inline mvp_builtin_string mvp_xml_attr_find(mvp_builtin_ptrany v,
                                             mvp_builtin_string const& key) {
    MivaXml* x = mvp_xml_as_node(v);
    if (x->kind != MivaXml::Kind::Element) return "";
    for (auto const& kv : x->attrs) {
        if (kv.first == key) return kv.second;
    }
    return "";
}

inline mvp_builtin_int mvp_xml_child_count(mvp_builtin_ptrany v) {
    MivaXml* x = mvp_xml_as_node(v);
    if (x->kind != MivaXml::Kind::Element && x->kind != MivaXml::Kind::Document) return 0;
    return static_cast<mvp_builtin_int>(x->children.size());
}

inline mvp_builtin_ptrany mvp_xml_child_get(mvp_builtin_ptrany v, mvp_builtin_int idx) {
    MivaXml* x = mvp_xml_as_node(v);
    if (x->kind != MivaXml::Kind::Element && x->kind != MivaXml::Kind::Document)
        mvp_panic("xml: expected element or document");
    if (idx < 0 || idx >= static_cast<mvp_builtin_int>(x->children.size()))
        mvp_panic("xml: child index out of range");
    return static_cast<mvp_builtin_ptrany>(x->children[idx].get());
}

inline mvp_builtin_string mvp_xml_text(mvp_builtin_ptrany v) {
    MivaXml* x = mvp_xml_as_node(v);
    return x->text;
}

inline mvp_builtin_string mvp_xml_comment(mvp_builtin_ptrany v) {
    MivaXml* x = mvp_xml_as_node(v);
    return x->text;
}

inline mvp_builtin_string mvp_xml_cdata(mvp_builtin_ptrany v) {
    MivaXml* x = mvp_xml_as_node(v);
    return x->text;
}

inline mvp_builtin_string mvp_xml_pi_target(mvp_builtin_ptrany v) {
    MivaXml* x = mvp_xml_as_node(v);
    return x->pi_target;
}

inline mvp_builtin_string mvp_xml_pi_data(mvp_builtin_ptrany v) {
    MivaXml* x = mvp_xml_as_node(v);
    return x->pi_data;
}

static void mvp_xml_escape_text(std::string const& in, std::string& out) {
    for (char c : in) {
        switch (c) {
            case '&': out += "&amp;"; break;
            case '<': out += "&lt;"; break;
            case '>': out += "&gt;"; break;
            default: out.push_back(c); break;
        }
    }
}

static void mvp_xml_escape_attr(std::string const& in, std::string& out) {
    for (char c : in) {
        switch (c) {
            case '&': out += "&amp;"; break;
            case '"': out += "&quot;"; break;
            case '<': out += "&lt;"; break;
            case '>': out += "&gt;"; break;
            case '\'': out += "&apos;"; break;
            default: out.push_back(c); break;
        }
    }
}

static void mvp_xml_stringify_impl(MivaXml const* x, std::string& out) {
    switch (x->kind) {
        case MivaXml::Kind::Document:
            for (auto const& c : x->children) mvp_xml_stringify_impl(c.get(), out);
            break;
        case MivaXml::Kind::Element: {
            out.push_back('<');
            out += x->tag;
            for (auto const& kv : x->attrs) {
                out.push_back(' ');
                out += kv.first;
                out += "=\"";
                mvp_xml_escape_attr(kv.second, out);
                out.push_back('"');
            }
            if (x->children.empty()) {
                out += "/>";
            } else {
                out.push_back('>');
                for (auto const& c : x->children) mvp_xml_stringify_impl(c.get(), out);
                out += "</";
                out += x->tag;
                out.push_back('>');
            }
            break;
        }
        case MivaXml::Kind::Text:
            mvp_xml_escape_text(x->text, out);
            break;
        case MivaXml::Kind::Comment:
            out += "<!--";
            mvp_xml_escape_text(x->text, out);
            out += "-->";
            break;
        case MivaXml::Kind::CData:
            out += "<![CDATA[";
            mvp_xml_escape_text(x->text, out);
            out += "]]>";
            break;
        case MivaXml::Kind::PI:
            if (x->pi_target == "xml") {
                out += "<?xml ";
                out += x->pi_data;
                out += "?>";
            } else {
                out += "<?";
                out += x->pi_target;
                if (!x->pi_data.empty()) {
                    out.push_back(' ');
                    out += x->pi_data;
                }
                out += "?>";
            }
            break;
        case MivaXml::Kind::Null:
            break;
    }
}

inline mvp_builtin_string mvp_xml_stringify(mvp_builtin_ptrany v) {
    MivaXml* x = mvp_xml_as_node(v);
    std::string out;
    out.reserve(64);
    mvp_xml_stringify_impl(x, out);
    return out;
}

inline void mvp_xml_free(mvp_builtin_ptrany v) {
    delete static_cast<MivaXml*>(v);
}

#endif // MVP_BUILTIN_H