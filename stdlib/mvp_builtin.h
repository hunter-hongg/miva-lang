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

// ── TOML parsing and serialization (mirrors std/json) ──────────────────────────
//
// TOML documents are parsed into the same `MivaJson` value tree used by JSON
// (the model is identical: Null/Bool/Number/String/Array/Object), so the
// `mvp_toml_*` accessors can delegate to the JSON ones. A parsed document is
// owned by its root handle; child handles are borrowed views (do not free them).
//
// Supported subset:
//   * comments (# ...) and blank lines
//   * dotted and bare keys, `key = value`
//   * values: basic strings, integers (dec/hex/oct/bin), floats, booleans,
//     arrays of scalars, nested [table] and arrays of tables [[table]]
//   * local date/time values are stored as strings

using MivaToml = MivaJson;

struct MivaTomlParser {
    mvp_builtin_string const& s;
    size_t i = 0;

    explicit MivaTomlParser(mvp_builtin_string const& src) : s(src) {}

    [[noreturn]] void fail(char const* msg) {
        mvp_panic(mvp_builtin_string("toml: ") + msg +
                   " (at position " + std::to_string(i) + ")");
    }

    void ws_inline() {
        while (i < s.size()) {
            char c = s[i];
            if (c == ' ' || c == '\t' || c == '\r') ++i;
            else break;
        }
    }

    void skip_comment() {
        if (i < s.size() && s[i] == '#') {
            while (i < s.size() && s[i] != '\n') ++i;
        }
    }

    bool eol() { return i >= s.size() || s[i] == '\n'; }

    char get() {
        if (i >= s.size()) fail("unexpected end of input");
        return s[i++];
    }

    std::string parse_string() {
        // current char is the opening quote
        get();
        std::string out;
        out.reserve(16);
        while (true) {
            if (i >= s.size()) fail("unterminated string");
            char c = get();
            if (c == '"') break;
            if (c == '\\') {
                if (i >= s.size()) fail("bad escape");
                char e = get();
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
                        if (i + 4 > s.size()) fail("bad \\u escape");
                        unsigned v = 0;
                        for (int k = 0; k < 4; ++k) {
                            char ch = s[i++];
                            v <<= 4;
                            if (ch >= '0' && ch <= '9') v |= static_cast<unsigned>(ch - '0');
                            else if (ch >= 'a' && ch <= 'f') v |= static_cast<unsigned>(ch - 'a' + 10);
                            else if (ch >= 'A' && ch <= 'F') v |= static_cast<unsigned>(ch - 'A' + 10);
                            else fail("bad hex digit");
                        }
                        MivaJsonParser::encode_utf8(v, out);
                        break;
                    }
                    default: fail("bad escape");
                }
            } else {
                out.push_back(c);
            }
        }
        return out;
    }

    std::string parse_key() {
        std::string key;
        while (i < s.size()) {
            char c = s[i];
            if (c == '=' || c == '.' || c == ' ' || c == '\t' || c == '\n'
                || c == '#' || c == ']')
                break;
            key.push_back(c);
            ++i;
        }
        if (key.empty()) fail("empty key");
        return key;
    }

    std::unique_ptr<MivaJson> parse_value() {
        ws_inline();
        if (i >= s.size()) fail("expected value");
        char c = s[i];
        if (c == '"') {
            auto v = std::make_unique<MivaJson>(MivaJson::Kind::String);
            v->str = parse_string();
            return v;
        }
        if (c == '[') return parse_array();
        if (c == '{') return parse_inline_table();
        if (c == 't' && s.compare(i, 4, "true") == 0) {
            i += 4;
            auto v = std::make_unique<MivaJson>(MivaJson::Kind::Bool);
            v->b = true;
            return v;
        }
        if (c == 'f' && s.compare(i, 5, "false") == 0) {
            i += 5;
            auto v = std::make_unique<MivaJson>(MivaJson::Kind::Bool);
            v->b = false;
            return v;
        }
        if (c == '-' || (c >= '0' && c <= '9')) {
            size_t start = i;
            bool is_float = false;
            if (s.compare(i, 2, "0x") == 0 || s.compare(i, 2, "0o") == 0 ||
                s.compare(i, 2, "0b") == 0) {
                i += 2;
                while (i < s.size() && ((s[i] >= '0' && s[i] <= '9') ||
                       (s[i] >= 'a' && s[i] <= 'f') || (s[i] >= 'A' && s[i] <= 'F') ||
                       s[i] == '_'))
                    ++i;
                auto v = std::make_unique<MivaJson>(MivaJson::Kind::Number);
                v->num = std::strtoll(s.substr(start, i - start).c_str(), nullptr, 0);
                return v;
            }
            if (s[i] == '-') ++i;
            while (i < s.size() && s[i] >= '0' && s[i] <= '9') ++i;
            if (i < s.size() && s[i] == '.') { is_float = true; ++i; while (i < s.size() && s[i] >= '0' && s[i] <= '9') ++i; }
            if (i < s.size() && (s[i] == 'e' || s[i] == 'E')) {
                is_float = true; ++i;
                if (i < s.size() && (s[i] == '+' || s[i] == '-')) ++i;
                while (i < s.size() && s[i] >= '0' && s[i] <= '9') ++i;
            }
            if (is_float) {
                auto v = std::make_unique<MivaJson>(MivaJson::Kind::Number);
                v->num = std::strtod(s.substr(start, i - start).c_str(), nullptr);
                return v;
            }
            auto v = std::make_unique<MivaJson>(MivaJson::Kind::Number);
            v->num = static_cast<double>(std::strtoll(s.substr(start, i - start).c_str(), nullptr, 10));
            return v;
        }
        // bare / date-time value: treat as string
        std::string raw;
        while (i < s.size() && s[i] != '\n' && s[i] != '#' && s[i] != '\r') {
            raw.push_back(s[i++]);
        }
        size_t b = 0, e = raw.size();
        while (b < e && (raw[b] == ' ' || raw[b] == '\t')) ++b;
        while (e > b && (raw[e - 1] == ' ' || raw[e - 1] == '\t')) --e;
        auto v = std::make_unique<MivaJson>(MivaJson::Kind::String);
        v->str = raw.substr(b, e - b);
        return v;
    }

    std::unique_ptr<MivaJson> parse_array() {
        get(); // '['
        auto v = std::make_unique<MivaJson>(MivaJson::Kind::Array);
        for (;;) {
            ws_inline();
            skip_comment();
            if (eol()) { ++i; ws_inline(); skip_comment(); }
            if (i >= s.size()) fail("unterminated array");
            if (s[i] == ']') { get(); break; }
            v->arr.push_back(parse_value());
            ws_inline();
            skip_comment();
            if (eol()) { ++i; ws_inline(); }
            if (i >= s.size()) fail("unterminated array");
            char c = get();
            if (c == ',') { ws_inline(); skip_comment(); if (eol()) { ++i; ws_inline(); } continue; }
            if (c == ']') break;
            fail("expected ',' or ']' in array");
        }
        return v;
    }

    std::unique_ptr<MivaJson> parse_inline_table() {
        get(); // '{'
        auto v = std::make_unique<MivaJson>(MivaJson::Kind::Object);
        ws_inline();
        if (i < s.size() && s[i] == '}') { get(); return v; }
        for (;;) {
            ws_inline();
            std::string key = parse_key();
            ws_inline();
            if (get() != '=') fail("expected '=' in inline table");
            auto val = parse_value();
            v->obj.emplace_back(std::move(key), std::move(val));
            ws_inline();
            if (i >= s.size()) fail("unterminated inline table");
            char c = get();
            if (c == ',') { ws_inline(); continue; }
            if (c == '}') break;
            fail("expected ',' or '}' in inline table");
        }
        return v;
    }

    // Descend into (creating as needed) the intermediate node for `key`,
    // descending into the last element when the node is an array of tables.
    MivaJson* descend(MivaJson* cur, std::string const& key) {
        MivaJson* next = nullptr;
        for (auto& kv : cur->obj) {
            if (kv.first == key) { next = kv.second.get(); break; }
        }
        if (!next) {
            cur->obj.emplace_back(key, std::make_unique<MivaJson>(MivaJson::Kind::Object));
            next = cur->obj.back().second.get();
        }
        if (next->kind == MivaJson::Kind::Array) {
            if (next->arr.empty())
                next->arr.push_back(std::make_unique<MivaJson>(MivaJson::Kind::Object));
            return next->arr.back().get();
        }
        return next;
    }

    void assign(std::unique_ptr<MivaJson>& root, std::vector<std::string> const& path,
                std::unique_ptr<MivaJson> val, bool as_array) {
        MivaJson* cur = root.get();
        for (size_t k = 0; k + 1 < path.size(); ++k) {
            cur = descend(cur, path[k]);
        }
        std::string const& leaf = path.back();
        if (as_array) {
            MivaJson* arr = nullptr;
            for (auto& kv : cur->obj) {
                if (kv.first == leaf) { arr = kv.second.get(); break; }
            }
            if (!arr) {
                cur->obj.emplace_back(leaf, std::make_unique<MivaJson>(MivaJson::Kind::Array));
                arr = cur->obj.back().second.get();
            }
            arr->arr.push_back(std::move(val));
        } else {
            cur->obj.emplace_back(leaf, std::move(val));
        }
    }

    // For `[[a.b]]`: ensure `a.b` is an array of tables and append a new
    // empty object as the current table.
    void open_array_table(std::unique_ptr<MivaJson>& root,
                         std::vector<std::string> const& path) {
        MivaJson* cur = root.get();
        for (size_t k = 0; k < path.size(); ++k) {
            cur = descend(cur, path[k]);
            if (k + 1 == path.size()) {
                if (cur->kind != MivaJson::Kind::Array) {
                    cur->kind = MivaJson::Kind::Array;
                    cur->arr.clear();
                }
                cur->arr.push_back(std::make_unique<MivaJson>(MivaJson::Kind::Object));
            }
        }
    }

    std::unique_ptr<MivaJson> parse_document() {
        auto root = std::make_unique<MivaJson>(MivaJson::Kind::Object);
        std::vector<std::string> cur_path;
        for (;;) {
            // skip blank/comment lines
            while (i < s.size()) {
                char c = s[i];
                if (c == '\n') { ++i; continue; }
                if (c == ' ' || c == '\t' || c == '\r') { ++i; continue; }
                if (c == '#') { while (i < s.size() && s[i] != '\n') ++i; continue; }
                break;
            }
            if (i >= s.size()) break;
            if (s[i] == '[') {
                get();
                bool array_table = false;
                if (i < s.size() && s[i] == '[') { array_table = true; get(); }
                std::vector<std::string> path;
                for (;;) {
                    ws_inline();
                    path.push_back(parse_key());
                    ws_inline();
                    if (i < s.size() && s[i] == '.') { get(); continue; }
                    break;
                }
                if (array_table) {
                    if (s[i] != ']') fail("expected ']' for array of tables");
                    get();
                    if (s[i] != ']') fail("expected ']' for array of tables");
                    get();
                    open_array_table(root, path);
                } else {
                    if (s[i] != ']') fail("expected ']' in table header");
                    get();
                }
                // consume rest of line
                while (i < s.size() && s[i] != '\n') ++i;
                cur_path = std::move(path);
                continue;
            }
            // key = value
            ws_inline();
            std::vector<std::string> path = cur_path;
            for (;;) {
                path.push_back(parse_key());
                ws_inline();
                if (i < s.size() && s[i] == '.') { get(); ws_inline(); continue; }
                break;
            }
            ws_inline();
            if (get() != '=') fail("expected '='");
            auto val = parse_value();
            assign(root, path, std::move(val), false);
            // consume rest of line
            while (i < s.size() && s[i] != '\n') ++i;
        }
        return root;
    }
};

inline mvp_builtin_ptrany mvp_toml_parse(mvp_builtin_string const& src) {
    MivaTomlParser parser(src);
    auto doc = parser.parse_document();
    return static_cast<mvp_builtin_ptrany>(doc.release());
}

// The TOML value model is identical to JSON, so all accessors delegate to the
// JSON runtime (MivaToml == MivaJson).
inline mvp_builtin_int mvp_toml_kind(mvp_builtin_ptrany v) { return mvp_json_kind(v); }
inline mvp_builtin_int mvp_toml_bool(mvp_builtin_ptrany v) { return mvp_json_bool(v); }
inline mvp_builtin_float mvp_toml_number(mvp_builtin_ptrany v) { return mvp_json_number(v); }
inline mvp_builtin_string mvp_toml_string(mvp_builtin_ptrany v) { return mvp_json_string(v); }
inline mvp_builtin_int mvp_toml_array_len(mvp_builtin_ptrany v) { return mvp_json_array_len(v); }
inline mvp_builtin_ptrany mvp_toml_array_get(mvp_builtin_ptrany v, mvp_builtin_int i) { return mvp_json_array_get(v, i); }
inline mvp_builtin_int mvp_toml_object_len(mvp_builtin_ptrany v) { return mvp_json_object_len(v); }
inline mvp_builtin_string mvp_toml_object_key(mvp_builtin_ptrany v, mvp_builtin_int i) { return mvp_json_object_key(v, i); }
inline mvp_builtin_ptrany mvp_toml_object_get(mvp_builtin_ptrany v, mvp_builtin_int i) { return mvp_json_object_get(v, i); }
inline mvp_builtin_ptrany mvp_toml_object_find(mvp_builtin_ptrany v, mvp_builtin_string const& k) { return mvp_json_object_find(v, k); }
inline void mvp_toml_free(mvp_builtin_ptrany v) { mvp_json_free(v); }

static void mvp_toml_stringify_impl(MivaJson const* j, std::string& out, std::string const& prefix) {
    // serialize an object as TOML [section] tables
    if (j->kind == MivaJson::Kind::Object) {
        bool any_scalar = false;
        for (auto const& kv : j->obj) {
            if (kv.second->kind != MivaJson::Kind::Object &&
                kv.second->kind != MivaJson::Kind::Array) {
                any_scalar = true;
                out += kv.first;
                out += " = ";
                mvp_toml_stringify_impl(kv.second.get(), out, prefix);
                out += "\n";
            }
        }
        if (any_scalar) out += "\n";
        for (auto const& kv : j->obj) {
            if (kv.second->kind == MivaJson::Kind::Object) {
                std::string p = prefix.empty() ? kv.first : prefix + "." + kv.first;
                out += "[";
                out += p;
                out += "]\n";
                mvp_toml_stringify_impl(kv.second.get(), out, p);
            } else if (kv.second->kind == MivaJson::Kind::Array) {
                // array of tables? emit [[...]] when elements are objects
                bool all_obj = !kv.second->arr.empty();
                for (auto const& el : kv.second->arr) {
                    if (el->kind != MivaJson::Kind::Object) { all_obj = false; break; }
                }
                if (all_obj) {
                    for (auto const& el : kv.second->arr) {
                        std::string p = prefix.empty() ? kv.first : prefix + "." + kv.first;
                        out += "[[";
                        out += p;
                        out += "]]\n";
                        mvp_toml_stringify_impl(el.get(), out, p);
                    }
                } else {
                    out += kv.first;
                    out += " = ";
                    mvp_toml_stringify_impl(kv.second.get(), out, prefix);
                    out += "\n\n";
                }
            }
        }
        return;
    }
    if (j->kind == MivaJson::Kind::Array) {
        out.push_back('[');
        for (size_t k = 0; k < j->arr.size(); ++k) {
            if (k) out.push_back(',');
            out.push_back(' ');
            mvp_toml_stringify_impl(j->arr[k].get(), out, prefix);
        }
        out.push_back(']');
        return;
    }
    switch (j->kind) {
        case MivaJson::Kind::Null: out += "null"; break;
        case MivaJson::Kind::Bool: out += j->b ? "true" : "false"; break;
        case MivaJson::Kind::Number: {
            std::string n = std::to_string(j->num);
            auto dot = n.find('.');
            if (dot != std::string::npos) {
                size_t last = n.find_last_not_of('0');
                if (last == std::string::npos || last <= dot) n.erase(dot);
                else n.erase(last + 1);
            }
            out += n;
            break;
        }
        case MivaJson::Kind::String:
            out.push_back('"');
            out += j->str;
            out.push_back('"');
            break;
        default: break;
    }
}

inline mvp_builtin_string mvp_toml_stringify(mvp_builtin_ptrany v) {
    MivaJson* j = mvp_json_as_node(v);
    std::string out;
    out.reserve(64);
    mvp_toml_stringify_impl(j, out, "");
    return out;
}

// ── YAML parsing and serialization (mirrors std/json) ──────────────────────────
//
// YAML documents (block style subset) are parsed into the same `MivaJson` value
// tree used by JSON. The `mvp_yaml_*` accessors delegate to the JSON ones.
//
// Supported subset:
//   * block mappings (indentation-based nesting), inline `{k: v}` maps
//   * block sequences (`- item`), inline `[a, b]` sequences
//   * scalars: plain / single- / double-quoted strings, ints, floats,
//     booleans, null (~/null/empty)

using MivaYaml = MivaJson;

struct MivaYamlParser {
    mvp_builtin_string const& s;
    size_t i = 0;
    std::vector<int> indent_stack;

    explicit MivaYamlParser(mvp_builtin_string const& src) : s(src) {}

    [[noreturn]] void fail(char const* msg) {
        mvp_panic(mvp_builtin_string("yaml: ") + msg +
                   " (at position " + std::to_string(i) + ")");
    }

    void skip_ws() {
        while (i < s.size() && (s[i] == ' ' || s[i] == '\t' || s[i] == '\r')) ++i;
    }
    // Indentation of the current line (scan back to the last newline so the
    // measurement is correct even when `i` points mid-line, e.g. at a dash).
    int cur_indent() {
        size_t start = i;
        while (start > 0 && s[start - 1] != '\n') --start;
        int n = 0;
        while (start < s.size() && (s[start] == ' ' || s[start] == '\t')) {
            if (s[start] != '\t') ++n;
            ++start;
        }
        return n;
    }
    bool eol() { return i >= s.size() || s[i] == '\n'; }
    char peek() { return i < s.size() ? s[i] : '\0'; }
    char get() {
        if (i >= s.size()) fail("unexpected end of input");
        return s[i++];
    }
    void skip_line() { while (i < s.size() && s[i] != '\n') ++i; if (i < s.size()) ++i; }
    void skip_comment() {
        skip_ws();
        if (i < s.size() && s[i] == '#') skip_line();
    }

    std::string parse_scalar() {
        // assumes i points at first scalar char (after optional quote)
        std::string out;
        if (i < s.size() && (s[i] == '"' || s[i] == '\'')) {
            char q = get();
            while (i < s.size() && s[i] != q) out.push_back(get());
            if (i < s.size()) get(); // closing quote
            return out;
        }
        while (i < s.size() && s[i] != '\n' && s[i] != '#' && s[i] != ',' &&
               !(s[i] == ' ' && (i + 1 >= s.size() || s[i + 1] == '#'))) {
            out.push_back(s[i++]);
        }
        size_t b = 0, e = out.size();
        while (b < e && (out[b] == ' ' || out[b] == '\t')) ++b;
        while (e > b && (out[e - 1] == ' ' || out[e - 1] == '\t')) --e;
        return out.substr(b, e - b);
    }

    // Parse a mapping/sequence key: stop at ':' (or ws immediately before
    // ':', newline, or '#'). Unlike parse_scalar, this never consumes
    // the value following the colon.
    std::string parse_key() {
        std::string out;
        while (i < s.size()) {
            char c = s[i];
            if (c == ':' || c == '\n' || c == '#') break;
            if (c == ' ' || c == '\t') {
                size_t j = i;
                while (j < s.size() && (s[j] == ' ' || s[j] == '\t')) ++j;
                if (j >= s.size() || s[j] == ':' || s[j] == '#' || s[j] == '\n') break;
            }
            out.push_back(c);
            ++i;
        }
        size_t b = 0, e = out.size();
        while (b < e && (out[b] == ' ' || out[b] == '\t')) ++b;
        while (e > b && (out[e - 1] == ' ' || out[e - 1] == '\t')) --e;
        return out.substr(b, e - b);
    }

    std::unique_ptr<MivaJson> coerce(std::string const& raw) {
        if (raw == "true") { auto v = std::make_unique<MivaJson>(MivaJson::Kind::Bool); v->b = true; return v; }
        if (raw == "false") { auto v = std::make_unique<MivaJson>(MivaJson::Kind::Bool); v->b = false; return v; }
        if (raw == "~" || raw == "null" || raw == "Null" || raw == "NULL" || raw.empty()) {
            return std::make_unique<MivaJson>(MivaJson::Kind::Null);
        }
        // int
        bool is_int = !raw.empty();
        for (size_t k = 0; k < raw.size(); ++k) {
            char c = raw[k];
            if (k == 0 && c == '-') continue;
            if (c < '0' || c > '9') { is_int = false; break; }
        }
        if (is_int) {
            auto v = std::make_unique<MivaJson>(MivaJson::Kind::Number);
            v->num = static_cast<double>(std::strtoll(raw.c_str(), nullptr, 10));
            return v;
        }
        // float
        bool is_float = !raw.empty();
        bool dot = false;
        for (size_t k = 0; k < raw.size(); ++k) {
            char c = raw[k];
            if (k == 0 && c == '-') continue;
            if (c == '.') { dot = true; continue; }
            if ((c < '0' || c > '9') && c != 'e' && c != 'E' && c != '+' && c != '-') { is_float = false; break; }
        }
        if (is_float && dot) {
            auto v = std::make_unique<MivaJson>(MivaJson::Kind::Number);
            v->num = std::strtod(raw.c_str(), nullptr);
            return v;
        }
        auto v = std::make_unique<MivaJson>(MivaJson::Kind::String);
        v->str = raw;
        return v;
    }

    // Parse a mapping at the given indent level (block style).
    std::unique_ptr<MivaJson> parse_mapping(int ind) {
        auto obj = std::make_unique<MivaJson>(MivaJson::Kind::Object);
        for (;;) {
            skip_ws();
            if (eol()) { if (i < s.size()) { ++i; continue; } break; }
            if (i >= s.size()) break;
            if (cur_indent() < ind) break;
            if (cur_indent() > ind) break; // shouldn't happen
            // key:
            std::string key = parse_key();
            skip_ws();
            if (peek() != ':') break;
            get(); // ':'
            skip_ws();
            if (eol() || i >= s.size()) {
                // nested block
                if (i < s.size()) ++i;
                skip_ws();
                if (i < s.size() && s[i] == '-') {
                    obj->obj.emplace_back(key, parse_sequence(ind + 2));
                } else {
                    obj->obj.emplace_back(key, parse_mapping(ind + 2));
                }
            } else if (peek() == '[') {
                obj->obj.emplace_back(key, parse_flow());
                skip_line();
            } else if (peek() == '{') {
                obj->obj.emplace_back(key, parse_flow());
                skip_line();
            } else {
                std::string val = parse_scalar();
                obj->obj.emplace_back(key, coerce(val));
                skip_line();
            }
        }
        return obj;
    }

    // Parse a block sequence (`- item`) at current indent.
    std::unique_ptr<MivaJson> parse_sequence(int ind) {
        auto arr = std::make_unique<MivaJson>(MivaJson::Kind::Array);
        for (;;) {
            skip_ws();
            if (eol()) { if (i < s.size()) { ++i; continue; } break; }
            if (i >= s.size()) break;
            if (cur_indent() < ind) break;
            if (peek() != '-') break;
            get(); // '-'
            skip_ws();
            if (eol() || i >= s.size()) {
                if (i < s.size()) ++i;
                skip_ws();
                if (i < s.size() && s[i] == '-') {
                    arr->arr.push_back(parse_sequence(ind + 2));
                } else {
                    arr->arr.push_back(parse_mapping(ind + 2));
                }
            } else if (peek() == '[') {
                arr->arr.push_back(parse_flow());
                skip_line();
            } else if (peek() == '{') {
                arr->arr.push_back(parse_flow());
                skip_line();
            } else {
                // could be `key: value` (sequence of mappings) or a scalar
                size_t j = i;
                std::string first = parse_key();
                skip_ws();
                if (i < s.size() && peek() == ':' && (i + 1 >= s.size() || s[i + 1] == ' ')) {
                    // sequence of mappings: rewind and parse a mapping at this dash indent
                    i = j;
                    auto m = std::make_unique<MivaJson>(MivaJson::Kind::Object);
                    // parse one mapping block that follows, treating it as nested under this item.
                    // Keys in `- k: v` items live at dash_indent + 2.
                    for (;;) {
                        skip_ws();
                        if (eol()) { if (i < s.size()) { ++i; continue; } break; }
                        if (i >= s.size()) break;
                        // a dash starts the next sequence item -> this mapping is done
                        if (peek() == '-') break;
                        int ci = cur_indent() + 2;
                        if (ci <= ind) break;
                        std::string key = parse_key();
                        skip_ws();
                        if (peek() != ':') break;
                        get();
                        skip_ws();
                        if (eol() || i >= s.size()) {
                            if (i < s.size()) ++i;
                            skip_ws();
                            if (i < s.size() && s[i] == '-') m->obj.emplace_back(key, parse_sequence(ci));
                            else m->obj.emplace_back(key, parse_mapping(ci));
                        } else if (peek() == '[') {
                            m->obj.emplace_back(key, parse_flow()); skip_line();
                        } else if (peek() == '{') {
                            m->obj.emplace_back(key, parse_flow()); skip_line();
                        } else {
                            m->obj.emplace_back(key, coerce(parse_scalar())); skip_line();
                        }
                    }
                    arr->arr.push_back(std::move(m));
                } else {
                    arr->arr.push_back(coerce(first));
                    skip_line();
                }
            }
        }
        return arr;
    }

    std::unique_ptr<MivaJson> parse_flow() {
        char open = get();
        auto node = (open == '[')
            ? std::make_unique<MivaJson>(MivaJson::Kind::Array)
            : std::make_unique<MivaJson>(MivaJson::Kind::Object);
        skip_ws();
        if (i < s.size() && (s[i] == ']' || s[i] == '}')) { get(); return node; }
        for (;;) {
            skip_ws();
            if (open == '[') {
                if (i < s.size() && s[i] == ']') { get(); break; }
                if (i < s.size() && (s[i] == '"' || s[i] == '\'')) {
                    node->arr.push_back(coerce(parse_scalar()));
                } else {
                    node->arr.push_back(coerce(parse_scalar()));
                }
            } else {
                if (i < s.size() && s[i] == '}') { get(); break; }
                std::string k = parse_key();
                skip_ws();
                if (i < s.size() && s[i] == ':') { get(); skip_ws(); }
                if (i < s.size() && s[i] == '[') node->obj.emplace_back(k, parse_flow());
                else if (i < s.size() && s[i] == '{') node->obj.emplace_back(k, parse_flow());
                else node->obj.emplace_back(k, coerce(parse_key()));
            }
            skip_ws();
            if (i < s.size() && s[i] == ',') { get(); skip_ws(); continue; }
            if (i < s.size() && (s[i] == ']' || s[i] == '}')) { get(); break; }
            skip_line();
        }
        return node;
    }

    std::unique_ptr<MivaJson> parse_document() {
        skip_ws();
        skip_comment();
        if (i >= s.size()) return std::make_unique<MivaJson>(MivaJson::Kind::Null);
        if (peek() == '-') return parse_sequence(0);
        if (peek() == '[' || peek() == '{') return parse_flow();
        return parse_mapping(0);
    }
};

inline mvp_builtin_ptrany mvp_yaml_parse(mvp_builtin_string const& src) {
    MivaYamlParser parser(src);
    auto doc = parser.parse_document();
    return static_cast<mvp_builtin_ptrany>(doc.release());
}

inline mvp_builtin_int mvp_yaml_kind(mvp_builtin_ptrany v) { return mvp_json_kind(v); }
inline mvp_builtin_int mvp_yaml_bool(mvp_builtin_ptrany v) { return mvp_json_bool(v); }
inline mvp_builtin_float mvp_yaml_number(mvp_builtin_ptrany v) { return mvp_json_number(v); }
inline mvp_builtin_string mvp_yaml_string(mvp_builtin_ptrany v) { return mvp_json_string(v); }
inline mvp_builtin_int mvp_yaml_array_len(mvp_builtin_ptrany v) { return mvp_json_array_len(v); }
inline mvp_builtin_ptrany mvp_yaml_array_get(mvp_builtin_ptrany v, mvp_builtin_int i) { return mvp_json_array_get(v, i); }
inline mvp_builtin_int mvp_yaml_object_len(mvp_builtin_ptrany v) { return mvp_json_object_len(v); }
inline mvp_builtin_string mvp_yaml_object_key(mvp_builtin_ptrany v, mvp_builtin_int i) { return mvp_json_object_key(v, i); }
inline mvp_builtin_ptrany mvp_yaml_object_get(mvp_builtin_ptrany v, mvp_builtin_int i) { return mvp_json_object_get(v, i); }
inline mvp_builtin_ptrany mvp_yaml_object_find(mvp_builtin_ptrany v, mvp_builtin_string const& k) { return mvp_json_object_find(v, k); }
inline void mvp_yaml_free(mvp_builtin_ptrany v) { mvp_json_free(v); }

static void mvp_yaml_stringify_impl(MivaJson const* j, std::string& out, int ind) {
    auto pad = [&](int n) { for (int k = 0; k < n; ++k) out.push_back(' '); };
    if (j->kind == MivaJson::Kind::Object) {
        bool first = true;
        for (auto const& kv : j->obj) {
            if (!first) out.push_back('\n');
            first = false;
            pad(ind);
            out += kv.first;
            out += ":";
            if (kv.second->kind == MivaJson::Kind::Object) { out.push_back('\n'); mvp_yaml_stringify_impl(kv.second.get(), out, ind + 2); }
            else if (kv.second->kind == MivaJson::Kind::Array) { out.push_back('\n'); mvp_yaml_stringify_impl(kv.second.get(), out, ind + 2); }
            else { out.push_back(' '); mvp_yaml_stringify_impl(kv.second.get(), out, 0); }
        }
        return;
    }
    if (j->kind == MivaJson::Kind::Array) {
        bool first = true;
        for (auto const& el : j->arr) {
            if (!first) out.push_back('\n');
            first = false;
            pad(ind);
            out += "-";
            if (el->kind == MivaJson::Kind::Object || el->kind == MivaJson::Kind::Array) { out.push_back('\n'); mvp_yaml_stringify_impl(el.get(), out, ind + 2); }
            else { out.push_back(' '); mvp_yaml_stringify_impl(el.get(), out, 0); }
        }
        return;
    }
    switch (j->kind) {
        case MivaJson::Kind::Null: out += "null"; break;
        case MivaJson::Kind::Bool: out += j->b ? "true" : "false"; break;
        case MivaJson::Kind::Number: {
            std::string n = std::to_string(j->num);
            auto dot = n.find('.');
            if (dot != std::string::npos) {
                size_t last = n.find_last_not_of('0');
                if (last == std::string::npos || last <= dot) n.erase(dot);
                else n.erase(last + 1);
            }
            out += n;
            break;
        }
        case MivaJson::Kind::String: out += j->str; break;
        default: break;
    }
}

inline mvp_builtin_string mvp_yaml_stringify(mvp_builtin_ptrany v) {
    MivaJson* j = mvp_json_as_node(v);
    std::string out;
    out.reserve(64);
    mvp_yaml_stringify_impl(j, out, 0);
    return out;
}

#endif // MVP_BUILTIN_H