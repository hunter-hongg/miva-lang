//! MVP Programming Language Built-in Functions
#ifndef MVP_BUILTIN_H
#define MVP_BUILTIN_H

#include <climits>
#include <cstdarg>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cinttypes>
#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>
#include <format>
#include <memory>
#include <utility>

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
    int len = std::snprintf(buf, sizeof(buf), "%.17g", v);
    return mvp_builtin_string(buf, static_cast<size_t>(len));
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

#endif // MVP_BUILTIN_H