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
  printf("panic: %s\n", str.c_str());
  std::abort();
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

#endif // MVP_BUILTIN_H