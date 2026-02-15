//! MVP Programming Language Built-in Functions
#ifndef MVP_BUILTIN_H
#define MVP_BUILTIN_H

#include <climits>
#include <cstdint>
#include <cstdlib>
#include <stdexcept>
#include <string>
#include <vector>

extern "C" {
typedef struct {
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
}
typedef std::string mvp_builtin_string; // 默认string类型

inline mvp_builtin_unit panic(mvp_builtin_string const &str) {
  printf("panic: %s\n", str.c_str());
  std::abort();
}

inline mvp_builtin_string mvp_string_concat(mvp_builtin_string const &str1,
                                            mvp_builtin_string const &str2) {
  return str1 + str2;
}
inline mvp_builtin_int mvp_string_length(mvp_builtin_string const &str) {
  return str.length();
}
inline mvp_builtin_int mvp_string_parse(mvp_builtin_string const &str) {
  if (str.empty()) {
    panic("string_parse: empty string");
  }

  int result = 0; // ← 初始化为 0！

  for (char c : str) {
    if (c < '0' || c > '9') {
      panic("string_parse: invalid digit in string");
    }

    // 检查乘法溢出：result * 10
    if (result > INT_MAX / 10) {
      panic("string_parse: integer overflow");
    }
    result *= 10;

    int digit = c - '0';
    if (result > INT_MAX - digit) {
      panic("string_parse: integer overflow");
    }
    result += digit;
  }

  return result;
}

inline mvp_builtin_unit mvp_print(mvp_builtin_string const &str) {
  printf("%s", str.c_str());
  return {};
}

inline mvp_builtin_unit mvp_println(mvp_builtin_string const &str) {
  printf("%s\n", str.c_str());
  return {};
}

#endif // MVP_BUILTIN_H