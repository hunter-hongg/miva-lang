//! MVP Programming Language Built-in Functions
#ifndef MVP_BUILTIN_H
#define MVP_BUILTIN_H

#include <cstdint>
#include <cstdlib>
#include <stdexcept>
#include <string>

typedef struct {} mvp_builtin_void;     // void type，为了避免return voidFunc()失效，采用这种写法
                                        // 所有Builtin返回void时都应当返回mvp_builtin_void类型
typedef int mvp_builtin_main_return;    // main函数返回值类型必须为int
typedef int64_t mvp_builtin_int;        // 默认int类型
typedef int32_t mvp_builtin_int32;      // 默认int32类型
typedef int16_t mvp_builtin_short;      // 默认short类型
typedef int8_t mvp_builtin_byte;        // 默认byte类型
typedef uint64_t mvp_builtin_uint;      // 默认unsigned int类型
typedef uint32_t mvp_builtin_uint32;    // 默认unsigned int32类型
typedef uint16_t mvp_builtin_ushort;    // 默认unsigned short类型
typedef uint8_t mvp_builtin_ubyte;      // 默认unsigned byte类型
typedef std::string mvp_builtin_string; // 默认string类型

inline mvp_builtin_void panic(mvp_builtin_string const &str) {
  printf("panic: %s\n", str.c_str());
  std::abort();
}

inline mvp_builtin_string string_concat(mvp_builtin_string const &str1,
                                        mvp_builtin_string const &str2) {
  return str1 + str2;
}
inline mvp_builtin_int string_length(mvp_builtin_string const &str) {
  return str.length();
}
inline mvp_builtin_int string_parse(mvp_builtin_string const &str) {
  try {
    auto res = std::stoi(str);
    return res;
  } catch (...) {
    panic("string_parse: invalid string");
    return 0; // never reach
  }
}

#endif // MVP_BUILTIN_H