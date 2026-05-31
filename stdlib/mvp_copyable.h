#ifndef IS_COPYABLE_HPP
#define IS_COPYABLE_HPP

#include <type_traits>
#include <boost/pfr.hpp>
#include <cstddef> // for std::size_t

// ------------------------------------------------------------------
// 1. 定义“固定类型”
// ------------------------------------------------------------------
template <typename T>
struct is_fixed_type : std::false_type {};

// 使用宏快速注册基础类型，随后立即取消定义以避免污染
#define DEFINE_FIXED_TYPE(T) template <> struct is_fixed_type<T> : std::true_type {}

DEFINE_FIXED_TYPE(bool);
DEFINE_FIXED_TYPE(char);
DEFINE_FIXED_TYPE(signed char);
DEFINE_FIXED_TYPE(unsigned char);
DEFINE_FIXED_TYPE(short);
DEFINE_FIXED_TYPE(unsigned short);
DEFINE_FIXED_TYPE(int);
DEFINE_FIXED_TYPE(unsigned int);
DEFINE_FIXED_TYPE(long);
DEFINE_FIXED_TYPE(unsigned long);
DEFINE_FIXED_TYPE(long long);
DEFINE_FIXED_TYPE(unsigned long long);
DEFINE_FIXED_TYPE(float);
DEFINE_FIXED_TYPE(double);
DEFINE_FIXED_TYPE(long double);

#undef DEFINE_FIXED_TYPE

template <typename T>
inline constexpr bool is_fixed_type_v = is_fixed_type<T>::value;

// ------------------------------------------------------------------
// 2. 主模板前向声明
// ------------------------------------------------------------------
template <typename T>
struct is_copyable;

template <typename T>
inline constexpr bool is_copyable_v = is_copyable<T>::value;

// ------------------------------------------------------------------
// 3. 辅助工具：递归检查所有字段 (模板结构体版，比 consteval 函数更适合头文件)
// ------------------------------------------------------------------

// 递归辅助结构体：检查第 I 个字段
template <typename T, std::size_t I>
struct field_checker {
    using CurrentType = boost::pfr::tuple_element_t<I, T>;
    // 当前字段合法 && 剩余字段合法
    static constexpr bool value = is_copyable_v<CurrentType> && 
                                  field_checker<T, I - 1>::value;
};

// 递归终点：检查第 0 个字段
template <typename T>
struct field_checker<T, 0> {
    using CurrentType = boost::pfr::tuple_element_t<0, T>;
    static constexpr bool value = is_copyable_v<CurrentType>;
};

// 入口：根据字段数量启动检查
template <typename T>
struct all_fields_check {
    static constexpr std::size_t count = boost::pfr::tuple_size_v<T>;
    
    // 空结构体返回 true，否则从最后一个字段开始递归
    static constexpr bool value = (count == 0) ? true : field_checker<T, count - 1>::value;
};

// ------------------------------------------------------------------
// 4. 特化逻辑 (Concepts)
// ------------------------------------------------------------------

// 情况 A: 固定类型 -> true
template <typename T>
    requires is_fixed_type_v<T>
struct is_copyable<T> : std::true_type {};

// 情况 B: 聚合体且所有字段合法 -> true
// 使用 all_fields_check<T>::value 替代 consteval 函数，避免定义顺序问题
template <typename T>
    requires (
        std::is_class_v<T> && 
        !std::is_array_v<T> && 
        std::is_aggregate_v<T> &&
        all_fields_check<T>::value 
    )
struct is_copyable<T> : std::true_type {};

// 情况 C: 默认 -> false
template <typename T>
struct is_copyable : std::false_type {};

#endif // IS_COPYABLE_HPP
