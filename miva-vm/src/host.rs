use std::sync::Arc;

use crate::value::Value;

/// C-compatible value passed between the MVM interpreter and host functions
/// (user `unsafe fn`s compiled into libhost.so). The layout MUST match the
/// `MivaValue` struct emitted in `mvp_host.h`.
#[repr(C)]
pub struct MivaValue {
    pub tag: i32,
    pub data: MivaValueData,
}

#[repr(C)]
pub union MivaValueData {
    pub i: i64,
    pub f: f64,
    pub b: bool,
    /// Pointer to a NUL-terminated UTF-8 string (owned by the host side).
    pub s: *mut std::os::raw::c_char,
}

/// Tag values; must match `mvp_host.h`.
pub const TAG_INT: i32 = 0;
pub const TAG_F64: i32 = 1;
pub const TAG_BOOL: i32 = 2;
pub const TAG_STRING: i32 = 3;
pub const TAG_NULL: i32 = 4;
pub const TAG_UNIT: i32 = 5;

impl MivaValue {
    pub fn int(v: i64) -> Self {
        MivaValue { tag: TAG_INT, data: MivaValueData { i: v } }
    }
    pub fn f64(v: f64) -> Self {
        MivaValue { tag: TAG_F64, data: MivaValueData { f: v } }
    }
    pub fn bool(v: bool) -> Self {
        MivaValue { tag: TAG_BOOL, data: MivaValueData { b: v } }
    }
    pub fn null() -> Self {
        MivaValue { tag: TAG_NULL, data: MivaValueData { i: 0 } }
    }
    pub fn unit() -> Self {
        MivaValue { tag: TAG_UNIT, data: MivaValueData { i: 0 } }
    }
    /// Take ownership of a NUL-terminated C string (must be freed by host).
    pub fn string_owned(s: *mut std::os::raw::c_char) -> Self {
        MivaValue { tag: TAG_STRING, data: MivaValueData { s } }
    }
}

impl From<Value> for MivaValue {
    fn from(v: Value) -> Self {
        match v {
            Value::Int(i) => MivaValue::int(i),
            Value::Float64(f) => MivaValue::f64(f),
            Value::Float32(f) => MivaValue::f64(f as f64),
            Value::Bool(b) => MivaValue::bool(b),
            Value::Char(c) => MivaValue::int(c as i64),
            Value::String(s) => {
                let c = std::ffi::CString::new(s.as_str()).unwrap_or_default();
                MivaValue::string_owned(c.into_raw())
            }
            Value::Null => MivaValue::null(),
            _ => MivaValue::unit(),
        }
    }
}

impl MivaValue {
    /// Convert a host-returned value back into a VM `Value`. Frees any
    /// transferred C string, taking ownership into a Rust `String`.
    pub fn into_value(self) -> Value {
        match self.tag {
            TAG_INT => Value::Int(unsafe { self.data.i }),
            TAG_F64 => Value::Float64(unsafe { self.data.f }),
            TAG_BOOL => Value::Bool(unsafe { self.data.b }),
            TAG_STRING => {
                let ptr = unsafe { self.data.s };
                if ptr.is_null() {
                    Value::String(Arc::new(String::new()))
                } else {
                    let cstr = unsafe { std::ffi::CString::from_raw(ptr) };
                    Value::String(Arc::new(cstr.to_string_lossy().into_owned()))
                }
            }
            TAG_NULL => Value::Null,
            _ => Value::Unit,
        }
    }
}

/// Generate the `mvp_host.h` header content for `libhost.c`.
pub fn host_header() -> String {
    r#"#ifndef MVP_HOST_H
#define MVP_HOST_H
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

typedef struct MivaValue MivaValue;
struct MivaValue {
    int32_t tag;
    union {
        int64_t i;
        double f;
        bool b;
        char* s;
    } data;
};

#define MVP_TAG_INT    0
#define MVP_TAG_F64    1
#define MVP_TAG_BOOL   2
#define MVP_TAG_STRING 3
#define MVP_TAG_NULL   4
#define MVP_TAG_UNIT   5

static inline MivaValue mvp_int(int64_t v)   { MivaValue x; x.tag = MVP_TAG_INT;    x.data.i = v; return x; }
static inline MivaValue mvp_f64(double v)    { MivaValue x; x.tag = MVP_TAG_F64;   x.data.f = v; return x; }
static inline MivaValue mvp_bool(bool v)     { MivaValue x; x.tag = MVP_TAG_BOOL;  x.data.b = v; return x; }
static inline MivaValue mvp_null(void)       { MivaValue x; x.tag = MVP_TAG_NULL;  x.data.i = 0; return x; }
static inline MivaValue mvp_unit(void)       { MivaValue x; x.tag = MVP_TAG_UNIT;  x.data.i = 0; return x; }
static inline MivaValue mvp_string(char* s)  { MivaValue x; x.tag = MVP_TAG_STRING; x.data.s = s; return x; }

static inline int64_t  mvp_as_int(const MivaValue* v)  { return v->data.i; }
static inline double   mvp_as_f64(const MivaValue* v)  { return v->data.f; }
static inline bool     mvp_as_bool(const MivaValue* v) { return v->data.b; }
static inline char*    mvp_as_string(const MivaValue* v) { return v->data.s; }

#endif
"#
    .to_string()
}

/// Signature of a host function loaded from libhost.so. Matches the C typedef
/// `MivaValue (*)(const MivaValue* args, int argc)`.
pub type HostFn = unsafe extern "C" fn(*const MivaValue, i32) -> MivaValue;

/// LLVM-backend variant of `host_header`. Uses a flat `{ int64_t tag; int64_t data; }`
/// layout (no `double` member) so the struct is returned in RAX:RDX per the
/// x86-64 System V ABI, matching the LLVM IR `%MivaValue = type { i64, i64 }`.
/// The standard `host_header` contains a `double` in the union, which forces
/// SSE-class return (XMM0) and breaks the LLVM IR struct-return convention.
pub fn host_header_llvm() -> String {
    r#"#ifndef MVP_HOST_LLVM_H
#define MVP_HOST_LLVM_H
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

typedef struct MivaValue MivaValue;
struct MivaValue {
    int64_t tag;
    int64_t data;
};

#define MVP_TAG_INT    0
#define MVP_TAG_F64    1
#define MVP_TAG_BOOL   2
#define MVP_TAG_STRING 3
#define MVP_TAG_NULL   4
#define MVP_TAG_UNIT   5

static inline MivaValue mvp_int(int64_t v)   { MivaValue x; x.tag = MVP_TAG_INT;    x.data = v; return x; }
static inline MivaValue mvp_f64(double v)    { MivaValue x; x.tag = MVP_TAG_F64;   memcpy(&x.data, &v, 8); return x; }
static inline MivaValue mvp_bool(bool v)     { MivaValue x; x.tag = MVP_TAG_BOOL;  x.data = v ? 1 : 0; return x; }
static inline MivaValue mvp_null(void)       { MivaValue x; x.tag = MVP_TAG_NULL;  x.data = 0; return x; }
static inline MivaValue mvp_unit(void)       { MivaValue x; x.tag = MVP_TAG_UNIT;  x.data = 0; return x; }
static inline MivaValue mvp_string(char* s)  { MivaValue x; x.tag = MVP_TAG_STRING; x.data = (int64_t)(intptr_t)s; return x; }

static inline int64_t  mvp_as_int(const MivaValue* v)  { return v->data; }
static inline double   mvp_as_f64(const MivaValue* v)  { double d; memcpy(&d, &v->data, 8); return d; }
static inline bool     mvp_as_bool(const MivaValue* v) { return v->data != 0; }
static inline char*    mvp_as_string(const MivaValue* v) { return (char*)(intptr_t)v->data; }

#endif
"#
    .to_string()
}
