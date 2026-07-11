use std::cmp::Ordering;
use std::fmt;
use std::rc::Rc;

/// Runtime value in the Miva Virtual Machine.
#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float64(f64),
    Float32(f32),
    Bool(bool),
    Char(u8),
    String(Rc<String>),
    Array(Rc<Vec<Value>>),
    MutableArray(Rc<std::cell::RefCell<Vec<Value>>>),
    Struct(Vec<Value>),
    Boxed(Rc<std::cell::RefCell<Value>>),
    Ptr(usize, Rc<std::cell::RefCell<Vec<Value>>>), // (index, RefCell to frame locals)
    Null,
    PtrAny,
    Range(i64, i64, i64), // start, end, current
    Unit,
}

impl Value {
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Int(_) => "int",
            Value::Float64(_) => "float64",
            Value::Float32(_) => "float32",
            Value::Bool(_) => "bool",
            Value::Char(_) => "char",
            Value::String(_) => "string",
            Value::Array(_) | Value::MutableArray(_) => "array",
            Value::Struct(_) => "struct",
            Value::Boxed(_) => "box",
            Value::Ptr(_, _) => "ptr",
            Value::Null => "null",
            Value::PtrAny => "ptrany",
            Value::Range(_, _, _) => "range",
            Value::Unit => "unit",
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Int(i) => *i != 0,
            Value::Float64(f) => *f != 0.0,
            Value::Float32(f) => *f != 0.0,
            Value::Char(c) => *c != 0,
            Value::Null => false,
            Value::Unit => false,
            Value::String(s) => !s.is_empty(),
            Value::Array(a) => !a.is_empty(),
            Value::MutableArray(a) => !a.borrow().is_empty(),
            Value::Struct(fields) => !fields.is_empty(),
            Value::Boxed(b) => !matches!(*b.borrow(), Value::Null | Value::Unit),
            Value::Ptr(_, _) => true,
            Value::PtrAny => true,
            Value::Range(_, _, _) => true,
        }
    }

    pub fn as_i64(&self) -> Option<i64> {
        match self {
            Value::Int(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_f64(&self) -> Option<f64> {
        match self {
            Value::Float64(f) => Some(*f),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Value::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn display(&self) -> String {
        match self {
            Value::Int(i) => i.to_string(),
            Value::Float64(f) => {
                if f.fract() == 0.0 && f.is_finite() {
                    format!("{}.0", f)
                } else {
                    format!("{}", f)
                }
            }
            Value::Float32(f) => {
                let f64 = *f as f64;
                if f64.fract() == 0.0 && f64.is_finite() {
                    format!("{}.0", f)
                } else {
                    format!("{}", f)
                }
            }
            Value::Bool(b) => b.to_string(),
            Value::Char(c) => format!("'{}'", *c as char),
            Value::String(s) => s.to_string(),
            Value::Array(a) => {
                let items: Vec<String> = a.iter().map(|v| v.display()).collect();
                format!("[{}]", items.join(", "))
            }
            Value::MutableArray(a) => {
                let items: Vec<String> = a.borrow().iter().map(|v| v.display()).collect();
                format!("[{}]", items.join(", "))
            }
            Value::Struct(fields) => {
                format!("({})", fields.iter().map(|v| v.display()).collect::<Vec<_>>().join(", "))
            }
            Value::Boxed(b) => format!("box({})", b.borrow().display()),
            Value::Ptr(_, _) => "<ptr>".to_string(),
            Value::Null => "null".to_string(),
            Value::PtrAny => "<ptrany>".to_string(),
            Value::Range(start, end, current) => format!("range({}, {}, {})", start, end, current),
            Value::Unit => "".to_string(),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.display())
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float64(a), Value::Float64(b)) => a == b,
            (Value::Float32(a), Value::Float32(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Char(a), Value::Char(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Null, Value::Null) => true,
            (Value::Unit, Value::Unit) => true,
            (Value::PtrAny, Value::PtrAny) => true,
            // Cross-type comparisons (Miva allows int-bool etc.)
            (Value::Int(a), Value::Bool(b)) => *a == if *b { 1 } else { 0 },
            (Value::Bool(b), Value::Int(a)) => *a == if *b { 1 } else { 0 },
            _ => false,
        }
    }
}

impl Eq for Value {}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a.partial_cmp(b),
            (Value::Float64(a), Value::Float64(b)) => a.partial_cmp(b),
            (Value::Float32(a), Value::Float32(b)) => a.partial_cmp(b),
            (Value::Bool(a), Value::Bool(b)) => a.partial_cmp(b),
            (Value::Char(a), Value::Char(b)) => a.partial_cmp(b),
            (Value::String(a), Value::String(b)) => Some(a.as_str().cmp(b.as_str())),
            (Value::Int(a), Value::Bool(b)) => a.partial_cmp(&if *b { 1 } else { 0 }),
            (Value::Bool(b), Value::Int(a)) => (if *b { 1 } else { 0 }).partial_cmp(a),
            (Value::Float64(a), Value::Int(b)) => a.partial_cmp(&(*b as f64)),
            (Value::Int(a), Value::Float64(b)) => (*a as f64).partial_cmp(b),
            _ => None,
        }
    }
}
