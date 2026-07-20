/// Opcode definitions for the Miva Virtual Machine (MVM).
///
/// Each instruction is: opcode (1 byte) + operands (variable length).
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Opcode {
    // --- Stack operations (0x00-0x0F) ---
    Nop = 0x00,
    /// Push i64 constant (8 bytes LE)
    PushI64 = 0x01,
    /// Push f64 constant (8 bytes LE)
    PushF64 = 0x02,
    /// Push f32 constant (4 bytes LE)
    PushF32 = 0x03,
    /// Push bool constant (1 byte: 0 or 1)
    PushBool = 0x04,
    /// Push char constant (1 byte)
    PushChar = 0x05,
    /// Push string from pool (4 bytes LE u32 index)
    PushString = 0x06,
    /// Push null
    PushNull = 0x07,
    /// Push unit (void)
    PushUnit = 0x08,
    /// Duplicate top of stack
    Dup = 0x09,
    /// Pop and discard
    Drop = 0x0A,
    /// Swap top two values
    Swap = 0x0B,
    /// Push values from stack as a range object (pops count, then count values)
    Pack = 0x0C,

    // --- Local variables (0x0D-0x0F) ---
    /// Load local variable (4 bytes LE u32 index) → push value
    LoadLocal = 0x0D,
    /// Store to local variable (4 bytes LE u32 index) → pop value
    StoreLocal = 0x0E,

    // --- Integer arithmetic (0x10-0x1F) ---
    I64Add = 0x10,
    I64Sub = 0x11,
    I64Mul = 0x12,
    I64Div = 0x13,
    I64Rem = 0x14,
    I64Neg = 0x15,
    I64Shl = 0x16,
    I64Shr = 0x17,
    I64And = 0x18,
    I64Or = 0x19,
    I64Xor = 0x1A,

    // --- Float arithmetic (0x1B-0x2F) ---
    F64Add = 0x1B,
    F64Sub = 0x1C,
    F64Mul = 0x1D,
    F64Div = 0x1E,
    F64Neg = 0x1F,
    F32Add = 0x20,
    F32Sub = 0x21,
    F32Mul = 0x22,
    F32Div = 0x23,
    F32Neg = 0x24,

    // --- Comparison (0x25-0x2F) ---
    CmpEq = 0x25,
    CmpNeq = 0x26,
    CmpLt = 0x27,
    CmpGt = 0x28,
    CmpLe = 0x29,
    CmpGe = 0x2A,

    // --- Control flow (0x30-0x3F) ---
    /// Unconditional jump (4 bytes LE i32 offset from current position)
    Jmp = 0x30,
    /// Jump if top of stack is true (4 bytes LE i32 offset)
    JmpIf = 0x31,
    /// Jump if top of stack is false (4 bytes LE i32 offset)
    JmpIfNot = 0x32,
    /// Call function (4 bytes LE u32 function index). Args already on stack.
    Call = 0x33,
    /// Call builtin function (1 byte u8 builtin index)
    CallBuiltin = 0x34,
    /// Return from function (no return value)
    Ret = 0x35,
    /// Return value from function (pop value, return)
    RetVal = 0x36,

    // --- Array operations (0x40-0x4F) ---
    /// Create array with elements from stack (pops size, then size values)
    ArrayNew = 0x40,
    /// Get element from array (pop index, pop array → push value)
    ArrayGet = 0x41,
    /// Set element in array (pop value, pop index, pop array)
    ArraySet = 0x42,
    /// Get array length (pop array → push i64)
    ArrayLen = 0x43,
    /// Push element to array (pop value, pop array)
    ArrayPush = 0x44,

    // --- Struct operations (0x50-0x5F) ---
    /// Create struct (4 bytes LE u32 field count; pops field_count values)
    StructNew = 0x50,
    /// Get struct field by index (4 bytes LE u32 field index; pop struct → push value)
    StructGet = 0x51,
    /// Set struct field by index (4 bytes LE u32 field index; pop value, pop struct)
    StructSet = 0x52,
    /// Duplicate top value (for struct field assignment like `s.f = expr`)
    StructDupAt = 0x53,
    /// Build an enum value: tag was pushed first, then `field_count` payload values.
    EnumNew = 0x54,
    /// Read enum payload field by index.
    EnumGet = 0x55,

    // --- Box operations (0x60-0x6F) ---
    /// Create a new box containing the popped value
    BoxNew = 0x60,
    /// Unbox (pop box → push inner value)
    BoxGet = 0x61,
    /// Set box contents (pop value, pop box)
    BoxSet = 0x62,

    // --- Pointer operations (0x63-0x6F) ---
    /// Take address of local (4 bytes LE u32 local index → push ptr)
    Addr = 0x63,
    /// Load through pointer (pop ptr → push value)
    PtrLoad = 0x64,
    /// Store through pointer (pop value, pop ptr)
    PtrStore = 0x65,

    // --- Closure operations (0x66-0x67) ---
    /// Build a closure value: read a 4-byte LE u32 capture count, pop that
    /// many capture values (in declaration order), then read a 4-byte LE u32
    /// thunk function index, and push a closure value carrying the captures
    /// and the thunk index.
    MakeClosure = 0x66,
    /// Call a closure value: pop `argc` argument values, then pop the closure
    /// value. Push the closure's captures, then the arguments, then call the
    /// thunk function index (arity = captures + arguments).
    CallClosure = 0x67,

    // --- Type conversions (0x70-0x7F) ---
    I64ToF64 = 0x70,
    F64ToI64 = 0x71,
    I64ToChar = 0x72,
    CharToI64 = 0x73,
    F64ToF32 = 0x74,
    F32ToF64 = 0x75,
    I64ToBool = 0x76,
    BoolToI64 = 0x77,
    I64ToF32 = 0x78,
    F32ToI64 = 0x79,

    // --- String operations (0x80-0x8F) ---
    /// Concatenate two strings (pop rhs, pop lhs → push result)
    StrConcat = 0x80,
    /// Get string length (pop string → push i64)
    StrLen = 0x81,
    /// Parse string to i64 (pop string → push i64)
    StrParse = 0x82,
    /// Make string from char × length (pop i64 len, pop u8 char → push string)
    StrMake = 0x83,
    /// Convert value to string (pop value → push string)
    StrFrom = 0x84,
    /// Get character from string at index
    StrGet = 0x85,

    // --- I/O (0x90-0x9F) ---
    Print = 0x90,
    Prints = 0x91,
    Println = 0x92,
    Printlns = 0x93,
    Error = 0x94,
    Errors = 0x95,
    Errorln = 0x96,
    Errorlns = 0x97,
    Exit = 0x98,
    Abort = 0x99,
    Panic = 0x9A,
    ReadInt = 0x9B,
    ReadLine = 0x9C,

    // --- Range / Iteration (0xA0-0xAF) ---
    RangeNew = 0xA0,
    RangeNext = 0xA1,

    // --- Miscellaneous (0xB0-0xFF) ---
    /// Halt execution
    Halt = 0xB0,
    /// Debug: print stack trace
    Debug = 0xB1,
    /// Clone value on top of stack
    Clone = 0xB2,
    /// Await a future: if top is a future, push its inner value; else identity
    Await = 0xB4,

    /// Call a host function (user `unsafe fn` implemented in libhost.so).
    /// Operands: (4 bytes LE u32 name string-pool index, 1 byte u8 arity).
    CallHost = 0xC0,
}

impl Opcode {
    pub fn from_u8(b: u8) -> Option<Opcode> {
        use Opcode::*;
        match b {
            0x00 => Some(Nop),
            0x01 => Some(PushI64),
            0x02 => Some(PushF64),
            0x03 => Some(PushF32),
            0x04 => Some(PushBool),
            0x05 => Some(PushChar),
            0x06 => Some(PushString),
            0x07 => Some(PushNull),
            0x08 => Some(PushUnit),
            0x09 => Some(Dup),
            0x0A => Some(Drop),
            0x0B => Some(Swap),
            0x0C => Some(Pack),
            0x0D => Some(LoadLocal),
            0x0E => Some(StoreLocal),
            0x10 => Some(I64Add),
            0x11 => Some(I64Sub),
            0x12 => Some(I64Mul),
            0x13 => Some(I64Div),
            0x14 => Some(I64Rem),
            0x15 => Some(I64Neg),
            0x16 => Some(I64Shl),
            0x17 => Some(I64Shr),
            0x18 => Some(I64And),
            0x19 => Some(I64Or),
            0x1A => Some(I64Xor),
            0x1B => Some(F64Add),
            0x1C => Some(F64Sub),
            0x1D => Some(F64Mul),
            0x1E => Some(F64Div),
            0x1F => Some(F64Neg),
            0x20 => Some(F32Add),
            0x21 => Some(F32Sub),
            0x22 => Some(F32Mul),
            0x23 => Some(F32Div),
            0x24 => Some(F32Neg),
            0x25 => Some(CmpEq),
            0x26 => Some(CmpNeq),
            0x27 => Some(CmpLt),
            0x28 => Some(CmpGt),
            0x29 => Some(CmpLe),
            0x2A => Some(CmpGe),
            0x30 => Some(Jmp),
            0x31 => Some(JmpIf),
            0x32 => Some(JmpIfNot),
            0x33 => Some(Call),
            0x34 => Some(CallBuiltin),
            0x35 => Some(Ret),
            0x36 => Some(RetVal),
            0x40 => Some(ArrayNew),
            0x41 => Some(ArrayGet),
            0x42 => Some(ArraySet),
            0x43 => Some(ArrayLen),
            0x44 => Some(ArrayPush),
            0x50 => Some(StructNew),
            0x51 => Some(StructGet),
            0x52 => Some(StructSet),
            0x53 => Some(StructDupAt),
            0x54 => Some(EnumNew),
            0x55 => Some(EnumGet),
            0x60 => Some(BoxNew),
            0x61 => Some(BoxGet),
            0x62 => Some(BoxSet),
            0x63 => Some(Addr),
            0x64 => Some(PtrLoad),
            0x65 => Some(PtrStore),
            0x66 => Some(MakeClosure),
            0x67 => Some(CallClosure),
            0x70 => Some(I64ToF64),
            0x71 => Some(F64ToI64),
            0x72 => Some(I64ToChar),
            0x73 => Some(CharToI64),
            0x74 => Some(F64ToF32),
            0x75 => Some(F32ToF64),
            0x76 => Some(I64ToBool),
            0x77 => Some(BoolToI64),
            0x78 => Some(I64ToF32),
            0x79 => Some(F32ToI64),
            0x80 => Some(StrConcat),
            0x81 => Some(StrLen),
            0x82 => Some(StrParse),
            0x83 => Some(StrMake),
            0x84 => Some(StrFrom),
            0x85 => Some(StrGet),
            0x90 => Some(Print),
            0x91 => Some(Prints),
            0x92 => Some(Println),
            0x93 => Some(Printlns),
            0x94 => Some(Error),
            0x95 => Some(Errors),
            0x96 => Some(Errorln),
            0x97 => Some(Errorlns),
            0x98 => Some(Exit),
            0x99 => Some(Abort),
            0x9A => Some(Panic),
            0x9B => Some(ReadInt),
            0x9C => Some(ReadLine),
            0xA0 => Some(RangeNew),
            0xA1 => Some(RangeNext),
            0xB0 => Some(Halt),
            0xB1 => Some(Debug),
            0xB2 => Some(Clone),
            0xB4 => Some(Await),
            0xC0 => Some(CallHost),
            _ => None,
        }
    }

    /// Number of operand bytes following the opcode.
    pub fn operand_size(&self) -> usize {
        use Opcode::*;
        match self {
            PushI64 => 8,
            PushF64 => 8,
            PushF32 => 4,
            PushBool => 1,
            PushChar => 1,
            PushString => 4,
            LoadLocal | StoreLocal => 4,
            Jmp | JmpIf | JmpIfNot => 4,
            Call => 4,
            CallBuiltin => 1,
            StructNew | StructGet | StructSet | StructDupAt | EnumNew | EnumGet => 4,
            Addr => 4,
            MakeClosure => 8,
            CallHost => 5,
            _ => 0,
        }
    }

    pub fn name(&self) -> &'static str {
        use Opcode::*;
        match self {
            Nop => "nop",
            PushI64 => "push_i64",
            PushF64 => "push_f64",
            PushF32 => "push_f32",
            PushBool => "push_bool",
            PushChar => "push_char",
            PushString => "push_string",
            PushNull => "push_null",
            PushUnit => "push_unit",
            Dup => "dup",
            Drop => "drop",
            Swap => "swap",
            Pack => "pack",
            LoadLocal => "load_local",
            StoreLocal => "store_local",
            I64Add => "i64_add",
            I64Sub => "i64_sub",
            I64Mul => "i64_mul",
            I64Div => "i64_div",
            I64Rem => "i64_rem",
            I64Neg => "i64_neg",
            I64Shl => "i64_shl",
            I64Shr => "i64_shr",
            I64And => "i64_and",
            I64Or => "i64_or",
            I64Xor => "i64_xor",
            F64Add => "f64_add",
            F64Sub => "f64_sub",
            F64Mul => "f64_mul",
            F64Div => "f64_div",
            F64Neg => "f64_neg",
            F32Add => "f32_add",
            F32Sub => "f32_sub",
            F32Mul => "f32_mul",
            F32Div => "f32_div",
            F32Neg => "f32_neg",
            CmpEq => "cmp_eq",
            CmpNeq => "cmp_neq",
            CmpLt => "cmp_lt",
            CmpGt => "cmp_gt",
            CmpLe => "cmp_le",
            CmpGe => "cmp_ge",
            Jmp => "jmp",
            JmpIf => "jmp_if",
            JmpIfNot => "jmp_if_not",
            Call => "call",
            CallBuiltin => "call_builtin",
            Ret => "ret",
            RetVal => "ret_val",
            ArrayNew => "array_new",
            ArrayGet => "array_get",
            ArraySet => "array_set",
            ArrayLen => "array_len",
            ArrayPush => "array_push",
            StructNew => "struct_new",
            StructGet => "struct_get",
            StructSet => "struct_set",
            StructDupAt => "struct_dup_at",
            EnumNew => "enum_new",
            EnumGet => "enum_get",
            BoxNew => "box_new",
            BoxGet => "box_get",
            BoxSet => "box_set",
            Addr => "addr",
            PtrLoad => "ptr_load",
            PtrStore => "ptr_store",
            MakeClosure => "make_closure",
            CallClosure => "call_closure",
            I64ToF64 => "i64_to_f64",
            F64ToI64 => "f64_to_i64",
            I64ToChar => "i64_to_char",
            CharToI64 => "char_to_i64",
            F64ToF32 => "f64_to_f32",
            F32ToF64 => "f32_to_f64",
            I64ToBool => "i64_to_bool",
            BoolToI64 => "bool_to_i64",
            I64ToF32 => "i64_to_f32",
            F32ToI64 => "f32_to_i64",
            StrConcat => "str_concat",
            StrLen => "str_len",
            StrParse => "str_parse",
            StrMake => "str_make",
            StrFrom => "str_from",
            StrGet => "str_get",
            Print => "print",
            Prints => "prints",
            Println => "println",
            Printlns => "printlens",
            Error => "error",
            Errors => "errors",
            Errorln => "errorln",
            Errorlns => "errorlns",
            Exit => "exit",
            Abort => "abort",
            Panic => "panic",
            ReadInt => "read_int",
            ReadLine => "read_line",
            RangeNew => "range_new",
            RangeNext => "range_next",
            Halt => "halt",
            Debug => "debug",
            Clone => "clone",
            Await => "await",
            CallHost => "call_host",
        }
    }
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}
