use std::cell::RefCell;
use std::collections::HashMap;
use std::io::{self, BufRead, Write};
use std::rc::Rc;

use crate::opcode::Opcode;
use crate::value::Value;

/// A compiled MVM function.
#[derive(Debug, Clone)]
pub struct MvmFunction {
    pub name_idx: u32,
    pub arity: u32,
    pub locals: u32,
    pub code: Vec<u8>,
}

impl MvmFunction {
    pub fn name<'a>(&self, strings: &'a [String]) -> &'a str {
        &strings[self.name_idx as usize]
    }
}

/// Serialized MVM binary program.
#[derive(Debug, Clone)]
pub struct MvmProgram {
    pub strings: Vec<String>,
    pub functions: Vec<MvmFunction>,
}

impl MvmProgram {
    /// Serialize to binary format.
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut buf = Vec::new();
        // Magic: "MVMF"
        buf.extend_from_slice(b"MVMF");
        // Version
        buf.extend_from_slice(&1u32.to_le_bytes());
        // Strings
        buf.extend_from_slice(&(self.strings.len() as u32).to_le_bytes());
        for s in &self.strings {
            buf.extend_from_slice(&(s.len() as u32).to_le_bytes());
            buf.extend_from_slice(s.as_bytes());
        }
        // Functions
        buf.extend_from_slice(&(self.functions.len() as u32).to_le_bytes());
        for f in &self.functions {
            buf.extend_from_slice(&f.name_idx.to_le_bytes());
            buf.extend_from_slice(&f.arity.to_le_bytes());
            buf.extend_from_slice(&f.locals.to_le_bytes());
            buf.extend_from_slice(&(f.code.len() as u32).to_le_bytes());
            buf.extend_from_slice(&f.code);
        }
        buf
    }

    /// Load from binary format.
    pub fn from_bytes(data: &[u8]) -> Result<Self, String> {
        let mut pos = 0;
        if data.len() < 4 || &data[0..4] != b"MVMF" {
            return Err("Invalid magic: expected 'MVMF'".into());
        }
        pos += 4;
        if data.len() < pos + 4 {
            return Err("Unexpected end of data (version)".into());
        }
        let _version = u32::from_le_bytes([data[pos], data[pos+1], data[pos+2], data[pos+3]]);
        pos += 4;

        // Read strings
        if data.len() < pos + 4 {
            return Err("Unexpected end of data (string count)".into());
        }
        let string_count = u32::from_le_bytes([data[pos], data[pos+1], data[pos+2], data[pos+3]]);
        pos += 4;
        let mut strings = Vec::with_capacity(string_count as usize);
        for _ in 0..string_count {
            if data.len() < pos + 4 {
                return Err("Unexpected end of data (string length)".into());
            }
            let len = u32::from_le_bytes([data[pos], data[pos+1], data[pos+2], data[pos+3]]) as usize;
            pos += 4;
            if data.len() < pos + len {
                return Err("Unexpected end of data (string content)".into());
            }
            let s = String::from_utf8(data[pos..pos+len].to_vec())
                .map_err(|e| format!("Invalid UTF-8 in string pool: {}", e))?;
            strings.push(s);
            pos += len;
        }

        // Read functions
        if data.len() < pos + 4 {
            return Err("Unexpected end of data (function count)".into());
        }
        let func_count = u32::from_le_bytes([data[pos], data[pos+1], data[pos+2], data[pos+3]]);
        pos += 4;
        let mut functions = Vec::with_capacity(func_count as usize);
        for _ in 0..func_count {
            if data.len() < pos + 4 {
                return Err("Unexpected end of data (func name_idx)".into());
            }
            let name_idx = u32::from_le_bytes([data[pos], data[pos+1], data[pos+2], data[pos+3]]);
            pos += 4;
            let arity = u32::from_le_bytes([data[pos], data[pos+1], data[pos+2], data[pos+3]]);
            pos += 4;
            let locals = u32::from_le_bytes([data[pos], data[pos+1], data[pos+2], data[pos+3]]);
            pos += 4;
            let code_size = u32::from_le_bytes([data[pos], data[pos+1], data[pos+2], data[pos+3]]) as usize;
            pos += 4;
            if data.len() < pos + code_size {
                return Err("Unexpected end of data (function code)".into());
            }
            let code = data[pos..pos+code_size].to_vec();
            functions.push(MvmFunction { name_idx, arity, locals, code });
            pos += code_size;
        }

        Ok(MvmProgram { strings, functions })
    }
}

/// Call frame tracking.
#[derive(Debug, Clone)]
struct CallFrame {
    /// Function index in the program.
    func_idx: usize,
    /// Return program counter (absolute position in bytecode).
    return_pc: usize,
    /// Number of values on the stack before this call's arguments.
    stack_base: usize,
    /// Local variables for this frame.
    locals: Rc<RefCell<Vec<Value>>>,
}

/// The Miva Virtual Machine.
pub struct Mvm {
    program: MvmProgram,
    /// The operand stack.
    stack: Vec<Value>,
    /// Call stack.
    call_stack: Vec<CallFrame>,
    /// Current PC (absolute position in current function's code).
    pc: usize,
    /// Current function index.
    current_func: usize,
    /// Current function's locals.
    current_locals: Rc<RefCell<Vec<Value>>>,
    /// Function index cache (name -> index).
    func_name_cache: HashMap<String, usize>,
    /// Builtin function name -> index mapping.
    builtin_map: HashMap<String, u8>,
    /// Whether execution should halt.
    halted: bool,
    /// Exit code.
    exit_code: i64,
    /// Maximum stack depth for safety.
    max_stack: usize,
}

impl Mvm {
    pub fn new(program: MvmProgram) -> Self {
        let mut builtin_map = HashMap::new();
        let builtins = [
            "print", "prints", "println", "printlns",
            "error", "errors", "errorln", "errorlns",
            "exit", "abort", "panic",
            "string_concat", "string_length", "string_parse",
            "string_make", "string_from", "string_get",
            "box_new", "box_deref", "box_set",
            "range", "to_string", "read_int", "read_line",
        ];
        for (i, name) in builtins.iter().enumerate() {
            builtin_map.insert(name.to_string(), i as u8);
        }

        let mut func_name_cache = HashMap::new();
        for (i, f) in program.functions.iter().enumerate() {
            let name = f.name(&program.strings).to_string();
            func_name_cache.entry(name).or_insert(i);
        }

        Mvm {
            program,
            stack: Vec::new(),
            call_stack: Vec::new(),
            pc: 0,
            current_func: 0,
            current_locals: Rc::new(RefCell::new(Vec::new())),
            func_name_cache,
            builtin_map,
            halted: false,
            exit_code: 0,
            max_stack: 100_000,
        }
    }

    /// Read a u32 operand from code at position `pc`.
    fn read_u32(code: &[u8], pc: usize) -> u32 {
        u32::from_le_bytes([
            code[pc], code[pc+1], code[pc+2], code[pc+3],
        ])
    }

    /// Read a i32 operand.
    fn read_i32(code: &[u8], pc: usize) -> i32 {
        i32::from_le_bytes([
            code[pc], code[pc+1], code[pc+2], code[pc+3],
        ])
    }

    /// Read a i64 operand.
    fn read_i64(code: &[u8], pc: usize) -> i64 {
        i64::from_le_bytes([
            code[pc], code[pc+1], code[pc+2], code[pc+3],
            code[pc+4], code[pc+5], code[pc+6], code[pc+7],
        ])
    }

    /// Read a f64 operand.
    fn read_f64(code: &[u8], pc: usize) -> f64 {
        f64::from_le_bytes([
            code[pc], code[pc+1], code[pc+2], code[pc+3],
            code[pc+4], code[pc+5], code[pc+6], code[pc+7],
        ])
    }

    /// Read a f32 operand.
    fn read_f32(code: &[u8], pc: usize) -> f32 {
        f32::from_le_bytes([
            code[pc], code[pc+1], code[pc+2], code[pc+3],
        ])
    }

    /// Read a u8 operand.
    fn read_u8(code: &[u8], pc: usize) -> u8 {
        code[pc]
    }

    /// Find the entry point function (main or the first function).
    fn find_entry(&self) -> Option<usize> {
        self.func_name_cache.get("main").copied()
            .or_else(|| if !self.program.functions.is_empty() { Some(0) } else { None })
    }

    /// Push a value onto the operand stack.
    #[inline]
    fn push(&mut self, val: Value) {
        if self.stack.len() >= self.max_stack {
            panic!("MVM stack overflow");
        }
        self.stack.push(val);
    }

    /// Pop a value from the operand stack.
    #[inline]
    fn pop(&mut self) -> Value {
        self.stack.pop().expect("MVM stack underflow")
    }

    /// Peek at the top of the stack.
    #[inline]
    fn peek(&self) -> &Value {
        self.stack.last().expect("MVM stack empty")
    }

    /// Run the VM to completion.
    pub fn run(&mut self) -> Result<i64, String> {
        let entry = self.find_entry()
            .ok_or_else(|| "No entry point (main function) found".to_string())?;
        self.call_internal(entry, vec![])?;
        Ok(self.exit_code)
    }

    /// Internal function call (used by the CALL instruction).
    fn call_internal(&mut self, func_idx: usize, args: Vec<Value>) -> Result<(), String> {
        let arity; // extract before mutable borrow of self
        let name;
        let locals_count;
        {
            let func = &self.program.functions[func_idx];
            arity = func.arity;
            name = func.name(&self.program.strings).to_string();
            locals_count = func.locals;
        }
        if args.len() != arity as usize {
            return Err(format!(
                "Function '{}' expects {} args, got {}",
                name, arity, args.len()
            ));
        }

        // Push args onto stack
        for arg in args {
            self.push(arg);
        }

        // Create frame (no return if this is the entry call)
        let frame = CallFrame {
            func_idx: self.current_func,
            return_pc: self.pc,
            stack_base: self.stack.len() - arity as usize,
            locals: self.current_locals.clone(),
        };

        // Setup new function context
        let _old_locals = std::mem::replace(
            &mut self.current_locals,
            Rc::new(RefCell::new(Vec::with_capacity(locals_count as usize))),
        );
        let _old_func = std::mem::replace(&mut self.current_func, func_idx);
        self.pc = 0;

        // Pop args from stack into locals
        {
            let mut locals = self.current_locals.borrow_mut();
            // First arity locals are function parameters (values on stack)
            // Pop from stack into locals in reverse order
            let arg_base = self.stack.len() - arity as usize;
            let temp_args: Vec<Value> = self.stack.drain(arg_base..).collect();
            // The args were pushed left-to-right, but we need them in order
            // Actually, we want locals[0] = first arg, locals[1] = second arg, etc.
            // temp_args right now are in left-to-right order since they were pushed
            // in order and popped in order from the front.
            // Let me think... if we push args left-to-right: [arg0, arg1, arg2]
            // then draining from offset gives [arg0, arg1, arg2] in order.
            // So locals[0..arity] = args.
            locals.extend(temp_args);
            // Extend with unit values for remaining locals
            while locals.len() < locals_count as usize {
                locals.push(Value::Unit);
            }
        }

        self.call_stack.push(frame);

        let result = self.execute_loop();

        // Restore caller context
        if let Some(frame) = self.call_stack.pop() {
            self.current_func = frame.func_idx;
            self.current_locals = frame.locals;
            self.pc = frame.return_pc;
        }

        result
    }

    /// Main execution loop.
    fn execute_loop(&mut self) -> Result<(), String> {
        loop {
            if self.halted {
                return Ok(());
            }

            let func = &self.program.functions[self.current_func];
            let code = &func.code;
            if self.pc >= code.len() {
                // Reached end of function without return
                if self.call_stack.is_empty() {
                    return Ok(());
                }
                self.push(Value::Unit);
                return Ok(());
            }

            let op_byte = code[self.pc];
            let op = Opcode::from_u8(op_byte)
                .ok_or_else(|| format!("Unknown opcode: 0x{:02X} at pc={}", op_byte, self.pc))?;
            self.pc += 1;

            match op {
                Opcode::Nop => {}

                Opcode::PushI64 => {
                    let val = Self::read_i64(code, self.pc);
                    self.pc += 8;
                    self.push(Value::Int(val));
                }
                Opcode::PushF64 => {
                    let val = Self::read_f64(code, self.pc);
                    self.pc += 8;
                    self.push(Value::Float64(val));
                }
                Opcode::PushF32 => {
                    let val = Self::read_f32(code, self.pc);
                    self.pc += 4;
                    self.push(Value::Float32(val));
                }
                Opcode::PushBool => {
                    let val = Self::read_u8(code, self.pc) != 0;
                    self.pc += 1;
                    self.push(Value::Bool(val));
                }
                Opcode::PushChar => {
                    let val = Self::read_u8(code, self.pc);
                    self.pc += 1;
                    self.push(Value::Char(val));
                }
                Opcode::PushString => {
                    let idx = Self::read_u32(code, self.pc) as usize;
                    self.pc += 4;
                    if idx >= self.program.strings.len() {
                        return Err(format!("String index {} out of bounds", idx));
                    }
                    self.push(Value::String(Rc::new(self.program.strings[idx].clone())));
                }
                Opcode::PushNull => self.push(Value::Null),
                Opcode::PushUnit => self.push(Value::Unit),
                Opcode::Dup => {
                    let val = self.peek().clone();
                    self.push(val);
                }
                Opcode::Drop => { self.pop(); }
                Opcode::Swap => {
                    let a = self.pop();
                    let b = self.pop();
                    self.push(a);
                    self.push(b);
                }
                Opcode::Pack => {
                    let count = match self.pop() {
                        Value::Int(n) => n as usize,
                        v => return Err(format!("Pack expected int count, got {}", v.type_name())),
                    };
                    let mut vals = Vec::with_capacity(count);
                    for _ in 0..count {
                        vals.push(self.pop());
                    }
                    vals.reverse();
                    self.push(Value::Array(Rc::new(vals)));
                }

                Opcode::LoadLocal => {
                    let idx = Self::read_u32(code, self.pc) as usize;
                    self.pc += 4;
                    let val = {
                        let locals = self.current_locals.borrow();
                        if idx >= locals.len() {
                            let func = &self.program.functions[self.current_func];
                            return Err(format!("Local variable index {} out of bounds (len={}) in function '{}' at pc={}", idx, locals.len(), func.name(&self.program.strings), self.pc - 5));
                        }
                        locals[idx].clone()
                    };
                    self.push(val);
                }
                Opcode::StoreLocal => {
                    let idx = Self::read_u32(code, self.pc) as usize;
                    self.pc += 4;
                    let val = self.pop();
                    {
                        let mut locals = self.current_locals.borrow_mut();
                        if idx >= locals.len() {
                            let func = &self.program.functions[self.current_func];
                            return Err(format!("Local variable index {} out of bounds (len={}) in function '{}' at pc={}", idx, locals.len(), func.name(&self.program.strings), self.pc - 5));
                        }
                        locals[idx] = val;
                    }
                }

                // Integer arithmetic (with fallback to string concat for Add)
                Opcode::I64Add => {
                    let b = self.pop();
                    let a = self.pop();
                    match (&a, &b) {
                        (Value::Int(ai), Value::Int(bi)) => self.push(Value::Int(ai + bi)),
                        (Value::String(as_), Value::String(bs_)) => {
                            self.push(Value::String(Rc::new(format!("{}{}", as_, bs_))));
                        }
                        (Value::String(as_), Value::Int(bi)) => {
                            self.push(Value::String(Rc::new(format!("{}{}", as_, bi))));
                        }
                        (Value::Int(ai), Value::String(bs_)) => {
                            self.push(Value::String(Rc::new(format!("{}{}", ai, bs_))));
                        }
                        _ => {
                            return Err(format!("Cannot add {} and {}", a.type_name(), b.type_name()));
                        }
                    }
                }
                Opcode::I64Sub => { let b = self.pop().as_i64().unwrap(); let a = self.pop().as_i64().unwrap(); self.push(Value::Int(a - b)); }
                Opcode::I64Mul => { let b = self.pop().as_i64().unwrap(); let a = self.pop().as_i64().unwrap(); self.push(Value::Int(a * b)); }
                Opcode::I64Div => { let b = self.pop().as_i64().unwrap(); let a = self.pop().as_i64().unwrap(); self.push(Value::Int(a / b)); }
                Opcode::I64Rem => { let b = self.pop().as_i64().unwrap(); let a = self.pop().as_i64().unwrap(); self.push(Value::Int(a % b)); }
                Opcode::I64Neg => { let a = self.pop().as_i64().unwrap(); self.push(Value::Int(-a)); }
                Opcode::I64Shl => { let b = self.pop().as_i64().unwrap(); let a = self.pop().as_i64().unwrap(); self.push(Value::Int(a << b)); }
                Opcode::I64Shr => { let b = self.pop().as_i64().unwrap(); let a = self.pop().as_i64().unwrap(); self.push(Value::Int(a >> b)); }
                Opcode::I64And => { let b = self.pop().as_i64().unwrap(); let a = self.pop().as_i64().unwrap(); self.push(Value::Int(a & b)); }
                Opcode::I64Or => { let b = self.pop().as_i64().unwrap(); let a = self.pop().as_i64().unwrap(); self.push(Value::Int(a | b)); }
                Opcode::I64Xor => { let b = self.pop().as_i64().unwrap(); let a = self.pop().as_i64().unwrap(); self.push(Value::Int(a ^ b)); }

                // Float64 arithmetic
                Opcode::F64Add => { let b = self.pop().as_f64().unwrap(); let a = self.pop().as_f64().unwrap(); self.push(Value::Float64(a + b)); }
                Opcode::F64Sub => { let b = self.pop().as_f64().unwrap(); let a = self.pop().as_f64().unwrap(); self.push(Value::Float64(a - b)); }
                Opcode::F64Mul => { let b = self.pop().as_f64().unwrap(); let a = self.pop().as_f64().unwrap(); self.push(Value::Float64(a * b)); }
                Opcode::F64Div => { let b = self.pop().as_f64().unwrap(); let a = self.pop().as_f64().unwrap(); self.push(Value::Float64(a / b)); }
                Opcode::F64Neg => { let a = self.pop().as_f64().unwrap(); self.push(Value::Float64(-a)); }

                // Float32 arithmetic
                Opcode::F32Add => {
                    let b = match self.pop() { Value::Float32(f) => f, v => return Err(format!("Expected float32, got {}", v.type_name())) };
                    let a = match self.pop() { Value::Float32(f) => f, v => return Err(format!("Expected float32, got {}", v.type_name())) };
                    self.push(Value::Float32(a + b));
                }
                Opcode::F32Sub => {
                    let b = match self.pop() { Value::Float32(f) => f, v => return Err(format!("Expected float32, got {}", v.type_name())) };
                    let a = match self.pop() { Value::Float32(f) => f, v => return Err(format!("Expected float32, got {}", v.type_name())) };
                    self.push(Value::Float32(a - b));
                }
                Opcode::F32Mul => {
                    let b = match self.pop() { Value::Float32(f) => f, v => return Err(format!("Expected float32, got {}", v.type_name())) };
                    let a = match self.pop() { Value::Float32(f) => f, v => return Err(format!("Expected float32, got {}", v.type_name())) };
                    self.push(Value::Float32(a * b));
                }
                Opcode::F32Div => {
                    let b = match self.pop() { Value::Float32(f) => f, v => return Err(format!("Expected float32, got {}", v.type_name())) };
                    let a = match self.pop() { Value::Float32(f) => f, v => return Err(format!("Expected float32, got {}", v.type_name())) };
                    self.push(Value::Float32(a / b));
                }
                Opcode::F32Neg => {
                    let a = match self.pop() { Value::Float32(f) => f, v => return Err(format!("Expected float32, got {}", v.type_name())) };
                    self.push(Value::Float32(-a));
                }

                // Comparisons
                Opcode::CmpEq => {
                    let b = self.pop(); let a = self.pop();
                    self.push(Value::Bool(a == b));
                }
                Opcode::CmpNeq => {
                    let b = self.pop(); let a = self.pop();
                    self.push(Value::Bool(a != b));
                }
                Opcode::CmpLt => {
                    let b = self.pop(); let a = self.pop();
                    self.push(Value::Bool(a < b));
                }
                Opcode::CmpGt => {
                    let b = self.pop(); let a = self.pop();
                    self.push(Value::Bool(a > b));
                }
                Opcode::CmpLe => {
                    let b = self.pop(); let a = self.pop();
                    self.push(Value::Bool(a <= b));
                }
                Opcode::CmpGe => {
                    let b = self.pop(); let a = self.pop();
                    self.push(Value::Bool(a >= b));
                }

                // Control flow
                Opcode::Jmp => {
                    let offset = Self::read_i32(code, self.pc) as isize;
                    self.pc = (self.pc as isize + offset + 4) as usize;
                }
                Opcode::JmpIf => {
                    let offset = Self::read_i32(code, self.pc) as isize;
                    self.pc += 4;
                    if self.pop().is_truthy() {
                        self.pc = (self.pc as isize + offset) as usize;
                    }
                }
                Opcode::JmpIfNot => {
                    let offset = Self::read_i32(code, self.pc) as isize;
                    self.pc += 4;
                    if !self.pop().is_truthy() {
                        self.pc = (self.pc as isize + offset) as usize;
                    }
                }
                Opcode::Call => {
                    let func_idx = Self::read_u32(code, self.pc) as usize;
                    self.pc += 4;
                    let (arity, call_locals_count) = {
                        let f = &self.program.functions[func_idx];
                        (f.arity as usize, f.locals as usize)
                    };

                    // Collect args from stack
                    let arg_start = if self.stack.len() >= arity { self.stack.len() - arity } else { 0 };
                    let args: Vec<Value> = self.stack.drain(arg_start..).collect();

                    // Save state and call
                    let frame = CallFrame {
                        func_idx: self.current_func,
                        return_pc: self.pc,
                        stack_base: 0,
                        locals: self.current_locals.clone(),
                    };

                    self.current_func = func_idx;
                    self.current_locals = Rc::new(RefCell::new(Vec::with_capacity(call_locals_count)));
                    self.pc = 0;

                    // Set up locals: args first, then unit-filled
                    {
                        let mut locals = self.current_locals.borrow_mut();
                        locals.extend(args);
                        while locals.len() < call_locals_count {
                            locals.push(Value::Unit);
                        }
                    }

                    self.call_stack.push(frame);

                    // Execute the called function
                    let call_result = self.execute_loop();
                    if self.halted {
                        return call_result;
                    }

                    // Restore caller context
                    if let Some(prev_frame) = self.call_stack.pop() {
                        self.current_func = prev_frame.func_idx;
                        self.current_locals = prev_frame.locals;
                        self.pc = prev_frame.return_pc;
                    }
                }
                Opcode::CallBuiltin => {
                    let builtin_idx = Self::read_u8(code, self.pc);
                    self.pc += 1;
                    self.call_builtin(builtin_idx)?;
                }
                Opcode::Ret => {
                    self.push(Value::Unit);
                    return Ok(());
                }
                Opcode::RetVal => {
                    // The return value is already on the stack
                    return Ok(());
                }

                // Array operations
                Opcode::ArrayNew => {
                    let size = match self.pop() {
                        Value::Int(n) => n as usize,
                        v => return Err(format!("ArrayNew expected int, got {}", v.type_name())),
                    };
                    let mut arr = Vec::with_capacity(size);
                    for _ in 0..size {
                        arr.push(self.pop());
                    }
                    arr.reverse();
                    self.push(Value::MutableArray(Rc::new(RefCell::new(arr))));
                }
                Opcode::ArrayGet => {
                    let index = self.pop().as_i64().ok_or("ArrayGet expected int index")? as usize;
                    let arr_val = self.pop();
                    match arr_val {
                        Value::Array(arr) => {
                            self.push(arr[index].clone());
                        }
                        Value::MutableArray(arr) => {
                            self.push(arr.borrow()[index].clone());
                        }
                        v => return Err(format!("ArrayGet expected array, got {}", v.type_name())),
                    }
                }
                Opcode::ArraySet => {
                    let value = self.pop();
                    let index = self.pop().as_i64().ok_or("ArraySet expected int index")? as usize;
                    let arr_val = self.pop();
                    match arr_val {
                        Value::MutableArray(arr) => {
                            arr.borrow_mut()[index] = value;
                        }
                        v => return Err(format!("ArraySet expected mutable array, got {}", v.type_name())),
                    }
                    self.push(Value::Unit);
                }
                Opcode::ArrayLen => {
                    let arr_val = self.pop();
                    let len = match arr_val {
                        Value::Array(arr) => arr.len(),
                        Value::MutableArray(arr) => arr.borrow().len(),
                        v => return Err(format!("ArrayLen expected array, got {}", v.type_name())),
                    };
                    self.push(Value::Int(len as i64));
                }
                Opcode::ArrayPush => {
                    let val = self.pop();
                    let arr_val = self.pop();
                    match arr_val {
                        Value::MutableArray(arr) => {
                            arr.borrow_mut().push(val);
                        }
                        v => return Err(format!("ArrayPush expected mutable array, got {}", v.type_name())),
                    }
                    self.push(Value::Unit);
                }

                // Struct operations
                Opcode::StructNew => {
                    let field_count = Self::read_u32(code, self.pc) as usize;
                    self.pc += 4;
                    let mut fields = Vec::with_capacity(field_count);
                    for _ in 0..field_count {
                        fields.push(self.pop());
                    }
                    fields.reverse();
                    self.push(Value::Struct(fields));
                }
                Opcode::StructGet => {
                    let field_idx = Self::read_u32(code, self.pc) as usize;
                    self.pc += 4;
                    let struct_val = self.pop();
                    match struct_val {
                        Value::Struct(fields) => {
                            if field_idx >= fields.len() {
                                return Err(format!("Struct field index {} out of bounds", field_idx));
                            }
                            self.push(fields[field_idx].clone());
                        }
                        v => return Err(format!("StructGet expected struct, got {}", v.type_name())),
                    }
                }
                Opcode::StructSet => {
                    let field_idx = Self::read_u32(code, self.pc) as usize;
                    self.pc += 4;
                    let value = self.pop();
                    let struct_val = self.pop();
                    match struct_val {
                        Value::Struct(mut fields) => {
                            if field_idx >= fields.len() {
                                return Err(format!("Struct field index {} out of bounds", field_idx));
                            }
                            fields[field_idx] = value;
                            // We can't push back the struct easily since we consumed it
                            // We need to push the result
                            self.push(Value::Unit);
                        }
                        v => return Err(format!("StructSet expected struct, got {}", v.type_name())),
                    }
                }
                Opcode::StructDupAt => {
                    // Duplicate the struct at stack pos (for chained field access)
                    // Not used in basic codegen; just duplicate top
                    let val = self.peek().clone();
                    self.push(val);
                }

                // Box operations
                Opcode::BoxNew => {
                    let val = self.pop();
                    self.push(Value::Boxed(Rc::new(RefCell::new(val))));
                }
                Opcode::BoxGet => {
                    let box_val = self.pop();
                    match box_val {
                        Value::Boxed(b) => self.push(b.borrow().clone()),
                        v => return Err(format!("BoxGet expected box, got {}", v.type_name())),
                    }
                }
                Opcode::BoxSet => {
                    let val = self.pop();
                    let box_val = self.pop();
                    match box_val {
                        Value::Boxed(b) => { *b.borrow_mut() = val; }
                        v => return Err(format!("BoxSet expected box, got {}", v.type_name())),
                    }
                    self.push(Value::Unit);
                }

                // Pointer operations
                Opcode::Addr => {
                    let local_idx = Self::read_u32(code, self.pc) as usize;
                    self.pc += 4;
                    self.push(Value::Ptr(local_idx, self.current_locals.clone()));
                }
                Opcode::PtrLoad => {
                    let ptr_val = self.pop();
                    match ptr_val {
                        Value::Ptr(idx, locals) => {
                            let val = locals.borrow()[idx].clone();
                            self.push(val);
                        }
                        v => return Err(format!("PtrLoad expected ptr, got {}", v.type_name())),
                    }
                }
                Opcode::PtrStore => {
                    let val = self.pop();
                    let ptr_val = self.pop();
                    match ptr_val {
                        Value::Ptr(idx, locals) => {
                            locals.borrow_mut()[idx] = val;
                        }
                        v => return Err(format!("PtrStore expected ptr, got {}", v.type_name())),
                    }
                    self.push(Value::Unit);
                }

                // Type conversions
                Opcode::I64ToF64 => { let a = self.pop().as_i64().unwrap(); self.push(Value::Float64(a as f64)); }
                Opcode::F64ToI64 => { let a = self.pop().as_f64().unwrap(); self.push(Value::Int(a as i64)); }
                Opcode::I64ToChar => {
                    match self.pop() {
                        Value::Int(i) => self.push(Value::Char(i as u8)),
                        v => return Err(format!("I64ToChar expected int, got {}", v.type_name())),
                    }
                }
                Opcode::CharToI64 => {
                    match self.pop() {
                        Value::Char(c) => self.push(Value::Int(c as i64)),
                        v => return Err(format!("CharToI64 expected char, got {}", v.type_name())),
                    }
                }
                Opcode::F64ToF32 => { let a = self.pop().as_f64().unwrap(); self.push(Value::Float32(a as f32)); }
                Opcode::F32ToF64 => {
                    match self.pop() {
                        Value::Float32(f) => self.push(Value::Float64(f as f64)),
                        v => return Err(format!("F32ToF64 expected float32, got {}", v.type_name())),
                    }
                }
                Opcode::I64ToBool => { let a = self.pop().as_i64().unwrap(); self.push(Value::Bool(a != 0)); }
                Opcode::BoolToI64 => {
                    match self.pop() {
                        Value::Bool(b) => self.push(Value::Int(if b { 1 } else { 0 })),
                        v => return Err(format!("BoolToI64 expected bool, got {}", v.type_name())),
                    }
                }
                Opcode::I64ToF32 => { let a = self.pop().as_i64().unwrap(); self.push(Value::Float32(a as f32)); }
                Opcode::F32ToI64 => {
                    match self.pop() {
                        Value::Float32(f) => self.push(Value::Int(f as i64)),
                        v => return Err(format!("F32ToI64 expected float32, got {}", v.type_name())),
                    }
                }

                // String operations
                Opcode::StrConcat => {
                    let b = match self.pop() { Value::String(s) => (*s).clone(), v => return Err(format!("StrConcat expected string, got {}", v.type_name())) };
                    let a = match self.pop() { Value::String(s) => (*s).clone(), v => return Err(format!("StrConcat expected string, got {}", v.type_name())) };
                    self.push(Value::String(Rc::new(a + &b)));
                }
                Opcode::StrLen => {
                    match self.pop() {
                        Value::String(s) => self.push(Value::Int(s.len() as i64)),
                        v => return Err(format!("StrLen expected string, got {}", v.type_name())),
                    }
                }
                Opcode::StrParse => {
                    match self.pop() {
                        Value::String(s) => {
                            let n = s.trim().parse::<i64>().unwrap_or(0);
                            self.push(Value::Int(n));
                        }
                        v => return Err(format!("StrParse expected string, got {}", v.type_name())),
                    }
                }
                Opcode::StrMake => {
                    let len = self.pop().as_i64().ok_or("StrMake expected int length")? as usize;
                    match self.pop() {
                        Value::Char(c) => {
                            let s: String = std::iter::repeat(c as char).take(len).collect();
                            self.push(Value::String(Rc::new(s)));
                        }
                        v => return Err(format!("StrMake expected char, got {}", v.type_name())),
                    }
                }
                Opcode::StrFrom => {
                    let v = self.pop();
                    self.push(Value::String(Rc::new(v.display())));
                }
                Opcode::StrGet => {
                    let idx = self.pop().as_i64().ok_or("StrGet expected int index")? as usize;
                    match self.pop() {
                        Value::String(s) => {
                            let c = s.chars().nth(idx).unwrap_or('\0');
                            self.push(Value::Char(c as u8));
                        }
                        v => return Err(format!("StrGet expected string, got {}", v.type_name())),
                    }
                }

                // I/O builtins implemented as opcodes
                Opcode::Print => print!("{}", self.pop().display()),
                Opcode::Prints => {
                    match self.pop() {
                        Value::String(s) => print!("{}", s),
                        v => print!("{}", v.display()),
                    }
                }
                Opcode::Println => println!("{}", self.pop().display()),
                Opcode::Printlns => {
                    match self.pop() {
                        Value::String(s) => println!("{}", s),
                        v => println!("{}", v.display()),
                    }
                }
                Opcode::Error => eprint!("{}", self.pop().display()),
                Opcode::Errors => {
                    match self.pop() {
                        Value::String(s) => eprint!("{}", s),
                        v => eprint!("{}", v.display()),
                    }
                }
                Opcode::Errorln => eprintln!("{}", self.pop().display()),
                Opcode::Errorlns => {
                    match self.pop() {
                        Value::String(s) => eprintln!("{}", s),
                        v => eprintln!("{}", v.display()),
                    }
                }
                Opcode::Exit => {
                    self.exit_code = self.pop().as_i64().unwrap_or(0);
                    self.halted = true;
                    return Ok(());
                }
                Opcode::Abort => {
                    eprintln!("MVM: abort called");
                    std::process::exit(1);
                }
                Opcode::Panic => {
                    let msg = match self.pop() {
                        Value::String(s) => (*s).clone(),
                        v => v.display(),
                    };
                    eprintln!("MVM panic: {}", msg);
                    std::process::exit(1);
                }
                Opcode::ReadInt => {
                    let mut line = String::new();
                    io::stdin().lock().read_line(&mut line).ok();
                    let n = line.trim().parse::<i64>().unwrap_or(0);
                    self.push(Value::Int(n));
                }
                Opcode::ReadLine => {
                    let mut line = String::new();
                    io::stdin().lock().read_line(&mut line).ok();
                    if line.ends_with('\n') { line.pop(); }
                    self.push(Value::String(Rc::new(line)));
                }

                // Range / Iteration
                Opcode::RangeNew => {
                    let end = self.pop().as_i64().ok_or("RangeNew expected int end")?;
                    let start = self.pop().as_i64().ok_or("RangeNew expected int start")?;
                    self.push(Value::Range(start, end, start));
                }
                Opcode::RangeNext => {
                    match self.pop() {
                        Value::Range(start, end, current) => {
                            if current < end {
                                let next = current + 1;
                                self.push(Value::Range(start, end, next));
                                self.push(Value::Bool(true));
                                self.push(Value::Int(current));
                            } else {
                                self.push(Value::Range(start, end, current));
                                self.push(Value::Bool(false));
                                self.push(Value::Unit);
                            }
                        }
                        v => return Err(format!("RangeNext expected range, got {}", v.type_name())),
                    }
                }

                Opcode::Halt => {
                    self.halted = true;
                    return Ok(());
                }
                Opcode::Debug => {
                    eprintln!("--- MVM Debug ---");
                    eprintln!("PC: {}, Function: {}, Stack depth: {}",
                        self.pc,
                        self.program.functions[self.current_func].name(&self.program.strings),
                        self.stack.len());
                    eprintln!("Stack ({}) items:", self.stack.len());
                    for (i, v) in self.stack.iter().enumerate() {
                        eprintln!("  [{}] {} ({})", i, v.display(), v.type_name());
                    }
                    eprintln!("Locals:");
                    let locals = self.current_locals.borrow();
                    for (i, v) in locals.iter().enumerate() {
                        eprintln!("  [{}] {} ({})", i, v.display(), v.type_name());
                    }
                    eprintln!("--- End Debug ---");
                }
                Opcode::Clone => {
                    let v = self.peek().clone();
                    self.push(v);
                }
            }
        }
    }

    /// Call a builtin function by index.
    fn call_builtin(&mut self, idx: u8) -> Result<(), String> {
        // Collect args from stack based on builtin
        match idx {
            // print, prints, println, printlns
            0 => { let v = self.pop(); print!("{}", v.display()); self.push(Value::Unit); } // print
            1 => { let v = self.pop(); match v { Value::String(s) => print!("{}", s), _ => print!("{}", v.display()) }; self.push(Value::Unit); } // prints
            2 => { let v = self.pop(); println!("{}", v.display()); self.push(Value::Unit); } // println
            3 => { let v = self.pop(); match v { Value::String(s) => println!("{}", s), _ => println!("{}", v.display()) }; self.push(Value::Unit); } // printlns
            // error, errors, errorln, errorlns
            4 => { let v = self.pop(); eprint!("{}", v.display()); self.push(Value::Unit); }
            5 => { let v = self.pop(); match v { Value::String(s) => eprint!("{}", s), _ => eprint!("{}", v.display()) }; self.push(Value::Unit); }
            6 => { let v = self.pop(); eprintln!("{}", v.display()); self.push(Value::Unit); }
            7 => { let v = self.pop(); match v { Value::String(s) => eprintln!("{}", s), _ => eprintln!("{}", v.display()) }; self.push(Value::Unit); }
            // exit
            8 => { self.exit_code = self.pop().as_i64().unwrap_or(0); self.halted = true; return Ok(()); }
            // abort
            9 => { eprintln!("MVM: abort called"); std::process::exit(1); }
            // panic
            10 => {
                let msg = match self.pop() { Value::String(s) => (*s).clone(), v => v.display() };
                eprintln!("MVM panic: {}", msg);
                std::process::exit(1);
            }
            // string_concat
            11 => {
                let b = match self.pop() { Value::String(s) => (*s).clone(), v => return Err(format!("string_concat expected string, got {}", v.type_name())) };
                let a = match self.pop() { Value::String(s) => (*s).clone(), v => return Err(format!("string_concat expected string, got {}", v.type_name())) };
                self.push(Value::String(Rc::new(a + &b)));
            }
            // string_length
            12 => {
                match self.pop() { Value::String(s) => self.push(Value::Int(s.len() as i64)), v => return Err(format!("string_length expected string, got {}", v.type_name())) };
            }
            // string_parse
            13 => {
                match self.pop() { Value::String(s) => { let n = s.trim().parse().unwrap_or(0); self.push(Value::Int(n)); } v => return Err(format!("string_parse expected string, got {}", v.type_name())) };
            }
            // string_make
            14 => {
                let len = self.pop().as_i64().ok_or("string_make expected int")? as usize;
                match self.pop() { Value::Char(c) => { let s: String = std::iter::repeat(c as char).take(len).collect(); self.push(Value::String(Rc::new(s))); } v => return Err(format!("string_make expected char, got {}", v.type_name())) };
            }
            // string_from (to_string)
            15 => {
                let v = self.pop();
                self.push(Value::String(Rc::new(v.display())));
            }
            // string_get
            16 => {
                let idx = self.pop().as_i64().ok_or("string_get expected int")? as usize;
                match self.pop() { Value::String(s) => { let c = s.chars().nth(idx).unwrap_or('\0'); self.push(Value::Char(c as u8)); } v => return Err(format!("string_get expected string, got {}", v.type_name())) };
            }
            // box_new
            17 => { let v = self.pop(); self.push(Value::Boxed(Rc::new(RefCell::new(v)))); }
            // box_deref
            18 => { match self.pop() { Value::Boxed(b) => self.push(b.borrow().clone()), v => return Err(format!("box_deref expected box, got {}", v.type_name())) }; }
            // box_set
            19 => { let val = self.pop(); match self.pop() { Value::Boxed(b) => { *b.borrow_mut() = val; self.push(Value::Unit); } v => return Err(format!("box_set expected box, got {}", v.type_name())) }; }
            // range
            20 => {
                let end = self.pop().as_i64().ok_or("range expected int end")?;
                // Handle both range(n) and range(start, end)
                // If there's another value on the stack, it's start
                let start = if self.stack.len() > 0 && matches!(self.stack.last(), Some(Value::Int(_))) {
                    self.pop().as_i64().unwrap()
                } else {
                    0i64
                };
                self.push(Value::Range(start, end, start));
            }
            // to_string (same as string_from)
            21 => {
                let v = self.pop();
                self.push(Value::String(Rc::new(v.display())));
            }
            // read_int
            22 => {
                let mut line = String::new();
                io::stdin().lock().read_line(&mut line).ok();
                let n = line.trim().parse::<i64>().unwrap_or(0);
                self.push(Value::Int(n));
            }
            // read_line
            23 => {
                let mut line = String::new();
                io::stdin().lock().read_line(&mut line).ok();
                if line.ends_with('\n') { line.pop(); }
                self.push(Value::String(Rc::new(line)));
            }
            _ => return Err(format!("Unknown builtin index: {}", idx)),
        }
        // Ensure stdout/stderr are flushed
        io::stdout().flush().ok();
        io::stderr().flush().ok();
        Ok(())
    }
}
