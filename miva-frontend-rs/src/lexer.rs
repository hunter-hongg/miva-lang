use std::fmt;

/// Tokens produced by the Miva lexer.
#[derive(Debug, Clone, PartialEq)]
pub enum Token<'input> {
    // Literals
    IntLit(&'input str),
    FloatLit(&'input str),
    CharLit(&'input str),
    StringLit(&'input str),
    BoolLit(bool),
    // Identifiers / directives
    Ident(&'input str),
    Intro(&'input str),
    Magical(&'input str),
    // Operators / punctuation
    Eq,           // =
    ColonEq,      // :=
    DArrow,       // =>
    LParen,       // (
    RParen,       // )
    LBracket,     // [
    RBracket,     // ]
    LBrace,       // {
    RBrace,       // }
    Comma,        // ,
    Dot,          // .
    Colon,        // :
    Semi,         // ;
    Plus,         // +
    Minus,        // -
    Star,         // *
    Slash,        // /
    EqEq,         // ==
    Neq,          // !=
    Lt,           // <
    Gt,           // >
    LtEq,         // <=
    GtEq,         // >=
    AndAnd,       // &&
    OrOr,         // ||
    Amp,          // &
    Pipe,         // |
    Not,          // !
    // Keywords
    Let,
    Struct,
    Ref,
    Own,
    Move,
    Clone,
    If,
    Elif,
    Else,
    Return,
    While,
    Loop,
    For,
    In,
    Choose,
    When,
    Otherwise,
    Mut,
    Int,
    Bool,
    Float32,
    Float64,
    Char,
    String,
    As,
    Test,
    Unsafe,
    Trusted,
    CKeyword,
    Inline,
    Async,
    Ptr,
    Box,
    Addr,
    Deref,
    Ptrany,
    Fn,
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
    OpEq,
    OpNeq,
    Module,
    Export,
    Import,
    Implements,
    Macro,
    Enum,
    Dollar,
}

impl<'input> fmt::Display for Token<'input> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::IntLit(v) => write!(f, "int({})", v),
            Token::FloatLit(v) => write!(f, "float({})", v),
            Token::CharLit(v) => write!(f, "char({})", v),
            Token::StringLit(v) => write!(f, "string({})", v),
            Token::BoolLit(v) => write!(f, "bool({})", v),
            Token::Ident(v) => write!(f, "ident({})", v),
            Token::Intro(v) => write!(f, "intro({})", v),
            Token::Magical(v) => write!(f, "magical({})", v),
            _ => write!(f, "{:?}", self),
        }
    }
}

/// A spanned token: (start_byte, token, end_byte).
pub type Spanned<'input> = Result<(usize, Token<'input>, usize), String>;

/// Convert a byte offset to line:col position.
pub fn offset_to_line_col(input: &str, offset: usize) -> (usize, usize) {
    let mut line = 1;
    let mut col = 1;
    for (i, c) in input.char_indices() {
        if i >= offset {
            break;
        }
        if c == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    (line, col)
}

/// Miva lexer — produces a token stream from source text.
#[derive(Clone)]
pub struct Lexer<'input> {
    input: &'input str,
    pos: usize,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Lexer { input, pos: 0 }
    }

    /// Peek at the current character without consuming.
    fn peek(&self) -> Option<u8> {
        self.input.as_bytes().get(self.pos).copied()
    }

    /// Peek at the next character after current.
    fn peek_next(&self) -> Option<u8> {
        self.input.as_bytes().get(self.pos + 1).copied()
    }

    /// Advance one byte and return it.
    fn advance(&mut self) -> Option<u8> {
        let b = self.input.as_bytes().get(self.pos).copied()?;
        self.pos += 1;
        Some(b)
    }

    /// Skip whitespace (spaces, tabs, carriage returns).
    fn skip_whitespace(&mut self) {
        while let Some(b) = self.peek() {
            if b == b' ' || b == b'\t' || b == b'\r' {
                self.advance();
            } else {
                break;
            }
        }
    }

    /// Skip a line comment starting at position (after `//`).
    fn skip_line_comment(&mut self) {
        // Already past `//`
        while let Some(b) = self.peek() {
            if b == b'\n' {
                break;
            }
            self.advance();
        }
    }

    /// Skip a block comment starting at position (after `/*`).
    fn skip_block_comment(&mut self) -> Result<(), String> {
        let mut nesting: u32 = 1;
        while nesting > 0 {
            match self.peek() {
                None => return Err("Unterminated block comment".to_string()),
                Some(b'*') if self.peek_next() == Some(b'/') => {
                    self.advance(); // *
                    self.advance(); // /
                    nesting -= 1;
                }
                Some(b'/') if self.peek_next() == Some(b'*') => {
                    self.advance(); // /
                    self.advance(); // *
                    nesting += 1;
                }
                Some(b'\n') => {
                    self.advance();
                }
                _ => {
                    self.advance();
                }
            }
        }
        Ok(())
    }

    /// Read a multi-line string (""" ... """).
    fn read_multi_string(&mut self) -> Result<&'input str, String> {
        let start = self.pos;
        // We already consumed the opening """ — find closing """
        loop {
            match self.peek() {
                None => return Err("Unterminated multi-line string".to_string()),
                Some(b'"') if {
                    let b1 = self.peek();
                    let b2 = self.peek_next();
                    let b3 = if self.pos + 2 < self.input.len() {
                        Some(self.input.as_bytes()[self.pos + 2])
                    } else {
                        None
                    };
                    b1 == Some(b'"') && b2 == Some(b'"') && b3 == Some(b'"')
                } => {
                    // Include the closing """ in the output slice
                    self.advance(); // "
                    self.advance(); // "
                    self.advance(); // "
                    // Return the content BETWEEN the two """ markers
                    // The content starts after the opening """ (start - 3)
                    // and ends before the closing """ (self.pos - 3)
                    let content_start = start; // start already points past opening """
                    let content_end = self.pos - 3;
                    return Ok(&self.input[content_start..content_end])
                }
                Some(b'\n') => {
                    self.advance();
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    /// Read a regular string literal (between `"` and `"`).
    fn read_string(&mut self) -> Result<&'input str, String> {
        let start = self.pos; // past the opening "
        loop {
            match self.peek() {
                None => return Err("Unterminated string literal".to_string()),
                Some(b'"') => {
                    self.advance(); // closing "
                    return Ok(&self.input[start..self.pos - 1]);
                }
                Some(b'\\') => {
                    self.advance(); // backslash
                    if self.peek().is_some() {
                        self.advance(); // escaped char
                    }
                }
                Some(b'\n') => {
                    return Err("Newline in string literal".to_string());
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    /// Read a char literal: `'x'`.
    fn read_char(&mut self) -> Result<&'input str, String> {
        let start = self.pos; // past the opening '
        match self.peek() {
            Some(b'\\') => {
                self.advance();
                if self.peek().is_some() {
                    self.advance();
                }
            }
            Some(_) => {
                self.advance();
            }
            None => return Err("Unterminated char literal".to_string()),
        }
        // Expect closing '
        let content = &self.input[start..self.pos];
        match self.peek() {
            Some(b'\'') => {
                self.advance();
                Ok(content)
            }
            _ => Err("Unterminated char literal".to_string()),
        }
    }

    /// Read a number (int or float).
    fn read_number(&mut self) -> Result<&'input str, String> {
        let start = self.pos;
        // Already consumed first digit
        let mut is_float = false;
        while let Some(b) = self.peek() {
            match b {
                b'0'..=b'9' => {
                    self.advance();
                }
                b'.' => {
                    // Check if next is also a digit (e.g., "3.14")
                    if self.peek_next().map_or(false, |n| n.is_ascii_digit()) {
                        is_float = true;
                        self.advance();
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
        Ok(&self.input[start..self.pos])
    }

    /// Read an identifier or keyword.
    fn read_ident(&mut self) -> &'input str {
        let start = self.pos;
        // Already consumed first char
        while let Some(b) = self.peek() {
            if b.is_ascii_alphanumeric() || b == b'_' {
                self.advance();
            } else {
                break;
            }
        }
        &self.input[start..self.pos]
    }

    /// Read a magical directive: `/! ...` until newline.
    fn read_magical(&mut self) -> &'input str {
        let start = self.pos;
        while let Some(b) = self.peek() {
            if b == b'\n' {
                break;
            }
            self.advance();
        }
        &self.input[start..self.pos]
    }

    /// Read an intro directive: `@ ...` until newline.
    fn read_intro(&mut self) -> &'input str {
        let start = self.pos;
        while let Some(b) = self.peek() {
            if b == b'\n' {
                break;
            }
            self.advance();
        }
        &self.input[start..self.pos]
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<'input>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // Skip whitespace (but NOT newlines — newlines are meaningful)
            self.skip_whitespace();

            let b = match self.peek() {
                None => return None, // EOF
                Some(b) => b,
            };

            let start = self.pos;

            match b {
                // Newline (skip)
                b'\n' => {
                    self.advance();
                    continue;
                }
                // Single-line comment
                b'/' if self.peek_next() == Some(b'/') => {
                    self.advance(); // /
                    self.advance(); // /
                    self.skip_line_comment();
                    continue;
                }
                // Block comment
                b'/' if self.peek_next() == Some(b'*') => {
                    self.advance(); // /
                    self.advance(); // *
                    match self.skip_block_comment() {
                        Ok(()) => continue,
                        Err(e) => return Some(Err(e)),
                    }
                }
                // Magical directive /!
                b'/' if self.peek_next() == Some(b'!') => {
                    self.advance(); // /
                    self.advance(); // !
                    let content = self.read_magical();
                    return Some(Ok((start, Token::Magical(content), self.pos)));
                }
                // Intro directive @
                b'@' => {
                    self.advance();
                    let content = self.read_intro();
                    return Some(Ok((start, Token::Intro(content), self.pos)));
                }
                // String literal
                b'"' => {
                    // Check for multi-line string """
                    if self.peek_next() == Some(b'"') && self.pos + 2 < self.input.len() && self.input.as_bytes()[self.pos + 2] == b'"' {
                        self.advance(); // "
                        self.advance(); // "
                        self.advance(); // "
                        match self.read_multi_string() {
                            Ok(content) => return Some(Ok((start, Token::StringLit(content), self.pos))),
                            Err(e) => return Some(Err(e)),
                        }
                    } else {
                        self.advance(); // opening "
                        match self.read_string() {
                            Ok(content) => return Some(Ok((start, Token::StringLit(content), self.pos))),
                            Err(e) => return Some(Err(e)),
                        }
                    }
                }
                // Char literal
                b'\'' => {
                    self.advance(); // opening '
                    match self.read_char() {
                        Ok(content) => return Some(Ok((start, Token::CharLit(content), self.pos))),
                        Err(e) => return Some(Err(e)),
                    }
                }
                // Operators / punctuation (single and multi-char)
                b'=' => {
                    self.advance();
                    if self.peek() == Some(b'=') {
                        self.advance();
                        return Some(Ok((start, Token::EqEq, self.pos)));
                    } else if self.peek() == Some(b'>') {
                        self.advance();
                        return Some(Ok((start, Token::DArrow, self.pos)));
                    }
                    return Some(Ok((start, Token::Eq, self.pos)));
                }
                b':' => {
                    self.advance();
                    if self.peek() == Some(b'=') {
                        self.advance();
                        return Some(Ok((start, Token::ColonEq, self.pos)));
                    }
                    return Some(Ok((start, Token::Colon, self.pos)));
                }
                b'!' => {
                    self.advance();
                    if self.peek() == Some(b'=') {
                        self.advance();
                        return Some(Ok((start, Token::Neq, self.pos)));
                    }
                    return Some(Ok((start, Token::Not, self.pos)));
                }
                b'<' => {
                    self.advance();
                    if self.peek() == Some(b'=') {
                        self.advance();
                        return Some(Ok((start, Token::LtEq, self.pos)));
                    }
                    return Some(Ok((start, Token::Lt, self.pos)));
                }
                b'>' => {
                    self.advance();
                    if self.peek() == Some(b'=') {
                        self.advance();
                        return Some(Ok((start, Token::GtEq, self.pos)));
                    }
                    return Some(Ok((start, Token::Gt, self.pos)));
                }
                b'&' => {
                    self.advance();
                    if self.peek() == Some(b'&') {
                        self.advance();
                        return Some(Ok((start, Token::AndAnd, self.pos)));
                    }
                    return Some(Ok((start, Token::Amp, self.pos)));
                }
                b'|' => {
                    self.advance();
                    if self.peek() == Some(b'|') {
                        self.advance();
                        return Some(Ok((start, Token::OrOr, self.pos)));
                    }
                    return Some(Ok((start, Token::Pipe, self.pos)));
                }
                b'+' => {
                    self.advance();
                    return Some(Ok((start, Token::Plus, self.pos)));
                }
                b'-' => {
                    self.advance();
                    if self.peek().map_or(false, |b| b.is_ascii_digit())
                        && self.pos == start + 1
                    {
                        match self.read_number() {
                            Ok(_) => {
                                let lit_start = start;
                                let lit_end = self.pos;
                                let text = &self.input[lit_start..lit_end];
                                let lit = if text.contains('.') {
                                    Token::FloatLit(text)
                                } else {
                                    Token::IntLit(text)
                                };
                                return Some(Ok((lit_start, lit, lit_end)));
                            }
                            Err(e) => return Some(Err(e)),
                        }
                    }
                    return Some(Ok((start, Token::Minus, self.pos)));
                }
                b'*' => {
                    self.advance();
                    return Some(Ok((start, Token::Star, self.pos)));
                }
                b'/' => {
                    self.advance();
                    return Some(Ok((start, Token::Slash, self.pos)));
                }
                b'(' => {
                    self.advance();
                    return Some(Ok((start, Token::LParen, self.pos)));
                }
                b')' => {
                    self.advance();
                    return Some(Ok((start, Token::RParen, self.pos)));
                }
                b'[' => {
                    self.advance();
                    return Some(Ok((start, Token::LBracket, self.pos)));
                }
                b']' => {
                    self.advance();
                    return Some(Ok((start, Token::RBracket, self.pos)));
                }
                b'{' => {
                    self.advance();
                    return Some(Ok((start, Token::LBrace, self.pos)));
                }
                b'}' => {
                    self.advance();
                    return Some(Ok((start, Token::RBrace, self.pos)));
                }
                b',' => {
                    self.advance();
                    return Some(Ok((start, Token::Comma, self.pos)));
                }
                b'$' => {
                    self.advance();
                    return Some(Ok((start, Token::Dollar, self.pos)));
                }
                b'.' => {
                    self.advance();
                    return Some(Ok((start, Token::Dot, self.pos)));
                }
                b';' => {
                    self.advance();
                    return Some(Ok((start, Token::Semi, self.pos)));
                }
                // Numbers
                b'0'..=b'9' => {
                    match self.read_number() {
                        Ok(text) => {
                            if text.contains('.') {
                                return Some(Ok((start, Token::FloatLit(text), self.pos)));
                            } else {
                                return Some(Ok((start, Token::IntLit(text), self.pos)));
                            }
                        }
                        Err(e) => return Some(Err(e)),
                    }
                }
                // Identifiers / keywords
                _ if b.is_ascii_alphabetic() || b == b'_' => {
                    self.advance(); // consume first byte
                    // Read remaining identifier characters from current position
                    while let Some(b) = self.peek() {
                        if b.is_ascii_alphanumeric() || b == b'_' {
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    let ident = &self.input[start..self.pos];
                    if ident == "NOT_A_REAL_TOKEN" {} // noop for debug
                    let token = match ident {
                        "let" => Token::Let,
                        "struct" => Token::Struct,
                        "ref" => Token::Ref,
                        "move" => Token::Move,
                        "clone" => Token::Clone,
                        "if" => Token::If,
                        "elif" => Token::Elif,
                        "else" => Token::Else,
                        "return" => Token::Return,
                        "while" => Token::While,
                        "loop" => Token::Loop,
                        "for" => Token::For,
                        "in" => Token::In,
                        "choose" => Token::Choose,
                        "when" => Token::When,
                        "otherwise" => Token::Otherwise,
                        "mut" => Token::Mut,
                        "int" => Token::Int,
                        "bool" => Token::Bool,
                        "float32" => Token::Float32,
                        "float64" => Token::Float64,
                        "char" => Token::Char,
                        "string" => Token::String,
                        "as" => Token::As,
                        "test" => Token::Test,
                        "unsafe" => Token::Unsafe,
                        "trusted" => Token::Trusted,
                        "c" => Token::CKeyword,
                        "inline" => Token::Inline,
                        "async" => Token::Async,
                        "ptr" => Token::Ptr,
                        "box" => Token::Box,
                        "ptrany" => Token::Ptrany,
                        "fn" => Token::Fn,
                        "addr" => Token::Addr,
                        "deref" => Token::Deref,
                        "op_add" => Token::OpAdd,
                        "op_sub" => Token::OpSub,
                        "op_mul" => Token::OpMul,
                        "op_div" => Token::OpDiv,
                        "op_eq" => Token::OpEq,
                        "op_neq" => Token::OpNeq,
                        "module" => Token::Module,
                        "export" => Token::Export,
                        "import" => Token::Import,
                        "impl" => Token::Implements,
                        "macro" => Token::Macro,
                        "enum" => Token::Enum,
                        "true" => Token::BoolLit(true),
                        "false" => Token::BoolLit(false),
                        _ => Token::Ident(ident),
                    };
                    return Some(Ok((start, token, self.pos)));
                }
                _ => {
                    self.advance();
                    let ch = b as char;
                    return Some(Err(format!("Unexpected character: '{}'", ch)));
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn collect(input: &str) -> Vec<(usize, Token, usize)> {
        let lexer = Lexer::new(input);
        lexer
            .filter_map(|r| r.ok())
            .collect()
    }

    fn collect_with_errors(input: &str) -> Vec<Result<(usize, Token, usize), String>> {
        Lexer::new(input).collect()
    }

    fn first_token(input: &str) -> Option<Token> {
        collect(input).into_iter().next().map(|(_, t, _)| t)
    }

    #[test]
    fn test_empty() {
        let tokens = collect("");
        assert!(tokens.is_empty());
    }

    #[test]
    fn test_whitespace_only() {
        let tokens = collect("   \t  \n  \n  ");
        assert!(tokens.is_empty());
    }

    #[test]
    fn test_integer_literal() {
        assert_eq!(first_token("42"), Some(Token::IntLit("42")));
        assert_eq!(first_token("0"), Some(Token::IntLit("0")));
        assert_eq!(first_token("12345"), Some(Token::IntLit("12345")));
    }

    #[test]
    fn test_negative_literal() {
        assert_eq!(first_token("-42"), Some(Token::IntLit("-42")));
        assert_eq!(first_token("-3.14"), Some(Token::FloatLit("-3.14")));
    }

    #[test]
    fn test_minus_operator() {
        let tokens = collect("a - 1");
        assert_eq!(tokens[1].1, Token::Minus);
        assert_eq!(tokens[2].1, Token::IntLit("1"));
    }

    #[test]
    fn test_float_literal() {
        assert_eq!(first_token("3.14"), Some(Token::FloatLit("3.14")));
        assert_eq!(first_token("0.5"), Some(Token::FloatLit("0.5")));
    }

    #[test]
    fn test_ident() {
        assert_eq!(first_token("hello"), Some(Token::Ident("hello")));
        assert_eq!(first_token("_foo"), Some(Token::Ident("_foo")));
        assert_eq!(first_token("x123"), Some(Token::Ident("x123")));
    }

    #[test]
    fn test_keywords() {
        assert_eq!(first_token("struct"), Some(Token::Struct));
        assert_eq!(first_token("if"), Some(Token::If));
        assert_eq!(first_token("else"), Some(Token::Else));
        assert_eq!(first_token("return"), Some(Token::Return));
        assert_eq!(first_token("while"), Some(Token::While));
        assert_eq!(first_token("for"), Some(Token::For));
        assert_eq!(first_token("in"), Some(Token::In));
        assert_eq!(first_token("loop"), Some(Token::Loop));
        assert_eq!(first_token("module"), Some(Token::Module));
        assert_eq!(first_token("export"), Some(Token::Export));
        assert_eq!(first_token("import"), Some(Token::Import));
        assert_eq!(first_token("impl"), Some(Token::Implements));
        assert_eq!(first_token("macro"), Some(Token::Macro));
        assert_eq!(first_token("true"), Some(Token::BoolLit(true)));
        assert_eq!(first_token("false"), Some(Token::BoolLit(false)));
        assert_eq!(first_token("let"), Some(Token::Let));
        assert_eq!(first_token("mut"), Some(Token::Mut));
        assert_eq!(first_token("unsafe"), Some(Token::Unsafe));
        assert_eq!(first_token("trusted"), Some(Token::Trusted));
        assert_eq!(first_token("c"), Some(Token::CKeyword));
        assert_eq!(first_token("inline"), Some(Token::Inline));
        assert_eq!(first_token("int"), Some(Token::Int));
        assert_eq!(first_token("bool"), Some(Token::Bool));
        assert_eq!(first_token("char"), Some(Token::Char));
        assert_eq!(first_token("string"), Some(Token::String));
        assert_eq!(first_token("float32"), Some(Token::Float32));
        assert_eq!(first_token("float64"), Some(Token::Float64));
        assert_eq!(first_token("ptr"), Some(Token::Ptr));
        assert_eq!(first_token("box"), Some(Token::Box));
        assert_eq!(first_token("ptrany"), Some(Token::Ptrany));
        assert_eq!(first_token("as"), Some(Token::As));
        assert_eq!(first_token("addr"), Some(Token::Addr));
        assert_eq!(first_token("deref"), Some(Token::Deref));
        assert_eq!(first_token("choose"), Some(Token::Choose));
        assert_eq!(first_token("when"), Some(Token::When));
        assert_eq!(first_token("otherwise"), Some(Token::Otherwise));
        assert_eq!(first_token("ref"), Some(Token::Ref));
        assert_eq!(first_token("move"), Some(Token::Move));
        assert_eq!(first_token("clone"), Some(Token::Clone));
        assert_eq!(first_token("test"), Some(Token::Test));
        assert_eq!(first_token("op_add"), Some(Token::OpAdd));
        assert_eq!(first_token("op_sub"), Some(Token::OpSub));
        assert_eq!(first_token("op_mul"), Some(Token::OpMul));
        assert_eq!(first_token("op_eq"), Some(Token::OpEq));
        assert_eq!(first_token("op_neq"), Some(Token::OpNeq));
    }

    #[test]
    fn test_enum_keyword() {
        assert_eq!(first_token("enum"), Some(Token::Enum));
    }

    #[test]
    fn test_dollar_token() {
        assert_eq!(first_token("$"), Some(Token::Dollar));
        let toks = collect("$x");
        assert_eq!(toks.len(), 2);
        assert_eq!(toks[0].1, Token::Dollar);
        assert_eq!(toks[1].1, Token::Ident("x"));
    }

    #[test]
    fn test_operators() {
        let toks = collect("== != := => + - * = < > ! . , ; : ( ) [ ] { }");
        let expected = vec![
            Token::EqEq, Token::Neq, Token::ColonEq, Token::DArrow,
            Token::Plus, Token::Minus, Token::Star, Token::Eq,
            Token::Lt, Token::Gt, Token::Not,
            Token::Dot, Token::Comma, Token::Semi, Token::Colon,
            Token::LParen, Token::RParen, Token::LBracket, Token::RBracket,
            Token::LBrace, Token::RBrace,
        ];
        for (i, exp) in expected.into_iter().enumerate() {
            assert_eq!(
                toks.get(i).map(|(_, t, _)| t),
                Some(&exp),
                "mismatch at position {}",
                i
            );
        }
    }

    #[test]
    fn test_string_literal() {
        let toks = collect(r#""hello world""#);
        assert_eq!(toks[0].1, Token::StringLit("hello world"));
    }

    #[test]
    fn test_multi_string() {
        let input = "\"\"\"\nhello\nworld\n\"\"\"";
        let toks = collect(input);
        assert_eq!(toks[0].1, Token::StringLit("\nhello\nworld\n"));
    }

    #[test]
    fn test_char_literal() {
        let toks = collect("'a'");
        assert_eq!(toks[0].1, Token::CharLit("a"));
    }

    #[test]
    fn test_line_comment() {
        let toks = collect("// this is a comment\n42");
        assert_eq!(toks.len(), 1);
        assert_eq!(toks[0].1, Token::IntLit("42"));
    }

    #[test]
    fn test_block_comment() {
        let toks = collect("/* comment */ 42");
        assert_eq!(toks.len(), 1);
        assert_eq!(toks[0].1, Token::IntLit("42"));
    }

    #[test]
    fn test_nested_block_comment() {
        let toks = collect("/* outer /* inner */ still comment */ 42");
        assert_eq!(toks.len(), 1);
        assert_eq!(toks[0].1, Token::IntLit("42"));
    }

    #[test]
    fn test_magical_directive() {
        let toks = collect("/! some magical content\n42");
        assert_eq!(toks.len(), 2);
        assert_eq!(toks[0].1, Token::Magical(" some magical content"));
        assert_eq!(toks[1].1, Token::IntLit("42"));
    }

    #[test]
    fn test_intro_directive() {
        let toks = collect("@ some intro content\n42");
        assert_eq!(toks.len(), 2);
        assert_eq!(toks[0].1, Token::Intro(" some intro content"));
        assert_eq!(toks[1].1, Token::IntLit("42"));
    }

    #[test]
    fn test_multiple_tokens() {
        let toks = collect("x = 42");
        assert_eq!(toks.len(), 3);
        assert_eq!(toks[0].1, Token::Ident("x"));
        assert_eq!(toks[1].1, Token::Eq);
        assert_eq!(toks[2].1, Token::IntLit("42"));
    }

    #[test]
    fn test_function_def() {
        let input = "add = (x: int, y: int): int => x + y";
        let toks: Vec<Token> = collect(input).into_iter().map(|(_, t, _)| t).collect();
        assert_eq!(toks[0], Token::Ident("add"));
        assert_eq!(toks[1], Token::Eq);
        assert_eq!(toks[2], Token::LParen);
        assert_eq!(toks[3], Token::Ident("x"));
        assert_eq!(toks[4], Token::Colon);
        assert_eq!(toks[5], Token::Int);
        assert_eq!(toks[6], Token::Comma);
        assert_eq!(toks[7], Token::Ident("y"));
        assert_eq!(toks[8], Token::Colon);
        assert_eq!(toks[9], Token::Int);
        assert_eq!(toks[10], Token::RParen);
        assert_eq!(toks[11], Token::Colon);
        assert_eq!(toks[12], Token::Int);
        assert_eq!(toks[13], Token::DArrow);
        assert_eq!(toks[14], Token::Ident("x"));
        assert_eq!(toks[15], Token::Plus);
        assert_eq!(toks[16], Token::Ident("y"));
    }

    #[test]
    fn test_struct_def() {
        let input = "Point = struct { x: int, y: int }";
        let toks: Vec<Token> = collect(input).into_iter().map(|(_, t, _)| t).collect();
        assert_eq!(toks[0], Token::Ident("Point"));
        assert_eq!(toks[1], Token::Eq);
        assert_eq!(toks[2], Token::Struct);
        assert_eq!(toks[3], Token::LBrace);
        assert_eq!(toks[4], Token::Ident("x"));
        assert_eq!(toks[5], Token::Colon);
        assert_eq!(toks[6], Token::Int);
        assert_eq!(toks[7], Token::Comma);
        assert_eq!(toks[8], Token::Ident("y"));
        assert_eq!(toks[9], Token::Colon);
        assert_eq!(toks[10], Token::Int);
        assert_eq!(toks[11], Token::RBrace);
    }

    #[test]
    fn test_unsafe_func() {
        let input = "unsafe foo = (): int => { return 0; }";
        let toks: Vec<Token> = collect(input).into_iter().map(|(_, t, _)| t).collect();
        assert_eq!(toks[0], Token::Unsafe);
        assert_eq!(toks[1], Token::Ident("foo"));
        assert_eq!(toks[2], Token::Eq);
    }

    #[test]
    fn test_block_comment_unterminated_error() {
        let results: Vec<_> = collect_with_errors("/* oops\n42");
        assert!(results.iter().any(|r| r.is_err()));
    }

    #[test]
    fn test_unterminated_string_error() {
        let results: Vec<_> = collect_with_errors(r#""unterminated"#);
        assert!(results.iter().any(|r| r.is_err()));
    }

    #[test]
    fn test_unterminated_multi_string_error() {
        let results: Vec<_> = collect_with_errors("\"\"\"\nhello\n");
        assert!(results.iter().any(|r| r.is_err()));
    }

    #[test]
    fn test_unterminated_char_error() {
        let results: Vec<_> = collect_with_errors("'a");
        assert!(results.iter().any(|r| r.is_err()));
    }

    #[test]
    fn test_imp_op_keywords() {
        let input = "impl Point { op_add my_add, op_sub my_sub }";
        let toks: Vec<Token> = collect(input).into_iter().map(|(_, t, _)| t).collect();
        assert_eq!(toks[0], Token::Implements);
        assert_eq!(toks[1], Token::Ident("Point"));
        assert_eq!(toks[2], Token::LBrace);
        assert_eq!(toks[3], Token::OpAdd);
        assert_eq!(toks[4], Token::Ident("my_add"));
        assert_eq!(toks[5], Token::Comma);
        assert_eq!(toks[6], Token::OpSub);
        assert_eq!(toks[7], Token::Ident("my_sub"));
        assert_eq!(toks[8], Token::RBrace);
    }

    #[test]
    fn test_elif() {
        let toks: Vec<Token> = collect("elif").into_iter().map(|(_, t, _)| t).collect();
        assert_eq!(toks[0], Token::Elif);
    }

    #[test]
    fn test_import_statement() {
        let input = "import \"std/io\";";
        let toks: Vec<Token> = collect(input).into_iter().map(|(_, t, _)| t).collect();
        assert_eq!(toks[0], Token::Import);
        assert_eq!(toks[1], Token::StringLit("std/io"));
        assert_eq!(toks[2], Token::Semi);
    }

    #[test]
    fn test_import_as() {
        let input = "import \"std/io\" as io;";
        let toks: Vec<Token> = collect(input).into_iter().map(|(_, t, _)| t).collect();
        assert_eq!(toks[0], Token::Import);
        assert_eq!(toks[1], Token::StringLit("std/io"));
        assert_eq!(toks[2], Token::As);
        assert_eq!(toks[3], Token::Ident("io"));
        assert_eq!(toks[4], Token::Semi);
    }

    #[test]
    fn test_module_decl() {
        let input = "module std.mem;";
        let toks: Vec<Token> = collect(input).into_iter().map(|(_, t, _)| t).collect();
        assert_eq!(toks[0], Token::Module);
        assert_eq!(toks[1], Token::Ident("std"));
        assert_eq!(toks[2], Token::Dot);
        assert_eq!(toks[3], Token::Ident("mem"));
        assert_eq!(toks[4], Token::Semi);
    }

    #[test]
    fn test_mut_let() {
        let input = "mut x := 42;";
        let toks: Vec<Token> = collect(input).into_iter().map(|(_, t, _)| t).collect();
        assert_eq!(toks[0], Token::Mut);
        assert_eq!(toks[1], Token::Ident("x"));
        assert_eq!(toks[2], Token::ColonEq);
        assert_eq!(toks[3], Token::IntLit("42"));
        assert_eq!(toks[4], Token::Semi);
    }

    #[test]
    fn test_for_loop() {
        let input = "for i in (range(3)) { };";
        let toks: Vec<Token> = collect(input).into_iter().map(|(_, t, _)| t).collect();
        assert_eq!(toks[0], Token::For);
        assert_eq!(toks[1], Token::Ident("i"));
        assert_eq!(toks[2], Token::In);
        assert_eq!(toks[3], Token::LParen);
        assert_eq!(toks[4], Token::Ident("range"));
        assert_eq!(toks[5], Token::LParen);
        assert_eq!(toks[6], Token::IntLit("3"));
        assert_eq!(toks[7], Token::RParen);
        assert_eq!(toks[8], Token::RParen);
        assert_eq!(toks[9], Token::LBrace);
        assert_eq!(toks[10], Token::RBrace);
        assert_eq!(toks[11], Token::Semi);
    }

    #[test]
    fn test_choose_when() {
        let input = "choose (x) { when (1) { 10 } otherwise { 0 } }";
        let toks: Vec<Token> = collect(input).into_iter().map(|(_, t, _)| t).collect();
        assert_eq!(toks[0], Token::Choose);
        assert_eq!(toks[1], Token::LParen);
        assert_eq!(toks[2], Token::Ident("x"));
        assert_eq!(toks[3], Token::RParen);
        assert_eq!(toks[4], Token::LBrace);
        assert_eq!(toks[5], Token::When);
        assert_eq!(toks[6], Token::LParen);
        assert_eq!(toks[7], Token::IntLit("1"));
        assert_eq!(toks[8], Token::RParen);
        assert_eq!(toks[9], Token::LBrace);
        assert_eq!(toks[10], Token::IntLit("10"));
        assert_eq!(toks[11], Token::RBrace);
        assert_eq!(toks[12], Token::Otherwise);
        assert_eq!(toks[13], Token::LBrace);
        assert_eq!(toks[14], Token::IntLit("0"));
        assert_eq!(toks[15], Token::RBrace);
        assert_eq!(toks[16], Token::RBrace);
    }

    #[test]
    fn test_string_with_escape() {
        let input = r#""hello\nworld""#;
        let toks = collect(input);
        assert_eq!(toks[0].1, Token::StringLit("hello\\nworld"));
    }

    #[test]
    fn test_offset_to_line_col() {
        assert_eq!(offset_to_line_col("abc", 0), (1, 1));
        assert_eq!(offset_to_line_col("abc", 2), (1, 3));
        assert_eq!(offset_to_line_col("a\nbc", 0), (1, 1));
        assert_eq!(offset_to_line_col("a\nbc", 1), (1, 2));
        assert_eq!(offset_to_line_col("a\nbc", 2), (2, 1));
        assert_eq!(offset_to_line_col("a\nbc\nd", 4), (2, 3));
    }

    #[test]
    fn test_u32_ident() {
        // Identifiers with digits are fine
        assert_eq!(first_token("foo123"), Some(Token::Ident("foo123")));
    }

    #[test]
    fn test_dot_in_ident_not_allowed() {
        // "std.mem" should be "std" "." "mem"
        let toks: Vec<Token> = collect("std.mem").into_iter().map(|(_, t, _)| t).collect();
        assert_eq!(toks.len(), 3);
        assert_eq!(toks[0], Token::Ident("std"));
        assert_eq!(toks[1], Token::Dot);
        assert_eq!(toks[2], Token::Ident("mem"));
    }
}
