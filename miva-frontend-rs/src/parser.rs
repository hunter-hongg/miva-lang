use crate::ast::*;
use crate::lexer::{offset_to_line_col, Lexer, Token};
use crate::util;

/// Recursive descent parser for Miva.
pub struct Parser<'input> {
    lexer: Lexer<'input>,
    input: &'input str,
    file_name: &'input str,
    peeked: Option<Option<(usize, Token<'input>, usize)>>,
    pending_type_args: Vec<Typ>,
}

impl<'input> Parser<'input> {
    pub fn new(lexer: Lexer<'input>, input: &'input str, file_name: &'input str) -> Self {
        Parser {
            lexer,
            input,
            file_name,
            peeked: None,
            pending_type_args: vec![],
        }
    }

    // ── Helpers ──────────────────────────────────────────────────────

    fn loc(&self, byte: usize) -> Loc {
        let (line, col) = offset_to_line_col(self.input, byte);
        Loc::new(line, col)
    }

    fn advance(&mut self) -> Result<(usize, Token<'input>, usize), String> {
        if let Some(peeked) = self.peeked.take() {
            peeked.ok_or_else(|| "unexpected EOF".to_string())
        } else {
            self.lexer
                .next()
                .unwrap_or(Err("unexpected EOF".to_string()))
        }
    }

    fn peek(&mut self) -> Result<Option<&(usize, Token<'input>, usize)>, String> {
        if self.peeked.is_none() {
            self.peeked = Some(self.lexer.next().transpose()?);
        }
        Ok(self.peeked.as_ref().unwrap().as_ref())
    }

    fn peek_token(&mut self) -> Result<Option<&Token<'input>>, String> {
        Ok(self.peek()?.map(|(_, t, _)| t))
    }

    fn expect(&mut self, expected: &Token<'input>) -> Result<(usize, usize), String> {
        let (start, tok, end) = self.advance()?;
        if &tok != expected {
            return Err(format!(
                "Expected {:?}, found {:?} at {}",
                expected,
                tok,
                self.loc(start).line
            ));
        }
        Ok((start, end))
    }

    fn expect_ident(&mut self) -> Result<(String, usize), String> {
        let (start, tok, _) = self.advance()?;
        match tok {
            Token::Ident(s) => Ok((s.to_string(), start)),
            _ => Err(format!("Expected identifier, found {:?}", tok)),
        }
    }

    // Field name after `.`: a normal identifier, or a numeric literal for
    // positional enum payload access (e.g. `x.0`, `x.1`).
    fn parse_field_name(&mut self) -> Result<(String, usize), String> {
        let (start, tok, _) = self.advance()?;
        match tok {
            Token::Ident(s) => Ok((s.to_string(), start)),
            Token::IntLit(s) => Ok((s.to_string(), start)),
            _ => Err(format!("Expected field name, found {:?}", tok)),
        }
    }

    fn expect_str_lit(&mut self) -> Result<(String, usize), String> {
        let (start, tok, _) = self.advance()?;
        match tok {
            Token::StringLit(s) => Ok((s.to_string(), start)),
            _ => Err(format!("Expected string literal, found {:?}", tok)),
        }
    }

    fn expect_int_lit(&mut self) -> Result<(i64, usize), String> {
        let (start, tok, _) = self.advance()?;
        match tok {
            Token::IntLit(s) => s
                .parse::<i64>()
                .map(|v| (v, start))
                .map_err(|e| format!("Invalid integer literal '{}': {}", s, e)),
            _ => Err(format!("Expected integer literal, found {:?}", tok)),
        }
    }

    fn expect_keyword(&mut self, kw: &Token<'input>) -> Result<usize, String> {
        let (start, tok, _) = self.advance()?;
        if &tok != kw {
            return Err(format!("Expected {:?}, found {:?}", kw, tok));
        }
        Ok(start)
    }

    // ── Program ──────────────────────────────────────────────────────

    pub fn parse_program(&mut self) -> Result<Vec<Def>, String> {
        let mut defs = Vec::new();
        while self.peek_token()?.is_some() {
            defs.push(self.parse_def()?);
        }
        Ok(defs)
    }

    // ── Definitions ──────────────────────────────────────────────────

    fn parse_def(&mut self) -> Result<Def, String> {
        // Check for defs that start with a keyword prefix
        if let Some(tok) = self.peek_token()?.cloned() {
            match &tok {
                Token::Unsafe => return self.parse_func_unsafe(),
                Token::Trusted => return self.parse_func_trusted(),
                Token::CKeyword => return self.parse_c_func(true),
                Token::Inline => return self.parse_c_func(false),
                Token::Async => return self.parse_func_async(),
                Token::Test => return self.parse_test(),
                Token::Module => return self.parse_module(),
                Token::Export => return self.parse_export(),
                Token::Import => return self.parse_import(),
                Token::Implements => return self.parse_impl(),
                Token::Macro => return self.parse_macro_def(),
                Token::Intro(_) => return self.parse_intro_def(),
                Token::Magical(_) => return self.parse_magical_def(),
                _ => {}
            }
        }

        // Try struct or function (starts with identifier)
        self.parse_struct_or_func()
    }

    fn parse_struct_or_func(&mut self) -> Result<Def, String> {
        let (start, tok, _) = self.advance()?;
        let name = match tok {
            Token::Ident(s) => s.to_string(),
            _ => {
                return Err(format!(
                    "Expected identifier at start of definition, found {:?}",
                    tok
                ))
            }
        };

        // Check for generic type parameters: name[T, U] =
        let type_params = if self.peek_token()? == Some(&Token::LBracket) {
            self.advance()?; // consume "["
            let mut params = Vec::new();
            loop {
                let (pname, _) = self.expect_ident()?;
                params.push(pname);
                if self.peek_token()? == Some(&Token::Comma) {
                    self.advance()?;
                } else {
                    break;
                }
            }
            self.expect(&Token::RBracket)?;
            params
        } else {
            vec![]
        };

        self.expect(&Token::Eq)?;

        // Check for struct
        if self.peek_token()? == Some(&Token::Struct) {
            return self.parse_struct_body(name, type_params, start);
        }

        // Check for enum
        if self.peek_token()? == Some(&Token::Enum) {
            return self.parse_enum_body(name, type_params, start);
        }

        // Must be a function
        self.parse_func_body(name, type_params, start, Safety::Safe, false)
    }

    fn parse_struct_body(
        &mut self,
        name: String,
        type_params: Vec<String>,
        start: usize,
    ) -> Result<Def, String> {
        self.advance()?; // consume "struct"
        self.expect(&Token::LBrace)?;
        let mut fields = Vec::new();
        while self.peek_token()? != Some(&Token::RBrace) {
            let (field_name, start) = self.expect_ident()?;
            self.expect(&Token::Colon)?;
            let typ = self.parse_typ()?;
            fields.push(FieldDef {
                name: field_name,
                typ,
            });
            // optional comma
            if self.peek_token()? == Some(&Token::Comma) {
                self.advance()?;
            }
        }
        self.expect(&Token::RBrace)?;
        Ok(Def::DStruct {
            loc: self.loc(start),
            name,
            fields,
            type_params,
        })
    }

    fn parse_enum_body(
        &mut self,
        name: String,
        type_params: Vec<String>,
        start: usize,
    ) -> Result<Def, String> {
        self.advance()?; // consume "enum"
        self.expect(&Token::LBrace)?;
        let mut variants = Vec::new();
        while self.peek_token()? != Some(&Token::RBrace) {
            let (vname, _) = self.expect_ident()?;
            let payload = if self.peek_token()? == Some(&Token::LParen) {
                self.advance()?; // consume "("
                let mut types = Vec::new();
                if self.peek_token()? != Some(&Token::RParen) {
                    loop {
                        types.push(self.parse_typ()?);
                        if self.peek_token()? == Some(&Token::Comma) {
                            self.advance()?;
                        } else {
                            break;
                        }
                    }
                }
                self.expect(&Token::RParen)?;
                types
            } else {
                vec![]
            };
            variants.push(EnumVariant { name: vname, payload });
            if self.peek_token()? == Some(&Token::Comma) {
                self.advance()?;
            }
        }
        self.expect(&Token::RBrace)?;
        Ok(Def::DEnum {
            loc: self.loc(start),
            name,
            variants,
            type_params,
        })
    }

    fn parse_func_body(
        &mut self,
        name: String,
        type_params: Vec<String>,
        start: usize,
        safety: Safety,
        is_async: bool,
    ) -> Result<Def, String> {
        let params = self.parse_func_params()?;
        let returns = if self.peek_token()? == Some(&Token::Colon) {
            self.advance()?; // consume ":"
            Some(self.parse_typ()?)
        } else {
            None
        };
        self.expect(&Token::DArrow)?;
        let body = self.parse_expr()?;
        Ok(Def::DFunc {
            loc: self.loc(start),
            name,
            type_params,
            params,
            returns,
            body: Box::new(body),
            safety,
            is_async,
        })
    }

    fn parse_func_async(&mut self) -> Result<Def, String> {
        let start = self.advance()?.0; // consume "async"
        let (name, _) = self.expect_ident()?;
        let type_params = if self.peek_token()? == Some(&Token::LBracket) {
            self.advance()?;
            let mut params = Vec::new();
            loop {
                let (pname, _) = self.expect_ident()?;
                params.push(pname);
                if self.peek_token()? == Some(&Token::Comma) {
                    self.advance()?;
                } else {
                    break;
                }
            }
            self.expect(&Token::RBracket)?;
            params
        } else {
            vec![]
        };
        self.expect(&Token::Eq)?;
        self.parse_func_body(name, type_params, start, Safety::Safe, true)
    }

    fn parse_func_unsafe(&mut self) -> Result<Def, String> {
        let start = self.advance()?.0; // consume "unsafe"
        let (name, _) = self.expect_ident()?;
        let type_params = if self.peek_token()? == Some(&Token::LBracket) {
            self.advance()?;
            let mut params = Vec::new();
            loop {
                let (pname, _) = self.expect_ident()?;
                params.push(pname);
                if self.peek_token()? == Some(&Token::Comma) {
                    self.advance()?;
                } else {
                    break;
                }
            }
            self.expect(&Token::RBracket)?;
            params
        } else {
            vec![]
        };
        self.expect(&Token::Eq)?;
        self.parse_func_body(name, type_params, start, Safety::Unsafe, false)
    }

    fn parse_func_trusted(&mut self) -> Result<Def, String> {
        let start = self.advance()?.0; // consume "trusted"
        let (name, _) = self.expect_ident()?;
        let type_params = if self.peek_token()? == Some(&Token::LBracket) {
            self.advance()?;
            let mut params = Vec::new();
            loop {
                let (pname, _) = self.expect_ident()?;
                params.push(pname);
                if self.peek_token()? == Some(&Token::Comma) {
                    self.advance()?;
                } else {
                    break;
                }
            }
            self.expect(&Token::RBracket)?;
            params
        } else {
            vec![]
        };
        self.expect(&Token::Eq)?;
        self.parse_func_body(name, type_params, start, Safety::Trusted, false)
    }

    fn parse_c_func(&mut self, used_c_keyword: bool) -> Result<Def, String> {
        self.advance()?; // consume "c" or "inline"
        let start = self.expect_keyword(&Token::Unsafe)?;
        let (name, _) = self.expect_ident()?;
        self.expect(&Token::Eq)?;
        let params = self.parse_func_params()?;
        let returns = if self.peek_token()? == Some(&Token::Colon) {
            self.advance()?;
            Some(self.parse_typ()?)
        } else {
            None
        };
        self.expect(&Token::DArrow)?;
        let code = if self.peek_token()? == Some(&Token::LBrace) {
            self.read_raw_brace_block()?
        } else {
            let (code, _) = self.expect_str_lit()?;
            code
        };
        Ok(Def::DCFuncUnsafe {
            loc: self.loc(start),
            name,
            params,
            returns,
            code,
            safety: Safety::Unsafe,
            used_c_keyword,
        })
    }

    /// Read raw C code between balanced braces `{ ... }` from the source input.
    /// Consumes lexer tokens to track brace depth, then extracts the raw text
    /// directly from `self.input`.
    fn read_raw_brace_block(&mut self) -> Result<String, String> {
        let (_, _, brace_end) = self.advance()?; // consume '{'
        let content_start = brace_end;
        let mut depth = 1u32;
        let mut content_end = content_start;
        while depth > 0 {
            let (tok_start, tok, _) = self.advance()?;
            match tok {
                Token::LBrace => depth += 1,
                Token::RBrace => {
                    depth -= 1;
                    if depth == 0 {
                        content_end = tok_start;
                    }
                }
                _ => {}
            }
        }
        Ok(self.input[content_start..content_end].to_string())
    }

    fn parse_test(&mut self) -> Result<Def, String> {
        let start = self.advance()?.0; // consume "test"
        let (name, _) = self.expect_ident()?;
        self.expect(&Token::Eq)?;
        self.expect(&Token::LParen)?;
        self.expect(&Token::RParen)?;
        self.expect(&Token::Colon)?;
        self.expect_keyword(&Token::Int)?;
        self.expect(&Token::DArrow)?;
        let body = self.parse_expr()?;
        Ok(Def::DTest {
            loc: self.loc(start),
            name,
            body: Box::new(body),
        })
    }

    fn parse_module(&mut self) -> Result<Def, String> {
        let start = self.advance()?.0; // consume "module"
        let mut parts = vec![self.expect_ident()?.0];
        while self.peek_token()? == Some(&Token::Dot) {
            self.advance()?; // "."
            parts.push(self.expect_ident()?.0);
        }
        self.expect(&Token::Semi)?;
        Ok(Def::DModule {
            loc: self.loc(start),
            name: parts.join("."),
        })
    }

    fn parse_export(&mut self) -> Result<Def, String> {
        let start = self.advance()?.0;
        let (sym, _) = self.expect_ident()?;
        self.expect(&Token::Semi)?;
        Ok(Def::SExport {
            loc: self.loc(start),
            symbol: sym,
        })
    }

    fn parse_import(&mut self) -> Result<Def, String> {
        let start = self.advance()?.0; // consume "import"
        let (path, _) = self.expect_str_lit()?;
        if self.peek_token()? == Some(&Token::Semi) {
            self.advance()?;
            return Ok(Def::SImport {
                loc: self.loc(start),
                path,
            });
        }
        // "as"
        self.expect_keyword(&Token::As)?;
        // Check for "as ." (import here)
        if self.peek_token()? == Some(&Token::Dot) {
            self.advance()?; // "."
            self.expect(&Token::Semi)?;
            return Ok(Def::SImportHere {
                loc: self.loc(start),
                path,
            });
        }
        // "as" ident (or ident.ident...)
        let mut parts = vec![self.expect_ident()?.0];
        while self.peek_token()? == Some(&Token::Dot) {
            self.advance()?; // "."
            parts.push(self.expect_ident()?.0);
        }
        self.expect(&Token::Semi)?;
        Ok(Def::SImportAs {
            loc: self.loc(start),
            path,
            alias: parts.join("."),
        })
    }

    fn parse_impl(&mut self) -> Result<Def, String> {
        let start = self.advance()?.0; // "impl"
        let (struct_name, _) = self.expect_ident()?;
        self.expect(&Token::LBrace)?;
        let mut impls = Vec::new();
        while self.peek_token()? != Some(&Token::RBrace) {
            let impl_start = match self.peek_token()? {
                Some(&Token::OpAdd) | Some(&Token::OpSub) | Some(&Token::OpMul)
                | Some(&Token::OpEq) | Some(&Token::OpNeq) => {
                    let s = self.advance()?.0;
                    let op = match self.lexer_last_token()? {
                        Token::OpAdd => ImplOp::ImAdd,
                        Token::OpSub => ImplOp::ImSub,
                        Token::OpMul => ImplOp::ImMul,
                        Token::OpDiv => ImplOp::ImDiv,
                        Token::OpEq => ImplOp::ImEq,
                        Token::OpNeq => ImplOp::ImNeq,
                        _ => unreachable!(),
                    };
                    let (func, _) = self.expect_ident()?;
                    impls.push(ImplExpr {
                        op,
                        func,
                        loc: self.loc(s),
                    });
                    s
                }
                _ => {
                    return Err("Expected operator keyword (op_add, etc.) in impl block".to_string())
                }
            };
            if self.peek_token()? == Some(&Token::Comma) {
                self.advance()?;
            }
        }
        self.expect(&Token::RBrace)?;
        Ok(Def::DImpl {
            loc: self.loc(start),
            struct_name,
            impls,
        })
    }

    fn parse_macro_def(&mut self) -> Result<Def, String> {
        let start = self.advance()?.0; // consume "macro"
        let (name, _) = self.expect_ident()?;
        self.expect(&Token::Eq)?;
        let params = self.parse_macro_params()?;
        self.expect(&Token::DArrow)?;
        let body = self.parse_expr()?;
        Ok(Def::DMacro {
            loc: self.loc(start),
            name,
            params,
            body: Box::new(body),
        })
    }

    fn parse_macro_params(&mut self) -> Result<Vec<MacroParam>, String> {
        self.expect(&Token::LParen)?;
        let mut params = Vec::new();
        if self.peek_token()? == Some(&Token::RParen) {
            self.advance()?;
            return Ok(params);
        }
        loop {
            // Expect: $name : type
            self.expect(&Token::Dollar)?;
            let (name, _) = self.expect_ident()?;
            self.expect(&Token::Colon)?;
            let typ = self.parse_typ()?;
            params.push(MacroParam { name, typ });
            if self.peek_token()? == Some(&Token::Comma) {
                self.advance()?;
            } else {
                break;
            }
        }
        self.expect(&Token::RParen)?;
        Ok(params)
    }

    fn parse_intro_def(&mut self) -> Result<Def, String> {
        let (start, tok, _) = self.advance()?;
        match tok {
            Token::Intro(s) => Ok(Def::DCIntro {
                loc: self.loc(start),
                content: s.to_string(),
            }),
            _ => Err("Expected intro directive".to_string()),
        }
    }

    fn parse_magical_def(&mut self) -> Result<Def, String> {
        let (start, tok, _) = self.advance()?;
        match tok {
            Token::Magical(s) => Ok(Def::DCMagical {
                loc: self.loc(start),
                content: s.to_string(),
            }),
            _ => Err("Expected magical directive".to_string()),
        }
    }

    fn lexer_last_token(&self) -> Result<Token<'input>, String> {
        // This is a helper to get the last consumed token — we track it differently
        Err("not implemented directly".to_string())
    }

    // ── Function Parameters ──────────────────────────────────────────

    fn parse_func_params(&mut self) -> Result<Vec<Param>, String> {
        self.expect(&Token::LParen)?;
        let mut params = Vec::new();
        if self.peek_token()? == Some(&Token::RParen) {
            self.advance()?;
            return Ok(params);
        }
        loop {
            params.push(self.parse_param()?);
            if self.peek_token()? == Some(&Token::Comma) {
                self.advance()?;
            } else {
                break;
            }
        }
        self.expect(&Token::RParen)?;
        Ok(params)
    }

    fn parse_param(&mut self) -> Result<Param, String> {
        let is_ref = self.peek_token()? == Some(&Token::Ref);
        if is_ref {
            self.advance()?;
        }
        let (name, start) = self.expect_ident()?;
        self.expect(&Token::Colon)?;
        let typ = self.parse_typ()?;
        if is_ref {
            Ok(Param::PRef { name, typ })
        } else {
            Ok(Param::POwn { name, typ })
        }
    }

    // ── Types ────────────────────────────────────────────────────────

    fn parse_typ(&mut self) -> Result<Typ, String> {
        match self.peek_token()? {
            Some(&Token::Int) => {
                self.advance()?;
                Ok(Typ::TInt)
            }
            Some(&Token::Bool) => {
                self.advance()?;
                Ok(Typ::TBool)
            }
            Some(&Token::Float32) => {
                self.advance()?;
                Ok(Typ::TFloat32)
            }
            Some(&Token::Float64) => {
                self.advance()?;
                Ok(Typ::TFloat64)
            }
            Some(&Token::Char) => {
                self.advance()?;
                Ok(Typ::TChar)
            }
            Some(&Token::String) => {
                self.advance()?;
                Ok(Typ::TString)
            }
            Some(&Token::Ptrany) => {
                self.advance()?;
                Ok(Typ::TPtrAny)
            }
            Some(&Token::Ptr) => {
                self.advance()?;
                self.expect(&Token::Lt)?;
                let t = self.parse_typ()?;
                self.expect(&Token::Gt)?;
                Ok(Typ::TPtr { to: Box::new(t) })
            }
            Some(&Token::Box) => {
                self.advance()?;
                self.expect(&Token::Lt)?;
                let t = self.parse_typ()?;
                self.expect(&Token::Gt)?;
                Ok(Typ::TBox { of: Box::new(t) })
            }
            Some(&Token::LBracket) => {
                self.advance()?;
                let t = self.parse_typ()?;
                self.expect(&Token::RBracket)?;
                Ok(Typ::TArray { of: Box::new(t) })
            }
            Some(&Token::Ident(_)) => {
                // Special built-in future[T] type
                let name = self.parse_type_path()?;
                if name == "future" {
                    self.expect(&Token::LBracket)?;
                    let of = self.parse_typ()?;
                    self.expect(&Token::RBracket)?;
                    return Ok(Typ::TFuture {
                        of: Box::new(of),
                    });
                }
                let type_args = if self.peek_token()? == Some(&Token::LBracket) {
                    self.advance()?; // consume "["
                    let mut args = Vec::new();
                    loop {
                        args.push(self.parse_typ()?);
                        if self.peek_token()? == Some(&Token::Comma) {
                            self.advance()?;
                        } else {
                            break;
                        }
                    }
                    self.expect(&Token::RBracket)?;
                    args
                } else {
                    vec![]
                };
                Ok(Typ::TStruct {
                    name,
                    fields: vec![],
                    type_args,
                })
            }
            Some(tok) => Err(format!("Expected type, found {:?}", tok)),
            None => Err("Expected type, found EOF".to_string()),
        }
    }

    fn parse_type_path(&mut self) -> Result<String, String> {
        let mut parts = vec![self.expect_ident()?.0];
        while self.peek_token()? == Some(&Token::Dot) {
            self.advance()?; // "."
            parts.push(self.expect_ident()?.0);
        }
        Ok(parts.join("::"))
    }

    // ── Statement ────────────────────────────────────────────────────

    fn parse_stmt(&mut self) -> Result<Stmt, String> {
        // Check statement-starting patterns using peek
        let peeked = self.peek_token()?;
        let is_ident_stmt = matches!(peeked, Some(Token::Ident(_)));

        match peeked {
            Some(Token::Let) => return self.parse_let_typed(),
            Some(Token::Mut) => return self.parse_let_mut(),
            Some(Token::Return) => return self.parse_return_stmt(),
            Some(Token::If) => return self.parse_if_stmt(),
            Some(Token::While) => return self.parse_while_stmt(),
            Some(Token::Loop) => return self.parse_loop_stmt(),
            Some(Token::For) => return self.parse_for_stmt(),
            Some(Token::Semi) => {
                let start = self.advance()?.0;
                return Ok(Stmt::SEmpty {
                    loc: self.loc(start),
                });
            }
            Some(Token::Intro(_)) => {
                let (start, tok, _) = self.advance()?;
                if let Token::Intro(s) = tok {
                    self.expect(&Token::Semi)?;
                    return Ok(Stmt::SCIntro {
                        loc: self.loc(start),
                        content: s.to_string(),
                    });
                }
            }
            _ => {} // fall through
        }

        if is_ident_stmt {
            // For ident-starting statements: advance, then check 2nd token
            let (start, tok, _) = self.advance()?;
            let name = match tok {
                Token::Ident(s) => s.to_string(),
                _ => unreachable!(),
            };
            match self.peek_token()? {
                Some(&Token::ColonEq) => {
                    // `name := expr` (no `let`/`mut` prefix) declares and
                    // assigns a new variable, like `let name = expr`. The
                    // `:=` shorthand is used pervasively in the std library
                    // (e.g. `k := json_kind(v)`) for first declaration, so it
                    // must introduce a binding rather than assign to an
                    // existing one. Mutability is allowed so a later `=`
                    // reassignment in the same scope type-checks.
                    self.advance()?; // ":="
                    let expr = self.parse_expr()?;
                    self.expect(&Token::Semi)?;
                    Ok(Stmt::SLet {
                        loc: self.loc(start),
                        mutable: true,
                        name,
                        expr: Box::new(expr),
                    })
                }
                Some(&Token::Eq) => {
                    self.advance()?; // "="
                    let expr = self.parse_expr()?;
                    self.expect(&Token::Semi)?;
                    Ok(Stmt::SAssign {
                        loc: self.loc(start),
                        name,
                        expr: Box::new(expr),
                    })
                }
                Some(&Token::Dot) => {
                    // `target.field := expr` / `target.field = expr`
                    // (chained `.field` allowed; each segment stores one
                    // SFieldAssign with `target` being the inner expression)
                    let mut target = Expr::EVar {
                        loc: self.loc(start),
                        name: name.clone(),
                    };
                    // Walk one `.field` segment at a time; if after consuming
                    // a segment the next token is `:=` or `=`, we emit a field
                    // assignment whose target is everything parsed so far.
                    // If instead there are more call/field suffixes, we fall
                    // through to the regular expression-statement path.
                    self.advance()?; // consume "."
                    let (field, _) = self.expect_ident()?;
                    match self.peek_token()? {
                        Some(&Token::ColonEq) | Some(&Token::Eq) => {
                            self.advance()?; // consume ":=" or "="
                            let rhs = self.parse_expr()?;
                            self.expect(&Token::Semi)?;
                            Ok(Stmt::SFieldAssign {
                                loc: self.loc(start),
                                target: Box::new(target),
                                field,
                                expr: Box::new(rhs),
                            })
                        }
                        _ => {
                            // Not a field assignment: rebuild as a field-access
                            // expression and continue parsing call/field suffixes
                            // the way a normal ident-statement would.
                            let mut expr = Expr::EFieldAccess {
                                loc: self.loc(start),
                                expr: Box::new(target),
                                field,
                            };
                            expr = self.parse_call_suffix(expr)?;
                            self.expect(&Token::Semi)?;
                            Ok(Stmt::SExpr {
                                loc: self.loc(start),
                                expr: Box::new(expr),
                            })
                        }
                    }
                }
                _ => {
                    // Expression starting with ident — continue parsing call/macro/field suffixes
                    let mut expr = Expr::EVar {
                        loc: self.loc(start),
                        name,
                    };
                    expr = self.parse_call_suffix(expr)?;
                    self.expect(&Token::Semi)?;
                    Ok(Stmt::SExpr {
                        loc: self.loc(start),
                        expr: Box::new(expr),
                    })
                }
            }
        } else {
            // Non-ident statement — parse as expression statement
            let expr = self.parse_expr()?;
            self.expect(&Token::Semi)?;
            let loc = expr_loc(&expr);
            Ok(Stmt::SExpr {
                loc,
                expr: Box::new(expr),
            })
        }
    }

    fn parse_let_typed(&mut self) -> Result<Stmt, String> {
        let start = self.advance()?.0; // "let"
        let (name, _) = self.expect_ident()?;
        let typ = self.parse_typ()?;
        self.expect(&Token::Eq)?;
        let expr = self.parse_expr()?;
        self.expect(&Token::Semi)?;
        Ok(Stmt::SLetTyped {
            loc: self.loc(start),
            name,
            typ,
            expr: Box::new(expr),
        })
    }

    fn parse_let_mut(&mut self) -> Result<Stmt, String> {
        let start = self.advance()?.0; // "mut"
        let (name, _) = self.expect_ident()?;
        self.expect(&Token::ColonEq)?;
        let expr = self.parse_expr()?;
        self.expect(&Token::Semi)?;
        Ok(Stmt::SLet {
            loc: self.loc(start),
            mutable: true,
            name,
            expr: Box::new(expr),
        })
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, String> {
        let start = self.advance()?.0; // "return"
        if self.peek_token()? == Some(&Token::Semi) {
            self.advance()?;
            let loc = self.loc(start);
            return Ok(Stmt::SReturn {
                loc: loc.clone(),
                expr: Box::new(Expr::EVoid { loc }),
            });
        }
        let expr = self.parse_expr()?;
        self.expect(&Token::Semi)?;
        Ok(Stmt::SReturn {
            loc: self.loc(start),
            expr: Box::new(expr),
        })
    }

    // (parse_let_or_assign_or_expr removed — logic inlined into parse_stmt)

    /// Parse call/macro/field suffixes starting from an already-known expression.
    fn parse_call_suffix(&mut self, mut expr: Expr) -> Result<Expr, String> {
        let saved = std::mem::take(&mut self.pending_type_args);
        loop {
            match self.peek_token()? {
                // Generic type arguments + call: name[Type1, Type2](args)
                Some(&Token::LBracket) => {
                    self.advance()?; // "["
                    let mut type_args = Vec::new();
                    loop {
                        type_args.push(self.parse_typ()?);
                        if self.peek_token()? == Some(&Token::Comma) {
                            self.advance()?;
                        } else {
                            break;
                        }
                    }
                    self.expect(&Token::RBracket)?;
                    if self.peek_token()? == Some(&Token::LParen) {
                        self.expect(&Token::LParen)?;
                        let mut args = Vec::new();
                        if self.peek_token()? != Some(&Token::RParen) {
                            loop {
                                args.push(self.parse_expr()?);
                                if self.peek_token()? == Some(&Token::Comma) {
                                    self.advance()?;
                                } else {
                                    break;
                                }
                            }
                        }
                        self.expect(&Token::RParen)?;
                        let call_path = extract_call_path_from_expr(&expr);
                        let processed = util::process_call_path(&call_path);
                        expr = enum_pattern_or_call(expr_loc(&expr), processed, type_args, args);
                    } else {
                        self.pending_type_args = type_args;
                    }
                }
                // Function call: expr(args)
                Some(&Token::LParen) => {
                    self.advance()?;
                    let mut args = Vec::new();
                    if self.peek_token()? != Some(&Token::RParen) {
                        loop {
                            args.push(self.parse_expr()?);
                            if self.peek_token()? == Some(&Token::Comma) {
                                self.advance()?;
                            } else {
                                break;
                            }
                        }
                    }
                    self.expect(&Token::RParen)?;
                    let call_path = extract_call_path_from_expr(&expr);
                    let processed = util::process_call_path(&call_path);
                    expr = enum_pattern_or_call(expr_loc(&expr), processed, vec![], args);
                }
                // Macro call: ident!(args)
                Some(&Token::Not) if matches!(&expr, Expr::EVar { .. }) => {
                    let name = match &expr {
                        Expr::EVar { name, .. } => name.clone(),
                        _ => unreachable!(),
                    };
                    self.advance()?; // "!"
                    self.expect(&Token::LParen)?;
                    let mut args = Vec::new();
                    if self.peek_token()? != Some(&Token::RParen) {
                        loop {
                            args.push(self.parse_expr()?);
                            if self.peek_token()? == Some(&Token::Comma) {
                                self.advance()?;
                            } else {
                                break;
                            }
                        }
                    }
                    self.expect(&Token::RParen)?;
                    expr = Expr::EMacro {
                        loc: expr_loc(&expr),
                        name,
                        args,
                    };
                }
                // Field access or method call: expr.field or expr.method(args)
                Some(&Token::Dot) => {
                    self.advance()?;
                    let (method, _) = self.parse_field_name()?;
                    match self.peek_token()? {
                        // Method call with type args: expr.method[T, U](args)
                        Some(&Token::LBracket)
                            if !matches!(&expr, Expr::EFieldAccess { .. }) =>
                        {
                            self.pending_type_args.clear();
                            self.advance()?;
                            let mut type_args = Vec::new();
                            loop {
                                type_args.push(self.parse_typ()?);
                                if self.peek_token()? == Some(&Token::Comma) {
                                    self.advance()?;
                                } else {
                                    break;
                                }
                            }
                            self.expect(&Token::RBracket)?;
                            self.expect(&Token::LParen)?;
                            let mut args = Vec::new();
                            if self.peek_token()? != Some(&Token::RParen) {
                                loop {
                                    args.push(self.parse_expr()?);
                                    if self.peek_token()? == Some(&Token::Comma) {
                                        self.advance()?;
                                    } else {
                                        break;
                                    }
                                }
                            }
                            self.expect(&Token::RParen)?;
                            let loc = expr_loc(&expr);
                            expr = Expr::EMethodCall {
                                loc,
                                expr: Box::new(expr),
                                method,
                                type_args,
                                args,
                            };
                        }
                        // Method call: expr.method(args)
                        Some(&Token::LParen)
                            if !matches!(&expr, Expr::EFieldAccess { .. }) =>
                        {
                            self.advance()?;
                            let mut args = Vec::new();
                            if self.peek_token()? != Some(&Token::RParen) {
                                loop {
                                    args.push(self.parse_expr()?);
                                    if self.peek_token()? == Some(&Token::Comma) {
                                        self.advance()?;
                                    } else {
                                        break;
                                    }
                                }
                            }
                            self.expect(&Token::RParen)?;
                            let loc = expr_loc(&expr);
                            expr = Expr::EMethodCall {
                                loc,
                                expr: Box::new(expr),
                                method,
                                type_args: self.pending_type_args.drain(..).collect(),
                                args,
                            };
                        }
                        // Regular field access: expr.field (also handles module paths like a.b(c))
                        _ => {
                            self.pending_type_args.clear();
                            let loc = expr_loc(&expr);
                            expr = Expr::EFieldAccess {
                                loc,
                                expr: Box::new(expr),
                                field: method,
                            };
                        }
                    }
                }
                // Cast: expr as Type
                Some(&Token::As) => {
                    self.advance()?;
                    let typ = self.parse_typ()?;
                    let loc = expr_loc(&expr);
                    expr = Expr::ECast {
                        loc,
                        expr: Box::new(expr),
                        to: typ,
                    };
                }
                _ => break,
            }
        }
        self.pending_type_args = saved;
        Ok(Self::method_call_or_pattern(expr))
    }

    /// Convert `Enum.Variant(x, y)` parsed as a method call (`EMethodCall` whose
    /// receiver is an uppercase enum name and whose arguments are all bare
    /// identifiers) into an enum destructuring pattern (`EEnumPattern`). Other
    /// method calls are left unchanged.
    ///
    /// If explicit type arguments are present, the construct is an enum
    /// constructor call, not a pattern, so it is kept as `EMethodCall`.
    fn method_call_or_pattern(expr: Expr) -> Expr {
        if let Expr::EMethodCall {
            loc,
            expr: receiver,
            method,
            type_args,
            args,
        } = expr
        {
            if type_args.is_empty() {
                if let Expr::EVar { name: enum_name, .. } = receiver.as_ref() {
                    if enum_name.chars().next().map_or(false, |c| c.is_uppercase()) {
                        let bindings: Option<Vec<String>> = args
                            .iter()
                            .map(|a| match a {
                                Expr::EVar { name, .. } => Some(name.clone()),
                                _ => None,
                            })
                            .collect();
                        if let Some(bindings) = bindings {
                            return Expr::EEnumPattern {
                                loc,
                                enum_name: enum_name.clone(),
                                variant: method,
                                bindings,
                            };
                        }
                    }
                }
            }
            return Expr::EMethodCall {
                loc,
                expr: receiver,
                method,
                type_args,
                args,
            };
        }
        expr
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt, String> {
        let start = self.advance()?.0; // "if"
        self.expect(&Token::LParen)?;
        let cond = self.parse_expr()?;
        self.expect(&Token::RParen)?;
        self.expect(&Token::LBrace)?;
        let stmts = self.parse_stmt_list()?;
        let res = self.parse_opt_expr()?;
        self.expect(&Token::RBrace)?;
        let elifs = self.parse_elif_chain()?;
        let else_ = self.parse_else_opt()?;
        self.expect(&Token::Semi)?;
        let loc = self.loc(start);
        Ok(Stmt::SExpr {
            loc: loc.clone(),
            expr: Box::new(util::build_if_chain(
                loc.clone(),
                cond,
                Expr::EBlock {
                    loc: loc.clone(),
                    stmts,
                    result: res,
                },
                elifs,
                else_,
            )),
        })
    }

    fn parse_elif_chain(&mut self) -> Result<Vec<(Expr, Expr)>, String> {
        let mut elifs = Vec::new();
        while self.peek_token()? == Some(&Token::Elif) {
            let start_e = self.advance()?.0; // "elif"
            self.expect(&Token::LParen)?;
            let cond = self.parse_expr()?;
            self.expect(&Token::RParen)?;
            self.expect(&Token::LBrace)?;
            let stmts = self.parse_stmt_list()?;
            let res = self.parse_opt_expr()?;
            self.expect(&Token::RBrace)?;
            elifs.push((
                cond,
                Expr::EBlock {
                    loc: self.loc(start_e),
                    stmts,
                    result: res,
                },
            ));
        }
        Ok(elifs)
    }

    fn parse_else_opt(&mut self) -> Result<Option<Expr>, String> {
        if self.peek_token()? == Some(&Token::Else) {
            let start_e = self.advance()?.0;
            self.expect(&Token::LBrace)?;
            let stmts = self.parse_stmt_list()?;
            let res = self.parse_opt_expr()?;
            self.expect(&Token::RBrace)?;
            Ok(Some(Expr::EBlock {
                loc: self.loc(start_e),
                stmts,
                result: res,
            }))
        } else {
            Ok(None)
        }
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt, String> {
        let start = self.advance()?.0; // "while"
        self.expect(&Token::LParen)?;
        let cond = self.parse_expr()?;
        self.expect(&Token::RParen)?;
        self.expect(&Token::LBrace)?;
        let stmts = self.parse_stmt_list()?;
        let res = self.parse_opt_expr()?;
        self.expect(&Token::RBrace)?;
        self.expect(&Token::Semi)?;
        let loc = self.loc(start);
        Ok(Stmt::SExpr {
            loc: loc.clone(),
            expr: Box::new(Expr::EWhile {
                loc: loc.clone(),
                cond: Box::new(cond),
                body: Box::new(Expr::EBlock {
                    loc: loc.clone(),
                    stmts,
                    result: res,
                }),
            }),
        })
    }

    fn parse_loop_stmt(&mut self) -> Result<Stmt, String> {
        let start = self.advance()?.0; // "loop"
        self.expect(&Token::LBrace)?;
        let stmts = self.parse_stmt_list()?;
        let res = self.parse_opt_expr()?;
        self.expect(&Token::RBrace)?;
        self.expect(&Token::Semi)?;
        let loc = self.loc(start);
        Ok(Stmt::SExpr {
            loc: loc.clone(),
            expr: Box::new(Expr::ELoop {
                loc: loc.clone(),
                body: Box::new(Expr::EBlock {
                    loc: loc.clone(),
                    stmts,
                    result: res,
                }),
            }),
        })
    }

    fn parse_for_stmt(&mut self) -> Result<Stmt, String> {
        let start = self.advance()?.0; // "for"
        let (var, _) = self.expect_ident()?;
        self.expect_keyword(&Token::In)?;
        self.expect(&Token::LParen)?;
        let range = self.parse_expr()?;
        self.expect(&Token::RParen)?;
        self.expect(&Token::LBrace)?;
        let stmts = self.parse_stmt_list()?;
        let res = self.parse_opt_expr()?;
        self.expect(&Token::RBrace)?;
        self.expect(&Token::Semi)?;
        let loc = self.loc(start);
        Ok(Stmt::SExpr {
            loc: loc.clone(),
            expr: Box::new(Expr::EFor {
                loc: loc.clone(),
                var,
                range: Box::new(range),
                body: Box::new(Expr::EBlock {
                    loc: loc.clone(),
                    stmts,
                    result: res,
                }),
            }),
        })
    }

    fn parse_stmt_list(&mut self) -> Result<Vec<Stmt>, String> {
        let mut stmts = Vec::new();
        loop {
            match self.peek_token()? {
                Some(&Token::RBrace) | None => break,
                _ => stmts.push(self.parse_stmt()?),
            }
        }
        Ok(stmts)
    }

    fn parse_opt_expr(&mut self) -> Result<Option<Box<Expr>>, String> {
        match self.peek_token()? {
            Some(&Token::RBrace) | Some(&Token::RBracket) | None => Ok(None),
            _ => {
                let e = self.parse_expr()?;
                Ok(Some(Box::new(e)))
            }
        }
    }

    // ── Expressions ──────────────────────────────────────────────────

    fn parse_expr(&mut self) -> Result<Expr, String> {
        self.parse_binary_expr(0)
    }

    // Operator precedence levels (higher = tighter binding)
    //   1: ||          (lowest)
    //   2: &&
    //   3: == != < > <= >=
    //   4: + -
    //   5: *            (highest)
    fn prece(&self, op: &Token<'input>) -> i32 {
        match op {
            Token::OrOr => 1,
            Token::AndAnd => 2,
            Token::EqEq | Token::Neq | Token::Lt | Token::Gt | Token::LtEq | Token::GtEq => 3,
            Token::Plus | Token::Minus => 4,
            Token::Star | Token::Slash => 5,
            _ => 0,
        }
    }

    fn parse_binary_expr(&mut self, min_prec: i32) -> Result<Expr, String> {
        let mut left = self.parse_unary_or_primary()?;

        loop {
            let op = match self.peek_token()? {
                Some(&Token::OrOr)
                | Some(&Token::AndAnd)
                | Some(&Token::EqEq)
                | Some(&Token::Neq)
                | Some(&Token::Lt)
                | Some(&Token::Gt)
                | Some(&Token::LtEq)
                | Some(&Token::GtEq)
                | Some(&Token::Plus)
                | Some(&Token::Minus)
                | Some(&Token::Star)
                | Some(&Token::Slash) => {
                    let tok = self.peek_token()?.unwrap().clone();
                    tok
                }
                _ => break,
            };

            let prec = self.prece(&op);
            if prec < min_prec {
                break;
            }

            self.advance()?; // consume operator

            // All operators are left-associative: recurse with prec + 1 so a
            // same-precedence operator to the right doesn't bind into us.
            let right = self.parse_binary_expr(prec + 1)?;

            let binop = match &op {
                Token::Plus => BinOp::Add,
                Token::Minus => BinOp::Sub,
                Token::Star => BinOp::Mul,
                Token::Slash => BinOp::Div,
                Token::EqEq => BinOp::Eq,
                Token::Neq => BinOp::Neq,
                Token::Lt => BinOp::Lt,
                Token::Gt => BinOp::Gt,
                Token::LtEq => BinOp::Le,
                Token::GtEq => BinOp::Ge,
                Token::AndAnd => BinOp::And,
                Token::OrOr => BinOp::Or,
                _ => unreachable!(),
            };

            let loc = expr_loc(&left);
            left = Expr::EBinOp {
                loc,
                op: binop,
                left: Box::new(left),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_unary_or_primary(&mut self) -> Result<Expr, String> {
        // Check for unary addr/deref
        match self.peek_token()? {
            Some(&Token::Addr) => {
                let start = self.advance()?.0;
                let e = self.parse_unary_or_primary()?;
                return Ok(Expr::EAddr {
                    loc: self.loc(start),
                    expr: Box::new(e),
                });
            }
            Some(&Token::Deref) => {
                let start = self.advance()?.0;
                let e = self.parse_unary_or_primary()?;
                return Ok(Expr::EDeref {
                    loc: self.loc(start),
                    expr: Box::new(e),
                });
            }
            _ => {}
        }

        self.parse_call_or_primary()
    }

    fn parse_call_or_primary(&mut self) -> Result<Expr, String> {
        let expr = self.parse_primary()?;
        self.parse_call_suffix(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, String> {
        match self.peek_token()? {
            Some(&Token::IntLit(_)) => {
                let (start, tok, _) = self.advance()?;
                if let Token::IntLit(s) = tok {
                    let v = s
                        .parse::<i64>()
                        .map_err(|e| format!("Invalid int: {}", e))?;
                    Ok(Expr::EInt {
                        loc: self.loc(start),
                        value: v,
                    })
                } else {
                    unreachable!()
                }
            }
            Some(&Token::FloatLit(_)) => {
                let (start, tok, _) = self.advance()?;
                if let Token::FloatLit(s) = tok {
                    let v = s
                        .parse::<f64>()
                        .map_err(|e| format!("Invalid float: {}", e))?;
                    Ok(Expr::EFloat {
                        loc: self.loc(start),
                        value: v,
                    })
                } else {
                    unreachable!()
                }
            }
            Some(&Token::CharLit(_)) => {
                let (start, tok, _) = self.advance()?;
                if let Token::CharLit(s) = tok {
                    Ok(Expr::EChar {
                        loc: self.loc(start),
                        value: util::process_escapes(s),
                    })
                } else {
                    unreachable!()
                }
            }
            Some(&Token::StringLit(_)) => {
                let (start, tok, _) = self.advance()?;
                if let Token::StringLit(s) = tok {
                    let processed = util::process_escapes(s);
                    Ok(Expr::EString {
                        loc: self.loc(start),
                        value: processed,
                    })
                } else {
                    unreachable!()
                }
            }
            Some(&Token::BoolLit(v)) => {
                let (start, tok, _) = self.advance()?;
                if let Token::BoolLit(v) = tok {
                    Ok(Expr::EBool {
                        loc: self.loc(start),
                        value: v,
                    })
                } else {
                    unreachable!()
                }
            }
            Some(&Token::Ident(_)) => {
                let (start, tok, _) = self.advance()?;
                let name = match tok {
                    Token::Ident(s) => s.to_string(),
                    _ => unreachable!(),
                };
                // Check for move/clone prefix (these are separate keywords in Miva)
                Ok(Expr::EVar {
                    loc: self.loc(start),
                    name,
                })
            }
            Some(&Token::Dollar) => {
                let start = self.advance()?.0; // consume "$"
                let (name, _) = self.expect_ident()?;
                Ok(Expr::EMacroVar {
                    loc: self.loc(start),
                    name,
                })
            }
            Some(&Token::Move) => {
                let start = self.advance()?.0;
                let (name, _) = self.expect_ident()?;
                Ok(Expr::EMove {
                    loc: self.loc(start),
                    name,
                })
            }
            Some(&Token::Clone) => {
                let start = self.advance()?.0;
                let (name, _) = self.expect_ident()?;
                Ok(Expr::EClone {
                    loc: self.loc(start),
                    name,
                })
            }
            Some(&Token::LParen) => {
                self.advance()?;
                let expr = self.parse_expr()?;
                self.expect(&Token::RParen)?;
                Ok(expr)
            }
            Some(&Token::LBrace) => {
                let start = self.advance()?.0; // "{"
                let stmts = self.parse_stmt_list()?;
                let res = self.parse_opt_expr()?;
                self.expect(&Token::RBrace)?;
                Ok(Expr::EBlock {
                    loc: self.loc(start),
                    stmts,
                    result: res,
                })
            }
            Some(&Token::LBracket) => {
                let start = self.advance()?.0;
                let mut values = Vec::new();
                if self.peek_token()? != Some(&Token::RBracket) {
                    loop {
                        values.push(self.parse_expr()?);
                        if self.peek_token()? == Some(&Token::Comma) {
                            self.advance()?;
                        } else {
                            break;
                        }
                    }
                }
                self.expect(&Token::RBracket)?;
                Ok(Expr::EArrayLit {
                    loc: self.loc(start),
                    values,
                })
            }
            Some(&Token::Struct) => self.parse_struct_init_expr(),
            Some(&Token::Choose) => self.parse_choose_expr(),
            Some(tok) => Err(format!("Unexpected token in expression: {:?}", tok)),
            None => Err("Unexpected EOF in expression".to_string()),
        }
    }

    fn parse_struct_init_expr(&mut self) -> Result<Expr, String> {
        let start = self.advance()?.0; // "struct"
        let path = self.parse_type_path()?;
        // Optional generic type args: Pair[T, U]
        let _type_args = if self.peek_token()? == Some(&Token::LBracket) {
            self.advance()?; // "["
            let mut args = Vec::new();
            loop {
                args.push(self.parse_typ()?);
                if self.peek_token()? == Some(&Token::Comma) {
                    self.advance()?;
                } else {
                    break;
                }
            }
            self.expect(&Token::RBracket)?;
            args
        } else {
            vec![]
        };
        self.expect(&Token::LBrace)?;
        let mut fields = Vec::new();
        while self.peek_token()? != Some(&Token::RBrace) {
            let (name, _) = self.expect_ident()?;
            self.expect(&Token::Eq)?;
            let value = self.parse_expr()?;
            fields.push(ValueField {
                name: name.to_string(),
                value,
            });
            if self.peek_token()? == Some(&Token::Comma) {
                self.advance()?;
            }
        }
        self.expect(&Token::RBrace)?;
        Ok(Expr::EStructLit {
            loc: self.loc(start),
            name: path,
            fields,
            type_args: _type_args,
        })
    }

    fn parse_choose_expr(&mut self) -> Result<Expr, String> {
        let start = self.advance()?.0; // "choose"
        self.expect(&Token::LParen)?;
        let var = self.parse_expr()?;
        self.expect(&Token::RParen)?;
        self.expect(&Token::LBrace)?;
        let mut cases = Vec::new();
        while self.peek_token()? == Some(&Token::When) {
            let case = self.parse_when_case()?;
            cases.push(case);
        }
        let otherwise = self.parse_otherwise_opt()?;
        self.expect(&Token::RBrace)?;
        Ok(Expr::EChoose {
            loc: self.loc(start),
            var: Box::new(var),
            cases,
            otherwise,
        })
    }

    fn parse_when_case(&mut self) -> Result<WhenCase, String> {
        let s = self.advance()?.0; // "when"
        self.expect(&Token::LParen)?;
        let value = self.parse_expr()?;
        self.expect(&Token::RParen)?;
        let guard = if self.peek_token()? == Some(&Token::If) {
            self.advance()?; // "if"
            self.expect(&Token::LParen)?;
            let cond = self.parse_expr()?;
            self.expect(&Token::RParen)?;
            Some(Box::new(cond))
        } else {
            None
        };
        self.expect(&Token::LBrace)?;
        let stmts = self.parse_stmt_list()?;
        let res = self.parse_opt_expr()?;
        self.expect(&Token::RBrace)?;
        Ok(WhenCase {
            when: Box::new(value),
            guard,
            then: Box::new(Expr::EBlock {
                loc: self.loc(s),
                stmts,
                result: res,
            }),
        })
    }

    fn parse_otherwise_opt(&mut self) -> Result<Option<Box<Expr>>, String> {
        if self.peek_token()? == Some(&Token::Otherwise) {
            let s = self.advance()?.0;
            self.expect(&Token::LBrace)?;
            let stmts = self.parse_stmt_list()?;
            let res = self.parse_opt_expr()?;
            self.expect(&Token::RBrace)?;
            Ok(Some(Box::new(Expr::EBlock {
                loc: self.loc(s),
                stmts,
                result: res,
            })))
        } else {
            Ok(None)
        }
    }
}

// ── Helper function ───────────────────────────────────────────────────────

fn extract_call_path_from_expr(expr: &Expr) -> String {
    match expr {
        Expr::EFieldAccess { expr: e, field, .. } => {
            let p = extract_call_path_from_expr(e);
            if p.is_empty() {
                field.clone()
            } else {
                format!("{}.{}", p, field)
            }
        }
        Expr::EVar { name, .. } => name.clone(),
        _ => String::new(),
    }
}

/// If `name` is a dotted `Enum.Variant`, has no explicit type arguments, and
/// every argument is a bare identifier, the call is treated as an enum
/// destructuring pattern (used in `when` arms of a `choose`), so it is emitted
/// as `EEnumPattern`. Otherwise it stays a normal call.
fn enum_pattern_or_call(
    loc: Loc,
    name: String,
    type_args: Vec<Typ>,
    args: Vec<Expr>,
) -> Expr {
    // Enum patterns in `when` arms never carry explicit type arguments; if they
    // are present, treat the construct as a normal enum-constructor call.
    //
    // A pattern is exactly `Enum.Variant` (two dotted segments). A path with
    // more segments (e.g. a cross-module call `std.json.parse`, rewritten by
    // `process_call_path` to `mvp_std.json.parse`) is a function call, not a
    // destructuring pattern, so it must keep the full module-qualified name.
    if type_args.is_empty() {
        let parts: Vec<&str> = name.split('.').collect();
        if parts.len() == 2 {
            if let Some((enum_name, variant)) = name.split_once('.') {
                let bindings: Option<Vec<String>> = args
                    .iter()
                    .map(|a| match a {
                        Expr::EVar { name, .. } => Some(name.clone()),
                        _ => None,
                    })
                    .collect();
                if let Some(bindings) = bindings {
                    return Expr::EEnumPattern {
                        loc,
                        enum_name: enum_name.to_string(),
                        variant: variant.to_string(),
                        bindings,
                    };
                }
            }
        }
    }
    Expr::ECall {
        loc,
        name,
        type_args,
        args,
    }
}

fn expr_loc(e: &Expr) -> Loc {
    match e {
        Expr::EInt { loc, .. }
        | Expr::EBool { loc, .. }
        | Expr::EFloat { loc, .. }
        | Expr::EChar { loc, .. }
        | Expr::EString { loc, .. }
        | Expr::EVar { loc, .. }
        | Expr::EMove { loc, .. }
        | Expr::EClone { loc, .. }
        | Expr::EStructLit { loc, .. }
        | Expr::EFieldAccess { loc, .. }
        | Expr::EBinOp { loc, .. }
        | Expr::EIf { loc, .. }
        | Expr::EChoose { loc, .. }
        | Expr::ECall { loc, .. }
        | Expr::EMacro { loc, .. }
        | Expr::ECast { loc, .. }
        | Expr::EBlock { loc, .. }
        | Expr::EArrayLit { loc, .. }
        | Expr::EVoid { loc, .. }
        | Expr::EWhile { loc, .. }
        | Expr::ELoop { loc, .. }
        | Expr::EFor { loc, .. }
        | Expr::EAddr { loc, .. }
        | Expr::EDeref { loc, .. }
        | Expr::EMethodCall { loc, .. }
        | Expr::EMacroVar { loc, .. }
        | Expr::EEnumPattern { loc, .. }
        | Expr::ELambda { loc, .. } => loc.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    fn parse_first(input: &str) -> Def {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer, input, "test.mv");
        parser.parse_program().unwrap().remove(0)
    }

    #[test]
    fn test_parse_enum() {
        let def = parse_first("Color = enum { Red, Green(int), Blue(int, string) }");
        match def {
            Def::DEnum { name, variants, .. } => {
                assert_eq!(name, "Color");
                assert_eq!(variants.len(), 3);
                assert_eq!(variants[0].name, "Red");
                assert!(variants[0].payload.is_empty());
                assert_eq!(variants[1].name, "Green");
                assert_eq!(variants[1].payload.len(), 1);
                assert_eq!(variants[2].name, "Blue");
                assert_eq!(variants[2].payload.len(), 2);
            }
            _ => panic!("expected DEnum"),
        }
    }

    #[test]
    fn test_parse_generic_enum_constructor_type_args() {
        let def = parse_first("Box[T] = enum { Empty, Value(T) }");
        match def {
            Def::DEnum { name, type_params, variants, .. } => {
                assert_eq!(name, "Box");
                assert_eq!(type_params, vec!["T".to_string()]);
                assert_eq!(variants.len(), 2);
            }
            _ => panic!("expected DEnum"),
        }
        // Explicit type args: Box[int](5) produces ECall { name: "Box", type_args: [TInt], args: [EInt] }
        let lexer = Lexer::new("Box[int](5)");
        let mut parser = Parser::new(lexer, "Box[int](5)", "test.mv");
        let expr = parser.parse_expr().unwrap();
        match expr {
            Expr::ECall { name, type_args, .. } => {
                assert_eq!(name, "Box");
                assert_eq!(type_args.len(), 1);
            }
            other => panic!("expected ECall, got {:?}", other),
        }
    }

    #[test]
    fn test_generic_enum_constructor_type_args_parsing() {
        let lexer = Lexer::new("Box.Value[int](5)");
        let mut parser = Parser::new(lexer, "Box.Value[int](5)", "test.mv");
        let expr = parser.parse_expr().unwrap();
        match expr {
            Expr::EMethodCall { method, type_args, args, .. } => {
                assert_eq!(method, "Value");
                assert_eq!(type_args.len(), 1);
                assert_eq!(args.len(), 1);
            }
            other => panic!("expected EMethodCall, got {:?}", other),
        }

        let lexer2 = Lexer::new("Box[int].Value(5)");
        let mut parser2 = Parser::new(lexer2, "Box[int].Value(5)", "test.mv");
        let expr2 = parser2.parse_expr().unwrap();
        match expr2 {
            Expr::EMethodCall { method, type_args, args, .. } => {
                assert_eq!(method, "Value");
                assert_eq!(type_args.len(), 1);
                assert_eq!(args.len(), 1);
            }
            other => panic!("expected EMethodCall, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_choose_enum_destructure_pattern() {
        let def = parse_first(
            "f = (s: Shape): int => choose (s) {\n  when (Shape.Circle(r)) { return r; }\n  when (Shape.Rect(w, h)) { return w + h; }\n  otherwise { return 0; }\n}",
        );
        match def {
            Def::DFunc { body, .. } => match body.as_ref() {
                Expr::EChoose { cases, .. } => {
                    assert_eq!(cases.len(), 2);
                    match cases[0].when.as_ref() {
                        Expr::EEnumPattern {
                            enum_name,
                            variant,
                            bindings,
                            ..
                        } => {
                            assert_eq!(enum_name, "Shape");
                            assert_eq!(variant, "Circle");
                            assert_eq!(bindings, &vec!["r".to_string()]);
                        }
                        other => panic!("expected EEnumPattern, got {:?}", other),
                    }
                    match cases[1].when.as_ref() {
                        Expr::EEnumPattern {
                            enum_name,
                            variant,
                            bindings,
                            ..
                        } => {
                            assert_eq!(enum_name, "Shape");
                            assert_eq!(variant, "Rect");
                            assert_eq!(bindings, &vec!["w".to_string(), "h".to_string()]);
                        }
                        other => panic!("expected EEnumPattern, got {:?}", other),
                    }
                }
                other => panic!("expected EChoose, got {:?}", other),
            },
            _ => panic!("expected DFunc"),
        }
    }

    #[test]
    fn test_parse_choose_with_guard() {
        let def = parse_first(
            "f = (x: int): int => choose (x) {\n  when (1) if (x > 0) { return x; }\n  otherwise { return 0; }\n}",
        );
        match def {
            Def::DFunc { body, .. } => match body.as_ref() {
                Expr::EChoose { cases, .. } => {
                    assert_eq!(cases.len(), 1);
                    assert!(cases[0].guard.is_some());
                    match cases[0].guard.as_ref().unwrap().as_ref() {
                        Expr::EBinOp { op, .. } => assert_eq!(*op, BinOp::Gt),
                        other => panic!("expected EBinOp, got {:?}", other),
                    }
                }
                other => panic!("expected EChoose, got {:?}", other),
            },
            _ => panic!("expected DFunc"),
        }
    }
}

// Ensure Lexer is Clone for backtracking
impl<'input> Clone for Parser<'input> {
    fn clone(&self) -> Self {
        Parser {
            lexer: self.lexer.clone(),
            input: self.input,
            file_name: self.file_name,
            peeked: None, // discard peeked state on clone
            pending_type_args: vec![],
        }
    }
}
