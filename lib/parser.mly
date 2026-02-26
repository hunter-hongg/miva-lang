%{
  open Ast
%}

%token <int64> INT_LIT
%token <float> FLOAT_LIT
%token <char> CHAR_LIT
%token <string> STRING_LIT
%token <bool> BOOL_LIT
%token <string> IDENT

%token EQ COLONEQ DARROW LPAREN RPAREN LBRACE RBRACE COMMA DOT  LBRACKET RBRACKET
%token PLUS MINUS STAR EQEQ NEQ AS
%token STRUCT REF MOVE CLONE PRINT IF ELSE MUT RETURN TEST
%token INT BOOL FLOAT32 FLOAT64 CHAR STRING
%token EOF
%token OWN COLON
%token CHOOSE WHEN OTHERWISE MODULE EXPORT IMPORT
%token UNSAFE TRUSTED C_KEYWORD VOID

%left PLUS MINUS          /* 最低优先级，左结合 */
%left STAR                /* 中等优先级，左结合 */
%left EQEQ NEQ            /* 较高优先级，左结合 */

%start program
%type <Ast.def list> program

%%

expr_opt:
    /* 空 */ { None }
  | expr { Some $1 }

program:
  | defs = list(def) EOF { defs }

def:
  | name = IDENT EQ STRUCT LBRACE fields = separated_list(COMMA, field_decl) RBRACE
    { DStruct ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, name, fields) }
  | name = IDENT EQ params = func_params COLON ret_typ = typ DARROW body = expr
    { DFunc ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, name, params, Some ret_typ, body) }
  | name = IDENT EQ params = func_params DARROW body = expr
    { DFunc ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, name, params, None, body) }
  | UNSAFE name = IDENT EQ params = func_params COLON ret_typ = typ DARROW body = expr
    { DFuncUnsafe ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, name, params, Some ret_typ, body) }
  | UNSAFE name = IDENT EQ params = func_params DARROW body = expr
    { DFuncUnsafe ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, name, params, None, body) }
  | TRUSTED name = IDENT EQ params = func_params COLON ret_typ = typ DARROW body = expr
    { DFuncTrusted ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, name, params, Some ret_typ, body) }
  | TRUSTED name = IDENT EQ params = func_params DARROW body = expr
    { DFuncTrusted ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, name, params, None, body) }
  | C_KEYWORD UNSAFE name = IDENT EQ params = func_params COLON ret_typ = typ DARROW c_code = STRING_LIT
    { DCFuncUnsafe ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, name, params, Some ret_typ, c_code) }
  | C_KEYWORD UNSAFE name = IDENT EQ params = func_params DARROW c_code = STRING_LIT
    { DCFuncUnsafe ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, name, params, None, c_code) }
  | TEST name = IDENT EQ LPAREN RPAREN COLON INT DARROW body = expr
    { DTest ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, name, body) }
  | MODULE name = STRING_LIT { DModule ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, name) }
  | EXPORT symbol = IDENT { SExport ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, symbol) }
  | IMPORT symbol = STRING_LIT { SImport ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, symbol) }


func_params:
  | LPAREN RPAREN { [] }
  | LPAREN ps = separated_nonempty_list(COMMA, param) RPAREN { ps }

param:
  | REF name = IDENT COLON t = typ { PRef (name, t) }
  | OWN name = IDENT COLON t = typ { POwn (name, t) }
  | name = IDENT COLON t = typ { POwn (name, t) }  (* 默认 owned *)

field_decl:
  | name = IDENT COLON t = typ { (name, t) }

expr:
  | e = call_expr { e }
  | e1 = expr PLUS e2 = expr { EBinOp ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, Add, e1, e2) }
  | e1 = expr MINUS e2 = expr { EBinOp ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, Sub, e1, e2) }
  | e1 = expr STAR e2 = expr { EBinOp ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, Mul, e1, e2) }
  | e1 = expr EQEQ e2 = expr { EBinOp ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, Eq, e1, e2) }
  | e1 = expr NEQ e2 = expr { EBinOp ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, Neq, e1, e2) }
  | LBRACE stmts = stmt_list expr_opt = expr_opt RBRACE
    { EBlock ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, stmts, expr_opt) }
  | struct_init_expr { $1 }
  | CHOOSE LPAREN var = expr RPAREN LBRACE cases = list(when_case) otherwise = otherwise_opt RBRACE
    { EChoose ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, var, cases, otherwise) }

atomic_expr:
  | i = INT_LIT { EInt ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, i) }
  | f = FLOAT_LIT { EFloat ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, f) }
  | c = CHAR_LIT { EChar ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, c) }
  | s = STRING_LIT { EString ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, s) }
  | b = BOOL_LIT { EBool ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, b) }
  | x = IDENT { EVar ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, x) }
  | MOVE x = IDENT { EMove ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, x) }
  | CLONE x = IDENT { EClone ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, x) }
  | PRINT LPAREN s = STRING_LIT RPAREN { ECall ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, "print", [EString ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, s)]) }
  | LPAREN e = expr RPAREN { e }
  | e = struct_init_expr { e }
  | e = atomic_expr AS t = typ { ECast ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, e, t) }

field_access_expr:
  | e = atomic_expr { e }
  | e = field_access_expr DOT field = IDENT { EFieldAccess ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, e, field) }

call_expr:
  | e = field_access_expr { e }
  | e = field_access_expr LPAREN args = separated_list(COMMA, expr) RPAREN { 
      (* 处理 a.b.c.foo(...) 形式的调用 *)
      let rec extract_call_path expr = 
        match expr with
        | EFieldAccess (_, e, field) ->
            let path = extract_call_path e in
            path ^ "." ^ field
        | EVar (_, name) -> name
        | _ -> failwith "Invalid call path"
      in
      let call_path = extract_call_path e in
      (* 处理特殊前缀，如std -> mvp_std *)
      let processed_path = 
        match String.split_on_char '.' call_path with
        | "std" :: rest -> 
            "mvp_std" :: rest
        | other :: rest -> 
            other :: rest
        | [] -> []
      in
      let final_path = String.concat "." processed_path in
      ECall ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, final_path, args)
    }
  | name = IDENT LPAREN args = separated_list(COMMA, expr) RPAREN { ECall ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, name, args) }

stmt:
  | IDENT COLONEQ expr { SLet ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, false, $1, $3) }
  | MUT IDENT COLONEQ expr { SLet ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, true, $2, $4) }
  | IDENT EQ expr { SAssign ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, $1, $3) }  (* 赋值语句：x = 值 *)
  | RETURN expr { SReturn ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, $2) }
  | RETURN { let loc = { line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 } in SReturn (loc, EVoid loc) }
  | IF LPAREN cond = expr RPAREN LBRACE t = stmt_list expr_opt = expr_opt RBRACE { let loc = { line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 } in SExpr (loc, EIf (loc, cond, EBlock (loc, t, expr_opt), None)) }
  | IF LPAREN cond = expr RPAREN LBRACE t = stmt_list expr_opt = expr_opt RBRACE ELSE LBRACE e = stmt_list expr_opt_else = expr_opt RBRACE { let loc = { line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 } in SExpr (loc, EIf (loc, cond, EBlock (loc, t, expr_opt), Some (EBlock (loc, e, expr_opt_else)))) }
  | expr { SExpr ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, $1) }

struct_init_expr:
  | STRUCT name = type_path LBRACE inits = struct_inits RBRACE { EStructLit ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, name, inits) }

stmt_list:
  | stmts = list(stmt) { stmts }

struct_inits:
  | /* empty */ { [] }
  | inits = separated_nonempty_list(COMMA, struct_init) { inits }

struct_init:
  | name = IDENT EQ e = expr { (name, e) }

when_case:
  | WHEN LPAREN value = expr RPAREN LBRACE stmts = stmt_list expr_opt = expr_opt RBRACE { (value, EBlock ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, stmts, expr_opt)) }

otherwise_opt:
  | /* empty */ { None }
  | OTHERWISE LBRACE stmts = stmt_list expr_opt = expr_opt RBRACE { Some (EBlock ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, stmts, expr_opt)) }

binop:
  | PLUS { Add }
  | MINUS { Sub }
  | STAR { Mul }
  | EQEQ { Eq }
  | NEQ { Neq }

typ:
  | INT { TInt }
  | BOOL { TBool }
  | FLOAT32 { TFloat32 }
  | FLOAT64 { TFloat64 }
  | CHAR { TChar }
  | STRING { TString }
  | LBRACKET t = typ RBRACKET { TArray t }
  | t = type_path { TStruct (t, []) }

type_path:
  | name = IDENT { name }
  | name = type_path DOT field = IDENT { name ^ "::" ^ field }