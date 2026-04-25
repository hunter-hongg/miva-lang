%{
  open Ast
  open Util
%}

%token <int64> INT_LIT
%token <float> FLOAT_LIT
%token <char> CHAR_LIT
%token <string> STRING_LIT
%token <bool> BOOL_LIT
%token <string> IDENT INTRO MAGICAL
%token NEWLINE

%token EQ COLONEQ DARROW LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET 
%token COMMA DOT COLON SEMI
%token PLUS MINUS STAR EQEQ NEQ NOT LT GT 
%token MUT REF OWN MOVE CLONE 
%token IF ELIF ELSE RETURN WHILE LOOP FOR IN
%token CHOOSE WHEN OTHERWISE 
%token INT BOOL FLOAT32 FLOAT64 CHAR STRING AS
%token STRUCT 
%token TEST UNSAFE TRUSTED C_KEYWORD 
%token EOF 
%token PTR BOX ADDR DEREF PTRANY
%token MODULE EXPORT IMPORT

%type <Ast.def> def
%type <Ast.def list> list(def)
%type <Ast.stmt> stmt
%type <Ast.stmt list> stmt_list
%type <Ast.expr> expr
%type <Ast.expr option> expr_opt
%type <Ast.expr> atomic_expr
%type <Ast.expr> field_access_expr
%type <Ast.expr> call_expr
%type <Ast.expr> macro_expr
%type <Ast.expr> struct_init_expr
%type <Ast.typ> typ
%type <string> type_path 
%type <Ast.param> param
%type <Ast.param list> separated_nonempty_list(COMMA,param)
%type <(string * Ast.typ)> field_decl
%type <(string * Ast.typ) list> separated_nonempty_list(COMMA,field_decl)
%type <(string * Ast.expr)> struct_init
%type <(string * Ast.expr) list> struct_inits
%type <(string * Ast.expr) list> separated_nonempty_list(COMMA,struct_init)
%type <Ast.expr list> separated_nonempty_list(COMMA,expr)
%type <Ast.expr list> loption(separated_nonempty_list(COMMA,expr))  (* Menhir 自动生成 *)
%type <(Ast.expr * Ast.expr) list> elif_chain
%type <(Ast.expr * Ast.expr) list> nonempty_elif_chain
%type <Ast.expr option> else_opt
%type <Ast.expr option> otherwise_opt

%left PLUS MINUS         
%left STAR               
%left EQEQ NEQ           
%left DOT        
%right AS        
%nonassoc ADDR DEREF

%start program
%type <Ast.def list> program

%%

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

  | MODULE name = separated_nonempty_list(DOT, IDENT) SEMI
    { 
    DModule (
      { line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, 
      String.concat "." name) }
  | EXPORT symbol = IDENT SEMI { SExport ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, symbol) }
  | IMPORT symbol = STRING_LIT SEMI { SImport ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, symbol) }
  | IMPORT symbol = STRING_LIT AS alias = separated_nonempty_list(DOT, IDENT) SEMI {
        SImportAs (
          { line = $startpos.Lexing.pos_lnum; 
            col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, 
            symbol, 
            String.concat "." alias)}
  | IMPORT symbol = STRING_LIT AS DOT SEMI {
    SImportHere(
      { line = $startpos.Lexing.pos_lnum; 
        col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, 
        symbol)
  }
  | i = INTRO { DCIntro (
    { line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }
  , i) }
  | m = MAGICAL { DCMagical ( 
    { line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }
  , m) }

func_params:
  | LPAREN RPAREN { [] }
  | LPAREN ps = separated_nonempty_list(COMMA, param) RPAREN { ps }

param:
  | REF name = IDENT COLON t = typ { PRef (name, t) }
  | OWN name = IDENT COLON t = typ { POwn (name, t) }
  | name = IDENT COLON t = typ { POwn (name, t) }  (* 默认 owned *)

(* ------------------------------------------------------ *)

expr:
  | e = call_expr { e }
  | e = macro_expr { e }
  | e1 = expr PLUS e2 = expr { EBinOp ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, Add, e1, e2) }
  | e1 = expr MINUS e2 = expr { EBinOp ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, Sub, e1, e2) }
  | e1 = expr STAR e2 = expr { EBinOp ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, Mul, e1, e2) }
  | e1 = expr EQEQ e2 = expr { EBinOp ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, Eq, e1, e2) }
  | e1 = expr NEQ e2 = expr { EBinOp ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, Neq, e1, e2) }
  | LBRACE stmts = stmt_list expr_opt = expr_opt RBRACE
    { EBlock ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, stmts, expr_opt) }
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
  | LPAREN e = expr RPAREN { e }
  | ADDR e = field_access_expr { EAddr ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, e) }
  | DEREF e = field_access_expr { EDeref ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, e) }
  | e = struct_init_expr { e }
  | e = atomic_expr AS t = typ { ECast ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, e, t) }

field_access_expr:
  | e = atomic_expr { e }
  | e = field_access_expr DOT field = IDENT { 
    EFieldAccess (
      { line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, 
      e, field
    ) }

call_expr:
  | e = field_access_expr { e }
  | e = field_access_expr LPAREN args = separated_list(COMMA, expr) RPAREN { 
      let rec extract_call_path expr = 
        match expr with
        | EFieldAccess (_, e, field) ->
            let path = extract_call_path e in
            path ^ "." ^ field
        | EVar (_, name) -> name
        | _ -> failwith "Invalid call path"
      in
      let call_path = extract_call_path e in
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

macro_expr: 
  | id = IDENT NOT LPAREN args = separated_list(COMMA, expr) RPAREN { EMacro ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, id, args) }

struct_init_expr:
  | STRUCT name = type_path LBRACE inits = struct_inits RBRACE { EStructLit ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, name, inits) }

struct_inits:
  | /* empty */ { [] }
  | inits = separated_nonempty_list(COMMA, struct_init) { inits }

struct_init:
  | name = IDENT EQ e = expr { (name, e) }

(* ------------------------------------------------------ *)

stmt:
  | IDENT COLONEQ expr SEMI { SLet ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, false, $1, $3) }
  | MUT IDENT COLONEQ expr SEMI { SLet ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, true, $2, $4) }
  | IDENT EQ expr SEMI { SAssign ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, $1, $3) }  (* 赋值语句：x = 值 *)
  | RETURN expr SEMI { SReturn ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, $2) }
  | RETURN SEMI { let loc = { line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 } in SReturn (loc, EVoid loc) }
  | IF LPAREN cond = expr RPAREN LBRACE t = stmt_list expr_opt = expr_opt RBRACE elifs = elif_chain else_opt = else_opt SEMI { 
      let loc = { line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }
       in SExpr (loc, build_if_chain loc cond (EBlock (loc, t, expr_opt)) elifs else_opt) }
  | WHILE LPAREN cond = expr RPAREN LBRACE t = stmt_list expr_opt = expr_opt RBRACE SEMI {
    let loc = { line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }
    in SExpr (loc, EWhile (loc, cond, EBlock (loc, t, expr_opt)))
  }
  | LOOP LBRACE t = stmt_list expr_opt = expr_opt RBRACE SEMI {
    let loc = { line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }
    in SExpr (loc, ELoop (loc, EBlock (loc, t, expr_opt)))
  }
  | FOR i = IDENT IN LPAREN range = expr RPAREN LBRACE t = stmt_list expr_opt = expr_opt RBRACE SEMI {
    let loc = { line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }
    in SExpr (loc, EFor (loc, i, range, EBlock (loc, t, expr_opt)))
  }
  | INTRO { SCIntro ( 
    { line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }
  , $1) }
  | expr SEMI { SExpr ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, $1) }
  | SEMI { SEmpty ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }) }

stmt_list:
  | stmts = list(stmt) { stmts }

expr_opt:
    /* 空 */ { None }
  | expr { Some $1 }

(* ------------------------------------------------------ *)

when_case:
  | WHEN LPAREN value = expr RPAREN LBRACE stmts = stmt_list expr_opt = expr_opt RBRACE { (value, EBlock ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, stmts, expr_opt)) }

otherwise_opt:
  | /* empty */ { None }
  | OTHERWISE LBRACE stmts = stmt_list expr_opt = expr_opt RBRACE { Some (EBlock ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, stmts, expr_opt)) }

elif_chain:
  | /* empty */ { [] }
  | elifs = nonempty_elif_chain { elifs }

nonempty_elif_chain:
  | ELIF LPAREN cond = expr RPAREN LBRACE t = stmt_list expr_opt = expr_opt RBRACE { [(cond, EBlock ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, t, expr_opt))] }
  | ELIF LPAREN cond = expr RPAREN LBRACE t = stmt_list expr_opt = expr_opt RBRACE rest = nonempty_elif_chain { (cond, EBlock ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, t, expr_opt)) :: rest }

else_opt:
  | /* empty */ { None }
  | ELSE LBRACE e = stmt_list expr_opt_else = expr_opt RBRACE { Some (EBlock ({ line = $startpos.Lexing.pos_lnum; col = $startpos.Lexing.pos_cnum - $startpos.Lexing.pos_bol + 1 }, e, expr_opt_else)) }

type_path:
  | name = IDENT { name }
  | name = type_path DOT field = IDENT { name ^ "::" ^ field }

field_decl:
  | name = IDENT COLON t = typ { (name, t) }

(* ------------------------------------------------------ *)

typ:
  | INT { TInt }
  | BOOL { TBool }
  | FLOAT32 { TFloat32 }
  | FLOAT64 { TFloat64 }
  | CHAR { TChar }
  | STRING { TString }
  | LBRACKET t = typ RBRACKET { TArray t }
  | PTR LT t = typ GT { TPtr t }
  | BOX LT t = typ GT { TBox t }
  | PTRANY { TPtrAny }
  | t = type_path { TStruct (t, []) }