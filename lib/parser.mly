%{
  open Ast
%}

%token <int64> INT_LIT
%token <float> FLOAT_LIT
%token <char> CHAR_LIT
%token <string> STRING_LIT
%token <bool> BOOL_LIT
%token <string> IDENT
%token EQ COLONEQ DARROW LPAREN RPAREN LBRACE RBRACE COMMA DOT SEMI
%token PLUS MINUS STAR EQEQ NEQ AS
%token STRUCT REF MOVE CLONE PRINT IF ELSE MUT RETURN
%token INT BOOL FLOAT32 FLOAT64 CHAR STRING
%token EOF
%token OWN THEN COLON
%token CHOOSE WHEN OTHERWISE

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
    { DStruct (name, fields) }
  | name = IDENT EQ params = func_params COLON ret_typ = typ DARROW body = expr
    { DFunc (name, params, Some ret_typ, body) }
  | name = IDENT EQ params = func_params DARROW body = expr
    { DFunc (name, params, None, body) }

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
  | e = atomic_expr { e }
  | e1 = expr PLUS e2 = expr { EBinOp (Add, e1, e2) }
  | e1 = expr MINUS e2 = expr { EBinOp (Sub, e1, e2) }
  | e1 = expr STAR e2 = expr { EBinOp (Mul, e1, e2) }
  | e1 = expr EQEQ e2 = expr { EBinOp (Eq, e1, e2) }
  | e1 = expr NEQ e2 = expr { EBinOp (Neq, e1, e2) }
  | LBRACE stmts = stmt_list expr_opt = expr_opt RBRACE
    { EBlock (stmts, expr_opt) }
  | struct_init_expr { $1 }
  | CHOOSE LPAREN var = expr RPAREN LBRACE cases = list(when_case) otherwise = otherwise_opt RBRACE
    { EChoose (var, cases, otherwise) }

atomic_expr:
  | i = INT_LIT { EInt i }
  | f = FLOAT_LIT { EFloat f }
  | c = CHAR_LIT { EChar c }
  | s = STRING_LIT { EString s }
  | b = BOOL_LIT { EBool b }
  | x = IDENT { EVar x }
  | MOVE x = IDENT { EMove x }
  | CLONE x = IDENT { EClone x }
  | PRINT LPAREN s = STRING_LIT RPAREN { ECall ("print", [EString s]) }
  | name = IDENT LPAREN args = separated_list(COMMA, expr) RPAREN { ECall (name, args) }
  | LPAREN e = expr RPAREN { e }
  | e = struct_init_expr { e }
  | e = atomic_expr DOT field = IDENT { EFieldAccess (e, field) }
  | e = atomic_expr AS t = typ { ECast (e, t) }

stmt:
  | IDENT COLONEQ expr { SLet (false, $1, $3) }
  | MUT IDENT COLONEQ expr { SLet (true, $2, $4) }
  | IDENT EQ expr { SAssign ($1, $3) }  (* 赋值语句：x = 值 *)
  | RETURN expr { SReturn $2 }
  | IF LPAREN cond = expr RPAREN LBRACE t = stmt_list expr_opt = expr_opt RBRACE { SExpr (EIf (cond, EBlock (t, expr_opt), None)) }
  | IF LPAREN cond = expr RPAREN LBRACE t = stmt_list expr_opt = expr_opt RBRACE ELSE LBRACE e = stmt_list expr_opt_else = expr_opt RBRACE { SExpr (EIf (cond, EBlock (t, expr_opt), Some (EBlock (e, expr_opt_else)))) }
  | expr { SExpr $1 }

struct_init_expr:
  | name = IDENT LBRACE inits = struct_inits RBRACE { EStructLit (name, inits) }

stmt_list:
  | /* empty */ { [] }
  | stmts = list(stmt) { stmts }

struct_inits:
  | /* empty */ { [] }
  | inits = separated_nonempty_list(COMMA, struct_init) { inits }

struct_init:
  | name = IDENT EQ e = expr { (name, e) }

when_case:
  | WHEN LPAREN value = expr RPAREN LBRACE stmts = stmt_list expr_opt = expr_opt RBRACE { (value, EBlock (stmts, expr_opt)) }

otherwise_opt:
  | /* empty */ { None }
  | OTHERWISE LBRACE stmts = stmt_list expr_opt = expr_opt RBRACE { Some (EBlock (stmts, expr_opt)) }

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
  | name = IDENT { TStruct (name, []) }