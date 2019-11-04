// parser.mly

%{
  open Absyn
%}

%token <bool>          LOGIC
%token <int>           INTEGER
%token <string>        STRING
%token <float>         REAL
%token <Symbol.symbol> ID
%token                 IF THEN ELSE
%token                 WHILE DO BREAK
%token                 LET IN END
%token                 VAR FUNCTION TYPE
%token                 LPAREN "(" RPAREN ")"
%token                 COLON ":" COMMA "," SEMI ";"
%token                 PLUS "+" MINUS "-" TIMES "*" DIV "/" MOD "%" POW "^"
%token                 EQ "=" NE "<>"
%token                 LT "<" LE "<=" GT ">" GE ">="
%token                 AND "&" OR "|"
%token                 ASSIGN ":="
%token                 EOF

%start <Absyn.lexp> program

%%

program:
 | x=exp EOF {x}

exp:
 | x=LOGIC              {$loc, BoolExp x}
 | x=INTEGER            {$loc, IntExp x}
 | WHILE t=exp DO b=exp {$loc, WhileExp (t, b)}
 | BREAK                {$loc, BreakExp}
 | LET d=decs IN e=exp  {$loc, LetExp (d, e)}
 | v=var                {$loc, VarExp v}
 | MINUS e=exp {$loc, OpExp (MinusOp, ($loc, IntExp(0)), e)}
 | e=exp PLUS f=exp {$loc, OpExp(PlusOp, e, f)}
 | e=exp MINUS f=exp {$loc, OpExp(MinusOp, e, f)}
 | e=exp TIMES f=exp {$loc, OpExp(TimesOp, e, f)}
 | e=exp DIV f=exp {$loc, OpExp(DivOp, e, f)}
 | e=exp MOD f=exp {$loc, OpExp(ModOp, e, f)}
 | e=exp POW f=exp {$loc, OpExp(PowOp, e, f)}
 | e=exp EQ f=exp {$loc, OpExp(EqOp, e, f)}
 | e=exp NE f=exp {$loc, OpExp(NeOp, e, f)}
 | e=exp LT f=exp {$loc, OpExp(LtOp, e, f)}
 | e=exp LE f=exp {$loc, OpExp(LeOp, e, f)}
 | e=exp GT f=exp {$loc, OpExp(GtOp, e, f)}
 | e=exp GE f=exp {$loc, OpExp(GeOp, e, f)}
 | e=exp AND f=exp {$loc, OpExp(AndOp, e, f)}
 | e=exp OR f=exp {$loc, OpExp(OrOp, e, f)}
 | v=var ASSIGN e=exp {$loc, AssignExp(v, e)}
 | IF x = exp THEN y = exp ELSE z = exp {$loc, IfExp(x, y, Some(z))}
 | IF x = exp THEN y = exp {$loc, IfExp(x, y, None)}
 | e = INTEGER {$loc, IntExp e}
 | e = REAL {$loc, RealExp e}
 | e = LOGIC {$loc, BoolExp e}
 | e = STRING {$loc, StringExp e}
 | func = ID "(" args = separated_list(",", exp) ")"  {$loc, CallExp(func, args)}
 | "(" exps = separated_list(";", exp) ")" {$loc, SeqExp exps}

decs:
 | l=list(dec) {l}

dec:
 | v=vardec {v}
 | typ = nonempty_list(typedec) {$loc, MutualTypeDecs typ}
 | funct = nonempty_list(funcdec) {$loc, MutualFunctionDecs funct}

vardec:
 | VAR v=ID ":" t=ID ":=" e=exp {$loc, VarDec (v, Some ($loc(t), t), e)}
 | VAR v=ID ":=" e=exp {$loc, VarDec (v, None, e)}

var:
 | x=ID {$loc, SimpleVar x}

typedec:
 | TYPE name = ID ":=" t = typeCons {$loc, (name, t)}

typeCons:
 | x = ID {$loc, NameCons(x)}

funcdec:
 | FUNCTION f = ID "(" ps = separated_list(",", parameter) ")" "=" e = exp {$loc, (f, ps, None, e)}
 | FUNCTION f = ID "(" ps = separated_list(",", parameter) ")" ":" v = ID "=" e = exp {$loc, (f, ps, Some ($loc(v), v), e)}

parameter:  
 | x = ID ":" y = ID {$loc, (x, y)}

                      (*

let var idade : int := 2 + 3
    var peso := idade * 5 + 1
in
    printint(idade - peso)



                       *)
