structure Absyn =
struct

type pos = int   and   symbol = string

datatype var = SimpleVar of symbol * pos
            | FieldVar of var * int * pos

and exp = VarExp of var
        | NilExp
        | IntExp of int
        | StringExp of string
        | BoolExp of bool
        | CallExp of {func: symbol, args: exp list, pos: pos}
        | OpExp of {left: exp, oper: oper, right: exp, pos: pos}
        | TupleExp of {fields: exp list, typ: symbol, pos: pos}
        | SeqExp of exp list
        | AssignExp of {var: var, exp: exp, pos: pos}
        | IfElseExp of {cond: exp, exp1: exp, exp2: exp, pos: pos}
        | WhileExp of {cond: exp, body: exp, pos: pos}
	      | BreakExp of pos

and dec = FunctionDec of {name: symbol, params: field list, result: symbol, body: exp, pos: pos}
        | VarDec of {name: symbol, init: exp, pos: pos}
        | TypeDec of {name: symbol, typ: ty, pos: pos}

and ty = NameTy of symbol * pos
       | TupleTy of ty list

and oper = PlusOp | MinusOp | TimesOp | DivideOp | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp | OrOp | AndOp

withtype field = symbol * symbol * pos

end
