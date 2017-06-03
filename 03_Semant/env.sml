structure Env =
struct
  structure Symbol = SplayMapFn(type ord_key = string; val compare = String.compare)

  datatype ty = TUPLE of ty list
          | NIL
          | INT
          | STRING
          | BOOL
          | NAME of string * ty option ref
          | BOTTOM

  type access = unit
  datatype enventry = VarEntry of {ty: ty}
                    | FunEntry of {inputs: ty list, result: ty}

  fun insert ((name, info), table) = Symbol.insert(table, name, info)
  val base_tenv = foldl insert Symbol.empty [("int",INT),("string",STRING),("bool",BOOL),("nil",NIL)]
  val base_venv = Symbol.empty

end
