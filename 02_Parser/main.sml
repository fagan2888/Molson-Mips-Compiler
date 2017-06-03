signature MAIN =
sig
  val parse : string -> Absyn.dec list
  val printabsyn : Absyn.dec list -> unit
  val compile : string -> unit
end

structure Main : MAIN =
struct
  structure A = Absyn
  structure LrVals = MolsonLrValsFun(structure Token = LrParser.Token)
  structure Lex = MolsonLexFun(structure Tokens = LrVals.Tokens)
  structure Parser = Join(structure ParserData = LrVals.ParserData structure Lex=Lex structure LrParser = LrParser)
  fun parse filename =
    let
      val _ = (Error.set filename)
	    val file = TextIO.openIn filename
	    fun get _ = TextIO.input file
	    fun parseerror(s,p1,p2) = Error.msg p1 s
	    val lexer = LrParser.Stream.streamify (Lex.makeLexer get)
	    val (absyn, _) = Parser.parse(30,lexer,parseerror,())
    in
      TextIO.closeIn file;
	    absyn
    end

  fun printabsyn absyn =
    let
      fun decs [] = ()
        | decs (d::l) = (dec(d); decs(l))

      and dec(A.FunctionDec{name, params, result, body, pos}) = print("FunctionDec(" ^ name ^ " : " ^ (field (params, "")) ^ " -> "^ result ^  ")\n[\n" ^ (exp body) ^ "]\n")
        | dec  (A.VarDec{name, init, pos}) = print("VarDec(" ^ name ^ " = " ^ exp init ^ ")\n")
        | dec (A.TypeDec{name, typ, pos}) = print("TypeDec(" ^ name ^ " : " ^ (ty typ) ^ ")\n")

      and field ([],str) = "()"
        | field([(s1,s2,pos)],str) = str ^ "(" ^ s1 ^ " : " ^ s2 ^ ")"
        | field((s1,s2,pos)::l,str) = field(l,str ^ "(" ^ s1 ^ " : " ^ s2 ^ ") * ")

      and ty (A.NameTy(s,p)) = s
        | ty (A.TupleTy([])) = "{}"
        | ty (A.TupleTy([f])) = ty (f)
        | ty (A.TupleTy(f::l)) = ty (f) ^ " * " ^ ty (A.TupleTy(l))

      and exp (A.VarExp(v)) = var v
        | exp (A.NilExp) = "nil"
        | exp (A.IntExp(i)) = Int.toString i
        | exp (A.StringExp(s)) = "\"" ^ s ^ "\""
        | exp (A.BoolExp(b)) = Bool.toString b
        | exp (A.CallExp{func,args,pos}) = func ^ "(" ^ arglist (args,"") ^ ")"
        | exp (A.OpExp{left=left,oper=opr,right=right,pos=pos}) = exp left ^ oper opr ^ exp right
        | exp (A.SeqExp([])) = ""
        | exp (A.TupleExp{fields, typ, pos}) = typ ^ "{" ^ arglist (fields,"") ^ "}"
        | exp (A.SeqExp(e::seq)) = exp e ^ "\n" ^ exp (A.SeqExp(seq))
        | exp (A.AssignExp{var=v,exp=e,pos=p}) = var v ^ "=" ^ exp e
        | exp (A.IfElseExp{cond,exp1,exp2,pos}) = "if (" ^ exp cond ^ ") {\n" ^ exp exp1 ^ "} else {\n" ^ exp exp2 ^ "}"
        | exp (A.WhileExp{cond,body,pos}) = "while (" ^ exp cond ^ ") {\n" ^ exp body ^ "}"
        | exp (A.BreakExp(pos)) = "break"

      and arglist ([],str) = str
        | arglist ([e],str) = str ^ exp e
        | arglist ((e::l),str) = arglist(l, str ^ exp e ^ ",")

      and var (A.SimpleVar(s,p)) = s
        | var (A.FieldVar(v,i,p)) = var v ^ "." ^ Int.toString i

      and oper opr = case opr of A.PlusOp => "+" | A.MinusOp => "-" | A.TimesOp => "*" | A.DivideOp => "/"
        | A.EqOp => "==" | A.NeqOp => "!=" | A.LtOp => "<" | A.GtOp => ">"
        | A.GeOp => ">=" | A.LeOp => "<=" | A.OrOp => "|" | A.AndOp => "&"

    in
      decs(absyn)
    end

  fun compile filename =
    let
      val absyn = parse filename
      val _ = printabsyn absyn
    in
      ()
    end
end
