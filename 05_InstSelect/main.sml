structure Main =
struct
  structure A = Absyn
  structure T = Translate
  structure C = Codegen
  structure R = Registers
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

  fun printir ir =
    let
      fun stmstr (T.SEQ(list)) = "SEQ(" ^ stmlist list ^ ")"
        | stmstr (T.LABEL lab) = "LABEL " ^ lab
        | stmstr (T.JUMP (e,_)) =  "JUMP(" ^ expstr(e) ^ ")"
        | stmstr (T.CJUMP(r,a,b,t,f)) = "CJUMP(" ^ T.relstr(r) ^ "," ^ expstr(a) ^ "," ^ expstr(b) ^ "," ^ t ^ "," ^ f ^ ")"
        | stmstr (T.MOVE(a,b)) = "MOVE(" ^ expstr(a) ^ "," ^ expstr(b) ^ ")"
        | stmstr (T.EXP e) = "EXP(" ^ expstr(e) ^ ")"

      and expstr (T.BINOP(p,a,b)) = "BINOP(" ^ T.binstr p ^ "," ^ expstr(a) ^ "," ^ expstr(b) ^ ")"
        | expstr (T.MEM(e)) = "MEM(" ^ expstr(e) ^ ")"
        | expstr (T.TEMP t) = "TEMP " ^ Temp.makestring t
        | expstr (T.ESEQ(s,e)) = "ESEQ(" ^ stmstr(s) ^ "," ^ expstr(e) ^ ")"
        | expstr(T.NAME lab) = "NAME " ^ lab
        | expstr(T.CONST i) = "CONST " ^ Int.toString i
        | expstr(T.CALL(e,el)) = "CALL(" ^ expstr(e) ^ explist el ^ ")"

      and stmlist [] = ""
        | stmlist [l] = stmstr l
        | stmlist (l::list) = (stmstr l) ^ ", " ^ (stmlist list)

      and explist [] = ""
        | explist [l] = "," ^ expstr l
        | explist (l::list) = ", " ^ (expstr l) ^ (explist list)

      fun printproc (T.PROC{body, name, formals}) = print(name ^ ":\n" ^ stmstr body ^ "\n")
        | printproc _ = ()

      fun printvardec (T.VARDEC(stm)) = print(stmstr stm ^ "\n")
        | printvardec _ = ()

      fun printstring (T.STRING(label, string)) = print(label ^ ": " ^ string ^ "\n")
        | printstring _ = ()

    in
      (map printvardec ir; map printproc ir; map printstring ir)
    end

  fun generate filename venv ir =
    let
      val fd = TextIO.openOut (filename ^ ".s")
      fun output content = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)

      fun jmain () = [C.OPER{assem="j main\n", src=[], dst=[], jump=SOME(["main"])}]

      fun genvardec (T.VARDEC(stm)::list) = (C.codegen venv stm) @ (genvardec list)
        | genvardec (_::list) = genvardec list
        | genvardec [] = []

      fun procEntryExit (body, name, formals) =
        let
          val lab = [C.LABEL{assem=name ^ ":\n", lab=name}]
          val swfp = [C.OPER{assem="sw `s1, -4(`s0)\n", src=[R.SP, R.FP], dst=[], jump=NONE}] (* sw $ra, -4($sp) *)
          val swra = [C.OPER{assem="sw `s1, -8(`s0)\n", src=[R.SP, R.RA], dst=[], jump=NONE}] (* sw $ra, -8($sp) *)
          val fpsp = [C.OPER{assem="addi `d0, `s0, 0\n", src=[R.SP], dst=[R.FP], jump=NONE}] (* fp = current sp *)
          val spdown = [C.OPER{assem="addi `d0, `s0, -" ^ (Int.toString 8) ^ "\n", src=[R.SP], dst=[R.SP], jump=NONE}](* move sp by 2 words (sp,fp) *)
          (* NOTE: perform context swap (save all registers and move sp down when entering function/restore registers and move sp up when exiting function) *)
          val spup = [C.OPER{assem="addi `d0, `s0, " ^ (Int.toString 8) ^ "\n", src=[R.SP], dst=[R.SP], jump=NONE}] (* move sp by 2 words (sp,fp) *)
          val lwfp = [C.OPER{assem="lw `d0, -4(`s0)\n", src=[R.SP], dst=[R.FP], jump=NONE}] (* lw $ra, -4($sp) *)
          val lwra = [C.OPER{assem="lw `d0, -8(`s0)\n", src=[R.SP], dst=[R.RA], jump=NONE}] (* lw $ra, -8($sp) *)
        in
          lab @ swfp @ swra @ fpsp @ spdown @ (C.codegen venv body) @ spup @ lwfp @ lwra
        end

      fun genproc (T.PROC{body, name, formals}::list) = (procEntryExit (body, name, formals)) @ (genproc list)
        | genproc (_::list) = genproc list
        | genproc [] = []

      fun printstring (T.STRING(label, string)) = output (label ^ ": " ^ string ^ "\n")
        | printstring _ = ()

      fun printinstrs instr = output (C.format Temp.makestring instr)

      val instrs = genvardec(ir) @ jmain() @ genproc(ir)
      val _ = map printinstrs instrs
      val _ = map printstring (rev ir)
      val _ = TextIO.closeOut fd

    in
      instrs
    end

  fun compile filename =
    let
      val absyn = parse filename
      (*val _ = printabsyn absyn*)
      val (venv,ir) = Semant.transProg absyn
      (* val _ = printir ir *)
      val instrs = generate filename venv ir
    in
      ()
    end
end
